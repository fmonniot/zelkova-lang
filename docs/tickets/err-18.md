# ERR-18 · An unexpected-token error names the token by its Rust variant, not as the user wrote it

**Sizing:** small-to-medium. One `Display` for `Token` and one changed line in `Error::diagnostic`
are small; what makes it medium is the number of variants (see *Approach*) and that the message
is quoted by tickets and may be pinned by tests that have to move with it.

**Location:** `crates/zelkova-syntax/src/parser/error.rs` — `Error::diagnostic`'s
`Error::UnexpectedToken` arm, whose message is `format!("unexpected token: `{:?}`", token.value)`
and carries a `// TODO display instead of debug` comment;
`crates/zelkova-syntax/src/parser/tokenizer.rs` — `Token`, which derives `Debug` and implements no
`Display`.

**Found:** while reviewing `LANG-47`, which made `{` and `}` tokens and so routed every brace
through this arm. Left unfixed there because the change is to every parse error, not to the
tokenizer's brace handling, and `LANG-47` is a one-file ticket.

**Problem:** `CLAUDE.md`'s *Standing invariants* say a phase error's message is written in the
vocabulary of the user's source and that a `Debug` dump is not an option, because it names Rust
types rather than source constructs. This arm is the one place a parser error does exactly that.
Before `LANG-47` the cost was small, since few tokens reached the grammar as strays. A brace now
does:

```
$ cargo run -- compile <package whose Main.zel is `f = { a = 1 }`>
error: unexpected token: `LBrace`
  ┌─ scratch:src/Main.zel:3:5
  │
3 │ f = { a = 1 }
  │     ^ unexpected token
  │
  = we were expecting one of the following tokens: ["lo_ident", "up_ident", "integer", ...]
```

and for a stray close brace, `f = 1 }`, ``unexpected token: `RBrace` ``. The same arm prints
``unexpected token: `LowerIdentifier("alias")` `` for `type alias Pair = ...` (quoted in
[LANG-86](lang-86.md)) and, per [LANG-71](lang-71.md), `Comma` for a stray comma. `LBrace`,
`RBrace`, `Comma` and `LowerIdentifier("alias")` are names in the compiler's source; the user
wrote `{`, `}`, `,` and `alias`. `LANG-48` did not remove the need: the trailing-comma form
`f = { a = 1, }` in `docs/spec/records.md` is rejected at an `RBrace`, which reaches this arm.

**Approach:** implement `std::fmt::Display` for `Token`, so each variant prints as its source
spelling, and use it in the message with the backticks the message already has. `Token` has 45
variants, so the work is one `match` and a test that pins every variant's spelling
(an exhaustive `match`, with no `_` arm, keeps a new variant from being added without one).
Choices this ticket does not make:

1. **Payload-carrying tokens.** `LowerIdentifier("alias")` can print as `alias`; `Integer`,
   `Float`, `Char` and `String` can print the value or the literal as written, and the tokenizer
   keeps only the value (a `Float` or an escaped string does not round-trip to its source text).
   Printing the value, or naming the kind (`an integer literal`), are both defensible.
2. **Layout tokens.** `OpenBlock` and `CloseBlock`, the two the layout pass injects, have no
   source spelling. They need a description (`the end of the block`) rather than a spelling.
3. **The `expected` note.** It is built from `Debug` of `Vec<String>` and shows the grammar's
   terminal names, quoted, in `lalrpop`'s vocabulary (`"lo_ident"`, `"close block"`). That is the
   same defect on the other half of the message. Whether it is in scope here or its own ticket is
   open; `Error::UnexpectedEOF` joins the same list unquoted with `unquote_tokens`, so the two
   arms already disagree.

**Acceptance:** `cargo test --workspace` passes, and:

- `crates/zelkova-syntax/tests/parser/expressions.rs` — the test for `f = { a = 1 }` asserts the
  message reads ``unexpected token: `{` `` and not `LBrace`; a second test does the same for a
  stray `}`;
- a test in `error.rs` or `tokenizer.rs` that matches `Token` exhaustively and fails to compile if
  a variant has no spelling;
- `cargo run -- compile` on the package from *Problem* prints the user's `{`;
- the two tickets that quote the old form ([LANG-86](lang-86.md), [LANG-71](lang-71.md)) are
  updated to the new message in the same diff, where they still quote it.
