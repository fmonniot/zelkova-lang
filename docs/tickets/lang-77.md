# LANG-77 · String literals are specified but not tokenized

**Sizing:** medium. A single-line form is a bounded extension of the existing character-literal
path; the multi-line `"""..."""` form and its escape handling are what could make this larger,
and the ticket does not require both to land in the same PR — see Approach.

**Part of:** no active program; filed from [`LANG-73`](README.md)'s work, which gave `String`
its declaration in `std/core` but explicitly left literals out of scope.

**Location:** `src/compiler/parser/tokenizer.rs` — `Token::Char`'s single-quote path (around the
`'\''` match arm) is the shape a string token follows; there is no `Token::String` or
`'"'` match arm today, so a bare `"` is read as whatever the tokenizer does with an unrecognised
character. `grammar.lalrpop`, the `parser` AST and `canonical/mod.rs`'s `from_parser*`
conversions all need the new token threaded through, per `CLAUDE.md`'s standing invariant that
a grammar change is never a one-file change. `docs/spec/lexical-structure.md`'s *Strings*
section (`## Strings`, docs/spec/lexical-structure.md:418) already specifies both forms with
`expect=unimplemented` examples — those blocks pin today's rejection and go red the moment
either form starts compiling.

**Problem:** [Strings](../spec/lexical-structure.md#strings) specifies a single-line form
(`"hello"`) and a multi-line form (`"""..."""`) using the same escapes as character literals,
and says a string may not contain an unescaped line ending. Neither is implemented: the
tokenizer has no token for `"`, so `"hello"` does not parse. `std/core/src/String.zel`
([`LANG-73`](README.md)) declares the opaque `String` type a literal would produce, but nothing
in the language writes one — the only way a `String` value reaches a program today is from a
JavaScript facade wrapper. `CLAUDE.md`'s Language notes section lists string literals among the
constructs not implemented.

**Approach:**

1. Tokenize the single-line form first. Follow `Token::Char`'s existing pattern in
   `tokenizer.rs`: recognise `"`, consume characters (and the same escape sequences character
   literals use, whatever `CharNotClosedError`'s sibling ends up being called for strings) until
   the closing `"`, and reject an unescaped line ending inside it the way the spec requires.
   Emit a new `Token::String { value: String }`.
2. Decide whether the multi-line `"""..."""` form lands in the same PR or a follow-up. It needs
   its own opening/closing sequence and permits embedded line endings and unescaped `"`, which
   the single-line lexer must not accept — that is a second, separably-testable path through the
   tokenizer, not a variant of the first. Say which was chosen.
3. Thread the new token through the grammar (`grammar.lalrpop`), the `parser` AST, and
   `canonical/mod.rs`'s conversions, in the same commit as the tokenizer change, per `CLAUDE.md`'s
   standing invariant. Decide what canonical/IR shape a string literal takes — this ticket does
   not pick one, since it depends on how the typer and `ir::build` already represent `String`
   values coming back from a facade wrapper, and getting that wrong is exactly the kind of
   decision `work-ticket`'s escalation contract exists for.
4. Update `docs/spec/lexical-structure.md`'s two `expect=unimplemented` examples (and their
   **Not implemented:** paragraph) to `expect=ok` for whichever form(s) this ticket lands, per
   `docs/spec/conventions.md`. If only the single-line form lands, the multi-line example and its
   own **Not implemented:** note stay as they are.

**Tests:** a `tests/typer.rs` or `tests/compiler/canonical.rs` case (whichever layer first gains
a representation) asserting a source string like `s = "hello"` checks with type `String`. A
tokenizer-level test asserting `"hello` (unclosed) produces the tokenizer's own not-closed error,
mirroring the existing `CharNotClosedError` tests. Mutation-check each: reverting the token
recognition should turn these red, not merely error differently.

**Acceptance:** the single-line example in [Strings](../spec/lexical-structure.md#strings)
changes from `expect=unimplemented` to `expect=ok` and `cargo test --test spec` stays green. A
module declaring `greeting = "hello"` with no annotation infers `String`. `cargo test --workspace`
is green.
