# ERR-12 · Leading indentation before `module` is rejected only by accident, and the caret lands on an unrelated line

**Sizing:** small. One check at a known point, plus the constant cleanup in *Approach*.

**Location:** `src/compiler/parser/layout.rs` — `Layout::handle_next_token`, both the
context-stack bootstrap (`Offside { context: Context::TopLevelDeclaration, indent: start.column, .. }`,
which appears twice) and the step-3 `_ =>` arm testing `token.span.start.column == 1`.

**Found:** while grounding [SPEC-1](spec-1.md)'s Layout chapter. Left unfixed there for the
same reason as [ERR-11](err-11.md).

**Decided (SPEC-1, by the language owner):** a source file's first token is at column 1. Any
space or tab before the `module` keyword is invalid. This ticket is not asking for indented
files to be *supported* — it is asking for them to be refused on purpose and with a usable
message.

**Problem:** two places disagree about where the top level is. The bootstrap takes
`TopLevelDeclaration`'s minimum indent from **whatever column the first token happened to be
on**; the step-3 arm that closes a top-level declaration when a new one begins tests the
**literal** `column == 1`. For any file starting at column 1 they agree and nothing is wrong.
For a file starting anywhere else, the context is opened at that column and then never
implicitly closed, because no token ever satisfies `column == 1`.

The observable result, for a file indented by two spaces throughout — legal as far as the
tokenizer is concerned, since `handle_indentation` only requires the leading run to be an even
number of spaces:

```
  module P exposing (f)

  f x =
    1
```

```
UnexpectedToken { token: Spanned { .. value: LowerIdentifier("f") }, expected: ["close block"] }
```

The caret sits on `f`, on the third line — the second declaration, which is not the problem —
and the one token the parser says it wanted, `close block`, is an internal artifact of the
layout pass that does not appear anywhere in the user's source and that they have no way to
write. Nothing in that sentence points at the two spaces on line 1.

**Approach:** report the rule directly rather than letting it fall out of an accident. Two
layers could hold the check and this ticket does not pick:

1. **Tokenizer** — `Tokenizer::handle_indentation` already owns "how much leading whitespace is
   on this line" and already raises `IndentationError` and `TabError` from there. A first-token
   check is a natural neighbour, and it would catch a leading *tab* with the same message
   rather than the current `TabError`, which may or may not be the wanted behaviour — decide it.
2. **Layout** — `handle_next_token`'s bootstrap is the exact point that currently reads the
   column and is already the only code that cares about where the top level sits.

Whichever layer takes it, **fold in the constant**: once the rule is explicit, the bootstrap's
`indent: start.column` should become `1`, so the two sites stop disagreeing and the
`column == 1` literal in step 3 stops being a latent contradiction. Leaving the bootstrap
reading `start.column` after the check exists would preserve the bug's shape with nothing left
to trigger it — the next reader has to re-derive that it is unreachable.

**Acceptance:** a test asserting that the four-line example above is rejected with an error
whose message names the leading whitespace on the first line, and whose primary label is on
line 1 — not on the `f` on line 3. Assert on the variant and on `labels[..].range` rather than
`is_err()`; `NodeSpan`'s `PartialEq` always returns `true`, so a whole-value assertion proves
nothing about position.

`cargo run` must still print `parsed 8 modules` and exit 0 — every module under
`std/core/src/` already starts at column 1, so this rule costs the stdlib nothing.

Once this lands, `docs/spec/layout.md` keeps its `expect=parse-error` block for the rule (which
stays green across the fix) and gains a sentence naming the message the reader will see.
