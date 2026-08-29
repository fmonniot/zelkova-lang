# BUG-10 · A `case` branch level with, or left of, the `case` keyword is accepted

**Severity:** low. The accepted shapes are unusual to write and are rejected by nothing else,
so no correct program depends on the current behaviour; nothing miscompiles. What it costs is
that a rule the language states is not enforced, and the file that violates it parses.

**Location:** `src/compiler/parser/layout.rs` — `Layout::handle_next_token`: the step-1
`(Token::Of, Context::CaseExpression)` arm, and the step-3 `(Token::Of, _)` arm that pushes
`Context::CaseBlock(None)`.

**Found:** during review of the PR that landed `docs/spec/layout.md`'s *The first branch fixes
the column for all of them* section. The section asserted a relationship to the `case` keyword's
column that the compiler does not implement; the language owner confirmed the rule and that the
gap is a compiler bug rather than a spec correction.

**Decided (by the language owner):** the column the first branch establishes must be strictly
deeper than the column of the `case` keyword itself. A branch level with `case`, or left of it,
is invalid Zelkova.

**Problem:** the `case` keyword's column never enters the computation of the branch block's
minimum indent.

Step 1 sees the `of` while `Context::CaseExpression` is on top of the stack and pops that
context, so that the scrutinee's block is closed. The token is then reprocessed. By the time
step 3's `(Token::Of, _)` arm runs and pushes the branch block, `offside` is the *enclosing*
context — for a top-level function body, `TopLevelDeclaration` at indent 1 — and the new block
is opened with `indent: offside.indent + 1`, i.e. 2. The floor is therefore "deeper than the
enclosing block", not "deeper than `case`". For every example in `docs/spec/layout.md` those two
happen to coincide, because `case` sits on column 3 in a top-level body, which is why this went
unnoticed.

Both of these parse today, and both are invalid under the rule above:

```
describe f =
  case f of
  On -> 1
  Off -> 0
```

```
describe f =
    case f of
  On -> 1
  Off -> 0
```

The second one is the worse of the two: the branches are two columns *left* of the `case` they
belong to, which reads as if they belonged to something enclosing it.

**Approach:** the branch block needs the `case` keyword's column, and step 3 no longer has it
because step 1 popped the context that carried it. Two ways, and this ticket does not pick:

1. **Carry the column through `Context::CaseExpression`.** Give the variant a payload — the
   column of its `case` keyword — and have step 1's `Of` arm hand it to the `CaseBlock` that
   step 3 opens, so the block's `indent` is derived from `case` rather than from whatever
   context happens to be on top. This is the change that makes the two arms stop talking past
   each other, and it is where the information actually is.
2. **Check it when the block's column is set.** The first token after the block opens is what
   fixes `CaseBlock(Some(col))` (step 1's `(_, Context::CaseBlock(c @ None))` arm). If the
   `case` column were reachable there, that arm could raise `LayoutError` directly and get the
   existing "the branches of a `case … of`" phrasing for free. It still needs the column
   carried from somewhere, so it is a variation on (1) rather than an alternative to it.

Either way the error should be the existing `Error::LayoutError` path, so the message names the
block that was violated the way the too-shallow-branch case already does.

**Acceptance:** a test under `tests/compiler/parser/` feeding each of the two examples above
through `parser::parse` and asserting the resulting error is a layout error whose primary label
sits on the offending branch's first token. Assert on the variant and on `labels[..].range`, not
on `is_err()` — `NodeSpan`'s `PartialEq` always returns `true`, so a whole-value comparison
proves nothing about position (`CLAUDE.md`, *Standing invariants*).

The three legal shapes must keep working: branches one level deeper than `case`, a nested
`case … of` in a branch body, and a multi-line scrutinee (`docs/spec/layout.md` has a block for
each). `cargo run` must still print `parsed 8 modules` and exit 0.

Once this lands, `docs/spec/layout.md`'s *The first branch fixes the column for all of them*
section goes red: its last block is tagged `expect=ok` to record today's behaviour and must be
retagged to a `parse-error`, and the paragraph naming this ticket comes out.
