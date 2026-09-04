# ERR-11 · A `case` branch indented deeper than its siblings is absorbed, and the error names the wrong token

**Sizing:** small-to-medium. The detection point is a single new arm in `handle_next_token`'s
step-3 match; what makes it medium is that the obvious arm rests on an assumption that expires
— see *Approach*.

**Location:** `src/compiler/parser/layout.rs` — `Layout::handle_next_token`, the step-3
`match (&token.value, &offside.context)` and its `_ =>` fallthrough; `Context::CaseBlock`,
`Context::CaseBranch`, `Offside::min_indent`.

**Found:** while writing `docs/spec/layout.md` under `SPEC-1` (closed — see
[README.md](README.md)), by running snippets through `parser::parse`. Left unfixed there because
a spec change and a semantics change in one diff is unreviewable; see
`docs/spec/conventions.md`'s *A spec change and a semantics change do not share a diff*.

**Problem:** the first branch of a `case … of` fixes the column for every branch in that block
(`CaseBlock(Some(col))`, read back by `Offside::min_indent`). A later branch **left** of that
column is diagnosed well: step 2 closes the block if the token is at or left of the context's
own indent, and otherwise step 3 raises `LayoutError`, whose message reaches for
`Context::description()` and says which block was violated.

A branch **right** of that column has no rule at all. It falls through step 3's `_ =>` arm and
is emitted as an ordinary token, so it is absorbed into the previous branch's body:

```
f m =
  case m of
    A -> 1
      B -> 2
```

Layout reads that as `A -> 1 B`, and the parser then trips on the `->`:

```
UnexpectedToken { token: Spanned { .. value: Arrow }, expected: ["lo_ident", "up_ident",
  "integer", "float", "char", "true", "false", "op", "close block", "(", ".", "-",
  "left", "right", "non"] }
```

The caret lands on the `->` of the *second* branch and the message offers fifteen tokens that
would have been acceptable there, none of which explains that the real mistake was two columns
of indentation on the line before. The user wrote a branch; they are told they wrote a bad
expression.

The input is correctly **rejected** — `->` is not valid in expression position, so an absorbed
branch always fails somewhere rather than silently parsing as something else. This ticket is
therefore about the diagnostic, not about a wrong parse.

**Decided (SPEC-1, by the language owner):** all branches of one `case … of` start on the same
column, the one the first branch established. A line deeper than that which begins a new branch
is an error, not a continuation.

**Approach — a real choice, this ticket does not pick:**

Layout cannot reject every deeper line. A deeper line that continues the previous branch's
expression is legal and common — `std/core/src/Maybe.zel` relies on it throughout, and
`case … of` bodies spanning several lines are the normal shape. Layout sees tokens, not
grammar, so "new branch" and "continuation of the previous expression" look alike to it.

1. **Catch it at the `->`.** Add an arm for `(Token::Arrow, Context::CaseBranch)` to step 3's
   match: an arrow seen while inside a branch body can only be a misindented branch, because
   there is no other construct in the language that puts an `->` in expression position. Cheap
   and precise, and it can name the column the block expects.

   **This assumption expires.** `CLAUDE.md`'s *Language notes* lists lambdas as not implemented;
   the moment `\x -> x` exists, an `->` inside a branch body is ordinary. Whoever takes this
   route should say so at the arm, so that whoever adds lambdas finds the note rather than the
   bug.

2. **Leave it to the grammar and improve the message there.** No expiring assumption, but
   `parser::Error::UnexpectedToken` would need to learn enough layout context to say "this
   looks like a branch indented too far", which is knowledge the grammar does not have and
   arguably should not grow.

**Acceptance:** a test — `tests/compiler/parser/` is where the layout-level ones live — that
feeds the four-line example above through `parser::parse` and asserts on the resulting error:
its message names the misindentation, and its primary label sits on the `B` (or on the line's
leading whitespace), not on the `->` two tokens later. Assert on the variant and on
`labels[..].range`, not on `is_err()` — `NodeSpan`'s `PartialEq` always returns `true`, so a
whole-value comparison proves nothing about position (`CLAUDE.md`, *Standing invariants*).

The shallow-branch case must keep its current behaviour: a sibling branch left of the block's
column still raises the existing `LayoutError`. Pin it with a second test so this change cannot
swallow it.

Once this lands, `docs/spec/layout.md` should keep its `expect=parse-error` block for this
shape — the rule is unchanged and the block stays green either way — and gain a sentence
naming the diagnostic the reader will actually see.
