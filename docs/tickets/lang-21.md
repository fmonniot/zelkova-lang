# LANG-21 · A `case … of` cannot be parenthesised, so it is not an expression

**Sizing:** medium. A grammar change tangled with the layout pass — the parenthesised form
fails because the branch block is still open when the `)` arrives, not because a production is
missing.

**Location:** `src/compiler/parser/grammar.lalrpop` — `AtomicExpr`, which has a
`"(" <e: Expr> ")"` alternative, and the `case` alternative of `Expr`, which ends by consuming
a `"close block"`. `src/compiler/parser/layout.rs` — the `CaseBlock` context and what closes
it.

**Decided ([`docs/spec/expressions.md`](../spec/expressions.md), *`case … of`*):** a `case` is
an expression like any other. It may be parenthesised, and so may appear as a function
argument, a tuple element, an operator operand, or another `case`'s scrutinee.

**Problem:** `AtomicExpr`'s grouping alternative admits any `Expr`, and `Expr` includes the
`case` form, so the grammar looks as though `(case v of …)` should parse. It does not, in any
position:

```zel
f g v =
  g (case v of
    On ->
      Off)
```

reports *UnexpectedToken `)`, expected `close block`*. The `case` production ends with a
`"close block"` that the layout pass emits at the point the branch block ends — which is on a
following line, at a shallower column. A `)` on the last branch's own line arrives while that
block is still open, so the grouping alternative can never close.

The one-line spelling `g (case v of\n  On -> Off)` fails identically, as does `(case v of …)`
standing alone as a declaration body, as does a `case` as a tuple element.

So a `case` is usable in exactly three positions today — a whole declaration body, a whole `if`
arm, or a whole branch body — and nowhere else. Together with [LANG-22](lang-22.md), which
keeps it out of an operator's right operand, that is every position an expression has.

Found while writing [`docs/spec/expressions.md`](../spec/expressions.md) (`SPEC-6`).

**Approach:** the fix is in the layout pass rather than the grammar: a `)` (and a `,`) at a
column deeper than the enclosing `CaseBlock`'s must close that block before the token is
emitted, the way a shallower line already does. That is the same class of question as
[BUG-23](bug-23.md) — which token closes an open block — and the two are worth reading
together, though neither is a prerequisite for the other.

Check the span arithmetic while there. The `case` production takes its end from the branches
rather than from an `@R`, with a long comment explaining that an `@R` past `CaseBranch+` would
be the end of the *layout* token; whatever closes the block early must keep that reasoning
true.

**Acceptance:** the three spellings above parse, with tests in the parser's own test module.
`cargo run` still prints `parsed 8 modules` and lists all eight as checked. The
`expect=parse-error:UnexpectedToken` block in
[`docs/spec/expressions.md`](../spec/expressions.md)'s *`case … of`* section goes red — that
pin's whole job — and is retagged `expect=ok` with its `**Known gap:**` paragraph deleted.
