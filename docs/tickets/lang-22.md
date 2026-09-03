# LANG-22 · An operator's right operand may not be an `if`, a `case`, or a negation

**Sizing:** small. One production's right-hand side, plus whatever ambiguity that opens.

**Location:** `src/compiler/parser/grammar.lalrpop` — `InfixExpr`'s
`<lhs: AppExpr> <op: SpannedOp> <rhs: InfixExpr>` alternative, and `Expr`, which is where
prefix negation, `if` and `case` live.

**Decided ([`docs/spec/expressions.md`](../spec/expressions.md), *Prefix negation* and
*`if … then … else`*):** a `-` with no left operand available is negation, and directly after a
binary operator there is no left operand — so `a - -b` is `a` minus the negation of `b`. An
`if` or a `case` used as an operator's right operand extends as far to the right as it can, so
`1 + if c then a else b` is `1 + (if c then a else b)`.

**Problem:** an operator's right operand is an `InfixExpr`, and `InfixExpr` is a strict subset
of `Expr`: it reaches `AppExpr` and stops. The three alternatives `Expr` adds — prefix
negation, `if`, `case` — are therefore all unreachable on the right of an operator, while all
three are reachable on the left of one only by being the whole expression.

```zel
a - -b                    -- UnexpectedToken `-`
a + -b                    -- likewise
1 + if c then 2 else 3    -- UnexpectedToken `if`
1 + case v of …           -- UnexpectedToken `case`
```

Each has a parenthesised workaround (`a - (-b)`, `1 + (if …)`), except the `case` one, which
has none at all — see [LANG-21](lang-21.md).

The negation case is the one that reads as a defect rather than a missing feature, because
[Lexical structure](../spec/lexical-structure.md#prefix-negation) already states the rule the
grammar contradicts: a `-` with no left operand available is negation, with no exception for
"except directly after another operator".

Found while writing [`docs/spec/expressions.md`](../spec/expressions.md) (`SPEC-6`).

**Approach:** widen the right operand from `InfixExpr` to `Expr`. Expect LALRPOP to report
conflicts — `Expr`'s own `-` alternative against `Op`'s `-`, and the `if`/`case` forms
extending rightward past an operator that could have continued the chain — and note that the
wanted resolution is the greedy one in both cases: the `if` swallows everything to its right,
which is what the chapter specifies.

This interacts with [BUG-22](bug-22.md), which replaces the whole `InfixExpr` shape with a flat
node re-associated in canonicalization. If BUG-22 is picked up first, this ticket collapses
into deciding what that flat node's operands are; if this one goes first, the widened operand
carries over. Neither ordering is wrong, but doing them in the same change is easier than doing
them in either order.

**Acceptance:** the four spellings above parse (the `case` one only once
[LANG-21](lang-21.md) has landed too — say so if it has not), with tests in the parser's own
test module asserting the grouping. `cargo run` still prints `parsed 8 modules` and lists all
eight as checked. Two blocks in [`docs/spec/expressions.md`](../spec/expressions.md) go red —
the `a - -b` block in *Prefix negation* and the `1 + if …` block in *`if … then … else`*, both
tagged `expect=parse-error:UnexpectedToken` — and are retagged `expect=ok` with their
`**Known gap:**` paragraphs deleted.
