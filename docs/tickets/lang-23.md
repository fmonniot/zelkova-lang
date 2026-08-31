# LANG-23 · An operator cannot be named in an expression, so an exported one is unusable as a value

**Sizing:** small. One `AtomicExpr` alternative; the spelling already exists in two other
positions.

**Location:** `src/compiler/parser/grammar.lalrpop` — `AtomicExpr`, and the `Op` production it
would reach; `Exposed` and `Infix`, which already spell `"(" <op:Op> ")"`.

**Decided ([`docs/spec/expressions.md`](../spec/expressions.md), *Naming an operator*):** an
operator wrapped in parentheses is an expression denoting the function it is bound to. It is
the same spelling an `infix` declaration and an `exposing` list use.

**Problem:** `(+)` in expression position is a parse error. `AtomicExpr`'s parenthesised
alternatives take an `Expr`, and an operator on its own is not one, so the `+` is reported as
an unexpected token.

An operator is therefore usable infix and in no other way. It cannot be passed to a
higher-order function, partially applied, or bound to a name — and a module writing
`exposing ((+))` exports something that no expression in an importing module can mention. The
export is not useless, since the operator can still be used infix, but the asymmetry is
invisible from the `exposing` list, which spells the operator exactly the way an expression
would have to.

`std/core/src/Basics.zel` exports nineteen operators this way.

Found while writing [`docs/spec/expressions.md`](../spec/expressions.md) (`SPEC-6`).

**Approach:** add `"(" <Op> ")"` to `AtomicExpr`, building an `ExpressionKind::Variable` with
the operator's name — which is exactly what `InfixExpr`'s rewrite already invents for the
operator position, so canonicalization resolves it through the same path and needs no change.
Take the span across the parentheses, not just the operator: `(+)` is what the user wrote and
`VariableNotFound`'s caret should cover it.

Watch for an ambiguity against the existing `"(" <Expr> ")"` grouping, since `-` is both an
`Op` and the head of `Expr`'s negation alternative: `(-)` is the subtraction function and
`(-x)` is a negation, and LALRPOP needs one token of lookahead past the `-` to tell them
apart.

Note that [BUG-15](bug-15.md) — an imported operator is unresolvable unless the function
behind it is also in scope — sits on the resolution path this reuses. It is not a prerequisite,
but a test of this ticket that imports its operator will trip over it.

**Acceptance:** `plus = (+)` and `two = (+) 1 1` parse and canonicalize, with a test in
`tests/compiler/canonical.rs` asserting the resolved name. `cargo run` still prints
`parsed 8 modules` and lists all eight as checked. The `expect=unimplemented` block in
[`docs/spec/expressions.md`](../spec/expressions.md)'s *Naming an operator* section goes red —
that tag's whole job — and is retagged `expect=ok` with its `**Not implemented:**` paragraph
deleted.
