# BUG-27 · A canonicalized infix operator is qualified under its own symbol, not the function its `infix` declaration names

**Severity:** medium (wrong behaviour under normal use — canonicalization succeeds and produces a
`QualName` that matches no top-level binding, so the failure surfaces later, during type
checking, naming the operator symbol rather than the identifier that is actually unresolved).

**Location:** `src/compiler/canonical/mod.rs` — `Expression::from_parser`'s
`parser::ExpressionKind::Variable(name)` arm, the `ValueType::TopLevel` branch:
`ExpressionKind::VarTopLevel(env.module_name().qualify_name(name))`.
`src/compiler/canonical/environment.rs` — `RootEnvironment::find_value`, whose redirect through
`self.infixes` to `Infix::function_name` is the source of the mismatch.

**Problem:** `a + b` is desugared in the grammar (`grammar.lalrpop`'s `InfixExpr`) into
`((+) a) b`, with `+` an ordinary `ExpressionKind::Variable("+")`. Canonicalization resolves
that variable through `find_value`:

```rust
fn find_value(&self, name: &Name) -> Option<&ValueType> {
    // TODO Not a principled change. Will require a bit more thought :)
    let name = if let Some(infix) = self.infixes.get(name) {
        &infix.function_name
    } else {
        name
    };
    self.variables.get(name)
}
```

`find_value` redirects internally — it looks `add` up in `self.variables` when handed `+` — but
it returns only a `ValueType`, not the name that redirect landed on. The caller,
`Expression::from_parser`'s `Variable` arm, still has the *original* `name` (`+`) in scope, and
that is what it qualifies:

```rust
ValueType::TopLevel => {
    ExpressionKind::VarTopLevel(env.module_name().qualify_name(name))
}
```

So `find_value("+")` succeeds — it finds `add`'s `ValueType::TopLevel` — but the `VarTopLevel`
built from it is `Test.+`, not `Test.add`. `Test.+` is not a key in any module's value table;
nothing ever inserts a top-level value under an operator's own symbol
(`RootEnvironment::insert_top_level_value` is driven by declared function names, and an
`infix` declaration inserts into `self.infixes`, never into `self.variables`). Downstream, the
typer's `canonical_expr_to_term` turns `VarTopLevel` into `TermKind::Identifier(qname.to_name()
.as_str())` — `"+"` — which will not resolve against an environment keyed by `"add"`.

This is `find_value`'s redirect doing the *opposite* of what [BUG-15](bug-15.md) is about.
BUG-15 is the case where the redirect target itself isn't in scope (an operator imported without
its backing function), and its `VariableNotFound` names the wrong symbol because the error is
built from the pre-redirect name. This ticket is the case where the redirect *succeeds* — the
function is in scope — and the qualified name built from that success still reverts to the
pre-redirect symbol. Both bugs share the same root (`find_value` discards which name it actually
matched), so a fix that changes how the redirect surfaces its result is worth checking against
both tickets at once, but they are independent defects with independent reproductions.

**Not reachable today.** None of the eight modules `cargo run` compiles (`Basics.zel`,
`Bitwise.zel`, `Js/Basics.zel`, `Js/Bitwise.zel`, `Js/Utils.zel`, `Maybe.zel`, `Result.zel`,
`Tuple.zel`) writes an infix expression whose operator and function name differ — `add`'s body
is `Js.Basics.add`, not a use of `(+)`, and [LANG-23](lang-23.md) means there is no syntax yet to
name an operator as a value directly. So the smoke test in `CLAUDE.md` (`parsed 8 modules`, all
eight checked) is unaffected, and this has no red test today.

It is still live code, not dead code: [BUG-22](bug-22.md)'s fix (re-associating an infix chain by
precedence in canonicalization) resolves each operator in the chain through
`resolve_infix_operator`, which builds a synthetic `parser::Expression::new(span,
ExpressionKind::Variable(name.clone()))` and hands it to `Expression::from_parser` specifically
*so that* an operator resolves the same way a written `Variable` would — meaning it goes through
this exact arm and inherits this exact defect. The first program that writes `a + b` with `a`,
`b` well-typed values will canonicalize to a `QualName` type checking cannot resolve.

**Fix:** `find_value` needs to surface the name it actually matched, not just the `ValueType` at
that name — either by returning the resolved `Name` alongside the `ValueType`, or by giving
`Environment` a way to ask "what does this operator's `infix` declaration name resolve to"
before calling `find_value` at all, so the `Variable` arm (and `resolve_infix_operator`, once
[BUG-22](bug-22.md) lands) qualifies using the redirected name in the `TopLevel` case. Check
whether the same discarding affects the `Local` and `Foreign`/`Foreigns` branches of the match —
`ValueType::Local => ExpressionKind::VarLocal(name.clone())` has the identical shape, though an
operator redirect through `RootEnvironment` is unlikely to ever return `Local` in practice, since
`ScopedEnvironment::find_value` does not redirect.

**Acceptance:** a `tests/compiler/canonical.rs` test with an infix declaration whose symbol and
`function_name` differ (`infix left 6 (+) = add`, matching `Basics.zel`'s own spelling) and an
expression using the operator, asserting the canonicalized `Apply` tree's operator position is
`VarTopLevel` qualified to the declaring module's `add`, not its `+`. `cargo run` still prints
`parsed 8 modules` and lists all eight as checked.
