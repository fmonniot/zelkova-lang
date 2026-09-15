# BUG-27 · A canonicalized infix operator is qualified under its own symbol, not the function its `infix` declaration names

**Severity:** medium (wrong behaviour under normal use — canonicalization succeeds and produces a
`QualName` that matches no top-level binding, so the failure surfaces later, during type
checking, naming the operator symbol rather than the identifier that is actually unresolved).

**Location (as filed):** `src/compiler/canonical/mod.rs` — `Expression::from_parser`'s
`parser::ExpressionKind::Variable(name)` arm, the `ValueType::TopLevel` branch:
`ExpressionKind::VarTopLevel(env.module_name().qualify_name(name))`.
`src/compiler/canonical/environment.rs` — `RootEnvironment::find_value`, whose redirect through
`self.infixes` to `Infix::function_name` was the source of the mismatch.

**Problem (as filed):** `a + b` is desugared in the grammar (`grammar.lalrpop`'s `InfixExpr`)
into `((+) a) b`, with `+` an ordinary `ExpressionKind::Variable("+")`. Canonicalization resolved
that variable through `find_value`, which redirected internally — it looked `add` up in
`self.variables` when handed `+` — but returned only a `ValueType`, not the name that redirect
landed on. The caller, `Expression::from_parser`'s `Variable` arm, still had the *original*
`name` (`+`) in scope, and qualified with that: `ExpressionKind::VarTopLevel(env.module_name()
.qualify_name(name))` built `Test.+`, not `Test.add`. `Test.+` is not a key in any module's
value table, so the typer's `canonical_expr_to_term` would turn it into
`TermKind::Identifier("+")`, which cannot resolve against an environment keyed by `"add"`.

This was `find_value`'s redirect doing the *opposite* of what BUG-15 (closed) was about. BUG-15
was the case where the redirect target itself isn't in scope, and its `VariableNotFound` named
the wrong symbol because the error was built from the pre-redirect name. This ticket was the
case where the redirect *succeeds* and the qualified name built from that success still reverted
to the pre-redirect symbol. Both shared the same root: `find_value` discarded which name it
actually matched.

**Overtaken by BUG-15's fix (#207).** BUG-15's PR removed the redirect from `find_value` entirely
— `RootEnvironment::find_value` is now a plain `self.variables.get(name)` — and moved operator
resolution out of the `Variable` arm altogether. `resolve_infix_operator`
(`src/compiler/canonical/mod.rs`) now resolves each operator in an `InfixChain` directly against
its `InfixEntry` (`src/compiler/canonical/environment.rs`), matching on `InfixFunction` and
building `ExpressionKind::VarTopLevel(env.module_name().qualify_name(function_name))` (the
`Local` arm) or `VarForeign(module.qualify_name(function_name), tpe)` (the `Imported` arm) —
`function_name`, not the operator symbol, in both cases. It no longer constructs a synthetic
`Variable(name)` and hands it to `Expression::from_parser`, so the `Variable` arm's
`ValueType::TopLevel` branch — this ticket's other named location — is no longer on an
operator's resolution path at all: with the redirect gone, `find_value` applied to an operator
symbol like `"+"` simply misses (no key `"+"` in `self.variables`) rather than silently
succeeding under the wrong name. The **Not reachable today** situation (no syntax to write a
bare operator as a `Variable`, per [LANG-23](lang-23.md)) is unchanged, but the mechanism this
ticket described as "still live code, not dead code" — `resolve_infix_operator` routing through
the `Variable` arm — has been deleted, not merely left unreached.

The mismatch this ticket reproduces no longer has a path to occur: `tests/compiler/canonical.rs`
now declares `infix left 6 (+) = add` (`ADD_SUB_MUL`, matching `Basics.zel`'s own spelling) and
asserts the operator position of `a + b` canonicalizes to `VarTopLevel` qualified to `add`, not
`+` (e.g. `higher_precedence_groups_first`, via `infix_shape`'s
`VarTopLevel(q) => q.unqualified_name()` check) — exactly this ticket's **Acceptance** clause,
satisfied as a side effect of BUG-15/BUG-22's work rather than a change made for this ticket.

Whether that means BUG-27 should be closed is a call for whoever reviews BUG-15's PR, not this
rewrite: the fix landed incidentally, was never verified against this ticket's specific
reproduction before now, and no one has confirmed there is no other path (e.g. through
`ScopedEnvironment::find_value`, which does not redirect and was never audited under this ticket)
that could still reach the old defect. Left **open** until a maintainer confirms and closes it.

**Fix:** none needed beyond what BUG-15 already did, on present evidence — see above. If a
maintainer finds a remaining path where an operator resolves through `find_value`'s `TopLevel`
branch and qualifies under its own symbol rather than `function_name`, reopen investigation
here; otherwise this ticket should close as resolved-by-BUG-15 rather than by its own patch.

**Acceptance:** a `tests/compiler/canonical.rs` test with an infix declaration whose symbol and
`function_name` differ (`infix left 6 (+) = add`, matching `Basics.zel`'s own spelling) and an
expression using the operator, asserting the canonicalized `Apply` tree's operator position is
`VarTopLevel` qualified to the declaring module's `add`, not its `+`. `cargo run` still prints
`parsed 8 modules` and lists all eight as checked. **Already satisfied** by
`tests/compiler/canonical.rs`'s `ADD_SUB_MUL`-based tests as of BUG-15's PR (#207) — see above.
