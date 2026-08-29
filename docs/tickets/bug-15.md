# BUG-15 · An imported operator is unresolvable unless the function behind it is also in scope

**Severity:** medium (wrong behaviour under normal use — the documented way to import an
operator does not work, and the error names a symbol that is not the missing one).

**Location:** `src/compiler/canonical/environment.rs` — `RootEnvironment`'s `find_value`
(the redirect through `self.infixes` to `Infix::function_name`), and the
`parser::ExposedKind::Operator` arm of `process_import`, which registers the infix without
registering anything for it to resolve to.

**Problem:** an operator expression is desugared in the grammar, not in canonicalization:
`a + b` becomes `((+) a) b`, with `+` an ordinary `ExpressionKind::Variable`
(`grammar.lalrpop`, `InfixExpr`). Canonicalization then resolves that variable through
`find_value`, which redirects an operator name to the function its `infix` declaration
names and looks *that* up in `variables`:

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

`process_import`'s `Operator` arm inserts the `Infix` into `env.infixes` and nothing into
`env.variables`. So importing an operator by name leaves the redirect pointing at a name
that is not in scope:

```zel
module Main exposing (x)

import Widget exposing (Size, one, (+))

x : Size
x = one + one      -- cannot find a value named `+`
```

Two things are wrong with that. The operator cannot be used at all, which is the defect;
and the error names `+`, while the name that actually failed to resolve is `add` — the
`VariableNotFound` is built from the original name before the redirect, so it points at a
symbol that *is* in scope and says it is not.

`import Widget exposing (..)` works, by accident: the `Exposing::Open` arm inserts every
value of the interface unqualified, so the backing function happens to be there. That is
what `std/core` relies on, which is why nothing has hit this.

An operator has no qualified spelling — `Widget.(+)` is not writable — so an operator entry
in an `exposing` list is the *only* way to use one across a module boundary. Whether the
backing function is in scope is not something the user chose or can see.

Found while writing [`docs/spec/modules.md`](../spec/modules.md) (`SPEC-3`), whose
*Operators* section carries the `**Known gap:**` block.

**Approach:** the ticket does not pick between two readings, and they differ in scope.

1. **Make the `Operator` arm insert the backing value too**, qualified-and-unqualified as
   the value arm does, keyed under the operator name — so `find_value("+")` finds a
   `ValueType` directly and the redirect is not needed for imports. Smallest diff. Leaves
   `find_value`'s redirect in place for the local case, and leaves the misleading name in
   the error.
2. **Resolve operators as operators.** Give `ExpressionKind` (or canonicalization) a real
   binary-operator node instead of desugaring in the grammar, so the operator name is
   looked up in `infixes`, and the function it names is resolved *in the module that
   declared the infix* rather than in the module using it. Larger, and it is what the
   `TODO` on `find_value` is asking for. It also removes the class of bug rather than this
   instance, and is the prerequisite for ever applying precedence and associativity — which
   are parsed, stored, and today ignored.

Whichever is chosen, the error message must name what was actually missing: a
`VariableNotFound` reporting `+` when `add` is the unresolved name is worth fixing in the
same change, since the redirect is where the substitution happens.

**Acceptance:** a `tests/pipeline.rs` (or `tests/compiler/canonical.rs`) test with two
modules where the second does `import First exposing ((+))` — naming the operator and *not*
the function behind it — and uses `a + b`, asserting a clean canonicalization. A second
test asserts that an operator entry naming an infix the module does not declare still fails
with `EnvError::InfixNotFound`, so the fix does not turn the check off. The
`**Known gap:**` block in `docs/spec/modules.md` (the `package=operators` group) goes red on
its `canonical-error:VariableNotFound` pin and is retagged `expect=ok` with its paragraph
deleted.
