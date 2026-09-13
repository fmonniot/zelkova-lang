# BUG-32 · An exposed infix's unannotated backing function is silently dropped from the interface

**Severity:** medium (wrong behaviour under normal use — a declaration that is genuinely
exposed, and would be caught by `BUG-14`'s check if exposed by name, disappears from the
interface instead, with no diagnostic anywhere).

**Location:** `src/compiler/canonical/mod.rs` — `Module::to_interface`'s `infix_functions`
construction, and `do_exports`'s `ExposedKind::Operator` arm.

**Problem:** [`BUG-14`](../tickets/README.md) made `do_exports` reject an exposed `Lower`
name whose declaration carries no type annotation (`Value::Value` rather than
`Value::TypedValue`), because `to_interface` cannot put an untyped value into
`Interface::values`. `to_interface` builds a second, parallel set of values the same way and
the same check was not extended to it:

```rust
let infix_functions = infixes
    .values()
    .filter(|infix| !values.contains_key(&infix.function_name))
    .filter_map(|infix| match self.values.get(&infix.function_name) {
        Some(Value::TypedValue { tpe, span, .. }) => {
            Some((infix.function_name.clone(), (*span, tpe.clone())))
        }
        _ => None,
    })
    .collect();
```

`Some(Value::TypedValue { .. })` is kept; `Some(Value::Value { .. })` — the unannotated case —
falls into `_ => None` and is silently dropped, exactly the shape `BUG-14` fixed for `values`.
An `infix left 6 (+) = add` declaration where `add` has no type annotation reproduces it: the
`Operator` export check (`do_exports`) only calls `env.local_infix_exists(name)`, which
confirms the *infix declaration* exists and says nothing about whether its *backing function*
is annotated, so `exposing ((+))` is accepted and `add` silently never reaches the interface.
Another module using `(+)` then gets whatever error looking up a nonexistent
`Widget.add` produces — not a diagnostic naming the missing annotation, the same
wrong-thing-blamed shape `BUG-14`'s Problem section described for the plain-value case.

`std/core/src/` is unaffected today: none of its `infix` declarations back onto an unannotated
function.

**Fix:** extend the check `do_exports`'s `Operator` arm already does. Once
`env.local_infix_exists(name)` confirms the infix exists, look up its backing function's
`Value` the same way the `Lower` arm now does (post-`BUG-14`) and raise
`Error::ExportedValueNotAnnotated` (or whatever name that variant carries after `BUG-14`
lands — it may already exist) if it is `Value::Value` rather than `Value::TypedValue`. That
makes the `_ => None` arm in `infix_functions` unreachable through normal use the same way
`BUG-14`'s PR made the parallel arm in `values` unreachable, rather than leaving it as the
active bug. Confirm what span to point at: the exposed operator in the `exposing (...)` list,
with a secondary label on the unannotated function declaration, mirroring `BUG-14`'s labels.

**Acceptance:** a module declaring `infix left 6 (+) = add` with `add` unannotated and
`exposing ((+))` raises the same "exposed value not annotated" error `BUG-14` introduced,
naming `add` — a canonicalization test. A `tests/pipeline.rs` test with two modules, the
second using the operator, asserts the error lands on the *exporting* module rather than the
importer failing to resolve `(+)`. A module keeping the infix and its function private still
compiles. `cargo run` still prints `parsed 8 modules` and lists all eight as checked.

**Related:** found while closing [`BUG-14`](../tickets/README.md) (PR #198), which fixed the
same defect for a value reached directly through `do_exports`'s `Lower`/`Open` handling; this
ticket is the same class of bug reached through the `infix` declaration path instead.
