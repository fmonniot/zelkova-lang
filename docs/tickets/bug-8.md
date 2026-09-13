# BUG-8 · `do_exports` never checks that an exposed value or type actually exists

**Severity:** medium (wrong behaviour under normal use — a module's own `exposing (...)`
header can name a value or type it never declares, and canonicalization accepts it silently).

**Location:** `src/compiler/canonical/mod.rs` — `do_exports` (line 1118, right under the
`// TODO Add existence checks for values and types` comment that has sat there since the
function was written).

**Problem:** `do_exports` walks a module's `exposing (...)` header and builds an `Exports`
value from it. Its three `ExposedKind` arms are not symmetric:

```rust
parser::ExposedKind::Lower(name) => Ok((name.clone(), ExportType::Value)),
parser::ExposedKind::Upper(name, parser::Privacy::Public) => {
    Ok((name.clone(), ExportType::UnionPublic))
}
parser::ExposedKind::Upper(name, parser::Privacy::Private) => {
    Ok((name.clone(), ExportType::UnionPrivate))
}
parser::ExposedKind::Operator(name) => {
    if env.local_infix_exists(name) {
        Ok((name.clone(), ExportType::Infix))
    } else {
        Err(Error::ExportNotFound(name.clone(), ExportType::Infix, exposed.span))
    }
}
```

Only the `Operator` arm checks that the name resolves to something the module actually
declares (`env.local_infix_exists`). `Lower` and `Upper` accept the name unconditionally — a
`module Foo exposing (bar)` header where `bar` is never declared in `Foo` type-checks with no
error at all. `Error::ExportNotFound` and its rendering (`export_type_noun`) already handle
`ExportType::Value`, `UnionPublic` and `UnionPrivate` generically — the message and diagnostic
plumbing are not the gap, only the check that would raise them for two of the three cases.

Found while grounding [ERR-9](../../docs/tickets/README.md) (PR #141): its `ExportNotFound`
acceptance test had to exercise the `Operator` case, because an undeclared value or type in a
`module … exposing (...)` header does not currently produce this error at all — there was no
way to reach it through `Lower`/`Upper`.

**Fix:** the `Environment` trait already exposes `find_value` and `find_type`
(`environment.rs:32,36`) — the same category of lookup `local_infix_exists` does for infixes,
just without a name matching that convention. Use them (or add `local_value_exists` /
`local_type_exists` alongside `local_infix_exists` if a call through `find_*` doesn't fit
cleanly at this call site) to reject a `Lower`/`Upper` name in `do_exports` that doesn't
resolve, mirroring the `Operator` arm's `Err(Error::ExportNotFound(..))` shape. Note `Upper`
carries a `Privacy` — check the type exists regardless of which privacy was written; privacy
governs whether its constructors are exposed, not whether the type itself is real (see
`process_import`'s `Upper` handling in `environment.rs` for how privacy already threads
through the corresponding import-side lookup).

**Acceptance:** two canonicalization tests — a module `exposing (missing)` where `missing` is
never declared as a value, and one where it is never declared as a type — each asserting
`Error::ExportNotFound(_, ExportType::Value | ExportType::UnionPublic | ExportType::UnionPrivate, _)`
is raised with the exposed name's own span (not the whole header).

**Related:** [BUG-9](README.md), found alongside this one and since closed, was the larger gap
in the same area — the `Exports` this function computes was not consulted anywhere once built,
so no `exposing` list restricted what an importer could see. `Module::to_interface` reads it
now, which is what gives the value this function computes a consumer; it does not add the
existence check, because a header entry naming nothing was never in `Module::values` to be
filtered out of the interface in the first place. This ticket does not depend on that one.
