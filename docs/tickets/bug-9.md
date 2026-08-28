# BUG-9 · A module's `exposing` list is computed and then never consulted

**Severity:** medium (wrong behaviour under normal use — no data loss or miscompile, but a
module boundary that the source explicitly declares is not enforced).

**Location:** `src/compiler/canonical/mod.rs` — `Module::exports` (line 41), populated by
`do_exports` at the end of `canonicalize` (around line 901), and `Module::to_interface`
(line 64).

**Problem:** `canonicalize` computes `Exports` from the module's `exposing (...)` header —
either `Exports::Everything` or `Exports::Specifics(HashMap<Name, ExportType>)` naming exactly
what was exposed — and stores it on `Module::exports`. `Module::to_interface`, the only thing
that turns a checked module into what other modules import against, ignores that field
entirely:

```rust
pub fn to_interface(&self, file: Option<SourceFileId>) -> super::Interface {
    let values = self.values.iter().filter_map(|(name, value)| match value {
        Value::Value { .. } => None,
        Value::TypedValue { tpe, span, .. } => Some((name.clone(), (*span, tpe.clone()))),
    }).collect();

    super::Interface {
        module_name: self.name.clone(),
        values,
        unions: self.types.clone(),
        infixes: self.infixes.clone(),
        file,
    }
}
```

`values`, `unions` and `infixes` are built from `self.values` / `self.types` / `self.infixes`
— every top-level declaration in the module — filtered only by whether a value carries a type
(`Value::TypedValue` vs `Value::Value`), never by whether the name is in `self.exports`. A
`grep -rn '\.exports\b'` over `src/` and `tests/` turns up exactly the one write (`canonicalize`
building it) and the one read (`Module.exports: Exports` in `to_interface`'s own struct — never
matched on). Nothing else in the tree reads the field.

The practical effect: `import Foo exposing (secret)` succeeds even when `Foo`'s own header is
`module Foo exposing (other)` and never mentions `secret` — `process_import`
(`environment.rs`) looks `secret` up in `interface.values`, which holds every declaration in
`Foo` regardless of what `Foo` exposed. A module cannot actually keep anything private today;
`exposing (...)` restricts nothing once the module has been checked.

Found while grounding [BUG-8](bug-8.md) (a narrower, related gap: `do_exports` itself doesn't
validate that an exposed `Lower`/`Upper` name exists). That ticket's fix does not depend on
this one, and this one's fix does not depend on that one — they touch different functions and
can land in either order or independently.

**Approach:** there is a real design choice here that this ticket does not make:

1. **Filter in `to_interface`.** Match `self.exports` against `self.values`/`types`/`infixes`
   and only include names that are actually exposed (for `Exports::Everything`, include
   everything, same as today). This is the smaller diff and matches where the filtering
   conceptually belongs — building the *external* view of a module.
2. **Filter earlier, when declarations are collected**, so an unexposed declaration is never
   present in a form `to_interface` could leak by omission of a check. Larger diff, touches
   `do_values`/`do_types`/`do_infixes`, and would need those private declarations kept around
   *somewhere* for the module's own internal type-checking to still see them (a private
   function is still callable from within the same module).

Either approach needs `UnionPrivate` handled correctly: a privately-exposed type is still the
*type* other modules can see (per `process_import`'s existing `Upper`/`Privacy::Private` arm,
which inserts the type without its constructors) — this is not "hide the type entirely," only
"hide its variants." `ExportType::UnionPrivate` already distinguishes this from
`UnionPublic`, so whichever approach is taken has the information needed to preserve it.

**Acceptance:** a `tests/pipeline.rs` (or `tests/compiler/canonical.rs`, if it fits without a
full package) test with two modules, where the second `import`s a value or type the first
declares but does not expose, asserting the import fails with an unresolved-name error
(`EnvError::ValueNotFound`/`UnionNotFound`) rather than succeeding. A second test confirms a
value the first module *does* expose still imports successfully, so the fix restricts rather
than breaks resolution.
