# BUG-14 · A top-level value with no type annotation never reaches the module's interface

**Severity:** medium (wrong behaviour under normal use — an exposed name is silently
absent from every importer's scope, and the diagnostic they get blames the wrong thing).

**Location:** `src/compiler/canonical/mod.rs` — `Module::to_interface`, the `filter_map`
over `self.values` that drops every `Value::Value` and keeps only `Value::TypedValue`.

**Problem:** `to_interface` is the only thing that turns a checked module into what other
modules resolve against, and it keeps a value only when that value carries a type:

```rust
let values = self.values.iter().filter_map(|(name, value)| match value {
    Value::Value { .. } => None,
    Value::TypedValue { tpe, span, .. } => Some((name.clone(), (*span, tpe.clone()))),
}).collect();
```

`Value::Value` is what `do_values` builds for a declaration with no annotation, which is
legal Zelkova — annotations are optional. So a module can declare `label = 1`, expose it,
and have no importer able to see it:

```zel
module Widget exposing (label)

label = 1
```

```zel
module Main exposing (x)

import Widget

x = Widget.label   -- cannot find a value named `Widget.label`
```

Adding `label : Size` to the first module makes the second compile. Nothing tells the user
that, and the error names `Widget.label` as though `Widget` had never declared it — the one
reading it has no reason to suspect the annotation.

The filter is not arbitrary: `Interface::values` is a `HashMap<Name, (NodeSpan, Type)>` and
an unannotated declaration has no `Type` to put there. Inference is what would supply one,
and the typer runs *after* canonicalization and does not write back
(`check_module`, `src/compiler/mod.rs`) — which is why this is a real design problem and not
a missing line.

Found while writing [`docs/spec/modules.md`](../spec/modules.md) (`SPEC-3`), whose
*Exposing is what other modules can see* section carries the `**Known gap:**` block. Related
but independent: [BUG-9](bug-9.md) is the same function leaking what it should hide, this is
it hiding what it should publish. Either can land first.

**Approach:** there is a real choice here and this ticket does not make it.

1. **Require an annotation on anything exposed.** A declaration named in a module's
   `exposing` list must carry a type; a private declaration need not. Small, needs no new
   machinery, and gives an error that says what to do. It makes annotations mandatory at
   exactly the boundary where they are documentation anyway. It is also a language rule, so
   it belongs in `docs/spec/` and is the owner's call, not this ticket's.
2. **Get the inferred type into the interface.** The typer already solves the declaration;
   the missing piece is a path from its substitution back into `canonical::Module` before
   `to_interface` runs. Larger, touches `check_module`'s phase order, and is the answer if
   annotations are to stay optional everywhere.

Either way `to_interface` stops silently dropping the value: option 1 makes the drop
unreachable, option 2 makes it unnecessary.

**Acceptance:** a `tests/pipeline.rs` test with two modules, where the first exposes an
unannotated value and the second imports it, asserting the outcome the chosen option
specifies — a `canonical::Error` naming the *unannotated declaration* under option 1, or a
clean compile under option 2. In neither case may the importer get `VariableNotFound` for a
name the exporting module plainly declares. The `**Known gap:**` block in
`docs/spec/modules.md` (the `package=unannotated` pair) goes red on its
`canonical-error:VariableNotFound` pin and is retagged with its paragraph rewritten.
