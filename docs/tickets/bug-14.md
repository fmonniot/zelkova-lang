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

**Decided (`SPEC-5`, by the language owner):** option 1 below. A declaration named in its
module's `exposing` list must carry a type annotation; a private declaration need not. Because
`exposing (..)` exposes everything a module declares, a module written that way must annotate
every top-level declaration — the rule applied, not an exception to it. Written up in
[`docs/spec/types.md`](../spec/types.md)'s *An exposed declaration must be annotated*, which
carries the `**Known gap:**` block this ticket turns red.

**Approach:** the two options this ticket was filed weighing, kept because the second is still
what someone will reach for if the first turns out to be wrong:

1. **Require an annotation on anything exposed** — the decided one. Small, needs no new
   machinery, and gives an error that says what to do. It makes annotations mandatory at
   exactly the boundary where they are documentation anyway.
2. **Get the inferred type into the interface.** The typer already solves the declaration;
   the missing piece is a path from its substitution back into `canonical::Module` before
   `to_interface` runs. Larger, touches `check_module`'s phase order, and would be the answer
   if annotations were to stay optional everywhere. They are not.

Either way `to_interface` stops silently dropping the value: option 1 makes the drop
unreachable, option 2 makes it unnecessary.

Under option 1 the check belongs where the `exposing` list is already walked — `do_exports`
(`canonical/mod.rs`), which is the one place that knows both what a module exposes and what it
declared, and already returns into a `Vec<canonical::Error>`. It wants a new `Error` variant
naming the declaration, with the caret under the name in the `exposing` list (`parser::Exposed`
carries a span — `ERR-9`) and a secondary label on the unannotated declaration itself. Note
[BUG-8](bug-8.md) is a defect in that same function and is worth reading first: `do_exports`
does not today check that an exposed `Lower` name exists at all, so the walk this check hangs
off is one that has to be written either way.

`std/core/src/` costs nothing here: checked while the rule was decided, every value exposed by
each of the eight compiling modules already carries an annotation, so none of them becomes an
error when this lands. The `.ignored` modules were not checked.

**Acceptance:** a module exposing an unannotated value is a `canonical::Error` naming that
declaration — a test in `tests/compiler/canonical.rs` — and a module keeping it private still
compiles. A `tests/pipeline.rs` test with two modules, where the first exposes an unannotated
value and the second imports it, asserts that the error lands on the *exporting* module: the
importer must not get `VariableNotFound` for a name the exporting module plainly declares. A
module using `exposing (..)` with one unannotated top-level declaration is an error too.
`cargo run` still prints `parsed 8 modules` and lists all eight as checked.

Two spec blocks go red and are retagged with their `**Known gap:**` paragraphs deleted: the
`package=unannotated` pair in `docs/spec/modules.md` (on its `canonical-error:VariableNotFound`
pin — the error moves modules, so the pin moves with it), and the `count` block under *An
exposed declaration must be annotated* in [`docs/spec/types.md`](../spec/types.md), which is
the block that pins the rule itself.
