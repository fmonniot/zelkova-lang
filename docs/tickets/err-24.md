# ERR-24 · An exposed name that was imported is reported as "not declared anywhere in this module"

**Sizing:** small. Larger if the answer is a payload change on `Error::ExportNotFound`, which
every test matching its three fields has to follow.

**Location:** `crates/zelkova-compiler/src/canonical/mod.rs` — `Error::ExportNotFound`,
`PhaseError for Error`'s `message()` ("`x` is exposed by this module but no value of that name is
declared in it"), `labels()` ("`x` is not declared anywhere in this module") and `notes()`
(which has no arm for it); `do_exports`'s `Lower` and `Upper` arms; `ValueType::Foreign`/`Foreigns`
and `TypeArity::name` in `crates/zelkova-compiler/src/canonical/environment.rs`.

**Problem:** since `BUG-31`, `do_exports` raises `ExportNotFound` for a name an import brought in,
with the message and label it raises for a name nothing declares. For

```zel
module Facade exposing (label)

import Widget exposing (label)
```

the label reads "`label` is not declared anywhere in this module" beside an `import` line that
names it two lines below, which reads as wrong. Both strings are accurate for the rule (a
module exposes only what it declares) and say nothing of why the name was refused. The error
carries `(Name, ExportType, NodeSpan)` and no module, so `notes()` cannot say where the name came
from; the information exists at the raise site (`ValueType::Foreign(module, ..)` for a value,
`TypeArity::name`'s module for a type).

**Approach:** add a note along the lines of "`label` is imported from `Widget`; a module exposes
only what it declares itself". The ticket does not pick how the module reaches `notes()`:

1. Extend `ExportNotFound` with an `Option<ModuleName>` (the module an import took the name
   from), which touches every construction and every match on the variant, including the ones in
   `crates/zelkova-compiler/tests/canonical.rs`.
2. A separate variant for the imported case, which leaves `ExportNotFound`'s shape alone.
   `docs/spec/modules.md`'s `package=reexport` `Facade` block, which exposes an imported value,
   is tagged `expect=canonical-error:ExportNotFound` and would be retagged with the new
   variant's name.

Option 2 changes what the spec harness reads; option 1 does not. For a class, `BUG-31` left
`classes::declaring_module` as the way to tell; the note applies to it as well.

**Acceptance:** a `crates/zelkova-compiler/tests/canonical.rs` test on the module above asserts
that the error's `notes()` names `Widget`, and goes red with the note removed. A value entry, a
type entry and an entry satisfied by an open import are each covered. `cargo test --workspace`
(which includes `cargo test --test spec`) is green and `cargo run -- compile std/core` still
prints `parsed 10 modules` and exits 0.

**Related:** raised in review of the `BUG-31` PR, which specified the variant and not the
wording. [`BUG-51`](bug-51.md) is the same arm's operator counterpart, which would report through
whatever this ticket settles.
