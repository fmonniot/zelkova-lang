# BUG-51 · `do_exports` accepts an exposed operator that resolves only through an import

**Severity:** medium (wrong behaviour under normal use — a module re-exports an operator it never
declared, against [*Everything exposed must be declared here*](../spec/modules.md#everything-exposed-must-be-declared-here),
and the entry is then dropped from its interface with no diagnostic anywhere).

**Location:** `crates/zelkova-compiler/src/canonical/mod.rs` — `do_exports`'s
`ExposedKind::Operator` arm; `crates/zelkova-compiler/src/canonical/environment.rs` —
`Environment::local_infix_exists` (`RootEnvironment`'s is `self.infixes.contains_key(name)`),
`process_import`, `InfixEntry::function` and `InfixFunction`.

**Problem:** the `Lower` and `Upper` arms reject a name only an import brought into scope
(`BUG-31`). The `Operator` arm calls `env.local_infix_exists(name)`, which despite its name asks
only whether the `infixes` map has the key. `process_import` fills that same map from an
`exposing ((op))` entry and from `exposing (..)`, and the default `Basics` import fills it too, so
the arm cannot tell a declaration from an import. Both of these canonicalize with no error:

```zel
module Facade exposing ((+++))

import Widget exposing ((+++))
```

and the same module with `import Widget exposing (..)`. In a package whose `src/Facade.zel` is

```zel
module Facade exposing ((+), (|>))

x : Int
x = 1
```

`cargo run -- compile <package>` prints `checked modules: ["…:Facade"]` and exits 0 (confirmed on
the `BUG-31` branch). The two entries are not in `Facade`'s interface. The error surfaces only in
a second module that writes `import Facade exposing ((+))`, as "the imported module does not
expose an infix operator named `+`", pointing at the importer rather than at the entry that
should have been refused.

**Fix:** an operator is exposed only when this module declared it. `InfixEntry::function` already
carries the signal: `insert_local_infix` is the one place that writes `InfixFunction::Local`, and
`imported_infix` writes `Imported`/`ImportedUntyped`. So the arm can test

```rust
matches!(
    env.find_infix(name),
    Some(InfixEntry { function: InfixFunction::Local, .. })
)
```

instead of `local_infix_exists`. A local `infix` declaration replaces an imported entry under the
same key, as `BUG-31` found for values and types, so a name both imported and declared still
reads as declared; that collision is [`LANG-29`](lang-29.md)'s to raise and this ticket does not
decide it. Whether `local_infix_exists` then has a caller left, or is renamed or removed, is for
the implementer; `ERR-10` and `BUG-32` also mention it.

**Acceptance:**

- `crates/zelkova-compiler/tests/canonical.rs`: a module exposing `((+++))` that only imports it
  (once with `exposing ((+++))`, once with `exposing (..)`) returns `ExportNotFound` with
  `ExportType::Infix`, labelled on the entry. Each test is seen to go red with the `Operator` arm
  reverted.
- A module that declares the infix and exposes it still canonicalizes, and `cargo run -- compile
  std/core` still prints `parsed 10 modules`, lists all ten as checked and exits 0 (`Basics`
  exposes operators it declares, and every module imports `Basics` openly).
- [*Everything exposed must be declared here*](../spec/modules.md#everything-exposed-must-be-declared-here)
  already states the rule for every entry, operators included, so the chapter prose needs no
  change. It has no operator example, so what pins the rule is a new block in that section's
  `package=reexport` group: a module that imports an operator from `Widget` and exposes it, tagged
  `expect=canonical-error:ExportNotFound`. The ticket does not decide whether that block lands
  before the fix, which is possible only as `expect=ok` with a **Known gap:** paragraph beside it
  (`docs/spec/conventions.md`, *Known gap:*), or with it, in which case the chapter claims more
  than the compiler does until then.

**Related:** found reviewing the `BUG-31` PR, which left operators out of scope and corrected
`do_exports`'s header comment to say so. [`BUG-32`](bug-32.md) is the same arm's other defect (an
exposed infix's unannotated backing function); the two fixes touch the same lines and can land in
either order.
