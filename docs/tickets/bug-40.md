# BUG-40 · Two same-named modules from different packages, both imported, emit colliding local import bindings

**Severity:** high (miscompile — a well-formed program compiles successfully, `compile_package`
returns `Ok(())`, and the `.mjs` file it writes throws `SyntaxError` at load).

**Location:** `src/compiler/javascript.rs` — `imported(module: &str, name: &str)` (~line 356)
and `hoisted(union: &QualName, constructor: &Name)` (~line 342), the two functions that name a
local JavaScript binding for something another module declares. Both build the name from the
declaring **module** alone. The import side is `Emitter::value`'s `ReferenceKind::Foreign(qname,
package)` arm, which keys `self.imports: BTreeMap<(String, String), BTreeSet<String>>` by
`(package, module)` — correctly keeping two packages' same-named modules on separate `import`
lines — but calls `imported(&module, &name)` for the local binding without `package`, so the two
lines bind the same local name. The emission loop that turns `self.imports` into text is in
`emit`, `for ((package, from), names) in &emitter.imports` (~line 666).

**Problem:** a module that imports two modules which are spelled the same but come from
different packages — its own local module, and a wrapped dependency's namespaced one, e.g.
`Size` and `AcmeWidgets.Size` — and uses a value from each, emits two `import` lines aliasing to
one identical local name. Reproduced against this tree with a package `app` holding its own
`src/Size.zel` (`small : Size`) and depending, wrapped, on `tests/fixtures/dep_widgets`
(`acme-widgets`, whose `Size.zel` also exposes `small : Size`):

```zel
module App exposing (mine, theirs)

import Size
import AcmeWidgets.Size

mine : Size.Size
mine = Size.small

theirs : AcmeWidgets.Size.Size
theirs = AcmeWidgets.Size.small
```

`compile_package_into` returns `Ok(())` and writes `app/App.mjs` as:

```js
import { small as Size$small } from "../acme-widgets/Size.mjs";
import { small as Size$small } from "./Size.mjs";

const mine = Size$small;
const theirs = Size$small;
```

Two `import` statements binding one name is a `SyntaxError` at module load, and both `mine` and
`theirs` — which are meant to hold two different packages' `small` — resolve to whichever import
the JavaScript engine binds last. Nothing in this repository's fixtures or in `std/core`
exercises this today: it needs two packages in one build that both hold a module of the same
name, one of them wrapped, with both reached from one importing module — the same precondition
[`BUG-37`](bug-37.md) needs, which is why `LANG-14` (unwrapped dependencies) is what made both
reachable.

`hoisted()` has the same shape — it also names its local binding from `union.module_name()`
alone, with no package — for a nullary constructor reached the same way (`Test.Red` from two
packages both spelling their module `Test`, both declaring a bare `Red`). Its failure mode may
not be identical to the `import`-line case above, though: `imported_constructors:
BTreeMap<String, Name>` is keyed by the *local hoisted name*, so two colliding constructors
would not produce a duplicate `const` declaration (a `BTreeMap` cannot hold two entries under
one key) — the second insert silently replaces the first, and whether that is observably wrong
depends on whether the two constructors' tags (`ctor.name`) agree, which is not guaranteed
merely by the module names colliding. Whoever fixes this should verify the constructor case's
actual failure mode with its own repro rather than assuming it matches the value case.

**Fix:** not decided, and this ticket does not pick a shape. The obvious direction is naming the
local binding from the package as well as the module and the value/constructor name — matching
how `self.imports` is already keyed — but that touches the local-name mangling scheme broadly:
every `Emitter::value` foreign-reference site and every hoisted nullary constructor, in every
emitted module. A session picking this up must check the smallest change that leaves existing
output unaffected where nothing collides: `std/core` is one package, so every one of its
emitted modules — pinned by `tests/javascript.rs` and the text `tests/pipeline.rs` fixtures
compare — must stay byte-for-byte identical if the mangling only starts including the package
when a collision is actually possible, or the ticket should say plainly if it instead always
mangles the package in and accepts that `std/core`'s output changes.

This may be worth resolving together with, or after, [`BUG-37`](bug-37.md): both bugs are a
package missing from an identity — there, the typer's `QualName`; here, the emitter's local
binding name — and a fix to one naming scheme chosen without regard for the other could leave
them inconsistent (e.g. the typer treating two modules as one type while the emitter now keeps
their bindings apart, or vice versa). This ticket does not depend on `BUG-37` being fixed first,
but a session should read it before choosing a mangling shape here.

**Acceptance:** a `tests/pipeline.rs` test compiling a fixture pair shaped like the repro above
(a package with its own module, depending wrapped on `tests/fixtures/dep_widgets` or an
equivalent, importing and using both same-named modules) writes an `.mjs` file with two distinct
local bindings and no duplicate `import` line — asserted on the emitted text, not merely that
`compile_package_into` returns `Ok`. `cargo test --workspace` stays green, `std/core`'s emitted
output (`tests/javascript.rs`, `the_stdlib_facades_emit`/`every_stdlib_module_emits` in
`tests/pipeline.rs`) is unaffected unless the ticket's fix note above says otherwise, and `cargo
run` still prints `parsed 8 modules` with all eight checked.

**Related:** found in review of #244 (`GEN-13`, "write the build"). Not fixed there: the PR's
fixtures never reach two packages with a same-named, both-imported module, and the fix's shape
depends on a design choice this ticket does not make. [`BUG-37`](bug-37.md) is the sibling bug
in the type checker.
