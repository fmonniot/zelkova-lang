# BUG-40 · Two same-named modules from different packages, both imported, emit colliding local import bindings

**Severity:** high (miscompile — a well-formed program compiles successfully, `compile_package`
returns `Ok(())`, and the `.mjs` file it writes throws `SyntaxError` at load).

**Depends on:** [`BUG-37`](bug-37.md), for the constructor half below — `ir::Constructor` has
no package to name a hoisted constant by until that ticket puts one in a constructor's
identity.

**Location:** `src/compiler/javascript.rs`, the *Names* section — `imported(module, name)`
(~line 356) and `hoisted(union, constructor)` (~line 342), the two functions that name a local
JavaScript binding for something a module declares, and `mangle`'s doc comment (~line 306),
which lists every shape of emitted name and why none can meet another. Their callers:
`Emitter::value`'s `ReferenceKind::Foreign(qname, package)` arm (~line 920), its
`ReferenceKind::Constructor` arm (~line 941), `Emitter::facade_declaration`'s companion alias
(~line 846), `hoisted_constructors`, and the loop in `emit` that writes `self.imports` out as
`import` lines (~line 666).

**Problem:** both name functions build the local name from the declaring **module** alone,
and a module's name is unique only within its package. `self.imports:
BTreeMap<(String, String), BTreeSet<String>>` is keyed by `(package, module)` — correctly
keeping two packages' same-named modules on separate `import` lines — and
`ReferenceKind::Foreign` already carries the `PackageName`, but `imported(&module, &name)` is
called without it, so the two lines bind the same local name.

Reproduced against this tree with a package `app` holding its own `src/Size.zel` (`small :
Size`) and depending, wrapped, on `tests/fixtures/dep_widgets` (`acme-widgets`, whose
`Size.zel` also exposes `small : Size`):

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

Two `import` statements binding one name is a `SyntaxError` at module load.

**The constructor half is not the same failure, and is not reachable yet.** Two things differ:

- Two nullary constructors whose `hoisted` names collide always carry the same tag: the name
  is built from the constructor's own name, and so is the `{$: "…"}` it holds. The
  `imported_constructors` map overwriting one with the other is therefore unobservable
  ([`DEC-18` decision
  4](../decisions/dec-18.md#4--a-constructor-of-no-arguments-is-hoisted-to-one-module-level-constant)).
- The real defect is `ctor.union.module_name() != self.module` in the `Constructor` arm, which
  decides whether a constructor is this module's own by comparing module names without their
  package. Module `Size` of package `app` mentioning `AcmeWidgets.Size.Small` would take
  `Small` for its own, hoist nothing for it, and refer to a `$Size$Small` that no `const`
  declares — a `ReferenceError`. Today that program does not reach the emitter at all: the
  typer's `translation.constructors` is keyed by a package-less `QualName`, and a build of it
  fails with `Emit([Unchecked { name: "theirs" }])`. That is [`BUG-37`](bug-37.md)'s, and it is
  why this ticket depends on it.

**Fix:** every local name that refers to a declaration names it by its full identity —
package, module, name — whether or not anything collides. Every emitted module's text
changes, `std/core`'s included; that is accepted.

- **The package is spelled by its name with each `-` replaced by `_`**: `zelkova-core` is
  `zelkova_core`, `acme-widgets` is `acme_widgets`. This is injective because a legal package
  name holds no `_` (`is_legal_package_name`), it is always a valid start of a JavaScript
  identifier because a package name starts with a lowercase letter, and its lowercase first
  letter keeps it apart from a module segment, which is uppercase.
  `PackageName::namespace()` is not used: the namespace is how one dependent spells the
  package, not the package's identity — `zelkova-core` is seen unwrapped and nobody writes
  `ZelkovaCore` — and [`DEC-18` decision
  5](../decisions/dec-18.md#5--output-is-written-per-package-beside-the-root-manifest) already
  keeps the namespace out of the output for that reason.
- **A value another module declares** is imported as `<package>$<Module$Segments>$<name>`:
  `Maybe.withDefault` is `zelkova_core$Maybe$withDefault`, and the repro's two bindings are
  `app$Size$small` and `acme_widgets$Size$small`.
- **A hoisted constructor**, this module's own or another's, is
  `$<package>$<Module$Segments>$<Constructor>`: `Test`'s `Red` in package `app` is
  `$app$Test$Red`. One scheme for both, so the constant a module hoists for its own
  constructor and the one an importer hoists for it have the same name.
- **Whether a constructor is this module's own** compares the full `ModuleName` — package and
  module — not the module's name alone.
- **The facade's companion alias** gets its own shape, `$companion$<name>`, instead of borrowing
  `imported(self.module, …)`, so it is disjoint from a real import by construction rather than
  because a facade happens to hold no foreign reference.
- **Uniqueness follows from identity, not from the resolver.** A `(package, module)` pair is
  unique in a build, so the argument that no two local names meet stays inside
  `javascript.rs`. Naming a binding by the spelling the importing package reaches the module
  by would also be collision-free, but only through `resolve::visible_modules`' collision
  rule, and the emitter holds no spelling to build it from.
- **`mangle`'s doc comment is rewritten** for the new shapes: a value import contains a `$`,
  does not start with one, and its first segment is lowercase; a hoisted constant starts with
  `$` and holds at least three more; `$companion$…` starts with `$` and a word not in
  `RESERVED`.

Named imports stay named imports — no `import * as`. A named import of an export that does not
exist fails when the module loads, which [`GEN-14`](gen-14.md)'s run under `node` catches for
free; a property read off a namespace object would be `undefined` at run time instead.

How the package reaches `ir::Constructor` is whatever shape [`BUG-37`](bug-37.md) picks for
putting it in a constructor's identity — a `QualName` carrying a `PackageName`, or the union's
`ModuleName`. The name functions here take the package as a separate argument, so either
shape feeds them without a change to this scheme.

**Acceptance:**

- A `tests/pipeline.rs` test compiling a fixture pair shaped like the repro above — a package
  with its own `Size`, depending wrapped on `tests/fixtures/dep_widgets`, importing and using
  both — writes an `App.mjs` holding two distinct local bindings, `app$Size$small` and
  `acme_widgets$Size$small`, and no duplicate `import` line: asserted on the emitted text, not
  merely that `compile_package_into` returns `Ok`.
- A second test in the same fixture pair, whose module `Size` mentions
  `AcmeWidgets.Size.Small`, emits a `const $acme_widgets$Size$Small = {$: "Small"};` in
  `Size.mjs` beside the reference to it.
- The text pins in `tests/javascript.rs`, `tests/pipeline.rs` (`the_stdlib_facades_emit`,
  `every_stdlib_module_emits`, and `a_build_writes_one_directory_per_package`'s
  `Size$small` prefix) and the unit tests in `javascript.rs` are updated to the new names.
- `cargo test --workspace` is green, and `cargo run` still prints `parsed 8 modules`, lists all
  eight as checked, and exits 0.

**Related:** found in review of #244 (`GEN-13`, "write the build"). [`BUG-37`](bug-37.md) is
the sibling bug in the type checker: both are a package missing from an identity — there, the
typer's `QualName`; here, the emitter's local binding name.
