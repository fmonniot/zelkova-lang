# BUG-37 · A package is not part of a type's identity, so two packages' same-named modules are one type

**Severity:** high (miscompile — two unrelated union types unify silently, and the build
reports success)

**Location:** `src/compiler/name.rs` — `QualName`, whose fields are `module: Vec<String>` and
`name: String` and nothing else. `src/compiler/canonical/mod.rs` — `Type::Type(QualName,
Vec<Type>)`, the head the typer unifies on, and `ModuleName`, which *does* carry a
`PackageName` but is not what a `Type` holds. `src/compiler/resolve.rs` —
`visible_modules`, whose collision rule is keyed by the spelling an importing package reaches
a module by.

**Problem:** a module's name within its package is not unique in a build, and `QualName` has
nothing else in it. `visible_modules` reports two modules answering to **one spelling**, so a
local module `Size` and a wrapped dependency's `AcmeWidgets.Size` are two different spellings
and no collision is raised — correctly, since the spec says that arrangement is legal. But the
`Interface` inserted for the dependency carries `module_name.name() == "Size"`, and a type
annotated `AcmeWidgets.Size.Size` and one annotated `Size.Size` both canonicalize to the one
`QualName` `Size.Size`.

Reproduced against `tests/fixtures/dep_widgets` (whose `Size.zel` declares `type Size = Small
| Large`), as a wrapped dependency of a package `ident-app` that also holds `src/Size.zel`
with `type Size = Mine`:

```zel
module App exposing (f)

import AcmeWidgets.Size
import Size


f : Size.Size -> AcmeWidgets.Size.Size
f s = s
```

```
success parsed 2 modules
success checked modules: [
    "ident-app:Size",
    "ident-app:App",
]
```

`compile_package` returns `Ok(())`. A union with one variant and a union with two are the same
type to the unifier, and every phase downstream of canonicalization inherits that.

This is [`AST-4`](README.md) and `BUG-35` one level up: the typer stopped identifying a union
by its *unqualified* name, and now identifies it by a qualified name that is only unique
**within one package**. Before `LANG-14` a build held exactly one package, so the program above
could not be written; `LANG-14` is what made it reachable.

**Fix:** put the package in the identity. Two shapes, and this ticket does not pick between
them:

- **`QualName` carries a `PackageName`.** Every `QualName` then answers "which declaration" on
  its own, which is what its own doc comment already claims ("once we are given a `QualName`,
  no further resolution is necessary"). It is also the larger change: `QualName::parse` and
  `QualName::from_strs` build one from text that has no package in it, `QualName::in_module`
  is how `scalars.rs` names `Basics.Int` without reading a file, and every test that spells a
  qualified name spells it without a package.
- **Only `canonical::Type`'s head carries it** — `Type::Type` holding a `ModuleName` (which
  already pairs a `PackageName` with a `Name`) plus the type's own name, or a small `TypeName`
  beside it. Narrower, since it touches only what the unifier compares, and it leaves
  `QualName` still ambiguous everywhere else it is used (`VarTopLevel`, `VarConstructor`,
  `VariableNotFound`), so the same bug can come back through a value rather than a type.

Either way this reaches **`DEC-15` decision 1**: a scalar is recognised by the bare qualified
name `Basics.Int`, and `scalars.rs` builds that name with `QualName::in_module("Basics",
"Int")`, which has no package to give it. What keeps that name pointing at one declaration
today is `resolve::CORE_PACKAGE` plus the collision rule — a package named `zelkova-core` is
seen unwrapped everywhere, so a second `Basics` is a collision. A packaged `QualName` would
let the scalar check name `zelkova-core` outright instead, which is stricter and is a decision
this ticket does not make either.

**Where this came from:** found in review of #226 (`LANG-14`). It was not fixed there because
neither shape above is a change `LANG-14` decided, and both touch `DEC-15`'s spelling of a
scalar and every test in the tree that writes a qualified name. [*What a package boundary
cannot rename*](../spec/packages.md#what-a-package-boundary-cannot-rename) is the section that
promises otherwise — "a type does not change identity by being mentioned in a file that spells
it short", "`AcmeWidgets.Size.Size` is one type wherever it is written" — and it carries a
**Not implemented:** paragraph naming this ticket, to be deleted when this lands.

**Acceptance:** a `tests/pipeline.rs` test over a fixture pair shaped like the repro above — a
package holding its own `Size` and depending, wrapped, on `acme-widgets` — where a function
annotated `Size.Size -> AcmeWidgets.Size.Size` with body `f s = s` fails to type check, naming
two types the user can tell apart in the message. `tests/fixtures/dep_widgets` is already the
dependency half. The existing boundary tests must stay green, in particular
`a_dependencys_module_is_imported_under_its_namespace` and
`a_dependencys_basics_collides_with_cores` — the second is what proves the scalar names still
resolve. `cargo run` must still print `parsed 8 modules`, list all eight as checked, and exit
0.
