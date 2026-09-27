# BUG-37 · A package is not part of a type's identity, so two packages' same-named modules are one type

**Severity:** high (miscompile — two unrelated union types unify silently, and the build
reports success)

**Blocks:** [`BUG-40`](bug-40.md), whose constructor half needs a constructor's package, which
only this ticket puts anywhere.

**Location:** `src/compiler/name.rs` — `QualName`, whose fields are `module: Vec<String>` and
`name: String` and nothing else. Everything keyed by one inherits the ambiguity:

- `src/compiler/canonical/mod.rs` — `Type::Type(QualName, Vec<Type>)`, the head the typer
  unifies on; `ExpressionKind::VarConstructor(qname, _)`, which carries no package where
  `VarForeign(qname, package, _)` does; and `ModuleName`, which *does* pair a `PackageName`
  with a `Name` but is not what a `Type` holds.
- `src/compiler/typer/mod.rs` — `Translation::of` (~line 751) inserts every interface's unions
  and then the module's own into one `HashMap` keyed by `QualName`; `constructors_of` (~line
  816) derives `Translation::constructors` from it, keyed the same way.
- `src/compiler/ir/mod.rs` — `Constructor::union`, a `QualName`.
- `src/compiler/scalars.rs` — `Scalar::declares` and `Scalar::qual_name`, which recognise and
  build a scalar's name from a module string and a type name.
- `src/compiler/resolve.rs` — `visible_modules`, whose collision rule is keyed by the spelling
  an importing package reaches a module by.

**Problem:** a module's name within its package is not unique in a build, and `QualName` has
nothing else in it. `visible_modules` reports two modules answering to **one spelling**, so a
local module `Size` and a wrapped dependency's `AcmeWidgets.Size` are two different spellings
and no collision is raised — correctly, since the spec says that arrangement is legal. But the
`Interface` inserted for the dependency carries `module_name.name() == "Size"`, and a type
annotated `AcmeWidgets.Size.Size` and one annotated `Size.Size` both canonicalize to the one
`QualName` `Size.Size`. Four symptoms follow from that one cause.

**Two unrelated types unify.** Reproduced against `tests/fixtures/dep_widgets` (whose
`Size.zel` declares `type Size = Small | Large`), as a wrapped dependency of a package
`ident-app` that also holds `src/Size.zel` with `type Size = Mine`:

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

**A well-typed constructor is rejected.** In the same pair, module `Size` of the app writing
`theirs : AcmeWidgets.Size.Size` and `theirs = AcmeWidgets.Size.Small` fails with
`Emit([Unchecked { name: "theirs" }])`: `Translation::of` inserts the module's own `Size.Size`
over the dependency's, so `Small` is no constructor of any union the typer can see.

**Which union wins can change between runs.** `App` above imports both modules, so both
interfaces insert `Size.Size`, and `Translation::of` walks `interfaces.values()` — a
`HashMap` with a randomly seeded hasher. Whichever interface is walked last owns the key, so a
`case` on `AcmeWidgets.Size.Small` in `App` can check on one run and fail on the next. This is
read from the code, not reproduced.

**A dependency's own `Basics` declares the scalar `Int`.** Reproduced with
`tests/fixtures/dep_rival_basics` (`acme-basics`, whose `Basics.zel` is `type Int = Int`)
as a **wrapped** dependency beside `zelkova-core` — no collision, since the two spellings are
`Basics` and `AcmeBasics.Basics`:

```zel
module App exposing (f, g)

import AcmeBasics.Basics


f : AcmeBasics.Basics.Int -> Int
f x = x


g : AcmeBasics.Basics.Int
g = 1
```

`compile_package_into` returns `Ok(())` and emits `const g = 1n;`. `acme-basics`' `Int` has the
qualified name `Basics.Int`, which `Scalar::declares` takes for the scalar; the literal `1`
checks against it, and `f` passes it off as the language's `Int`. [*Scalar
types*](../spec/types.md#scalar-types) says a scalar is identified "by where it is declared",
and this one is declared in the wrong package.

This is [`AST-4`](README.md) and `BUG-35` one level up: the typer stopped identifying a union
by its *unqualified* name, and now identifies it by a qualified name that is only unique
**within one package**. Before `LANG-14` a build held exactly one package, so none of the
programs above could be written; `LANG-14` is what made them reachable.

**Fix:** **`QualName` carries a `PackageName`.** Every `QualName` then answers "which
declaration" on its own, which is what its own doc comment already claims ("once we are given
a `QualName`, no further resolution is necessary").

Putting the package on `canonical::Type`'s head alone was the other shape considered, and it
is ruled out: it fixes what the unifier compares and nothing else. The constructor symptom
above goes through `Translation::constructors` and `VarConstructor`, not a type head, and
[`BUG-40`](bug-40.md) needs the package on `ir::Constructor` to name a hoisted constant. The
narrow shape leaves all three ambiguous.

What follows from the shape:

- **`QualName::parse`, `QualName::from_strs` and `QualName::in_module`** build one from text
  that has no package in it. Each takes a `PackageName` or goes; a test that spells a qualified
  name spells its package too, through a helper in `tests/support/mod.rs` beside
  `test_package()` rather than by hand at every site.
- **`VarConstructor` gains its package**, the way `VarForeign` already carries one, and so does
  `ir::Constructor`, through its `union: QualName`.
- **A scalar is named in `zelkova-core`** — [`DEC-15` decision
  1](../decisions/dec-15.md#1--a-scalar-type-is-known-by-its-qualified-name), as amended on
  2026-09-26. `scalars.rs` builds each of the five from `resolve::CORE_PACKAGE`, the one
  package name the compiler already knows on its own, and `Scalar::declares` compares the
  package too. What keeps a scalar's name pointing at one declaration is then its identity,
  not the collision rule in `visible_modules`.
- **A message names a type as the package being compiled spells it.** `AdtNames` already
  qualifies every union in a message when two of them share a bare name; with a package in the
  identity, two can also share a *qualified* name, and `Size.Size` against `Size.Size` is no
  better than `Size` against `Size`. When that happens, each union is written by the spelling
  the checked package reaches its module by — `AcmeWidgets.Size.Size` for the wrapped
  dependency's, `Size.Size` for the local one — which is the key `compile_package` already
  stores its interface under in the map it hands `type_check`. Never the package name itself:
  `acme-widgets` is a spelling no Zelkova source contains.

**Where this came from:** found in review of #226 (`LANG-14`). It was not fixed there because
neither shape was a change `LANG-14` decided, and both touch `DEC-15`'s spelling of a scalar
and every test in the tree that writes a qualified name. [*What a package boundary cannot
rename*](../spec/packages.md#what-a-package-boundary-cannot-rename) is the section that
promises otherwise — "a type does not change identity by being mentioned in a file that spells
it short", "`AcmeWidgets.Size.Size` is one type wherever it is written" — and it carries a
**Not implemented:** paragraph naming this ticket, to be deleted when this lands.

**Acceptance:** `tests/pipeline.rs` tests over fixture pairs shaped like the repros above:

- A package holding its own `Size` and depending, wrapped, on `acme-widgets`, where a function
  annotated `Size.Size -> AcmeWidgets.Size.Size` with body `f s = s` fails to type check. The
  message names `Size.Size` and `AcmeWidgets.Size.Size`, asserted on its text.
- In the same pair, the app's module `Size` writing `theirs = AcmeWidgets.Size.Small` at
  `AcmeWidgets.Size.Size` type checks. [`BUG-40`](bug-40.md)'s constructor test compiles this
  same module, so the two tickets share the fixture.
- A package depending, wrapped, on `acme-basics` beside `zelkova-core`, where `f :
  AcmeBasics.Basics.Int -> Int` with body `f x = x` fails to type check, and so does `g :
  AcmeBasics.Basics.Int` with body `g = 1`.

The existing boundary tests stay green, in particular
`a_dependencys_module_is_imported_under_its_namespace` and
`a_dependencys_basics_collides_with_cores`. `cargo run` must still print `parsed 8 modules`,
list all eight as checked, and exit 0 — which is what proves `std/core`'s scalars still
resolve once they are named in `zelkova-core`.
