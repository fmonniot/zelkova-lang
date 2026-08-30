# Packages and source layout

A module is one file, and it is the unit of encapsulation
([Modules](modules.md)). A **package** is a directory of modules, and it is the unit of
distribution — the thing that has a name and a version, that depends on other things, and
that a compiler is pointed at. Every module belongs to exactly one package, and no module
belongs to two.

The two units nest, and each has its own way of saying what is public. A module chooses
which of its declarations other modules may see; a package chooses which of its modules
other packages may see. Neither can be worked around from the other side: a name a module
does not expose is unreachable even from inside its own package, and a module a package
keeps private is unreachable from outside even though every name in it is exposed. A
package boundary is also where a module's name changes: what a package calls `Model`,
everything outside it calls `Todo.Model`.

## What a package is

A package is a directory containing a `zelkova.json` manifest. The manifest names the
package and describes it; a directory without one is not a package, and there is no way to
compile a loose collection of source files. Beside the manifest is `src/`, which holds the
modules.

```text
todo/
  zelkova.json
  zelkova.lock
  src/
    App.zel          module App
    Model.zel        module Model
    Model/
      Internal.zel   module Model.Internal
```

Requiring the manifest is what gives those three questions an answer for every package
without exception. A package always has a name, so a diagnostic can say which package a
module came from; it always has a stated public surface, so "may I import this" is
decidable; and it always has a dependency list, so "where could this module have come from"
is answerable by reading one file.

**Not implemented:** the compiler reads no manifest. It is handed a source directory
directly, and the name of the package it is compiling is fixed at the call site rather than
declared — so every module it compiles, from any directory, belongs to one package with a
name nothing wrote down ([`docs/tickets/lang-13.md`](../tickets/lang-13.md)).

## The source root

`src/` is the package's only source root. It is not configurable, and a package cannot have
a second one.

Every file under `src/` whose name ends in `.zel` is a module of the package, found by
walking `src/` recursively. That is true whether or not the package keeps the module private
and whether or not anything imports it: an unreferenced module still has to parse and still
has to type-check, so a package cannot carry a broken file by leaving it unmentioned.

A module's name is its path under `src/`, with each directory separator written as a `.` and
the `.zel` dropped — the rule stated in full under
[Modules](modules.md#the-name-and-the-file). That is the module's name *within* its package,
which is what its neighbours import it by; how a package's modules are named from outside is
[Imports across a package boundary](#imports-across-a-package-boundary), below. Every
directory under `src/` is a segment of a module name, and must be spelled like one: an
uppercase-initial identifier. `src/js/Basics.zel` is not a legal file, because `js.Basics` is
not a legal module name.

**Known gap:** neither half is checked, and the path-derived name is computed and then
discarded ([`docs/tickets/lang-6.md`](../tickets/lang-6.md)). A file may declare any module
name regardless of where it sits, so a lowercase directory is accepted today.

A file whose name does not end in `.zel` is not a module and is not read. That is what
allows a `module javascript` facade's companion `.mjs` file to sit next to it in the same
directory, which is where [JS interop](js-interop.md) requires it to be:

```text
src/
  Js/
    Basics.zel     module javascript Js.Basics
    Basics.mjs     the JavaScript behind it
```

## The manifest

`zelkova.json` is a JSON object with five fields, of which four are required:

```text
{
  "name": "todo",
  "version": "0.4.1",
  "main": "App",
  "private-modules": [ "Model.Internal" ],
  "dependencies": {
    "zelkova-widgets": "^1.2.0",
    "acme-json": { "version": "^0.4.1", "wrapped": false }
  }
}
```

**`name`** is one identifier: ASCII lowercase letters, digits and hyphens. It begins with a
letter, and every hyphen is followed by a letter. It is not a dotted name and has no author
segment. A package name and a module name are never confusable — one is lowercase-with-hyphens,
the other uppercase-with-dots — and the shape of a package name is what makes the namespace it
[derives](#the-namespace) unambiguous.

**`version`** is exactly three non-negative integers separated by dots. There are no
pre-release suffixes and no build metadata.

**`main`** is optional and names the module holding a program's entry point; see
[Programs](#programs).

**`private-modules`** lists the modules other packages may not import. It is required, and
it may be empty — a package that keeps nothing private writes `[]`, so that a reader can tell
a wholly public package from a forgotten field. Every entry must name a module the package
actually holds.

**`dependencies`** maps package names to what is wanted of each; see
[Dependencies](#dependencies). It is required and may be empty.

**Not implemented:** no part of this is read ([`docs/tickets/lang-13.md`](../tickets/lang-13.md)).
A manifest is not source text, so no tagged example can hold the compiler to any of it, and
the ticket carries the obligation to rewrite this section when it lands.

## What a package exposes

Every module of a package is importable from outside it, except the ones `private-modules`
names. Those are **package-internal**: importable from other modules of the same package and
from nowhere else, no matter what their own `exposing` lists say.

A module is written to be used, and in most packages most modules are; the manifest records
the exceptions rather than restating the rule. What that asks of a package is that a module
meant to stay internal is listed in the change that creates it — a module is importable the
moment it exists, and a package that lists it later is taking something away from whoever
imported it in between.

Within that, a package's internal layout stays changeable. A package can split a private
module in two, rename one, or introduce a helper without any of it being a breaking change,
as long as the public modules keep exposing what they exposed.

A `module javascript` facade is never importable from outside, whatever the manifest says,
because its guarantees are about a companion `.mjs` file that ships with the package that
declares it — [JS interop](js-interop.md) is where that rule and its reasoning live. Listing
a facade in `private-modules` changes nothing; the declaration itself is what makes it
internal.

```zel expect=ok
module javascript Js.Widget exposing (measure)

measure : a -> a
```

**Not implemented:** nothing consults `private-modules`, because nothing in the compiler
represents a package other than the one being compiled
([`docs/tickets/lang-14.md`](../tickets/lang-14.md)).

## Dependencies

A package may only use another package it has declared. `dependencies` maps a package name
to a version constraint, in one of three forms:

```text
"dependencies": {
  "zelkova-widgets": "^1.2.0",
  "fmonniot-json":   "=0.4.1",
  "acme-parser":     ">=2.1.0, <2.5.0"
}
```

`^x.y.z` is the ordinary form. It admits any version at or above `x.y.z` that does not change
the leftmost non-zero position — so `^1.2.0` allows `1.9.3` and not `2.0.0`, `^0.3.1` allows
`0.3.9` and not `0.4.0`, and `^0.0.4` allows nothing but `0.0.4`. The leftmost non-zero
position is where breaking changes go, and below `1.0.0` there is no major position for them
to go in. `=x.y.z` admits that version alone. A two-sided range admits everything between its
bounds, the lower inclusive and the upper exclusive.

A dependency may also be written as an object, which is the same constraint under a `version`
field plus anything else the depending package wants to say about it. One thing is sayable
today: `wrapped`, which is `true` unless written, and which decides
[how that dependency's modules are named](#unwrapping-a-dependency) in this package.

```text
"dependencies": {
  "acme-parser": { "version": ">=2.1.0, <2.5.0", "wrapped": false }
}
```

Two rules bind the resolved set of packages a build is made from. The package graph is
**acyclic** — a package may not depend on itself, directly or through a chain — for the same
reason the module graph is ([Modules](modules.md#imports-may-not-form-a-cycle)): there is no
order in which the members of a cycle could be compiled. And **at most one version of a
package** is in a build. Two versions of one package would present the same namespace to the
same importers, and a namespace is what an import resolves through, so the second copy would
be unreachable under any spelling.

Beyond satisfying every constraint, which version a resolver picks is not a question the
language answers. What it picked is recorded in `zelkova.lock`, generated beside the
manifest: the manifest says what is acceptable and the lock file says what was chosen, so a
build is reproducible without the manifest having to be rewritten to pin it.

**Only direct dependencies are usable.** A package listed in `dependencies` is importable; a
package reached only through one of those is not. So the packages a module may draw on are
exactly the ones written in the manifest beside it, and a reader never has to walk a
dependency chain to find out where a module could have come from. It also means upgrading a
dependency cannot change what any import in your own package resolves to.

### `zelkova-core` is a dependency of every package

Every module behaves as though it began with a fixed list of imports, drawn from `Basics`,
`Maybe`, `Result`, `List`, `Char`, `String` and `Tuple` — the list is
[in the Modules chapter](modules.md#the-default-imports). Those modules belong to
`zelkova-core`, which is a dependency of every package and is not written in `dependencies`.
Its version is the compiler's.

It is seen unwrapped, in every package, and that is not a property of the package but of the
dependency on it: there is no entry in `dependencies` in which a `wrapped` field could be
written, so `Basics` is `Basics` and `List` is `List` everywhere in the language. The names of
core's public modules are therefore taken in every package — a module of your own called
`List` would be [a second module answering to one name](#when-two-modules-answer-to-one-name).

Nothing else about it is special. `zelkova-core` is an ordinary package with an ordinary
manifest, its facades are package-internal like any other package's, and a package that wants
one of its modules beyond the default list imports it in the ordinary way.

## Imports across a package boundary

An `import` names a module, never a package. Which modules a name can reach is decided by the
manifest — the package's own modules, and the public modules of its direct dependencies — and
a package's modules are named from outside through that package's **namespace**.

### The namespace

A package's namespace is derived from its name: split it at the hyphens, uppercase the first
letter of each piece, and join. `acme-widgets` is `AcmeWidgets`, `fmonniot-json` is
`FmonniotJson`, `todo` is `Todo`. A dependent names one of its modules by writing the
namespace, a `.`, and the module's name within its package.

```text
acme-widgets/
  src/
    Size.zel        module Size, and AcmeWidgets.Size to a dependent
    Style/
      Dark.zel      module Style.Dark, and AcmeWidgets.Style.Dark to a dependent
```

Nothing has to be checked for two packages to keep out of each other's way. A package name is
unique in a build, and the derivation is reversible — every uppercase letter marks a piece
boundary, which is what requiring a letter after each hyphen buys — so distinct package names
give distinct namespaces, and no two wrapped modules can be reached by one name.

The namespace is not a directory and does not appear under `src/`. Nor does a package ever
write its own: inside `acme-widgets`, `Size` is `Size`, and `AcmeWidgets.Size` names nothing.
A module's name within its package is the one thing its file path decides, and the namespace
is added at the boundary by whoever crosses it.

```zel expect=unimplemented
module App exposing (start)

import AcmeWidgets.Size

start : AcmeWidgets.Size.Size
start = AcmeWidgets.Size.small
```

The prefix an import brings into scope is the module's name as written, here all of
`AcmeWidgets.Size`. `as` shortens it, exactly as it does for a neighbour, and is how a file
that leans on one module of a dependency keeps its uses short:

```zel expect=unimplemented
module App exposing (start)

import AcmeWidgets.Size as Size

start : Size.Size
start = Size.small
```

Within one package nothing is prefixed, because no boundary is crossed:

```zel expect=ok package=one-package
module Model exposing (Task, empty)

type Task
  = Task

empty : Task
empty = Task
```

```zel expect=ok package=one-package
module App exposing (start)

import Model

start : Model.Task
start = Model.empty
```

A name that is neither a module of the package nor a public module of a dependency is an
error:

```zel expect=canonical-error:EnvironmentErrors
module App exposing (start)

import Elsewhere

type Task
  = Task

start : Task
start = Task
```

### Unwrapping a dependency

A package that finds the prefix costs more than it is worth may drop it, for one dependency,
by writing `"wrapped": false` in that dependency's entry. That package's modules are then
named by their own names in every file of the depending package: `Size`, not
`AcmeWidgets.Size`.

```text
"dependencies": {
  "acme-widgets": { "version": "^1.2.0", "wrapped": false }
}
```

Unwrapping is a property of the pair — this package, that dependency — and not of the
dependency itself. It is written by the package doing the importing because that is where the
cost of the prefix is paid: a package used once in a file and a package used on every other
line are the same package, and only its user knows which it is. Two packages depending on one
package may spell it differently, and neither choice is visible to anyone else. A package
cannot decide how it is spelled by its dependents, and does not need to.

A module has exactly one spelling in any file. Wrapped, `Size` names nothing; unwrapped,
`AcmeWidgets.Size` names nothing. Two ways to write one import would put two prefixes on one
module, which is the thing [one module, one import](modules.md#one-module-one-import) exists
to prevent.

### When two modules answer to one name

Two wrapped dependencies cannot collide, whatever they contain, and that is what the namespace
is for. Unwrapping is what puts names in the same space: an unwrapped dependency's modules sit
beside the depending package's own modules, beside `zelkova-core`'s, and beside any other
unwrapped dependency's.

Two modules answering to one name in one package is an error. It is reported when the build is
resolved, before any module of that package is compiled, and it names both modules and the
packages they come from:

```text
error: two modules are named `Size` in package `todo`
  acme-widgets 1.2.0, imported unwrapped
  fmonniot-ui 3.0.1, imported unwrapped
```

It is not reported at an `import` line, and not only in the files that would have been
ambiguous. The manifest is what created the ambiguity and the manifest is what has to change —
by wrapping one of the two, or by renaming the package's own module — so a build that has no
coherent answer for a name is stopped before any file is read for one.

**Not implemented:** the compiler compiles one package and has no representation of another,
so no namespace is ever applied, nothing is unwrappable, and the two blocks above that import
`AcmeWidgets.Size` fail on a module that cannot be found
([`docs/tickets/lang-14.md`](../tickets/lang-14.md)).

## Programs

A package whose manifest has no `main` field is a library: it is something other packages
depend on, and it does not run.

`main` names one module, by its name within the package, which must be a module of the package
and must expose a value called `main`. Naming it in the manifest rather than fixing it by
convention means the entry point is findable in the same file as everything else about the
package, and a module can be a program's entry point without its name having to say so.

A package can be both. `main` and `private-modules` are independent, so a program may also be
depended on as a library, and the module holding `main` may be one of the private ones.

**Not implemented:** the field is not read, and nothing yet turns a package into something
that runs ([`docs/tickets/lang-13.md`](../tickets/lang-13.md)).

## Open questions

- **What type `main` must have.** A program is more than a value of an arbitrary type — it
  has to describe how it starts, what it reacts to and what it does to the outside world —
  and the type that says so is undesigned.
- **Where a dependency's code lives.** How a resolved package's source or compiled interfaces
  reach the machine doing the compiling, and where they sit once they do, is a question about
  a toolchain rather than about the language.
- **Dependencies needed only for tests.** There is one `dependencies` map, so a package that
  wants a library for its tests alone must ship that dependency to everyone who uses it.
- **Whether a package may present a module under a name of its choosing.** The namespace
  prefix is the only renaming a package boundary does, and it is mechanical, so a package
  cannot present `Model.Internal` as `Todo.Parser`, and a public module's path is its name.
  Nor can a package re-export a module of a dependency as one of its own.
