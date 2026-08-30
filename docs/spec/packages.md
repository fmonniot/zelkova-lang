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
does not expose is unreachable from outside even though every name in it is exposed.

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
    Todo/
      App.zel        module Todo.App
      Model.zel      module Todo.Model
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
walking `src/` recursively. That is true whether or not the package exposes the module and
whether or not anything imports it: an unreferenced module still has to parse and still has
to type-check, so a package cannot carry a broken file by leaving it unmentioned.

A module's name is its path under `src/`, with each directory separator written as a `.` and
the `.zel` dropped — the rule stated in full under
[Modules](modules.md#the-name-and-the-file). Every directory under `src/` is therefore a
segment of a module name, and must be spelled like one: an uppercase-initial identifier.
`src/js/Basics.zel` is not a legal file, because `js.Basics` is not a legal module name.

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
  "main": "Todo.App",
  "exposed-modules": [ "Todo.Model" ],
  "dependencies": {
    "zelkova-widgets": "^1.2.0"
  }
}
```

**`name`** is one identifier: ASCII lowercase letters, digits and hyphens, beginning with a
letter. It is not a dotted name and has no author segment. A package name and a module name
are never confusable — one is lowercase-with-hyphens, the other uppercase-with-dots — which
is what makes `zelkova-widgets:Widget` readable in the one place both appear.

**`version`** is exactly three non-negative integers separated by dots. There are no
pre-release suffixes and no build metadata.

**`main`** is optional and names the module holding a program's entry point; see
[Programs](#programs).

**`exposed-modules`** lists the modules other packages may import. It is required, and it
may be empty — a package that exposes nothing writes `[]`, so that a reader can tell an
empty surface from a forgotten field, exactly as an empty `exposing ()` does for a module.
Every entry must name a module the package actually holds.

**`dependencies`** maps package names to version constraints; see
[Dependencies](#dependencies). It is required and may be empty.

**Not implemented:** no part of this is read ([`docs/tickets/lang-13.md`](../tickets/lang-13.md)).
A manifest is not source text, so no tagged example can hold the compiler to any of it, and
the ticket carries the obligation to rewrite this section when it lands.

## What a package exposes

`exposed-modules` is the whole of a package's public surface. A module not listed there is
**package-internal**: importable from other modules of the same package and from nowhere
else, no matter what its own `exposing` list says.

That is what makes a package's internal layout changeable. A package can split a module in
two, rename one, or introduce a helper without any of it being a breaking change, as long as
the listed modules keep exposing what they exposed. Without the list, every file in the
package would be part of its interface the moment it was written.

A `module javascript` facade is never listed. A facade is usable only inside the package that
declares it, because its guarantees are about a companion `.mjs` file that ships with that
package — [JS interop](js-interop.md) is where that rule and its reasoning live. Nothing
about the facade's own syntax marks it as internal; the manifest simply does not name it.

```zel expect=ok
module javascript Js.Widget exposing (measure)

measure : a -> a
```

**Not implemented:** nothing consults `exposed-modules`, because nothing in the compiler
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

Two rules bind the resolved set of packages a build is made from. The package graph is
**acyclic** — a package may not depend on itself, directly or through a chain — for the same
reason the module graph is ([Modules](modules.md#imports-may-not-form-a-cycle)): there is no
order in which the members of a cycle could be compiled. And **at most one version of a
package** is in a build. Two versions of one package would expose the same module names to
the same importers, and module names are what an import resolves against, so the second copy
would be unreachable under any spelling.

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
[in the Modules chapter](modules.md#the-default-imports). Those modules are exposed by
`zelkova-core`, so that package is a dependency of every package and is not written in
`dependencies`. Its version is the compiler's.

Nothing else about it is special. `zelkova-core` is an ordinary package with an ordinary
manifest, its facades are package-internal like any other package's, and a package that wants
one of its modules beyond the default list imports it in the ordinary way.

## Imports across a package boundary

Module names are **one flat namespace** across the package being compiled and the exposed
modules of its direct dependencies. An `import` names a module, not a package:

```zel expect=ok package=one-package
module Todo.Model exposing (Task, empty)

type Task
  = Task

empty : Task
empty = Task
```

```zel expect=ok package=one-package
module Todo.App exposing (start)

import Todo.Model

start : Todo.Model.Task
start = Todo.Model.empty
```

That is the ordinary spelling and it is the same one whether the module is a neighbour or
comes from a dependency. A module that is neither is an error:

```zel expect=canonical-error:EnvironmentErrors
module Todo.App exposing (start)

import Elsewhere

type Task
  = Task

start : Task
start = Task
```

### Naming the package

Two importable packages can expose a module of the same name, and that is not in itself an
error — a package cannot be expected to know what its dependents' other dependencies are
called. It becomes an error at the `import` line that is actually ambiguous, and the error
names both packages.

The fix is to write the package before the module, separated by a `:`. A package name is
unique in a build, so naming it always picks exactly one module — including when the two
modules have the same name:

```zel expect=unimplemented
module Todo.App exposing (start)

import acme-widgets:Widget

start : Widget.Size
start = Widget.small
```

The qualified form is only about *which* module is meant. The prefix it brings into scope is
still the module's own name, so importing both of a colliding pair into one file needs an
`as` on one of them — the rule that one prefix may name only one module
([Modules](modules.md#one-module-one-import)) applies here unchanged:

```zel expect=unimplemented
module Todo.App exposing (start)

import acme-widgets:Widget
import fmonniot-ui:Widget as UiWidget

start : Widget.Size
start = Widget.small
```

Everything else about an `import` is unaffected: `as` and `exposing` work as they do on a
bare import, in the same order, and the package prefix may be written even when nothing is
ambiguous.

**Not implemented:** a package name cannot be written before a module name — the two blocks
above are rejected on the package name, where the grammar expects a module name. Nor is there
anything to be ambiguous *between*: the compiler compiles one package and has no
representation of another ([`docs/tickets/lang-14.md`](../tickets/lang-14.md)).

## Programs

A package whose manifest has no `main` field is a library: it is something other packages
depend on, and it does not run.

`main` names one module, which must be a module of the package and must expose a value called
`main`. Naming it in the manifest rather than fixing it by convention means the entry point is
findable in the same file as everything else about the package, and a module can be a
program's entry point without its name having to say so.

A package can be both. `main` and `exposed-modules` are independent, so a program may also be
depended on as a library, and the module holding `main` need not be one of the exposed ones.

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
- **Whether a package may rename a module it exposes.** `exposed-modules` names modules by
  their own names, so a package cannot present `Todo.Internal.Parser` to the world as
  `Todo.Parser`, and a module's public name is therefore also its path.
