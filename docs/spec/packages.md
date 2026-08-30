# Packages and source layout

A module is one file, and it is the unit of encapsulation
([Modules](modules.md)). A **package** is a directory of modules, and it is the unit of
distribution — the thing that has a name and a version, that depends on other things, and
that a compiler is pointed at. Every module belongs to exactly one package.

The two units nest: a module chooses
which of its declarations other modules may see; a package chooses which of its modules
other packages may see. A
package boundary is also where a module's name changes: what a package calls `Model`,
everything outside it calls `Todo.Model`.

## What a package is

A package is a directory containing a `zelkova.json` manifest. The manifest names the
package and describes it; a directory without one is not a package, and the compiler will 
refuse to compile a raw collection of source files. Beside the manifest are the two source roots,
`src/` and `tests/`.

```text
todo/
  zelkova.json
  zelkova.lock
  src/
    App.zel            module App
    Model.zel          module Model
    Model/
      Internal.zel     module Model.Internal
  tests/
    ModelTest.zel      module ModelTest
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

## Source roots

A package has two fixed source roots: `src/` and `tests/`.

`src/` holds the package's modules — what the package *is*, and the only thing it ships.
`tests/` holds modules that exercise them, compiled when this package's own tests are run
and at no other time; a package that depends on this one never reads a file under `tests/`.
[Tests](#tests) describes the second root in full. Everything below is true of both.

Every file under a root whose name ends in `.zel` is a module of the package, found by
walking that root recursively. That is true whether or not the package keeps the module
private and whether or not anything imports it: an unreferenced module still has to parse
and still has to type-check, so a package cannot carry a broken file by leaving it
unmentioned.

A module's name is its path under its root, with each directory separator written as a `.`
and the `.zel` dropped — the rule stated in full under
[Modules](modules.md#the-name-and-the-file). That is the module's name *within* its package,
which is what its neighbours import it by; how a package's modules are named from outside is
[Imports across a package boundary](#imports-across-a-package-boundary), below. Every
directory under a root is a segment of a module name, and must be spelled like one: an
uppercase-initial identifier. `src/js/Basics.zel` is not a legal file, because `js.Basics` is
not a legal module name.

The two roots share one set of names. `src/Model.zel` and `tests/Model.zel` are both `Model`,
which is a module name declared twice in one package, and that is an error — the same error
as declaring it twice under one root. A test module is imported by its name like any other.

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

`zelkova.json` is a JSON object with six fields, of which five are required:

```text
{
  "name": "todo",
  "version": "0.4.1",
  "main": "App",
  "private-modules": [ "Model.Internal" ],
  "dependencies": {
    "acme-widgets": { "version": "^1.2.0",
                      "git": "https://github.com/acme/widgets" },
    "acme-json":    { "version": "^0.4.1",
                      "git": "https://github.com/acme/json",
                      "wrapped": false }
  },
  "test-dependencies": {
    "acme-expect": { "version": "^2.0.0",
                     "git": "https://github.com/acme/expect" }
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

**`test-dependencies`** is the same map for packages the tests need and the package itself
does not; see [Tests](#tests). It is required and may be empty.

**Not implemented:** no part of this is read ([`docs/tickets/lang-13.md`](../tickets/lang-13.md)).
A manifest is not source text, so no tagged example can hold the compiler to any of it, and
the ticket carries the obligation to rewrite this section when it lands.

## What a package exposes

Every module of a package under `src/` is importable from outside it, except the ones
`private-modules` names. Those are **package-internal**: importable from other modules of the
same package and from nowhere else.


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
to an object, which says both what is wanted of that package and where it comes from:

```text
"dependencies": {
  "acme-widgets": { "version": "^1.2.0",
                    "git": "https://github.com/acme/widgets" },
  "acme-json":    { "version": "=0.4.1",
                    "git": "https://github.com/acme/json", "tag": "v0.4.1" },
  "acme-parser":  { "version": ">=2.1.0, <2.5.0",
                    "path": "../acme-parser" }
}
```

### The version constraint

`^x.y.z` is the ordinary form. It admits any version at or above `x.y.z` that does not change
the leftmost non-zero position — so `^1.2.0` allows `1.9.3` and not `2.0.0`, `^0.3.1` allows
`0.3.9` and not `0.4.0`, and `^0.0.4` allows nothing but `0.0.4`. The leftmost non-zero
position is where breaking changes go, and below `1.0.0` there is no major position for them
to go in. `=x.y.z` admits that version alone. A two-sided range admits everything between its
bounds, the lower inclusive and the upper exclusive.

### Where a dependency comes from

Every entry names a **source**, and names exactly one. There is no index anywhere that a bare
name could be looked up in, and so no version constraint alone is enough to find a package:
the manifest is where a package says where its dependencies are, and reading one file is
enough to know.

`git` is a repository URL. It may be accompanied by one of `tag`, `branch` or `rev`, and by at
most one; with none of them, the repository's default branch is used. `path` is a directory,
relative to the manifest that names it, holding a package being developed alongside this one.
Whichever is written, the version constraint still applies and is checked against the `version`
the package found there declares — a `path` dependency is a different way of obtaining a
package, not a way of skipping what is asked of it.

The key is the package's name and the fetched package's own `name` field must equal it.
Otherwise a manifest could call a package anything, and the namespace a dependent derives from
that key would name modules the package itself has never heard of.

Two entries anywhere in a build that give one name two different sources is an error, and so
is one name given a source and, elsewhere, another. A name is a package's identity for the
whole build, not a label local to the manifest that writes it, and two packages under one name
would collide in exactly the way [at most one version](#one-version-of-each) already forbids.

How a source is fetched, where the fetched copy is kept, and what a build does when the network
is unavailable are matters for the toolchain rather than the language, and the
[toolchain appendix](toolchain.md) describes them.

### `wrapped`

An entry may also carry `wrapped`, which is `true` unless written, and which decides
[how that dependency's modules are named](#unwrapping-a-dependency) in this package.

```text
"dependencies": {
  "acme-parser": { "version": ">=2.1.0, <2.5.0",
                   "path": "../acme-parser", "wrapped": false }
}
```

### One version of each

Two rules bind the resolved set of packages a build is made from. The package graph is
**acyclic** — a package may not depend on itself, directly or through a chain — for the same
reason the module graph is ([Modules](modules.md#imports-may-not-form-a-cycle)): there is no
order in which the members of a cycle could be compiled. And **at most one version of a
package** is in a build. Two versions of one package would present the same namespace to the
same importers.

Beyond satisfying every constraint, which version a resolver picks is not a question the
language answers. What it picked is recorded in `zelkova.lock`, generated beside the
manifest: the manifest says what is acceptable and the lock file says what was chosen, so a
build is reproducible without the manifest having to be rewritten to pin it. Its contents are
[in the toolchain appendix](toolchain.md#resolution-and-zelkovalock).

### Only direct dependencies are usable

Only a package listed in `dependencies` is importable within this package.
So the packages a module may draw on are exactly the ones written in the manifest
beside it, and a reader never has to walk a dependency chain to find out where a module could
have come from. It also means upgrading a dependency cannot change what any import in your own
package resolves to.

### `zelkova-core` is a dependency of every package

Every module behaves as though it began with a fixed list of imports, drawn from `Basics`,
`Maybe`, `Result`, `List`, `Char`, `String` and `Tuple` — the list is
[in the Modules chapter](modules.md#the-default-imports). Those modules belong to
`zelkova-core`, which is a dependency of every package and is not written in `dependencies`.
Its version is the compiler's.

It is seen unwrapped, in every package. So `Basics` is `Basics` and `List` is `List` everywhere in the language. The names of
core's public modules are therefore taken in every package — a module of your own called
`List` would be [a second module answering to one name](#when-two-modules-answer-to-one-name).

## Imports across a package boundary

An `import` names a module, never a package. Which modules a name can reach is decided by the
manifest — the package's own modules, and the public modules of its direct dependencies — and
a package's modules are named from outside through that package's **namespace**.

### The namespace

A package's namespace is derived from its name: split it at the hyphens, uppercase the first
letter of each piece, and join. `acme-widgets` is `AcmeWidgets`, `todo` is `Todo`.
A dependent names one of its modules by writing the namespace, a `.`,
and the module's name within its package.

```text
acme-widgets/
  src/
    Size.zel        module Size, and AcmeWidgets.Size to a dependent
    Style/
      Dark.zel      module Style.Dark, and AcmeWidgets.Style.Dark to a dependent
```

Nothing has to be checked for two packages to keep out of each other's way. A package name is
unique in a build, so distinct package names
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

A package that finds the prefix costs more than it is worth may drop it, per dependency,
by writing `"wrapped": false` in that dependency's entry. That package's modules are then
named by their own names in every file of the depending package: `Size`, not
`AcmeWidgets.Size`.

```text
"dependencies": {
  "acme-widgets": { "version": "^1.2.0",
                    "git": "https://github.com/acme/widgets", "wrapped": false }
}
```

Unwrapping is a property of the pair (this package, that dependency) and not of the
dependency itself. It is written by the package doing the importing because that is where the
cost of the prefix is paid: a package used once in a file and a package used on every other
line are the same package, and only its user knows which it is. Two packages depending on one
package may spell it differently, and neither choice is visible to anyone else. A package
cannot decide how it is spelled by its dependents, and does not need to.

A module has exactly one spelling in any file. Wrapped, `Size` names nothing; unwrapped,
`AcmeWidgets.Size` names nothing. Two ways to write one import would put two prefixes on one
module, which is the thing [one module, one import](modules.md#one-module-one-import) exists
to prevent.

### A local spelling stays local

A module's **name** is its namespace and its path within its package: `AcmeWidgets.Size`. That
is its identity, everywhere, and it is what every package other than the one holding it sees.

Unwrapping and `as` produce **spellings**, and a spelling is a convenience the file or the
package granting it keeps to itself. A file that writes `import AcmeWidgets.Size as Size` has
chosen how to write that name in that file; a package that unwraps `acme-widgets` has chosen
how to write it throughout its own source. Neither choice travels. What a module offers to
whoever imports it is written in canonical names, so a type does not change identity by being
mentioned in a file that spells it short, and two packages that spell one dependency
differently still agree about every type in it.

Three things follow, and together they answer what a package boundary can and cannot rename.

**A package cannot rename a module it exposes.** A public module's name is its path, so
`src/Model/Internal.zel` is `Todo.Model.Internal` to everyone outside `todo` and cannot be
presented as `Todo.Parser`. Moving the file is the only way to change the name, and it is a
breaking change for the same reason renaming an exposed function is.

**A package cannot re-export a module of a dependency as one of its own.** `todo` may depend
on `acme-widgets` and use it throughout, but `AcmeWidgets.Size` is a module of `acme-widgets`
under every spelling and there is no `Todo.Size` to be had. A package's public surface is its
own modules and nothing else, which is what makes the namespace prefix reliable: the package a
module comes from is readable from the name.

**A signature that names a dependency's type asks the consumer for that dependency.** Values
flow freely — a consumer can call the function, hold the result and pass it on, knowing nothing
about where its type was declared. Writing that type down is what needs the dependency, and a
consumer adds it to its own manifest, at which point both packages mean the same type: one
version of `acme-widgets` is in the build, so `AcmeWidgets.Size.Size` is one type wherever it
is written.

```text
-- todo exposes:   resize : Task -> AcmeWidgets.Size.Size

-- in a package depending on todo alone:
import Todo.Model

bigger = Todo.Model.resize task            -- fine

f : Todo.Model.Task -> AcmeWidgets.Size.Size
                       ^^^^^^^^^^^^^^^^^^^^
-- error: there is no module named `AcmeWidgets.Size`
--   `acme-widgets` is not a dependency of this package
```

That cost falls on whoever exposes the type, and it is the design pressure it looks like: a
type in a public signature is part of the package's interface, and a package that puts a
dependency's type there has made that dependency part of what it asks of its users. Wrapping
the type in one of its own is how a package chooses not to.

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

## Tests

`tests/` is the package's second source root. A module under it is an ordinary module, with
one difference: it is compiled when this package's own tests are run, and never otherwise.
A package that depends on this one does not compile it, does not resolve what it needs, and
cannot observe that it exists.

A test module may import any module of its own package, the private ones included.
A package whose internals
could only be tested through its public surface would be pushed into exposing them. It may
also import the public modules of the package's dependencies and of its test-dependencies.

Nothing may import a test module. A module under `src/` cannot, which is what keeps `tests/`
out of what the package ships.

### `test-dependencies`

`test-dependencies` maps package names to entries of exactly the shape `dependencies` takes.
It is required and may be empty.

A package listed there is available to `tests/` and to nothing else.

A package name appears in at most one of the two maps. Anything already in
`dependencies` is usable from `tests/` without being written twice.

The rest of the resolution rules are unchanged and apply to the union of the two maps. The
graph stays acyclic, at most one version of each package is in the build, only direct
dependencies are usable, and `zelkova.lock` records what was chosen for both.

**Not implemented:** the compiler has one source root and no notion of a test at all
([`docs/tickets/lang-15.md`](../tickets/lang-15.md)).

## Programs

A package whose manifest has no `main` field is a library: it is something other packages
depend on, and it does not run.

`main` names one module, by its name within the package, which must be a module under `src/`
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
- **What a test is.** A module under `tests/` is compiled, and nothing yet says what makes a
  declaration in it something a runner will run: an exposed value of a particular type, a
  naming convention, or a declaration form the language does not have. It waits on the same
  design `main`'s type waits on, since both are a value the outside world picks up and acts
  on. How a runner is invoked, and what it reports, are
  [toolchain](toolchain.md#running-a-packages-tests) questions rather than language ones.
