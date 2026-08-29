# Modules, exposing and imports

Every Zelkova source file is exactly one module, and every module is exactly one source
file. A module names itself, says which of its declarations the rest of the world may
see, and names the other modules it draws on. Those three things are the module header
and the `import` declarations under it, and they are what this chapter specifies.

There is no other unit of encapsulation. Zelkova has no nested modules, no namespaces
within a file, and no way to reopen a module from somewhere else — which means the
questions "where does this name come from" and "who is allowed to use this" both have
answers a reader can find by looking at the top of one file.

## The module header

The first token of a file is the keyword `module`, in column 1 — [Layout](layout.md#a-file-starts-at-column-1)
carries that rule and the reasoning for it. What follows is the module's name and its
`exposing` list:

```zel expect=ok
module Widget exposing (label)

label = 1
```

A module name is one or more segments separated by `.`, each an uppercase-initial
identifier. The dots are punctuation inside one name rather than a path through
anything: `Ui.Widget` is a single identifier, and there is no module `Ui` implied by it —
importing `Ui.Widget` gives `Ui` no meaning of its own. Where the file lives does mirror
those dots, which is the next section, but that is a rule about files and not about how
the name resolves.

```zel expect=ok
module Ui.Widget exposing (label)

label = 1
```

A lowercase segment is not a module name:

```zel expect=parse-error:UnexpectedToken
module ui.Widget exposing (label)

label = 1
```

The `exposing` clause is not optional. A module that means to expose nothing writes an
empty list, so that a reader can tell an empty interface from a forgotten one:

```zel expect=ok
module Widget exposing ()

label = 1
```

Leaving the clause out entirely is a syntax error:

```zel expect=parse-error:UnexpectedToken
module Widget

label = 1
```

The header is one layout block, so the `exposing` clause may be carried onto later lines
as long as they are indented past column 1:

```zel expect=ok
module Widget exposing
  ( label
  , size
  )

label = 1

size = 2
```

Continuing it in column 1 does not work, because a line starting in column 1 begins a new
top-level declaration:

```zel expect=parse-error:UnexpectedToken
module Widget
exposing (label)

label = 1
```

### The name and the file

**Not implemented:** a module's declared name must match the path of the file that holds
it, relative to the package's source root, with each `.` in the name standing for a
directory separator and `.zel` appended. `module Ui.Widget` lives in `Ui/Widget.zel` and
nowhere else. Two files may not declare the same module name.

```text
src/
  Widget.zel        module Widget exposing (…)
  Ui/Widget.zel     module Ui.Widget exposing (…)
```

That rule is what makes an `import` answerable without searching: a reader who sees
`import Ui.Widget` knows which file to open, and a compiler that has to resolve it can
find the file without parsing every module in the package first. Today neither half is
checked — a file at `Sub/Thing.zel` may declare `module Elsewhere` and be imported as
`Elsewhere`, and two files may both declare `module Same` with the second silently
replacing the first. [`docs/tickets/lang-6.md`](../tickets/lang-6.md) is the ticket. The
rule cannot be shown as a tagged block here at all: a block is source text with no path
behind it, so there is nothing for an example to violate.

## The `exposing` list

An `exposing` list is either `(..)`, which exposes everything the module declares, or a
parenthesised list of entries. There are exactly four kinds of entry:

| Entry | Exposes |
|---|---|
| `label` | a value or function declared in this module |
| `Size` | a type, but not its constructors — an *opaque* type |
| `Size(..)` | a type together with every one of its constructors |
| `(+)` | an operator, which must have an `infix` declaration in this module |

```zel expect=ok
module Widget exposing (Color, Shape(..), label, (+))

type Color
  = Red
  | Blue

type Shape
  = Round

label = 1

infix left 6 (+) = add

add a b =
  a
```

The `Size` / `Size(..)` split is the whole of Zelkova's data hiding. Exposing a type
without its constructors gives other modules a name they can write in a type annotation
and values they can pass around, while leaving them unable to build one or take one
apart — so the module's own functions are the only way in, and the representation stays
changeable. Exposing `Size(..)` gives that up deliberately and permanently.

There is no way to expose *some* of a type's constructors. A type is opaque or it is not:

```zel expect=parse-error:UnexpectedToken
module Widget exposing (Color(Red))

type Color
  = Red
  | Blue
```

An operator entry names the operator, in parentheses, and the module must carry the
`infix` declaration that gives it a meaning. Operators are not built in — exposing `(+)`
from a module that never declared it is an error:

```zel expect=canonical-error:ExportNotFound
module Widget exposing ((+))

label = 1
```

An entry is always a bare name. A qualified name in an `exposing` list is a syntax
error, since a module can only expose what it declares itself:

```zel expect=parse-error:UnexpectedToken
module Widget exposing (Ui.label)

label = 1
```

A trailing comma is allowed, and means nothing:

```zel expect=ok
module Widget exposing (label,)

label = 1
```

This is a deliberate divergence — most languages of this family reject it — and it is
here for one reason: a one-name-per-line `exposing` list is the normal way to write a
long one, and with no trailing comma every addition to the end of such a list touches
two lines instead of one.

`(..)` is all or nothing. It cannot be combined with named entries:

```zel expect=parse-error:UnexpectedToken
module Widget exposing (.., label)

label = 1
```

### Everything exposed must be declared here

Every entry in a module's own `exposing` list names one of that module's declarations.
A module may not re-export something it imported: a name that reaches other modules
through `Widget` is a name `Widget` declared, which is what keeps "where does this come
from" answerable by reading one header.

**Known gap:** the first and the last of the three blocks below should both be rejected —
the first exposes a name nothing declares, the last exposes one it only imported. Today
only an *operator* entry is checked; a lowercase or uppercase entry in a module's own
header is accepted unconditionally, whether it names a declaration, an import, or nothing
at all ([`docs/tickets/bug-8.md`](../tickets/bug-8.md)). Both are therefore tagged for
what happens now, and go red when that lands.

```zel expect=ok
module Widget exposing (missing)

label = 1
```

```zel expect=ok package=reexport
module Widget exposing (Size, label)

type Size
  = Small

label : Size
label = Small
```

```zel expect=ok package=reexport
module Facade exposing (label)

import Widget exposing (label)
```

### Exposing is what other modules can see

A module's `exposing` list is the complete list of what any other module may reach,
qualified or not. A declaration left out of it is private to the module: invisible to
every importer, under every spelling.

**Known gap:** it is not enforced. `Module::to_interface` builds the view other modules
import against out of every top-level declaration, ignoring the `exposing` list entirely,
so nothing is private today — [`docs/tickets/bug-9.md`](../tickets/bug-9.md). The
importing block below should fail to resolve `Widget.hidden`.

```zel expect=ok package=privacy
module Widget exposing (Size, label)

type Size
  = Small

label : Size
label = Small

hidden : Size
hidden = Small
```

```zel expect=ok package=privacy
module Main exposing (x)

import Widget

x : Widget.Size
x = Widget.hidden
```

**Known gap:** the reverse also happens — a value the module *does* expose can fail to
cross the boundary. A top-level declaration written without a type annotation is dropped
when the module's interface is built, so importers cannot see it at all, and the
diagnostic they get says the name does not exist rather than saying why
([`docs/tickets/bug-14.md`](../tickets/bug-14.md)). Annotating `label` in the first block
below makes the second compile. Note that the first block is not valid Zelkova either way:
an exposed declaration [must be annotated](types.md#an-exposed-declaration-must-be-annotated),
and enforcing that rule is what removes this gap — the error moves to the declaration that
failed to describe itself, instead of landing on the importer.

```zel expect=ok package=unannotated
module Widget exposing (label)

label = 1
```

```zel expect=canonical-error:VariableNotFound package=unannotated
module Main exposing (x)

import Widget

x = Widget.label
```

## Imports

An `import` declaration names a module and, optionally, an alias for it and a list of
names to bring into scope unqualified:

```text
import <ModuleName> [as <Alias>] [exposing (<list>)]
```

The order is fixed: the module name, then `as`, then `exposing`. Writing them the other
way round is a syntax error rather than a tolerated variation, so there is one shape for
an `import` line and a reader scanning a column of them is never re-reading one:

```zel expect=parse-error:UnexpectedToken
module Main exposing (x)

import Widget exposing (label) as W

x = 1
```

An alias is a single uppercase-initial identifier. It has no dots in it — an alias
introduces one new prefix, and a dotted one would introduce a prefix that looks like a
module name of its own:

```zel expect=parse-error:UnexpectedToken
module Main exposing (x)

import Widget as W.Inner

x = 1
```

Every import brings the imported module's exposed names into scope **qualified** — under
the module's name, or under its alias when it has one:

```zel expect=ok package=qualified
module Widget exposing (Size, label)

type Size
  = Small

label : Size
label = Small
```

```zel expect=ok package=qualified
module Main exposing (x)

import Widget

x : Widget.Size
x = Widget.label
```

An alias **replaces** the module's own name rather than adding to it. After
`import Widget as W`, `W.label` resolves and `Widget.label` does not — there is one
spelling for one module in one file, and a reader never has to check whether two
prefixes mean the same thing:

```zel expect=ok package=alias
module Widget exposing (Size, label)

type Size
  = Small

label : Size
label = Small
```

```zel expect=ok package=alias
module Main exposing (x)

import Widget as W

x : W.Size
x = W.label
```

```zel expect=canonical-error:VariableNotFound package=alias
module Other exposing (y)

import Widget as W

y = Widget.label
```

### Where imports go

**Not implemented:** every `import` in a file sits between the module header and the
first other declaration. An import after a value, type or `infix` declaration is a syntax
error. The list of what a module depends on is a property of the module rather than of
the point it is written at, and putting it in one place at the top is what lets a reader
find it without reading the file.

**Known gap:** the grammar treats `import` as an ordinary top-level declaration and
accepts one anywhere among the others, which is what the second block below shows
([`docs/tickets/lang-5.md`](../tickets/lang-5.md)).

```zel expect=ok package=position
module Widget exposing (Size)

type Size
  = Small
```

```zel expect=ok package=position
module Main exposing (x)

x = 1

import Widget
```

### What an import's `exposing` list does

An import's `exposing` list takes the same four entry forms as a module header, and does
one extra thing: it makes those names available **unqualified**, in addition to the
qualified spelling every import already provides.

```zel expect=ok package=unqualified
module Widget exposing (Size, label)

type Size
  = Small

label : Size
label = Small
```

```zel expect=ok package=unqualified
module Main exposing (x)

import Widget exposing (Size, label)

x : Size
x = label
```

Without the entry, the qualified spelling is the only one:

```zel expect=ok package=unqualified2
module Widget exposing (Size, label)

type Size
  = Small

label : Size
label = Small
```

```zel expect=canonical-error:VariableNotFound package=unqualified2
module Main exposing (x)

import Widget

x : Widget.Size
x = label
```

`Size` and `Size(..)` differ on the import side exactly as they do on the export side:
the first brings the type's name into scope, the second brings its constructors too.

```zel expect=ok package=variants
module Widget exposing (Size(..))

type Size
  = Small
```

```zel expect=ok package=variants
module Main exposing (x)

import Widget exposing (Size(..))

x : Size
x = Small
```

```zel expect=canonical-error:VariantNotFound package=variants
module Other exposing (y)

import Widget exposing (Size)

y : Size
y = Small
```

Constructors are the only thing the bare entry gives up. The type's name is in scope
unqualified, so a module can name it in its own signatures while leaving every way of
building one to `Widget`:

```zel expect=ok package=variants
module Shape exposing (grow)

import Widget exposing (Size)

grow : Size -> Size
grow s =
  s
```

**Known gap:** that block states the rule rather than testing it. It compiles unchanged
with the `exposing (Size)` dropped, because neither the entry nor the annotation is
checked today ([`docs/tickets/bug-16.md`](../tickets/bug-16.md), which covers both sites
and is the gap described further down).

An `exposing (..)` on an import brings in everything the module exposes, unqualified. A
bare `import Widget` with no `exposing` clause brings in nothing unqualified, and means
the same as `import Widget exposing ()`.

An entry naming something the imported module does not expose is an error, and the
diagnostic points at the entry rather than at the whole `import` line:

```zel expect=ok package=missing-entry
module Widget exposing (Size, label)

type Size
  = Small

label : Size
label = Small
```

```zel expect=canonical-error:EnvironmentErrors package=missing-entry
module Main exposing (x)

import Widget exposing (missing)

x = 1
```

**Known gap:** that check is done for value entries and operator entries, and for
`Size(..)`, but not for a bare `Size`. An opaque-type entry naming a type that does not
exist is accepted, and a type by that name is invented on the spot — so the mistake
surfaces later, as a confusing type error, or not at all
([`docs/tickets/bug-16.md`](../tickets/bug-16.md)).

```zel expect=ok package=missing-type
module Widget exposing (Size, label)

type Size
  = Small

label : Size
label = Small
```

```zel expect=ok package=missing-type
module Main exposing (x)

import Widget exposing (Missing)

x = 1
```

### Operators

An operator has no qualified spelling: `Widget.(+)` is a syntax error, and `Widget.+` is
not a qualified operator either — it is the constructor `Widget` and an operator named
`.+`. So an operator entry in an import's `exposing` list is the only way to use one from
another module. Naming the operator is enough on its own: the function that its `infix`
declaration points at need not be in scope, and the operator means the same thing either
way.

**Known gap:** it does depend on that function being in scope today. An operator is
resolved by looking up the name its `infix` declaration gives, unqualified, in the
*importing* module — so an operator entry that does not also bring in `add` leaves `+`
unresolvable, and the error names `+` when the name that actually failed to resolve is
`add` ([`docs/tickets/bug-15.md`](../tickets/bug-15.md)). `exposing (..)` happens to work,
because it drags the backing function in as well; the third block below is that.

```zel expect=ok package=operators
module Widget exposing (Size, one, (+), add)

type Size
  = Small

one : Size
one = Small

infix left 6 (+) = add

add : Size -> Size -> Size
add a b =
  a
```

```zel expect=canonical-error:VariableNotFound package=operators
module Main exposing (x)

import Widget exposing (Size, one, (+))

x : Size
x = one + one
```

```zel expect=ok package=operators
module Other exposing (y)

import Widget exposing (..)

y : Size
y = one + one
```

### Two imports exposing the same name

A name brought in unqualified by two different imports is not an error at the `import`
line — it becomes one at each place it is *used*, and only there. Importing two modules
that both expose `label` is fine right up until something writes `label`, which is what
lets a module import broadly and still be told precisely what went wrong.

```zel expect=ok package=ambiguous
module Widget exposing (Size, label)

type Size
  = Small

label : Size
label = Small
```

```zel expect=ok package=ambiguous
module Gadget exposing (label)

label : Int
label = 1
```

```zel expect=canonical-error:AmbiguousVariables package=ambiguous
module Main exposing (x)

import Widget exposing (label)
import Gadget exposing (label)

x = label
```

The qualified spelling is unaffected: `Widget.label` and `Gadget.label` both still
resolve, and are the fix. *Name resolution and scoping* — planned; see
[the chapter list](README.md#chapters) — is where the general rule lives, including what
shadows what.

### One module, one import

A module may be imported at most once in a file, an alias may name at most one module,
and an alias may not collide with the name of another imported module. Each of the three
would otherwise let one prefix mean two things at once, and the reader has no way to see
that from the line in front of them.

**Not implemented:** none of the three is checked at the `import` line
([`docs/tickets/lang-7.md`](../tickets/lang-7.md)), and the two that quietly succeed are
the worse pair. An alias colliding with another imported module's name merges the two
namespaces, so `Gadget.` in the fourth block below reaches into both `Widget` and
`Gadget` and nothing says so — that block is tagged `expect=ok` for exactly that reason.

**Known gap:** importing the same module twice does fail today, but not as a duplicate
import. Every one of that module's names is registered twice over, so each *use* of one
is reported as ambiguous between two modules that happen to be the same module — the
third block below is tagged for an error whose message names `Widget` twice, at the use
site rather than at the `import` line.

**Known gap:** a module importing itself is rejected too, and also for the wrong reason:
a module's own interface does not exist while it is being checked, so `import Alone`
inside `module Alone` is reported as a module that cannot be found. The last block is
tagged for that. Both are [`lang-7`](../tickets/lang-7.md).

```zel expect=ok package=duplicates
module Widget exposing (Size, label)

type Size
  = Small

label : Size
label = Small
```

```zel expect=ok package=duplicates
module Gadget exposing (Size, volume)

type Size
  = Loud

volume : Size
volume = Loud
```

```zel expect=canonical-error:AmbiguousVariables package=duplicates
module Main exposing (x)

import Widget
import Widget

x = Widget.label
```

```zel expect=ok package=duplicates
module Other exposing (y, z)

import Widget as Gadget
import Gadget

y = Gadget.label

z = Gadget.volume
```

```zel expect=canonical-error:EnvironmentErrors
module Alone exposing (x)

import Alone

x = 1
```

### Imports may not form a cycle

The `import` declarations of a package form a graph, and that graph must be acyclic. Two
modules that import each other, or any longer loop, is an error reported before any of
the modules involved is checked — there is no order in which they could be, since each
needs the other's exposed types to make sense of its own.

The error names every module on the cycle and points at the `import` line that forms each
edge, so breaking it is a matter of picking one of those lines.

```zel expect=dependency-error package=cycle
module Left exposing (x)

import Right

x = 1
```

```zel expect=dependency-error package=cycle
module Right exposing (y)

import Left

y = 2
```

## The default imports

**Not implemented:** every module behaves as though it began with these seven imports,
whether they are written or not:

```text
import Basics exposing (..)
import List exposing (List)
import Maybe exposing (Maybe(..))
import Result exposing (Result(..))
import Char
import String
import Tuple
```

So `Int`, `Bool`, `True`, `+` and `<|` are in scope in every module with nothing written
at the top of it, `Maybe` and `Just` likewise, and `List.map`, `Char.toUpper` and
`String.length` are reachable under their qualified names. Nothing else is: a module that
wants `Dict` imports it.

The list is chosen so that the types appearing in ordinary type annotations are always
writable. `Maybe` and `Result` are exposed with their constructors because matching on
them is the ordinary way to use them, and a qualified `Maybe.Just` in every `case` branch
would spell out a module name on one of the most common patterns in the language.
`List` is exposed as a bare type because its module's functions read better qualified —
`List.map`, not `map`. Writing any of these imports out explicitly is allowed and changes
nothing.

**Known gap:** none of it exists. Every module resolves only what it declares and what it
imports by hand, which is why `std/core`'s modules all begin with `import Basics`
([`docs/tickets/lang-8.md`](../tickets/lang-8.md)). The block below is pinned on the
error today's compiler gives, not on a bare rejection, so that it goes red whichever way
the ticket lands.

```zel expect=canonical-error:VariableNotFound
module Main exposing (x)

x = 1 + 2
```

## Packages

A module name is unique within a package, and an `import` names a module in the package
being compiled or in one of its dependencies. What a package is, how its dependencies are
declared, and what visibility means at a package boundary are the subject of *Packages and
source layout* — planned; see [the chapter list](README.md#chapters).
