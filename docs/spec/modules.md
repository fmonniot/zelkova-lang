# Modules, exposing and imports

Every Zelkova source file is exactly one module, and every module is exactly one source
file. A module names itself, says which of its declarations the rest of the world may
see, and names the other modules it draws on. Those three things are the module header
and the `import` declarations under it.

There is no other unit of encapsulation. Zelkova has no nested modules, no namespaces
within a file, and no way to reopen a module from somewhere else — so "where does this
name come from" and "who is allowed to use this" are both answered at the top of one file.

## The module header

The first token of a file is the keyword `module`, in column 1 — [Layout](layout.md#a-file-starts-at-column-1)
carries that rule and the reasoning for it. What follows is the module's name and its
`exposing` list:

```zel expect=ok
module Widget exposing (label)

label : Int
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

label : Int
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

label : Int
label = 1

size : Int
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

label : Int
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

label : Int
label = 1
```

This is a deliberate divergence — most languages of this family reject it. A
one-name-per-line `exposing` list is the normal way to write a long one, and with no
trailing comma every addition to the end of such a list touches two lines instead of one.

`(..)` is all or nothing. It cannot be combined with named entries:

```zel expect=parse-error:UnexpectedToken
module Widget exposing (.., label)

label = 1
```

### Everything exposed must be declared here

Every entry in a module's own `exposing` list names one of that module's declarations.
A module may not re-export something it imported: a name that reaches other modules
through `Widget` is a name `Widget` declared.

**Known gap:** the last of the three blocks below should also be rejected — it exposes a
name it only imported, not one it declared itself. A name nothing declares at all is now an
error, the same way an undeclared operator entry already was. But a name an
`import ... exposing (...)` brought in still reads exactly like one the module wrote itself,
so the last block is tagged for what it still does
([`docs/tickets/bug-31.md`](../tickets/bug-31.md)).

```zel expect=canonical-error:ExportNotFound
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
qualified or not. A declaration left out of it is private to the module.

```zel expect=ok package=privacy
module Widget exposing (Size, label)

type Size
  = Small

label : Size
label = Small

hidden : Size
hidden = Small
```

```zel expect=canonical-error:VariableNotFound package=privacy
module Main exposing (x)

import Widget

x : Widget.Size
x = Widget.hidden
```

Privacy is a property of the boundary: `hidden` is an ordinary value inside `Widget`, and
`label` may call it.

The reverse matters too: a value the module *does* expose still has to actually cross the
boundary, which is only possible once the checker knows its type. An exposed declaration
with no type annotation is rejected at the declaration itself — [an exposed declaration
must be annotated](types.md#an-exposed-declaration-must-be-annotated).

```zel expect=canonical-error:ExportedValueNotAnnotated package=unannotated
module Widget exposing (label)

label = 1
```

`Widget` never canonicalizes, so it never publishes an interface for `Main` to resolve
against — the import itself fails, rather than the one name inside it that was the actual
problem.

```zel expect=canonical-error:EnvironmentErrors package=unannotated
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
way round is a syntax error, so there is one shape for an `import` line and a reader
scanning a column of them is never re-reading one:

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
spelling for one module in one file:

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
the point it is written at.

**Known gap:** the grammar treats `import` as an ordinary top-level declaration and
accepts one anywhere among the others, which is what the second block below shows
([`docs/tickets/lang-5.md`](../tickets/lang-5.md)).

```zel expect=ok package=position
module Widget exposing (Size)

type Size
  = Small
```

```zel expect=ok package=position
module Main exposing ()

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
with the `exposing (Size)` dropped, because an annotation naming a type nothing brings
into scope is accepted ([`docs/tickets/bug-16.md`](../tickets/bug-16.md)).

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

A bare `Size` entry is checked the same way, and so is `Size(..)`: whether the
constructors come along is the only difference between the two.

```zel expect=ok package=missing-type
module Widget exposing (Size, label)

type Size
  = Small

label : Size
label = Small
```

```zel expect=canonical-error:EnvironmentErrors package=missing-type
module Main exposing ()

import Widget exposing (Missing)

x = 1
```

### Operators

An operator has no qualified spelling: `Widget.(+)` is a syntax error, and `Widget.+` is
not a qualified operator either — it is the constructor `Widget` and an operator named
`.+`. So an operator entry in an import's `exposing` list is the only way to use one from
another module. Naming the operator is enough on its own: the function that its `infix`
declaration points at need not be in scope.

An `exposing (..)` import brings in every operator the module exposes, the same way it
brings in every value.

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

```zel expect=ok package=operators
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
that both expose `label` is fine right up until something writes `label`.

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
resolve, and are the fix.
[Name resolution and scoping](name-resolution.md#ambiguous-rather-than-unresolved) is where
the general rule lives, including what shadows what.

### One module, one import

A module may be imported at most once in a file, an alias may name at most one module,
and an alias may not collide with the name of another imported module. Each of the three
would otherwise let one prefix mean two things at once.

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
module Other exposing ()

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

Every module behaves as though it began with these eight imports, whether they are
written or not:

```text
import Basics exposing (..)
import List exposing (List)
import Maybe exposing (Maybe(..))
import Result exposing (Result(..))
import Task exposing (Task)
import Char exposing (Char)
import String exposing (String)
import Tuple
```

So `Int`, `Bool`, `String`, `True`, `+` and `<|` are in scope in every module with nothing
written at the top of it, `Maybe` and `Just` likewise, and `List.map`, `Char.toUpper` and
`String.length` are reachable under their qualified names. Nothing else is: a module that
wants `Dict` imports it.

```zel expect=ok package=defaults
module Basics exposing (Int, (+), add)

type Int = Int

infix left 6 (+) = add

add : Int -> Int -> Int
add a b =
  a
```

```zel expect=ok package=defaults
module Main exposing (x)

x : Int
x = 1 + 2
```

The list is chosen so that the types appearing in ordinary type annotations are always
writable. `Maybe` and `Result` are exposed with their constructors because matching on
them is the ordinary way to use them, and a qualified `Maybe.Just` in every `case` branch
would spell out a module name on one of the most common patterns in the language.
`List`, `Char` and `String` are exposed as bare types because their modules' functions read
better qualified — `List.map` and `String.length`, not `map` and `length`. [`Task`](evaluation-semantics.md#effects) is exposed the same way and for
the same reason, and it is on the list because [`main`](packages.md#programs) names it in an
annotation every program has to write.
[`Failure`](evaluation-semantics.md#an-effect-that-can-fail) does not come with it: the modules
naming that type are the ones declaring or consuming an
[effectful facade](interop.md#an-effectful-facade), and a module that names it imports it.

An `import` of one of the eight **replaces** the implicit one, the way [an alias replaces
a module's own name](#imports): writing a line of the list out verbatim changes nothing,
and `import Maybe as M` means `M.map` and no unqualified `Maybe`.

The eight modules receive none of the list themselves: `Basics` cannot import `Basics`,
and `Maybe` and `Result` would import each other — [an import
cycle](#imports-may-not-form-a-cycle) either way. They write the imports they need.

Every other module is judged **one entry at a time**. It drops the entry for a module
that already depends on it, since that implicit import would close a cycle back, and
keeps every other entry. So a facade `Basics` imports does not receive `Basics` — but it
still receives `Tuple`, which depends on nothing. Where a module sits in the import graph
is what decides its set, and two modules of one package need not have the same one.

A drop can propagate, because the imports the compiler supplies are dependencies like any
other. Say `Basics` imports `A`: then `A` loses `Basics`, and if the compiler goes on to
give `Basics` to some module `Maybe` imports, `Maybe` reaches `A` through it and `A`
loses `Maybe` as well. Which entry survives such a collision is settled by the order the
list above writes them — an entry is only ever dropped for a dependency an *earlier*
entry created, never a later one. `Basics` is first, so it is the entry a module keeps
when it can keep only one.

**A module that drops the `Basics` entry receives the scalar type names in its place.** `Int`,
`Float`, `Char`, `String` and `Bool` are in its scope with nothing written at the top of the
file, bound to the same declarations `Basics` exposes. The compiler [knows each of the five by
qualified name](types.md#scalar-types), so it supplies them without reading `Basics`, and the
dependency the drop exists to avoid is never created.

What arrives is the five type names and nothing else. A module reaching `Bool` this way writes
it in a signature and cannot write a `True`: the constructors are `Basics`' values, and no rule
brings a value down. The case is a [facade](interop.md) underneath `Basics`, which has no
bodies to write one in.

```zel expect=ok package=below
module Basics exposing (Int)

import Below

type Int = Int
```

```zel expect=ok package=below
module foreign Below exposing (twice)

unsafe twice : Int -> Int
```

Only the package that declares `Basics` can hold such a module, since a package's dependencies
run one way, so no module outside it ever meets this rule.

**Known gap:** the second block above is green for the wrong reason. Nothing supplies the
scalar names yet; `Int` resolves to nothing there and a type is fabricated for it
([`BUG-16`](../tickets/bug-16.md)), and the fabrication passes for `Basics`' `Int` only because
a type is identified today by its unqualified name.

**Known gap:** `std/core` ships four of the eight, so `List`, `Char`, `String` and `Task`
bring nothing. A program naming `String.length` is rejected where the name is used.
Naming `List` in a type annotation is *accepted* today, but only because an unknown type
name is accepted anywhere ([`BUG-16`](../tickets/bug-16.md)) — not because the entry
works. Each entry starts working on the day its module compiles — except `Char` and `String`,
which would still bring only their qualified names, since `DEFAULT_IMPORTS` gives both entries
no unqualified name at all ([`LANG-55`](../tickets/lang-55.md)).

## Packages

A module belongs to exactly one package, and its name is unique within it. An `import`
names a module of the package being compiled or a public module of one of that package's
direct dependencies — the second under that dependency's namespace, so the name written is
longer than the name the module has at home. What a package is, which of its modules it keeps
private, how its dependencies are declared, and how a namespace is derived and when a
depending package may drop one are the subject of
[Packages and source layout](packages.md).
