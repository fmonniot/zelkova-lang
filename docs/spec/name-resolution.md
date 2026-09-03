# Name resolution and scoping

Every name a program writes is resolved statically, from the text around it. Resolution asks
one question of each occurrence — *which declaration does it name?* — and there are only three
answers: exactly one, which is the ordinary case; none, which is an **unresolved** name; or
more than one, which is an **ambiguous** name. The last two are both errors, and they are
different errors, because the fixes are different: an unresolved name has to be declared or
imported, an ambiguous one has to be spelled so that it picks.

Nothing in the answer depends on a value, so a reader answers that question with exactly the
information the compiler has: the file, and the modules it imports.

## Namespaces

A program has five namespaces, and a name in one never collides with a name in another:

| Namespace | Holds | Written |
|---|---|---|
| values | top-level bindings, parameters, pattern bindings | lowercase-initial |
| types | `type` declarations | uppercase-initial, in a type expression |
| constructors | the variants of a `type` declaration | uppercase-initial, in an expression or pattern |
| operators | `infix` declarations | operator characters, never letters |
| modules | the prefix of a qualified name | uppercase-initial, before the final `.` |

Which namespace an occurrence is looked up in is decided by how it is written and where it
sits, never by what happens to be in scope. A lowercase-initial name is a value; an
uppercase-initial one is a type in a [type expression](types.md#type-names) and a constructor
[in an expression](expressions.md#names) or a [pattern](patterns.md#constructor-patterns); a
name before the final `.` of a qualified name is a module. So one spelling can be several
unrelated things at once, and each occurrence still has exactly one meaning:

```zel expect=ok
module Example exposing (Reading, Celsius, boiling)

type Reading
  = Celsius

type Celsius
  = Absolute

boiling : Reading
boiling =
  Celsius
```

`Celsius` is a constructor of `Reading` and also the name of a second type. The annotation
`boiling : Reading` names a type, the body `Celsius` names a constructor, and neither could
have meant the other.

The one lowercase name that is not a value is a [type variable](#type-variables), which a type
expression is the only place to write. A type variable is not resolved against a namespace at
all: it is bound by the declaration it appears in, and the section at the foot of this chapter
is what says by which.

Module names are a namespace of their own for the same reason. A constructor named `Widget`
and an imported module named `Widget` coexist, because a module name is only ever read to the
left of a `.`:

```zel expect=ok package=prefix
module Widget exposing (Size, label)

type Size
  = Small

label : Size
label =
  Small
```

```zel expect=ok package=prefix
module Main exposing (Named, here, there)

import Widget

type Named
  = Widget

here : Named
here =
  Widget

there : Widget.Size
there =
  Widget.label
```

A [module name may contain dots](modules.md#the-module-header) and is still one name, so the
module part of a qualified name is everything before its **final** `.` and the local part is
what follows. `Ui.Widget.label` names `label` in `Ui.Widget`; there is no module `Ui` in it to
look anything up in.

```zel expect=ok package=prefix
module Ui.Widget exposing (Size, label)

type Size
  = Small

label : Size
label =
  Small
```

```zel expect=ok package=prefix
module Deep exposing (x)

import Ui.Widget

x : Ui.Widget.Size
x =
  Ui.Widget.label
```

## Scopes

A name is resolved in the scopes enclosing the place it is written, innermost first. There are
two kinds, and no more:

1. **A binding position** — a declaration's parameters, or a `case` branch's pattern. Each
   binds the [variables of its patterns](patterns.md#variable-patterns) for the extent of one
   body, and these nest: a `case` inside a `case` branch is inside that branch's scope.
2. **The module's top level** — everything the module declares, plus every name its imports
   bring in unqualified.

There is no scope outside the module. A name is in scope because this module declares it or
because this module imports it; being declared in a neighbouring module of the same package is
not being in scope, and neither is being declared in the same package's dependency. The
[default imports](modules.md#the-default-imports) are the one thing a module gets without
writing anything, and they are imports — they bring in what they bring in and nothing else.

**Not implemented:** [`let … in`](expressions.md#let--in) and
[lambdas](expressions.md#lambdas) each add a binding position, and each is the same kind of
scope as the two above: bindings visible in one body, invisible outside it. Neither construct
exists yet.

An inner binding **shadows** an outer name of the same spelling — that rule and its
consequences for patterns are [Patterns](patterns.md#variable-patterns)'s, and it applies to
imported names exactly as it does to a module's own declarations:

```zel expect=ok package=shadow
module Widget exposing (Size, label)

type Size
  = Small

label : Size
label =
  Small
```

```zel expect=ok package=shadow
module Main exposing (f)

import Widget exposing (Size(..), label)

f : Size -> Size
f label =
  case label of
    Small ->
      Widget.label
```

### A qualified name cannot be shadowed

`Widget.label` in that body still reaches the imported value, even though `label` alone means
the parameter. A binding position binds bare names — a pattern has no way to write a dotted
one — so nothing an inner scope does can change what a qualified name means. That is what
makes the qualified spelling a reliable escape: it is the fix for a shadowed name and for an
[ambiguous](#ambiguous-rather-than-unresolved) one, and it works without knowing what else is
in scope.

### A module has no prefix for itself

A qualifier names an import, and a module does not import itself. Inside `module Example`,
`Example.first` names nothing:

```zel expect=canonical-error:VariableNotFound
module Example exposing (Size, first, second)

type Size
  = Small

first : Size
first =
  Small

second : Size
second =
  Example.first
```

A module's own declarations are in scope under their bare names throughout it, so the prefix
would say nothing the bare name does not — and reading it as "this module" would make one
prefix mean something different in every file it is written in.

Which prefix an import contributes is [Modules](modules.md#imports)' subject: the module's
name as written, or its alias, which **replaces** that name rather than adding to it.

## A top-level name comes from exactly one place

The two sources of a top-level name — what the module declares and what it imports
unqualified — are one scope, not two. Neither shadows the other, because neither is inside the
other: a module that declares `label` and also imports `label` unqualified is rejected.

Shadowing is a relationship between a scope and the scope it sits inside. Two names arriving at
the same level have no such relationship, and nothing that reads only the declaration, or only
the `import` line, could tell that the other one exists. The fix is to pick one: drop the entry
from the `exposing` list and write `Widget.label` where the imported value is wanted, or rename
the declaration.

**Known gap:** the module's own declaration silently wins today, for values and for types
alike. Both blocks below are accepted, and in each of them the imported name is dead without
anything saying so ([`docs/tickets/lang-29.md`](../tickets/lang-29.md)).

```zel expect=ok package=clash
module Widget exposing (Size, label)

type Size
  = Small

label : Size
label =
  Small
```

```zel expect=ok package=clash
module Main exposing (x)

import Widget exposing (Size, label)

type Size
  = Big

label : Size
label =
  Big

x : Size
x =
  label
```

The rule holds for `exposing (..)` too, where the error is reported on the `import` line
because there is no entry to point at. An open import is a claim about a whole module's
exposed names, and it collides the same way a named entry does. The cost is real and is the
cost `(..)` already carries: a module gaining an export can break a file that imports it
openly. What that buys is that no name in a file is quietly two names.

```zel expect=ok package=clash
module Other exposing (y)

import Widget exposing (..)

label : Size
label =
  Small

y : Size
y =
  label
```

### A name is declared at most once

Within one module, each type is declared once, and each operator carries [one `infix`
declaration](declarations.md#an-operator-has-one-infix-declaration). Two declarations of one
type introduce two types with one name, and no rule chooses between them.

Repeated *value* bindings are not an exception to that. They are not two declarations at all
but the [clauses](declarations.md#clauses) of one, which is why they are allowed and why they
must agree on their parameter count.

**Known gap:** a second `type` declaration of a name silently replaces the first, taking its
constructors with it — after the block below the module has one type `Size`, built by `Big`,
and `Small` no longer resolves anywhere
([`docs/tickets/lang-32.md`](../tickets/lang-32.md)).

```zel expect=ok
module Example exposing (Size)

type Size
  = Small

type Size
  = Big
```

## Unresolved names

A name that no scope binds is an error where it is written, with the caret under the name
rather than under the declaration containing it. That is the whole of the rule; what follows
are the shapes it takes.

A bare name that nothing declares or imports:

```zel expect=canonical-error:VariableNotFound
module Example exposing (Size, x)

type Size
  = Small

x : Size
x =
  missing
```

A constructor is looked up in its own namespace, and an uppercase name that names no variant
fails the same way:

```zel expect=canonical-error:VariantNotFound
module Example exposing (Size, x)

type Size
  = Small

x : Size
x =
  Large
```

A qualified name resolves only if its prefix names an import *and* that module exposes the
name. Neither half is assumed: a prefix that names no import is not a hint to go looking for
the module.

```zel expect=canonical-error:VariableNotFound
module Example exposing (Size, x)

type Size
  = Small

x : Size
x =
  Widget.label
```

**Known gap:** the diagnostic there reports a missing *value* called `Widget.label`, which is
true but unhelpful — the thing actually missing is the `import`, and the message never says the
word ([`docs/tickets/err-14.md`](../tickets/err-14.md)).

Being in the same package is not being in scope. `Widget` below is compiled alongside `Main`
and `Main` still cannot see it:

```zel expect=ok package=notimported
module Widget exposing (Size, label)

type Size
  = Small

label : Size
label =
  Small
```

```zel expect=canonical-error:VariableNotFound package=notimported
module Main exposing (Size, x)

type Size
  = Small

x : Size
x =
  Widget.label
```

## Ambiguous rather than unresolved

A name brought into scope unqualified by two different imports is not an error at either
`import` line. It becomes one at each place the bare name is *written*, and only there — which
is what lets a module import broadly and be told precisely what went wrong, instead of being
made to prune imports it may never use.

The report names both candidates, and the qualified spelling of either is the fix.

```zel expect=ok package=ambiguous
module Widget exposing (Size(..), label, one, add, (+))

type Size
  = Small

label : Size
label =
  Small

one : Size
one =
  Small

add : Size -> Size -> Size
add a b =
  a

infix left 6 (+) = add
```

```zel expect=ok package=ambiguous
module Gadget exposing (Size(..), label, two, mul, (+))

type Size
  = Small

label : Size
label =
  Small

two : Size
two =
  Small

mul : Size -> Size -> Size
mul a b =
  a

infix left 6 (+) = mul
```

```zel expect=canonical-error:AmbiguousVariables package=ambiguous
module Main exposing (x)

import Widget exposing (label)
import Gadget exposing (label)

x : Widget.Size
x =
  label
```

The same rule holds in every namespace, because the situation is the same one: two imports,
one spelling, no reason to prefer either. A type and a constructor are ambiguous exactly as a
value is, and `Widget.Size` and `Widget.Small` are the fixes.

**Known gap:** ambiguity is detected for values only. A type, a constructor or an operator
arriving from two imports is taken from whichever `import` line is written last, silently, so
the meaning of the two blocks below depends on the order of two lines that look
interchangeable ([`docs/tickets/lang-30.md`](../tickets/lang-30.md)).

```zel expect=ok package=ambiguous
module Other exposing (y)

import Widget exposing (Size(..))
import Gadget exposing (Size(..))

y : Size
y =
  Small
```

An operator is the one case with no qualified spelling to fall back on —
[`Widget.(+)` is not writable](modules.md#operators) — so the fix there is an edit to an
`import` line: name what the file wants, rather than taking everything two modules have.

```zel expect=ok package=ambiguous
module Third exposing (z)

import Widget exposing (..)
import Gadget exposing (..)

z : Widget.Size
z =
  one + one
```

### Order never decides

Once ambiguity is an error, nothing about resolution depends on the order things are written
in. [Declarations are unordered](declarations.md#declarations-are-unordered), and so are
imports: reordering the `import` lines of a file never changes what any name in it means, and
neither does reordering the declarations that follow them. A file whose meaning turns on which
of two lines came first is a file whose meaning cannot be read off the line in front of you.

## Type variables

A [type variable](types.md#type-variables) is bound by the declaration it appears in, and by
nothing outside it. No lowercase spelling is privileged — `a`, `number` and `comparable` are
the same kind of name, which [Types](types.md#type-variables) settles — so binding is all
there is to say about where one means something.

In an annotation, every lowercase name is a variable of that annotation. Two annotations that
happen to use the same letter share nothing:

```zel expect=ok
module Example exposing (Box, first, second)

type Box a
  = Box a

first : a -> a
first x =
  x

second : a -> Box a
second x =
  Box x
```

In a `type` declaration, the parameters between the type's name and the `=` are the binders,
and they are in scope throughout the variants. A variant may use those and no others: a
variable the declaration does not bind has no value to stand for, since there is no argument
position that would ever fix it.

**Known gap:** an unbound variable in a variant is accepted, and `Box` below is a type whose
constructor takes an argument of a type nothing can name
([`docs/tickets/lang-31.md`](../tickets/lang-31.md)).

```zel expect=ok
module Example exposing (Box)

type Box
  = Box a
```
