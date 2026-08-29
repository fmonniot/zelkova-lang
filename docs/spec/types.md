# Types and type annotations

A **type expression** is the language Zelkova uses to talk about its own values. It appears in
three places: to the right of the `:` in a type annotation, as the argument of a constructor in
a `type` declaration, and inside another type expression. This chapter specifies that language
in full — every form it has, how the forms nest, and what writing one down commits a program
to.

It also specifies the two declarations that are made of type expressions: the annotation
`name : Type`, and the `type` declaration that introduces a new type. The neighbouring
*Declarations* chapter — planned; see [the chapter list](README.md#chapters) — covers value and
function bindings and `infix` declarations, and refers here for these two.

**Not implemented:**  `cargo test --test spec` runs each example through the
parser and canonicalization and stops; it never invokes the type checker. So a claim about what
the *type checker* does — the annotation rules in [An annotation is a promise](#an-annotation-is-a-promise),
and half of what [Applying a type to arguments](#applying-a-type-to-arguments) says — is prose
here rather than an executable example, and a block that ought to be a type error is tagged
`expect=ok` because that is what the harness sees. Each such place says so. [`docs/tickets/test-2.md`](../tickets/test-2.md)
is the ticket to close that hole.

## The forms of a type expression

There are six, and nothing else is a type:

| Form | Written | Section |
|---|---|---|
| A type name | `Colour`, `Widget.Size` | [below](#type-names) |
| A type variable | `a`, `value` | [below](#type-variables) |
| An application | `Maybe Colour` | [below](#applying-a-type-to-arguments) |
| A function | `Colour -> Colour` | [below](#the-function-arrow) |
| A tuple | `(Colour, Colour)` | [below](#tuple-types) |
| The unit type | `()` | [below](#the-unit-type) |

Parentheses may be written around any type expression and mean grouping only. `(Colour)` is
`Colour`; the parentheses have no effect and leave no trace.

```zel expect=ok
module Example exposing (Colour, red)

type Colour
  = Red

red : (Colour)
red = Red
```

Records and list types are part of the language and are specified with the constructs they
belong to, neither of which exists yet — see the *Not implemented* note on brackets and braces
in [Lexical structure](lexical-structure.md#punctuation).

### Type names

A type name is an uppercase-initial identifier, optionally qualified by the module it comes
from. The case is what makes it a name rather than a variable — that rule and its reasoning
are in [Lexical structure](lexical-structure.md#identifiers).

```zel expect=ok
module Example exposing (Celsius, freezing)

type Celsius
  = Celsius

freezing : Celsius
freezing = Celsius
```

A qualified name is spelled and resolved exactly as it is in an expression: the module's name,
or the alias standing in for it, then a `.`, then the type's own name. See
[Modules](modules.md#imports) for what an import brings into scope.

```zel expect=ok package=qualified
module Widget exposing (Size, small)

type Size
  = Small

small : Size
small = Small
```

```zel expect=ok package=qualified
module Main exposing (x)

import Widget

x : Widget.Size
x = Widget.small
```

### Type variables

A type variable is a lowercase-initial identifier. It stands for a complete type, and it is
universally quantified over the declaration it appears in: a variable in an annotation means
*the caller chooses*, and the declaration must work for every choice.

```zel expect=ok
module Example exposing (first)

first : (a, b) -> a
first p =
  p
```

Nothing distinguishes one lowercase spelling from another. `celsius` is a type variable in
exactly the way `a` is — the length of the name carries no meaning, and a variable is never
mistaken for a type whose name happens to be lowercase, because no type's name can be.

```zel expect=ok
module Example exposing (identity)

identity : celsius -> celsius
identity x =
  x
```

Three lowercase spellings — `number`, `comparable` and `appendable` — are written throughout
`std/core/src/` as though they meant something more than an ordinary variable. Whether they
become real constraints is an **open design question**, deliberately not answered anywhere in
this directory yet; the *Constrained type variables* chapter (planned; see
[the chapter list](README.md#chapters)) is where it will be recorded. Until then this chapter's
rule stands unqualified: every lowercase-initial name in a type is an ordinary type variable.

A type variable is never **applied**. Only a type name may head an application, so `m a` — a
variable standing for a type constructor rather than for a type — is a syntax error:

```zel expect=parse-error
module Example exposing (Box, lift)

type Box a
  = Box a

lift : m a -> m a
lift x =
  x
```

That is a deliberate limit and not an oversight. Allowing it would mean type variables ranging
over type constructors as well as types, which needs a kind system — a large
commitment the language is not ready for it yet. A variable always stands for a complete
type.

## Applying a type to arguments

A type declared with parameters is used by applying it to that many arguments, written after
the name and separated by spaces. The number of arguments must equal the number of parameters
the `type` declaration gave it: an application is not a partial one.

```zel expect=ok
module Example exposing (Maybe, nothing)

type Maybe a
  = Just a
  | Nothing

nothing : Maybe a
nothing = Nothing
```

An argument may be any type expression. Where that expression is itself an application, a
function, or a tuple, it is parenthesised — application binds tighter than everything else, so
`Maybe Maybe Colour` would otherwise read as `Maybe` applied to two arguments.

```zel expect=unimplemented
module Example exposing (Maybe, nested)

type Maybe a
  = Just a
  | Nothing

nested : Maybe (Maybe a)
nested = Nothing
```

```zel expect=unimplemented
module Example exposing (Box, boxed)

type Box a
  = Box a

boxed : Box (a -> a)
boxed = Box
```

**Not implemented:** neither block parses. An argument must be a bare name or a variable
today — the grammar has no parenthesised form in argument position at all — so every nested
type is rejected, including `type Tree a = Node (Tree a) (Tree a)`, the shape a recursive
container is written in. [`docs/tickets/lang-9.md`](../tickets/lang-9.md) is the ticket. The
workaround the grammar leaves open is a trap rather than a workaround: `Box Maybe a` parses, as
`Box` applied to two arguments.

### Arity is part of the application

```zel expect=ok
module Example exposing (Maybe, bare)

type Maybe a
  = Just a
  | Nothing

bare : Maybe
bare = Nothing
```

```zel expect=ok
module Example exposing (Maybe, Size, tooMany)

type Maybe a
  = Just a
  | Nothing

type Size
  = Small

tooMany : Maybe Size Size
tooMany = Nothing
```

**Known gap:** both blocks should be rejected — `Maybe` takes exactly one argument, and the
first supplies none while the second supplies two. Neither is checked, for the reason the next
section gives.

### An applied type still means what it says

Applying `Maybe` to `Size` produces a type that is not the same as applying it to anything
else. Two applications of one type name are the same type exactly when their arguments are.

```zel expect=ok
module Example exposing (Maybe, Size, sized)

type Maybe a
  = Just a
  | Nothing

type Size
  = Small

sized : Maybe Size
sized = Just Small
```

**Known gap:** the argument is discarded. Canonicalization resolves the head `Maybe`, returns
the type as the declaration stored it — `Maybe a` — and never looks at what was written after
it ([`docs/tickets/bug-17.md`](../tickets/bug-17.md)). So `Maybe Size` and `Maybe Colour` are
the same type as far as the compiler is concerned, and the annotation above stops constraining
its body: replacing `Just Small` with a value of any other type still type checks. That is also
why the two arity blocks above are accepted — once the arguments are gone there is nothing left
to count.

**This gap has no red test.** The block above is valid Zelkova and stays `expect=ok` across the
fix; the wrong behaviour is a *missing* type error, and the spec harness never runs the type
checker. The arity blocks in the previous section do go red, which is what will bring someone
back to this paragraph.

## The function arrow

`a -> b` is the type of a function from `a` to `b`. The arrow is **right-associative**, so
`a -> b -> c` means `a -> (b -> c)` — a function of one argument returning a function — and a
function of several arguments needs no parentheses.

```zel expect=ok
module Example exposing (Size, clamp)

type Size
  = Small

clamp : Size -> Size -> Size
clamp a b =
  a
```

That associativity is the one that makes the common case parenthesis-free, and it is why a
function *argument* is the case that needs them. `(a -> b) -> c` is a function taking a
function; without the parentheses it would be a function of two arguments.

```zel expect=ok
module Example exposing (Size, apply)

type Size
  = Small

apply : (Size -> Size) -> Size -> Size
apply f x =
  x
```

An arrow needs a type on both sides:

```zel expect=parse-error
module Example exposing (Size, f)

type Size
  = Small

f : Size ->
f = Small
```

## Tuple types

A tuple type is two or three types between parentheses, separated by commas. The elements may
be any type expression, including another tuple.

```zel expect=ok
module Example exposing (Size, first, second)

type Size
  = Small

first : (Size, Size) -> Size
first p =
  Small

second : (Size, Size, Size) -> Size
second p =
  Small
```

**Two or three, and nothing else.** There is no one-element tuple — `(Colour)` is grouping, as
[above](#the-forms-of-a-type-expression) — and no tuple of four or more. A four-element tuple is
a *syntax* error, caught by the grammar, rather than a type error found later:

```zel expect=parse-error
module Example exposing (Size, quad)

type Size
  = Small

quad : (Size, Size, Size, Size) -> Size
quad p =
  Small
```

The limit is deliberate, and it is one rule rather than three. A tuple of four is where a
record belongs: past three elements, position stops being a usable way to say which field is
which, and the type `(Int, Int, Int, Int)` tells a reader nothing that a record's field names
would not have told them. Drawing the line in the grammar rather than in the type checker is
what makes the answer immediate and the diagnostic about the shape of the text.

The same limit applies to tuple *patterns* and tuple *expressions*, so no tuple of any other
size is representable anywhere in the language.

## The unit type

```zel expect=unimplemented
module Example exposing (nothingUseful)

nothingUseful : ()
nothingUseful = ()
```

`()` is the type with exactly one value, and that value is also written `()`. It is what a
function returns when it has nothing to say, and it is the argument of a function that takes
nothing meaningful.

**Not implemented:** `()` is not recognised in either position. In a type the grammar reaches
for a type expression after the `(` and finds the `)`; in an expression the same happens. The
`.ignored` modules under `std/core/src/` use it freely (`Task x ()`), which is one of the
things keeping them ignored.

## Type annotations

A type annotation gives a declaration a type. It is the declaration's name, a `:`, and a type
expression.

```zel expect=ok
module Example exposing (Size, grow)

type Size
  = Small

grow : Size -> Size
grow s =
  s
```

An annotation is optional. A declaration without one gets whatever type the checker infers for
it, and is no less typed for the absence.

### An annotation is a promise

An annotation is not a hint and not a starting point for inference. It is what callers may rely
on, so the declared type may be **no more general** than what the body can actually support.

`f : a -> a` promises to return whatever the caller passed in. A body that always returns one
particular type cannot honour that, and the declaration is an error — even though there is a
type (`Size -> Size`) that would make the body check.

```zel expect=ok
module Example exposing (Size, f)

type Size
  = Small

f : a -> a
f x =
  Small
```

**Known gap:** that block should be rejected and is accepted. The annotation's type variables
become ordinary unification variables, so `a` is quietly solved to `Size` and the type the
compiler works with is not the one written in the file
([`docs/tickets/lang-12.md`](../tickets/lang-12.md)).

**This gap has no red test**, for the same reason as the one under
[An applied type still means what it says](#an-applied-type-still-means-what-it-says): the
harness stops before the type checker, and this block canonicalizes cleanly either way. The
paragraph above has to be deleted by hand when the ticket lands.

Going the other way is fine. An annotation *more* specific than the body would allow is an
ordinary, useful thing to write — it is how a general function is given a narrower published
type.

### Where an annotation goes

An annotation is written immediately above the declaration it annotates, with nothing between
them but blank lines and comments, and a declaration carries **at most one**.

Both halves are about the same thing: the annotation is the first line of a declaration a
reader reads, and it is only that if it is there and there is one of it.

```zel expect=ok
module Example exposing (Size, first, second)

type Size
  = Small

first : Size

second = Small

first = Small
```

```zel expect=ok
module Example exposing (Size, Other, ambiguous)

type Size
  = Small

type Other
  = Other

ambiguous : Size
ambiguous : Other
ambiguous = Small
```

**Known gap:** both blocks should be rejected and both are accepted. Declarations are grouped
by name into a map with their order thrown away, so an annotation may sit anywhere among the
top-level declarations; and when a name carries two, the **last** silently wins — the second
block above is checked against `Other`, not against `Size`
([`docs/tickets/lang-11.md`](../tickets/lang-11.md)).

A [JS interop](js-interop.md) facade is the one place an annotation stands alone: a
`module javascript` module is annotations with no bodies at all, and that is what makes it a
facade.

### An annotation may span several lines

An annotation is one declaration, so it may be carried onto following lines as long as they are
indented — [Layout](layout.md#top-level-declarations) carries that rule and its reasoning.

```zel expect=ok
module Example exposing (Size, combine)

type Size
  = Small

combine :
  Size -> Size -> Size
combine a b =
  a
```

The conventional way to break a long signature is one argument per line, with each continuation
line starting with the arrow:

```zel expect=parse-error:IndentationError
module Example exposing (Size, combine)

type Size
  = Small

combine : Size
  -> Size
  -> Size
combine a b =
  a
```

**Known gap:** that block should be `expect=ok`. A line whose first token starts with `-`
leaves the tokenizer believing it is still scanning leading whitespace, so the spaces *after*
the `->` are measured against the two-space indentation rule and an odd number of them is
reported as an indentation error. Whether the line is accepted therefore depends on the parity
of a space run in the middle of it — `  ->Size` and `  ->  Size` both pass.
[`docs/tickets/bug-19.md`](../tickets/bug-19.md) is the ticket.

### The annotation and the declaration's parameters

A function's parameter list and its annotation have to agree: the annotation's arrows, read
left to right, are the parameters, and the last type is what the body produces.

```zel expect=canonical-error:BindingPatternsInvalidLen
module Example exposing (Size, f)

type Size
  = Small

f : Size -> Size
f a b =
  a
```

`Size -> Size` describes one parameter; `f a b` supplies two. This is checked during
canonicalization, before the type checker runs, because it is a disagreement about *shape*
rather than about types — and the same check is what makes
`f _x` two parameters rather than one, in [Lexical structure](lexical-structure.md#the-underscore-is-not-a-letter).

A tuple or a parenthesised function counts as **one** parameter, which is the point of the
parentheses:

```zel expect=ok
module Example exposing (Size, f)

type Size
  = Small

f : (Size, Size) -> Size
f p =
  Small
```

## `type` declarations

A `type` declaration introduces a new type together with the constructors that build it. It is
the keyword `type`, the type's name, zero or more type parameters, an `=`, and the variants
separated by `|`.

```zel expect=ok
module Example exposing (Colour)

type Colour
  = Red
  | Green
  | Blue
```

The type's name is uppercase-initial; its parameters are lowercase-initial, because they are
type variables and are in scope throughout the variants.

```zel expect=ok
module Example exposing (Either)

type Either a b
  = Left a
  | Right b
```

An uppercase parameter is a syntax error — it would be a type name, and a `type` declaration
does not take types as parameters:

```zel expect=parse-error
module Example exposing (Box)

type Box A
  = Box A
```

Each declaration introduces a genuinely new type. Two declarations of the same shape, in the
same module or in different ones, are different types and are not interchangeable. A
constructor may share its type's name; nothing is ambiguous, because one is a type and the
other is a value.

```zel expect=ok
module Example exposing (Celsius)

type Celsius
  = Celsius
```

A type may refer to itself, and two types declared in one module may refer to each other in
either order — the declarations of a module are not read top to bottom.

```zel expect=ok
module Example exposing (Chain)

type Chain
  = Link Chain
  | End
```

What a `type` declaration exposes to other modules — the type alone, or the type with its
constructors — is the `Size` / `Size(..)` split in
[Modules](modules.md#the-exposing-list).

### What a variant may be

A variant is a constructor name followed by zero or more type arguments. That is the whole
form: a variant is not a type expression, and a tuple, an arrow, or a bare type variable in
variant position is not a variant.

```zel expect=ok
module Example exposing (Colour)

type Colour
  = red
```

```zel expect=ok
module Example exposing (Size, Pair)

type Size
  = Small

type Pair
  = (Size, Size)
```

```zel expect=ok
module Example exposing (Size, Wrapper)

type Size
  = Small

type Wrapper
  = Wrap Size -> Size
```

**Known gap:** all three should be rejected and all three are accepted — with the variant
**silently deleted**. The grammar parses a variant as a full type expression and
canonicalization then keeps only the ones that happen to be a constructor application,
discarding the rest without a word ([`docs/tickets/bug-18.md`](../tickets/bug-18.md)). Each
block above declares a type with *zero* variants. The first is the one most likely to be
written by accident — a mistyped constructor name — and its only symptom is that every later
mention of the constructor fails to resolve, pointing anywhere but here.

A variant's arguments are type expressions under the same rules as anywhere else, including
the parenthesisation limit in [Applying a type to arguments](#applying-a-type-to-arguments):

```zel expect=unimplemented
module Example exposing (Tree)

type Tree a
  = Node (Tree a) (Tree a)
  | Leaf a
```

**Not implemented:** [`lang-9`](../tickets/lang-9.md) again, seen from the declaration side.

### A variant list has at least one variant

`|` appears strictly between variants — never before the first, never after the last — and a
`type` declaration has at least one.

```zel expect=parse-error
module Example exposing (Colour)

type Colour
  | Red
```

```zel expect=ok
module Example exposing (Colour)

type Colour
  = Red
  |
```

```zel expect=ok
module Example exposing (Empty)

type Empty
  =
```

**Known gap:** the first block is correctly rejected; the second and third should be and are
not. A trailing `|` is ignored, and `type Empty =` declares a type with no constructors at all
— something nothing can build, arrived at by accident rather than on purpose
([`docs/tickets/lang-10.md`](../tickets/lang-10.md)).

The trailing-separator argument that won for an `exposing` list — where a trailing comma is
[deliberately allowed](modules.md#the-exposing-list) so that appending a name touches one line
— does not carry over here. A variant list is written with the separator *leading* each line,
so appending a variant already touches exactly one line and a trailing `|` would buy nothing.

## Type aliases

A type alias gives an existing type a second name. It introduces no new type: the alias and
what it names are interchangeable everywhere, in both directions, including across module
boundaries.

```zel expect=unimplemented
module Example exposing (Size, Pair)

type Size
  = Small

type alias Pair = (Size, Size)
```

**Not implemented:** `type alias` is a syntax error — the grammar reads `type` and then wants
an uppercase name, and `alias` is neither. Several modules under `std/core/src/` already write
aliases (`Task.ignored`, `Array.ignored`) and the documentation comments in `Maybe.zel` and
`Result.zel` use them in their examples, so this is a hole in the compiler rather than a
question about the language.

The transparency is the point and is worth stating separately from the syntax. `Pair` above is
*not* a new type that happens to be a pair; it is `(Size, Size)`, spelled differently. A
function annotated `Pair -> Size` accepts a `(Size, Size)` with no conversion, and a type error
mentioning one may mention the other. Where a genuinely distinct type is wanted — one the
compiler will keep apart from its representation — that is a `type` declaration with a single
variant, and the difference between the two is the whole reason both exist.
