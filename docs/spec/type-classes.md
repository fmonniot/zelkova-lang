# Type classes

A signature can say that a function takes any type at all, and it can say that a function takes
exactly one. It has nothing to say in between, and *in between* is where most of the interesting
functions live: `min` works for anything that can be ordered and for nothing else, and `add` for
the two numeric types.

A **type class** is how Zelkova writes that middle. A class names a set of operations; a type
joins the class by declaring an **instance** that implements them; and a signature says what it
needs by naming the class in front of the type it constrains.

```zel expect=ok
module Example exposing (Order, Comparable)

type Order
  = LT
  | EQ
  | GT

class Comparable a where
  compare : a -> a -> Order
```

**Not implemented:** a constraint required of a record type is accepted without being checked,
and the build then refuses a use of it, because no instance or derivation answers for a record
([`LANG-85`](../tickets/lang-85.md)).

## Declaring a class

A class declaration is the keyword `class`, the class's name, one type variable, `where`, and an
indented block of **member signatures** — one per line, each an ordinary `name : Type`.

```zel expect=ok
module Example exposing (Order, Comparable)

type Order
  = LT
  | EQ
  | GT

class Comparable a where
  compare : a -> a -> Order
  lt : a -> a -> Bool
```

The first member starts a line of its own, indented past the `class`, and sets the column every
later member starts on. A member on the `where` line, a `where` that begins its line, and a
member whose column is not the first one's are all rejected. The same holds for the bindings of
an [instance](#declaring-an-instance), and for the bindings under a derivation.

```zel expect=parse-error:LayoutError
module Example exposing (Comparable)

class Comparable a where compare : a -> a -> Bool
```

```zel expect=parse-error:LayoutError
module Example exposing (Comparable)

class Comparable a where
    compare : a -> a -> Bool
  lt : a -> a -> Bool
```

The variable in the head — `a` above — is the **class variable**. It is bound by the class, and
it is the thing every instance chooses. Inside a member's signature it stands for whichever type
the instance is for; outside, it is what the constraint constrains.

A member is an ordinary value. `compare` is callable by name, from anywhere the class is in
scope, and its type outside the class is its member signature with the class's own constraint in
front:

```zel expect=fragment
-- given the class above, `compare` has this type everywhere else
compare : Comparable a => a -> a -> Order
```

Declaring `Comparable` puts `compare` and `lt` into the module's value namespace, the way a
`type` declaration puts its constructors there. There is no separate step that exports a member,
and no qualified spelling that reaches "the class's `compare`" as distinct from the `compare`.

A class has exactly one variable, and every member's signature must mention it: a member that
does not is an error, since no use of it could say which instance it meant.

A member signature is a type and nothing more. It carries no constraint of its own, so `=>` in
one is an error at the class declaration.

A class is declared among the module's types, in [the same
namespace](name-resolution.md#namespaces): a module cannot declare a type and a class of one
name.

### Exposing and importing a class

A class is exposed by its name, written in an [`exposing` list](modules.md#the-exposing-list)
the way a type's is — `exposing (Order, Comparable)` above. That one entry exposes the class and
every member. A member is not listed in its own module's `exposing` list: it is exposed with its
class, or not at all.

An import list that names the class brings the class name and every member into scope
unqualified. An import list may also name a member by itself, as it may any exposed value, and a
member is reachable qualified — `Example.compare` — with neither.

## Declaring an instance

An instance declaration is the keyword `instance`, the class's name, the type joining it,
`where`, and an indented block of **member bindings**: one ordinary function declaration per
member, with no type annotations, because the class already gave each its type.

```zel expect=ok
module Example exposing (Colour)

type Colour
  = Red
  | Blue

type Order
  = LT
  | EQ
  | GT

class Comparable a where
  compare : a -> a -> Order
  lt : a -> a -> Bool

instance Comparable Colour where
  compare a b =
    EQ
  lt a b =
    False
```

An instance must implement **every** member of its class, and nothing else. A missing member is
an error naming the member and the class: a constrained caller may rely on the members being
there. A binding that names no member of the class is an error too.

An instance has no name and is never mentioned by one. It is not exposed, not imported, and
never written in an `exposing` list. It is in scope wherever its class and its type are, and how
far "wherever" reaches is [the orphan rule](#where-an-instance-may-be-declared), below.

### What an instance is declared for

The type an instance is for is its **head**, and a head is one of three things: a declared type
applied to as many distinct type variables as it has parameters, a tuple type whose elements are
distinct type variables, or `()`.

```zel expect=fragment
instance Eq Colour where …

instance Eq (Maybe a) where …

instance Eq (a, b) where …
```

An argument that is not a variable is an error — `instance Eq (Maybe Int)` — and so is one
variable written twice. An instance is therefore identified by its class and the name at the
front of its head, and two instances that agree on both are the same instance declared twice.

A function type is not a head, and neither is a [record type](records.md#records-and-derivation).
A head written through a [type alias](types.md#type-aliases) is the type the alias names, and is
held to this rule as that type.

An instance for a type with parameters may need something of them. It says so with a context,
in the notation [a signature uses](#constraining-an-annotation):

```zel expect=ok
module Example exposing (Box, Eq)

type Box a
  = Box a

class Eq a where
  eq : a -> a -> Bool

instance Eq a => Eq (Box a) where
  eq (Box left) (Box right) =
    eq left right
```

Each constraint of the context is on one of the head's variables. Inside the instance's bindings
the context holds as it does inside a constrained function, and a use of the instance at
`Box Colour` requires an `Eq Colour` in turn.

### An instance may be derived

An instance may ask for the definition its type's shape already implies rather than write it.
Its body is the single word `derived`:

```zel expect=ok
module Example exposing (Colour)

type Colour
  = Red
  | Green
  | Blue

class Eq a where
  eq : a -> a -> Bool

  derived eq
    matched = True
    differed _ _ = False
    combine x y =
      case x of
        True ->
          y

        False ->
          False

instance Eq Colour where
  derived
```

It is an ordinary instance declaration in every other respect.

`derived` is the **whole** body: an instance is either derived or written out, never a mixture.
A derivation defines every member of the class at once, out of one description of the type's
shape.

### A class says how it is derived

What a derivation can see is the **shape** of a value — which constructor, in what position, with
what arguments — and shape alone says nothing about what an answer to a member means. A class
that may be derived is one whose own declaration supplies that half, in ordinary Zelkova.

A member signature may be followed by a **derivation**: the word `derived`, the member it is for,
and the bindings that member's signature calls for.

```zel expect=ok
module Example exposing (Eq)

class Eq a where
  eq : a -> a -> Bool

  derived eq
    matched = True
    differed _ _ = False
    combine x y =
      case x of
        True ->
          y

        False ->
          False
```

`matched` is the answer when the walk finds nothing to tell the two values apart. `differed` is
the answer when the two values are of different constructors, and receives the **position** each
constructor is declared at, counting from zero. `combine` folds the answers from the parts into
the answer for the whole. None of the three mentions the class variable, so all three stand for
every type that derives the class.

A member's signature decides whether it may carry a derivation, and which one. The derivation
above walks **two** values and needs `a -> a -> R`, with the class variable absent from `R`: two
values to walk in step, and an answer that is not itself of the type being walked. `matched` is
then an `R`, `differed` a `Position -> Position -> R`, and `combine` an `R -> R -> R`. A member
at `a -> R` carries [a derivation over one value](#a-derivation-over-one-value), which is the
same walk with two bindings in place of three. Any other signature is an error at the class
declaration.

`Position` is the declaration position of a constructor, and it is a type rather than a number:
`std/core` declares it, gives it `Eq` and `Comparable` instances and a `positionIndex :
Position -> Int`, and offers nothing else. A class that wants to order two constructors compares
them; a class that wants to compute with them converts.

Two member shapes therefore cannot carry a derivation at all, and the reasons differ:

- **A member returning `a`** — the shape `add` and `append` have in
  [what the standard library declares](#what-the-standard-library-declares) — would ask the walk
  for a third value of the type it is walking, and nothing a class says about itself can lend it
  the means.
- **A member taking no `a`** — `bottom : a`, `allValues : List a` — asks the walk to run
  backwards and build a value from a description of the type's constructors. That description is
  the thing this design does not have.

```zel expect=canonical-error:DerivationSignature
module Example exposing (Number)

class Number a where
  add : a -> a -> a

  derived add
    matched = 0
    differed _ _ = 0
    combine x y =
      x
```

A class is derivable when **every** member carries a derivation, and the two forms mix freely
across one class: each member takes the form its own signature admits. Covering some members and
not the rest is an error naming the ones left out: an instance of such a class could only be half
derived and half written.

```zel expect=canonical-error:DerivationIncomplete
module Example exposing (Eq)

class Eq a where
  eq : a -> a -> Bool
  neq : a -> a -> Bool

  derived eq
    matched = True
    differed _ _ = False
    combine x y =
      case x of
        True ->
          y

        False ->
          False
```

A derivation takes exactly the bindings its member's signature calls for. A binding left out, one
written twice and one the signature does not call for are each an error naming the binding, and
so is a derivation for a name the class does not declare as a member, or a second one for a member
that has one.

**Known gap:** a binding may take more parameters than the walk supplies it, because `R` may
itself be a function type. A member at `hashWith : a -> Int -> Int` has `R = Int -> Int`, and
`atConstructor p n = n` has the type `Position -> Int -> Int` that this section gives
`atConstructor`. That block should be accepted and is rejected, because canonical code has no
lambda to place a binding with a parameter the walk did not supply it, so a derivation for such a
member can only be written without the extra parameter.
[`LANG-87`](../tickets/lang-87.md) is the ticket.

```zel expect=canonical-error:DerivationBindingTakesTooMany
module Example exposing (HashWith)

class HashWith a where
  hashWith : a -> Int -> Int

  derived hashWith
    atConstructor p n =
      n

    combine x y =
      x
```

### What a derived instance computes

The derivation walks the two values in step, and every answer it collects comes from an instance.

- **The constructors first.** When the two values have different constructors, `differed` answers
  and the walk stops. There is nothing further to compare: two different constructors need not
  take the same number of arguments, nor arguments of the same types, so no pair of arguments
  lines up. `differed` is handed one **position** per value — where that value's constructor is
  declared, counting from zero — and what it makes of the two is the class's own business.
  Because positions follow the order the variants are written in, and not, say, the alphabetical
  order of their names, reordering a type's variants changes what a derived member computes.
- **Then the arguments,** when the constructors agree: each pair in turn, left to right, through
  the instance belonging to *that argument's* type.
- **`combine` folds those answers** into one, in that order, ending at `matched`.

The walk never looks inside an argument. It hands each pair to the instance that argument's type
declares and takes whatever that instance answers, so a type keeps its own definition of a member
wherever it appears inside a derived one.

`Eq`'s three definitions and `Comparable`'s are that one walk with different answers filled into
it. `Eq`'s turn it into
[structural equality](evaluation-semantics.md#what-structural-equality-computes): `differed`
discards the positions to answer `False` outright, `combine` carries a single unequal pair of
arguments out to the answer, and `matched` makes a constructor with no arguments equal to itself.

`Comparable`'s three turn it into a lexicographic ordering — different constructors ordered by
the positions they are declared at, so `Red` is less than `Green` in the `Colour` type above;
two of the same constructor by their arguments, left to right, the first unequal pair deciding:

```zel expect=fragment
-- `Basics`' declaration of `Comparable`
class Eq a => Comparable a where
  compare : a -> a -> Order

  derived compare
    matched = EQ
    differed i j =
      compare i j

    combine x y =
      case x of
        EQ ->
          y

        _ ->
          x
```

### A derivation over one value

A derivation for a member at `a -> R` walks **one** value, and asks the class for two bindings
rather than three.

```zel expect=ok
module Example exposing (Hashable)

-- `positionIndex` and `add` are `Basics`'s
class Hashable a where
  hash : a -> Int

  derived hash
    atConstructor p =
      positionIndex p

    combine x y =
      add x y
```

`atConstructor` is the answer for the constructor the value is of, and receives the position that
constructor is declared at.

With one value there are no two constructors to disagree, so the half of the walk that compares
them falls away and `differed` with it: `atConstructor` answers for *the* constructor, each
argument is answered in turn, left to right, by the instance belonging to that argument's type,
and `combine` folds those answers onto the constructor's own. Everything else holds as it does
[over two values](#what-a-derived-instance-computes).

There is no `matched`, because the fold begins at `atConstructor`'s answer and is therefore never
empty, not even for a constructor with no arguments. What `combine` is
[trusted to keep](#what-a-derivation-is-trusted-to-keep) is associativity alone.

### Deriving for a tuple

A tuple has one shape, so the half of the walk that answers for a constructor is never reached:
there are no two constructors to differ, and no position to hand `differed` or `atConstructor`.
What is left is the argument half — each element in turn, through the instance belonging to that
element's type, folded with `combine`. A two-value derivation walks the elements in pairs and
ends at `matched`; a one-value derivation walks them singly and starts at the first element's
answer.

```zel expect=ok
module Example exposing (Eq)

class Eq a where
  eq : a -> a -> Bool

  derived eq
    matched = True
    differed _ _ = False
    combine x y =
      case x of
        True ->
          y

        False ->
          False

instance Eq (a, b) where
  derived
```

`()` has no element. A two-value derivation answers `matched` for it; a one-value derivation has
nothing for its fold to begin at, so `instance C () where derived` is an error for a class
whose derivation walks one value.

```zel expect=canonical-error:DerivedInstanceNoShape
module Example exposing (Hashable)

class Hashable a where
  hash : a -> Int

  derived hash
    atConstructor _ =
      1

    combine x y =
      x

instance Hashable () where
  derived
```

### What a derivation cannot render

`toString : a -> String` has the signature a one-value derivation takes, so a class may carry one
for it. What the walk produces is `RedGreen` where the reader wanted `Red Green`, and three
things stand between the two.

1. **A constructor's name.** A `Position` orders constructors and converts to an `Int`. Neither
   yields `"Red"`.
2. **Parenthesisation**, which is context handed *downwards* into the arguments. A walk that
   folds answers upwards has no downward channel, so an argument cannot be told that it is being
   rendered somewhere brackets are wanted around it.
3. **Which argument is the first**, so that a separator goes between two answers and not before
   the leftmost. `combine` is handed two answers and cannot tell where in the fold it sits.

Rendering a value needs a mechanism that reads constructor names, or a compiler primitive that
reads a value's representation. Zelkova has neither, and a program that wants a rendering writes
the instance.

### The bindings are inlined, not called

A derivation is a **compile-time** step. The compiler reads the class's bindings and the type's
shape and writes the member's definition out of them; what runs is that definition. Neither the
walk nor the bindings exist at run time: a derivation's bindings are substituted into the
generated definition at each step rather than called as functions.

`combine`'s two parameters are not alike. Its first names a **value**: the answer for one part,
computed once however often the body mentions it. Its second stands for **the rest of the
walk** — everything after that part — which is performed where the body reaches it and nowhere
else.

Under [strict evaluation](evaluation-semantics.md#evaluation-is-strict) the difference is
observable, and it is not one the compiler could make on its own. An argument is a value before
the function it is passed to is entered, so were `combine` an ordinary call,
`combine (eq a1 b1) (combine (eq a2 b2) matched)` would compare every pair of arguments in the
whole value before the outermost `combine` ran — including every pair after the one that already
settled the answer. Substituted, the same three definitions sit inside the walk, where
[`case` evaluates one branch and not the other](evaluation-semantics.md#conditional-evaluation)
like any other `case` in the language.

So **a class decides its own short-circuiting, by what it writes**. `Eq`'s `combine` above is a
`case` on its first argument, so a derived `eq` stops at the first unequal pair and never looks
at the rest of the value; `Comparable`'s stops at the first answer other than `EQ`. Written
instead as `combine x y = and x y`, the ordinary function call, `Eq`'s derivation would compare
every pair — the same answer, at the cost of the whole value — because
[nothing short-circuits](evaluation-semantics.md#nothing-short-circuits) and `and` is a function
like any other. Both spellings are legal and the compiler prefers neither. Which one a class
writes is visible in the class's own declaration.

Inlining reaches only those bindings. Anything they *call* is an ordinary call, evaluated
under the ordinary rules — so a `combine` whose short-circuiting hides behind a helper does not
get it back.

### What a derivation is trusted to keep

`combine` is a fold, and a fold has a shape the answers must not notice: **`combine` has to be
associative, and a two-value derivation's `matched` an identity on both sides.**

```zel expect=fragment
combine (combine x y) z  =  combine x (combine y z)

combine matched x        =  x
combine x matched        =  x
```

A [one-value derivation](#a-derivation-over-one-value) owes the associativity and not the
identity. Its fold starts at `atConstructor`'s answer and never at an empty one, so there is
nothing for an identity to stand in for.

Keeping the law is what makes a derived member's answer a property of the value rather than of
the walk: how many arguments a constructor happens to have, and how the walk groups their
answers, cannot change it.

**Nothing checks this.** The law is about values of a type the compiler is not reading, and the
language has no way to state it, so a derivation that breaks it compiles and computes whatever
the fold order gives it. Breaches come in two shapes.

The first is **an answer whose meaning depends on how many things were combined**: an average, a
ratio, a proportion of the arguments that matched. The same idea counted rather than averaged
(`matched = 0`, `differed _ _ = 1`, `combine = add`) is a monoid, and the walk gets it right for
a constructor of any size.

The second is **an answer that weights each part by where the fold reached it**, and it is what
the usual way of mixing a hash does: `combine x y = add (mul 31 x) y` multiplies its left answer
once more for every `combine` closing over it, so the same parts grouped differently mix to
different numbers. A hash keeps the law by mixing with an operation grouping cannot see —
`Bitwise.xor`, or the plain `add` above.

**Nothing will check it either.** Establishing the law before a program runs means reading what
a derivation's answers mean, which is the one thing this mechanism does not do, so the law is
permanently the class author's to keep ([DEC-10](../decisions/dec-10.md)).

### What a derived instance requires

Every argument of every variant needs an instance of the class being derived, and nothing else
does. In particular a class owes no `Position` instance for the positions `differed` and
`atConstructor` receive: what a class makes of them is written in its own bindings.

Where an argument's type is a variable, the requirement becomes a **constraint on the derived
instance**, inferred rather than written:

```zel expect=ok
module Example exposing (Box)

type Box a
  = Box a

class Eq a where
  eq : a -> a -> Bool

  derived eq
    matched = True
    differed _ _ = False
    combine x y =
      case x of
        True ->
          y

        False ->
          False

instance Eq (Box a) where
  derived
```

```zel expect=fragment
-- the instance that declaration yields
Eq a => Eq (Box a)
```

Two `Box`es are equal when their contents are, which is only a definition of equality once the
contents have one. A parameter no variant uses carries no constraint.

The context is never written. A context written on a `derived` instance is an error, and what one
would mean beside the inferred one is [an open question](#open-questions):

```zel expect=canonical-error:DerivedInstanceWritesContext
module Example exposing (Box)

type Box a
  = Box a

class Eq a where
  eq : a -> a -> Bool

  derived eq
    matched = True
    differed _ _ = False
    combine x y =
      case x of
        True ->
          y

        False ->
          False

instance Eq a => Eq (Box a) where
  derived
```

Where the argument's type is concrete, the requirement is checked at the declaration, and an
argument whose type has no instance is an error there, naming the variant and the type:

```zel expect=canonical-error:DerivedInstanceRequires
module Example exposing (Key, Entry)

type Key
  = Key

type Entry
  = Entry Key

class Eq a where
  eq : a -> a -> Bool

  derived eq
    matched = True
    differed _ _ = False
    combine x y =
      case x of
        True ->
          y

        False ->
          False

instance Eq Entry where
  derived
```

The instance is the claim that an `Entry` can be compared for equality, and the claim is false
where it is written. A variant holding a **function** is the case no instance can rescue, since a
function type [has no useful equality at all](evaluation-semantics.md#functions-are-not-comparable).

A superclass obligation is unchanged: a derived `Comparable Colour` is rejected unless an
`Eq Colour` instance exists, derived in its turn or written out. For a type with parameters the
obligation is held to the context the derived instance was inferred, as a written instance's is: a
derived `Comparable (Phantom a)` for `type Phantom a = Phantom Int` has no constraint on `a`, so it
is an error beside `instance Eq a => Eq (Phantom a)`, which needs one.

The type's constructors must be in scope where the instance is written, since the walk is read
off them: a derived instance for a type imported [without its
constructors](modules.md#the-exposing-list) is an error.

```zel expect=ok package=opaque
module Colour exposing (Colour)

type Colour
  = Red
  | Green
```

```zel expect=canonical-error:DerivedInstanceNoShape package=opaque
module Example exposing (Eq)

import Colour exposing (Colour)

class Eq a where
  eq : a -> a -> Bool

  derived eq
    matched = True
    differed _ _ = False
    combine x y =
      case x of
        True ->
          y

        False ->
          False

instance Eq Colour where
  derived
```

A [scalar type](types.md#scalar-types) has no shape for a walk to read either, so its instances
are written.

```zel expect=canonical-error:DerivedInstanceNoShape
module Example exposing (Eq)

class Eq a where
  eq : a -> a -> Bool

  derived eq
    matched = True
    differed _ _ = False
    combine x y =
      case x of
        True ->
          y

        False ->
          False

instance Eq Int where
  derived
```

And the class must be one that says how it is derived. `derived` under a class whose declaration
carries no derivation is an error naming the class — not because the compiler holds a list of the
classes that do, but because the declaration the instance names has nothing in it to run.

```zel expect=canonical-error:DerivedInstanceNotDerivable
module Example exposing (Colour, Eq)

type Colour
  = Red
  | Green

class Eq a where
  eq : a -> a -> Bool

instance Eq Colour where
  derived
```

## Constraining an annotation

A constraint is written in front of the type, separated from it by `=>`. It names a class and
the variable that class applies to, and that variable must be one the type mentions:
`Eq b => a -> a` is an error, since no caller could say which `b` it meant.

```zel expect=ok
module Example exposing (Order, Comparable, min)

type Order
  = LT
  | EQ
  | GT

class Comparable a where
  compare : a -> a -> Order

min : Comparable a => a -> a -> a
min x y =
  x
```

Read it as a precondition on the caller: *`min` works for any type `a`, provided `a` is
`Comparable`*. A caller supplying a type with no `Comparable` instance is an error at the call
site, pointing at the call.

```zel expect=type-error:NoInstance
module Example exposing (Order, Comparable, Colour, min, smaller)

type Order
  = LT
  | EQ
  | GT

type Colour
  = Red
  | Blue

class Comparable a where
  compare : a -> a -> Order

min : Comparable a => a -> a -> a
min x y =
  x

smaller : Colour
smaller =
  min Red Blue
```

Several constraints are parenthesised and comma-separated:

```zel expect=ok
module Example exposing (Bit, Eq, Comparable, describe)

type Bit
  = Zero
  | One

class Eq a where
  eq : a -> a -> Bool

class Eq a => Comparable a where
  lt : a -> a -> Bool

describe : (Comparable k, Eq v) => k -> v -> Bit
describe a b =
  Zero
```

The list may be of any length:

```zel expect=ok
module Example exposing (Bit, Eq, four)

type Bit
  = Zero

class Eq a where
  eq : a -> a -> Bool

four : (Eq a, Eq b, Eq c, Eq d) => a -> b -> c -> d -> Bit
four a b c d =
  Zero
```

### A constraint is never inferred

A constraint is part of a declaration's type only where its annotation wrote it. A declaration
with no annotation, whose body needs a class of a type nothing determines, is an error:

```zel expect=type-error:ConstraintNeedsAnnotation
module Example exposing (Eq)

class Eq a where
  eq : a -> a -> Bool

same x y =
  eq x y
```

`same : Eq a => a -> a -> Bool` is what that declaration has to say. A declaration whose body
pins the type down needs no annotation — the class is required of a known type, and that is
settled where it stands.

```zel expect=ok
module Example exposing (Eq)

class Eq a where
  eq : a -> a -> Bool

instance Eq Int where
  eq a b =
    True

isZero n =
  eq n 0
```

### A constraint belongs to a signature, not to a type

`=>` may appear once, at the very front of an annotation, and nowhere else. A constraint is a
statement about the declaration being annotated; it is not a piece of type syntax that can be
nested inside a larger type.

```zel expect=parse-error
module Example exposing (Size, f)

type Size
  = Small

f : Size -> (Comparable a => a)
f x =
  Small
```

The left of `=>` must be constraints; a type there is an error:

```zel expect=canonical-error:InvalidConstraint
module Example exposing (Size, f)

type Size
  = Small

f : Size -> Size => Size -> Size
f x =
  x
```

`Size -> Size` is a perfectly good type, and `(Comparable k, Eq v)` is — read as a type — a
perfectly good two-tuple. Nothing about the tokens distinguishes a constraint list from a type,
so a constrained annotation is read as a type first and checked to be constraint-shaped
afterwards, and this is the error that check produces.

## Superclasses

A class may require another. `Comparable` needs equality, so it is declared with `Eq` in front
of its own head, in the same `=>` notation a signature uses:

```zel expect=ok
module Example exposing (Order, Eq, Comparable)

type Order
  = LT
  | EQ
  | GT

class Eq a where
  eq : a -> a -> Bool

class Eq a => Comparable a where
  compare : a -> a -> Order
```

Two things follow.

**An instance acquires an obligation.** `instance Comparable Colour` is rejected unless
`instance Eq Colour` also exists. A type cannot be ordered without being comparable for equality
first, and the declaration is where that is enforced.

**A signature loses one.** A function constrained by `Comparable a` may use `eq` as well as
`compare`, without naming `Eq`. The superclass is implied by the subclass, so
`Comparable a => …` is the whole precondition and `(Eq a, Comparable a) => …` says nothing more.

```zel expect=ok
module Example exposing (Order, Eq, Comparable, same)

type Order
  = LT
  | EQ
  | GT

class Eq a where
  eq : a -> a -> Bool

class Eq a => Comparable a where
  compare : a -> a -> Order

same : Comparable a => a -> a -> Bool
same x y =
  eq x y
```

## Where an instance may be declared

An `instance C T` declaration is legal in **the module that declares `C`**, and in **the module
that declares `T`**, and nowhere else.

A tuple type and `()` are declared in no module, so for them only the first clause applies: an
instance whose [head](#what-an-instance-is-declared-for) is a tuple or `()` is legal in the
module that declares the class, and nowhere else.

Given a class and a type, each declared in a module of its own:

```zel expect=ok package=orphan
module Comparable exposing (Comparable, Order(..))

type Order
  = LT
  | EQ
  | GT

class Comparable a where
  compare : a -> a -> Order
```

```zel expect=ok package=orphan
module Colour exposing (Colour(..))

type Colour
  = Red
  | Blue
```

an instance of the one for the other in any third module is rejected:

```zel expect=canonical-error:OrphanInstance package=orphan
module App exposing (Main)

import Colour exposing (Colour)
import Comparable exposing (Comparable, Order(..))

type Main
  = Main

instance Comparable Colour where
  compare a b =
    EQ
```

The rule exists because an instance is the one thing that crosses a module boundary without
being named. Everything else — a value, a type, an operator — arrives because an importer wrote
it down, so two modules disagreeing about a name is a question the importer can settle. An
instance arrives unasked, and it has to: a constrained call must mean the same thing in every
module that can write it, so an instance cannot wait to be imported.

Given that, two instances of one class for one type would make the meaning of a call depend on
what else happened to be linked into the program — which is not a question any source file can
answer. Tying an instance to the class's module or the type's module makes a duplicate
impossible to write accidentally: the only way to produce one is for both of those two modules
to declare it, and that collision is visible to whoever reads either file.

The cost falls on the third party: if neither the class nor the type is yours, you cannot make
the one an instance of the other, and the way out is a type of your own that wraps the one you
wanted.

A **package** boundary adds nothing to this rule. Every module belongs to exactly one package
([Packages and source layout](packages.md)), so an instance can only be written by whoever owns
the class's module or the type's module — which is to say by whoever ships one of those two
packages. A third package that depends on both still cannot pair them.

## A class is always over a complete type

A type variable stands for a complete type and is never applied — that is the rule
[Types](types.md#type-variables) already states, and a class mechanism does not relax it. A class
variable is a type variable, so a class is always over a complete type.

So classes whose variable stands for a *type constructor* cannot be written. There is no
`Functor`, no `Monad`, no class over "a thing that takes one type argument".

```zel expect=parse-error
module Example exposing (Box)

type Box a
  = Box a

class Functor f where
  map : (a -> b) -> f a -> f b
```

That block is rejected because `f a` is not a type — it applies a variable. Allowing it would
need variables ranging over type constructors as well as types, which is what a kind system is
for, and Zelkova does not have one.

## Numeric literals

A numeric literal carries no constraint. Its type is decided by how it is spelled — `1` is an
`Int`, `1.5` is a `Float` — and [Expressions](expressions.md#a-literals-type-is-its-spelling)
is where that rule lives.

```zel expect=ok
module Example exposing ()

x =
  1
```

**Nothing in the language defaults, and the compiler knows no class by name.** A constraint the
solver cannot discharge is an error rather than a guess, in every case and with no exception
carved out for arithmetic; there is no way to declare what a class falls back to, and no class
the compiler treats differently from one a program declares itself. Not even
[a derived instance](#a-class-says-how-it-is-derived) is an exception: what it computes is read
off the class's own declaration.

The price is paid inside a constrained function, where a literal is already concrete and so
cannot be used at the constrained type — `double x = mul x 2` under `Number a => a -> a`
forces `a` to be `Int`. A class that wants numeric constants declares them as members, the way
it declares everything else.

## A constrained function may not be a foreign facade

A `module foreign` facade declares signatures with no bodies, backed by a companion
file. None of those signatures may carry a constraint.

```zel expect=canonical-error:FacadeConstrained
module foreign Core.Cmp exposing (compare)

unsafe compare : Comparable a => a -> a -> Int
```

The reason is a rule in [Foreign
interoperability](interop.md#what-a-facade-signature-may-not-name): a facade signature names
the types the code behind it really handles, so a facade is monomorphic. `Comparable a => a`
is still a signature over `a`.

A constrained function is **specialised** — the compiler generates one ordinary function per
type the constraint is discharged at — and a facade has no body to generate one from. Its
companion's export is the whole implementation, so a constrained facade would have to serve
every instance from that one foreign function, which could only tell them apart by inspecting
arguments whose type its signature never named — dispatch on a type variable.

So the constraint moves up one level. The facade stays monomorphic and is called only at types
the code behind it can actually handle; the class, its instances, and the constraint live in
ordinary Zelkova above it:

```zel expect=fragment
-- Core/Utils.zel — no constraint, and one signature per type it really supports
compareInt : Int -> Int -> Int
compareChar : Char -> Char -> Int

-- Basics.zel — the constraint lives here
instance Comparable Int where
  compare a b =
    orderOf (Js.Utils.compareInt a b)
```

Specialisation asks one thing of a program: the specialisations it needs must be a finite set.
A constrained function that calls itself at an ever-larger type has no such set.

```zel expect=fragment
loop : Eq a => a -> Bool
loop x =
  loop (Box x)
```

`loop` at `Colour` needs `loop` at `Box Colour`, which needs it at `Box (Box Colour)`, without
end. That is an error, reported against the declaration with the type that kept growing. It is
the same error when the chain runs through two functions that call each other.

## The words this reserves

`class` and `instance` are reserved words, usable nowhere but at the start of the declarations
they introduce. `where` is reserved in two narrower senses: it opens a class or instance body,
and it may not be a type variable. Everywhere a *value* is named — a declaration, a parameter,
an `exposing` entry — `where` stays an ordinary identifier. `class` and `instance` share the
start of a line with a function declaration, so nothing tells the two apart unless those words
are reserved; `where` needs only its one type-variable position excluded.

`=>` is a token of the language rather than a name, so it cannot be declared as an operator.

```zel expect=parse-error
module Example exposing (both)

type Size
  = Small

infix left 5 (=>) = both

both : Size -> Size -> Size
both a b =
  a
```

A class's own name reserves nothing either. `Comparable` is an ordinary uppercase name that a
module declares, no different from a type.

`derived` reserves nothing either. It is a [soft keyword](lexical-structure.md#reserved-words) —
a keyword in a class body and in an instance body, an ordinary identifier in every other
position, the name of a class's own member included. What tells the readings apart is the token
after it: `derived` alone is [the request](#an-instance-may-be-derived), `derived eq` opens
[a derivation](#a-class-says-how-it-is-derived) for the member `eq`, and `derived : …` or
`derived = …` declares a member called `derived`. One token of lookahead settles it.

`class` and `instance` cannot name a value, and `where` cannot be a type variable. Each is a
syntax error. `class` and `instance` as value names:

```zel expect=parse-error
module Example exposing (Size)

type Size
  = Small

class : Size
class =
  Small

instance : Size
instance =
  Small
```

`where` as a type variable:

```zel expect=parse-error
module Example exposing (Box)

type Box where
  = Box where
```

## What the standard library declares

Four classes, and they are ordinary declarations in ordinary modules. A program may declare its
own alongside, and nothing about what a member means, or about which types have instances, is
built into the compiler.

No name is an exception. `Number` is as ordinary as the other three: the compiler does not know
it by name and knows no instance of it (*[Numeric literals](#numeric-literals)*, above).

| Class | Members | What it constrains a variable to |
|---|---|---|
| `Eq` | `eq` | types that can be compared for equality |
| `Comparable` (superclass `Eq`) | `compare` | types that are ordered |
| `Number` | `add`, `sub`, `mul`, `pow`, `negate`, `abs` | the numeric types |
| `Appendable` | `append` | types `++` joins |

`Eq` and `Comparable` have one member each, and what is built on them is ordinary functions
constrained by the class: `neq : Eq a => a -> a -> Bool`, `lt : Comparable a => a -> a -> Bool`,
and `le`, `gt`, `ge`, `min`, `max` and `clamp` likewise. A class is derivable only when *every*
member is, and `lt` is not: a lexicographic `lt` cannot be folded out of the `lt` of each pair
of arguments, because it has to know whether the pair before it was *equal*. `compare` can, so
`Comparable` keeps `compare` and builds the rest on top.

`Number` carries `negate` and `abs` as members because neither can be written over the other
four: both need a zero, and [a literal is an `Int`](#numeric-literals).

| Class | Instances |
|---|---|
| `Eq` | `Int`, `Float`, `Char`, `String`, `Bool`, `Order`, `Maybe`, `Result`, `Failure`, `Position`, a tuple of two and of three, `()` |
| `Comparable` | `Int`, `Float`, `Char`, `String`, `Position`, a tuple of two and of three |
| `Number` | `Int`, `Float` |
| `Appendable` | `String`, and `List` |

The tuple and `()` instances are [declared beside the class](#where-an-instance-may-be-declared)
and are [derived](#deriving-for-a-tuple).

Two of the four carry [derivations](#a-class-says-how-it-is-derived): `Eq` and `Comparable`.
`Number` and `Appendable` carry none and could not — `add` and `append` return the class
variable, which no walk over a value has a way to produce. A program's own class is derivable on
exactly the same terms as either.

`Basics`, the module declaring `Eq` and `Comparable`, also declares `Position`, the type a
derivation [is handed for a constructor](#a-class-says-how-it-is-derived) — with an `Eq`
instance, a `Comparable` instance and `positionIndex : Position -> Int`, and with no way to
construct one. It
is the one type here the compiler knows by name, because it has to give those parameters a type
before any class has been read. That is a name for a *type*, which the compiler already has four
of; it is not a name for a class.

**Not implemented:** [Lists](lists.md) are not implemented, so `Appendable`'s `List` instance
waits on them.

## Open questions

- **A context written on a `derived` instance.** `instance Eq a => Eq (Box a) where derived`
  parses, and the context of a derived instance is
  [inferred](#what-a-derived-instance-requires). Whether a written context may stand beside the
  inferred one, has to equal it, bounds it from above or is an error is unanswered. The compiler
  rejects one, which is the choice a later answer cannot break.
  [`SPEC-39`](../tickets/spec-39.md) carries it.
- **Which constructors a derived instance needs in scope.**
  [*What a derived instance requires*](#what-a-derived-instance-requires) rejects a derived
  instance for a type imported [without its constructors](modules.md#the-exposing-list). Whether
  that is read off the import list (`import Colour exposing (Colour)`) or off what the declaring
  module exposes (`Colour(..)`) is unanswered; the compiler reads the second.
  [`SPEC-39`](../tickets/spec-39.md) carries it.
- **What lists add.** Having them makes an n-ary `combine : List R -> R` writable, which would
  let a class see how many answers it is folding and retire
  [the law above](#what-a-derivation-is-trusted-to-keep) by making the fold the class's to
  perform rather than the walk's. Which of the two shapes `combine` takes is unsettled, and is
  not a reason to hold this design. Records reach the mechanism too and are settled:
  [a record is walked field by field in label order](records.md#records-and-derivation), with the
  bindings a class already supplies.
- **A record and a class that carries no derivation.** A record is walked by a class that says
  how it is derived and is [the head of no instance](#what-an-instance-is-declared-for). A class
  that carries no derivation — one turning a value into JSON, say — therefore cannot reach a
  record at all, where it reaches a tuple through an instance its own module declares. Whether a
  record type may be a head, and what such an instance would be declared over, is unsettled.
