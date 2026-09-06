# Type classes

A signature can say that a function takes any type at all, and it can say that a function takes
exactly one. It has nothing to say in between, and *in between* is where most of the interesting
functions live. `min` works for anything that can be ordered and for nothing else; `add` works
for the two numeric types and not for a `Char`. Neither of those is "any type", and neither is
one type.

A **type class** is how Zelkova writes that middle. A class names a set of operations; a type
joins the class by declaring an **instance** that implements them; and a signature says what it
needs by naming the class in front of the type it constrains.

```zel expect=unimplemented
module Example exposing (Order, Comparable)

type Order
  = LT
  | EQ
  | GT

class Comparable a where
  compare : a -> a -> Order
```

**Not implemented:** the compiler has none of this, and none of it parses. Every block here
showing a class, an instance or a constraint is tagged `expect=unimplemented` for that reason,
and each goes red the day the construct it shows starts working. The `CLASS-` program in
[`docs/tickets/README.md`](../tickets/README.md) is the implementation, in the order it has to
land.

## Declaring a class

A class declaration is the keyword `class`, the class's name, one type variable, `where`, and an
indented block of **member signatures** — one per line, each an ordinary `name : Type`.

```zel expect=unimplemented
module Example exposing (Order, Comparable)

type Order
  = LT
  | EQ
  | GT

class Comparable a where
  compare : a -> a -> Order
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
There is one `compare`.

A class has exactly one variable. A class over two types at once — a relation rather than a
property — is not part of this design.

## Declaring an instance

An instance declaration is the keyword `instance`, the class's name, the type joining it,
`where`, and an indented block of **member bindings**: one ordinary function declaration per
member, with no type annotations, because the class already gave each its type.

```zel expect=unimplemented
module Example exposing (Colour)

type Colour
  = Red
  | Blue

type Order
  = LT
  | EQ
  | GT

instance Comparable Colour where
  compare a b =
    EQ
  lt a b =
    False
```

An instance must implement **every** member of its class. A missing member is an error naming
the member and the class, not a value that silently does not exist — the whole point of the
constraint is that a caller may rely on the members being there.

An instance has no name and is never mentioned by one. It is not exposed, not imported, and
never written in an `exposing` list. It is in scope wherever its class and its type are, which
is what makes a constrained call mean the same thing in every module that can write it. How far
"wherever" reaches is [the orphan rule](#where-an-instance-may-be-declared), below.

```zel expect=ok
module Example exposing (Thing)

type Thing
  = Comparable
  | Colour
  | EQ

instance Comparable Colour where
  compare a b =
    EQ
```

**Known gap:** an `instance` declaration is not rejected today. It is *accepted as something
else*. `instance` is an ordinary lowercase identifier, so the parser reads the line as a function
declaration named `instance` whose parameters are `Comparable`, `Colour`, `where`, `compare`, `a`
and `b` — and when those names happen to resolve. [`CLASS-2`](../tickets/class-2.md) is the ticket,
and this block goes red when it lands.

### An instance may be derived

An instance may ask for the definition its type's shape already implies rather than write it.
Its body is the single word `derived`:

```zel expect=unimplemented
module Example exposing (Colour)

type Colour
  = Red
  | Green
  | Blue

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
and three bindings.

```zel expect=unimplemented
module Example exposing (Eq)

class Eq a where
  eq : a -> a -> Bool

  derived eq
    matched = True
    differed _ _ = False
    combine x y =
      and x y
```

`matched` is the answer when the walk finds nothing to tell the two values apart. `differed` is
the answer when the two values are of different constructors, and receives the **position** each
constructor is declared at, counting from zero. `combine` folds the answers from the parts into
the answer for the whole. None of the three mentions the class variable, so all three stand for
every type that derives the class.

A member may carry a derivation only when its signature is `a -> a -> R`, with the class variable
absent from `R`: two values to walk in step, and an answer that is not itself of the type being
walked. `matched` is then an `R`, `differed` an `Int -> Int -> R`, and `combine` an
`R -> R -> R`. Anything else is an error at the class declaration. A member returning `a` — the
shape `add` and `append` have in
[what the standard library declares](#what-the-standard-library-declares) — would ask the walk for
a third value of the type it is walking, and nothing a class says about itself can lend it the
means.

A class is derivable when **every** member carries a derivation. Covering some and not the rest
is an error naming the members left out: an instance of such a class could only be half derived
and half written, which is the mixture a `derived` body rules out.

### What a derived instance computes

The derivation walks the two values in step, and every answer it collects comes from an instance —
never from a definition invented for the occasion.

- **The constructors first.** Two values of different constructors are answered by `differed`,
  handed the **position** each constructor is declared at, counting from zero, and the walk stops
  there — there is nothing else the two values have in common to look at. Reordering the variants
  of a type therefore changes what a derived member computes, which is why the order is the
  declaration's rather than, say, alphabetical: the one a reader can see is the one that decides.
- **Then the arguments,** when the constructors agree: each pair in turn, left to right, through
  the instance belonging to *that argument's* type.
- **`combine` folds those answers** into one, in that order, ending at `matched`.

Nothing in that walk reaches inside an argument. Each pair is answered by the instance its own
type declares, whatever that instance computes — so a type whose equality is defined up to a
normal form keeps that meaning wherever it appears inside a derived one.

`Eq`'s three definitions above turn the walk into
[structural equality](evaluation-semantics.md#what-structural-equality-computes): `differed`
discards the positions to answer `False` outright, `and` carries a single unequal pair of
arguments out to the answer, and `matched` makes a constructor with no arguments equal to itself.

`Comparable`'s three turn the same walk into a lexicographic ordering:

```zel expect=unimplemented
module Example exposing (Comparable)

type Order
  = LT
  | EQ
  | GT

class Comparable a where
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

Two values of different constructors are ordered by the positions those constructors are declared
at, so `Red` is less than `Green` in the `Colour` type above; two of the same constructor by their
arguments, left to right, the first unequal pair deciding. Neither sentence is written anywhere in
the compiler. Both are what these three definitions say, applied to the one walk — the first
sentence is `differed` handing its two positions to `Comparable`'s own instance at `Int`, and the
second is a `combine` that stops at the first answer other than `EQ`.

### What a derived instance requires

The arguments a derivation compares are the ones the variants write down, and each of their
types needs an instance of the class being derived. Nothing else does. In particular a class owes
no `Int` instance for the positions `differed` receives: what a class makes of them is written in
its own `differed`, and a class whose answer does not depend on them — `Eq`'s, above — never
mentions `Int` at all.

Where an argument's type is a variable, that requirement cannot be checked at the derivation —
the variable is whatever a use of the type chooses — so it becomes a **constraint on the derived
instance**, inferred rather than written:

```zel expect=unimplemented
module Example exposing (Box)

type Box a
  = Box a

instance Eq (Box a) where
  derived
```

```zel expect=fragment
-- the instance that declaration yields
Eq a => Eq (Box a)
```

Two `Box`es are equal when their contents are, which is only a definition of equality once the
contents have one. A parameter no variant uses carries no constraint, because no value of the
type holds anything of that type to compare.

Where the argument's type is concrete, the requirement is checked on the spot, and an argument
whose type has no instance is an error at the `instance` declaration, naming the variant and
the type:

```zel expect=unimplemented
module Example exposing (Key, Entry)

type Key
  = Key

type Entry
  = Entry Key

instance Eq Entry where
  derived
```

Reporting that at the `instance` rather than at some later use is the point: the instance is the
claim that an `Entry` can be compared for equality, and the claim is false where it is written.
A variant holding a **function** is the case that no instance can rescue, since a function type
[has no useful equality at all](evaluation-semantics.md#functions-are-not-comparable).

A superclass obligation is unchanged too. A derived `Comparable Colour` is rejected unless an
`Eq Colour` instance exists — derived in its turn, or written out.

And the class has to be one that says how it is derived. `derived` under a class whose
declaration carries no derivation is an error naming the class — not because the compiler holds
a list of the classes that do, but because the declaration the instance names has nothing in it
to run.

## Constraining an annotation

A constraint is written in front of the type, separated from it by `=>`. It names a class and
the variable that class applies to.

```zel expect=unimplemented
module Example exposing (Order, min)

type Order
  = LT
  | EQ
  | GT

min : Comparable a => a -> a -> a
min x y =
  x
```

Read it as a precondition on the caller: *`min` works for any type `a`, provided `a` is
`Comparable`*. A caller supplying a type with no `Comparable` instance is an error at the call
site, pointing at the call — not at `min`, which is fine, and not at the instance, which does not
exist.

Several constraints are parenthesised and comma-separated:

```zel expect=unimplemented
module Example exposing (Bit, describe)

type Bit
  = Zero
  | One

describe : (Comparable k, Eq v) => k -> v -> Bit
describe a b =
  Zero
```

### A constraint belongs to a signature, not to a type

`=>` may appear once, at the very front of an annotation, and nowhere else. A constraint is a
statement about the declaration being annotated; it is not a piece of type syntax that can be
nested inside a larger type, and there is no such thing as a constrained argument type.

```zel expect=unimplemented
module Example exposing (Size, f)

type Size
  = Small

f : Size -> (Comparable a => a)
f x =
  Small
```

The left of `=>` must be constraints. A type there is not a constraint, and the compiler says so
rather than accepting it:

```zel expect=unimplemented
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

```zel expect=unimplemented
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

Two things follow, and they pull in opposite directions.

**An instance acquires an obligation.** `instance Comparable Colour` is rejected unless
`instance Eq Colour` also exists. A type cannot be ordered without being comparable for equality
first, and the declaration is where that is enforced.

**A signature loses one.** A function constrained by `Comparable a` may use `eq` as well as
`compare`, without naming `Eq`. The superclass is implied by the subclass, so
`Comparable a => …` is the whole precondition and `(Eq a, Comparable a) => …` says nothing more.

## Where an instance may be declared

An `instance C T` declaration is legal in **the module that declares `C`**, and in **the module
that declares `T`**, and nowhere else.

An instance in any third module is rejected:

```zel expect=unimplemented
module App exposing (Main)

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
wanted. That is the price of a call meaning one thing.

A **package** boundary adds nothing to this rule and does not need to. Every module belongs
to exactly one package ([Packages and source layout](packages.md)), so an instance can only be
written by whoever owns the class's module or the type's module — which is to say by whoever
ships one of those two packages. A third package that depends on both still cannot pair them.

## A class is always over a complete type

A type variable stands for a complete type and is never applied — that is the rule
[Types](types.md#type-variables) already states, and a class mechanism does not relax it. A class
variable is a type variable, so a class is always over a complete type.

So classes whose variable stands for a *type constructor* cannot be written. There is no
`Functor`, no `Monad`, no class over "a thing that takes one type argument".

```zel expect=unimplemented
module Example exposing (Box)

type Box a
  = Box a

class Functor f where
  map : (a -> b) -> f a -> f b
```

That block fails today because `class` does not parse, and it will still be rejected when it
does, because `f a` is not a type — it applies a variable. Allowing it would need variables
ranging over type constructors as well as types, which is what a kind system is for, and Zelkova
does not have one.

## Numeric literals

A numeric literal carries no constraint. Its type is decided by how it is spelled — `1` is an
`Int`, `1.5` is a `Float` — and [Expressions](expressions.md#a-literals-type-is-its-spelling)
is where that rule lives.

```zel expect=ok
module Example exposing (x)

x =
  1
```

It is worth saying here because of what it means for classes: **nothing in the language
defaults, and the compiler knows no class by name.** A constraint the solver cannot discharge
is an error rather than a guess, in every case and with no exception carved out for arithmetic;
there is no way to declare what a class falls back to, and no class the compiler treats
differently from one a program declares itself. Not even
[a derived instance](#a-class-says-how-it-is-derived) is an exception: what it computes is read
off the class's own declaration.

The price is paid inside a constrained function, where a literal is already concrete and so
cannot be used at the constrained type — `double x = mul x 2` under `Number a => a -> a`
forces `a` to be `Int`. A class that wants numeric constants declares them as members, the way
it declares everything else.

## A constrained function may not be a JavaScript facade

A `module javascript` facade declares signatures with no bodies, backed by a companion `.mjs`
file. None of those signatures may carry a constraint.

```zel expect=unimplemented
module javascript Js.Cmp exposing (compare)

compare : Comparable a => a -> a -> Int
```

The reason is a promise made in [JS interop](js-interop.md): a facade's companion export takes a
**plain parameter list**, and a hand-written JavaScript file is never asked to know how the
compiler represents anything.

A constrained function is **specialised** — the compiler generates one ordinary function per
type the constraint is discharged at — and a facade has no body to generate one from. Its `.mjs`
export is the whole implementation, so a constrained facade would have to serve every instance
from that one JavaScript function, which could only tell them apart by inspecting how its
arguments are represented at runtime. That is precisely the knowledge the promise keeps out of a
`.mjs` file, and it is the shape of the gap noted at the end of this section. A dictionary — an
extra, invisible argument carrying a table of the class's operations — is the other way to
implement a class, and it breaks the same promise more directly.

So the constraint moves up one level. The facade stays monomorphic and is called only at types
its JavaScript can actually handle; the class, its instances, and the constraint live in ordinary
Zelkova above it:

```zel expect=fragment
-- Js/Utils.zel — no constraint, and one signature per type it really supports
compareInt : Int -> Int -> Int
compareChar : Char -> Char -> Int

-- Basics.zel — the constraint lives here
instance Comparable Int where
  compare a b =
    orderOf (Js.Utils.compareInt a b)
```

**Not implemented:** specialisation is a rule about code generation, and code generation has not
started. When it exists, the generated JavaScript holds one ordinary function per instantiation
and no table of operations is built or passed at runtime. Two consequences: a program is compiled as a whole rather than a module at a time, and a constrained
function cannot call itself at a different type than it was called with. The second is already
impossible — a class variable stands for a complete type, so there is no different type for it to
recurse at.

**Known gap:** the comparison and append facades in `std/core` are declared over any type at all,
and the JavaScript behind them assumes its arguments are numbers, strings or tuples; handed a
value of a user union type it reads fields that are not there.
[`BUG-20`](../tickets/bug-20.md) tracks it, and it closes when those six signatures get a real
constraint.

## The words this reserves

`class` and `instance` are reserved words, usable nowhere but at the start of the declarations
they introduce. `where` is reserved in two narrower senses: it opens a class or instance body,
and it may not be a type variable. Everywhere a *value* is named — a declaration, a parameter,
an `exposing` entry — `where` stays an ordinary identifier. `class` and `instance` share the
start of a line with a function declaration, so nothing tells the two apart unless those words
are reserved; `where` needs only its one type-variable position excluded.

`=>` is a token of the language rather than a name, so it cannot be declared as an operator.

A class's own name reserves nothing either. `Comparable` is an ordinary uppercase name that a
module declares, no different from a type.

`derived` reserves nothing either. It is a [soft keyword](lexical-structure.md#reserved-words) —
a keyword in a class body and in an instance body, an ordinary identifier in every other
position, the name of a class's own member included. What tells the readings apart is the token
after it: `derived` alone is [the request](#an-instance-may-be-derived), `derived eq` opens
[a derivation](#a-class-says-how-it-is-derived) for the member `eq`, and `derived : …` or
`derived = …` declares a member called `derived`. One token of lookahead settles it, so
reserving a word a program has every right to want would buy nothing.

**Known gap:** none of those four reservations exists today, and each of these blocks goes red
when the ticket naming it lands. `class` and `instance` as value names ([`CLASS-2`](../tickets/class-2.md)):

```zel expect=ok
module Example exposing (class, instance)

type Size
  = Small

class : Size
class =
  Small

instance : Size
instance =
  Small
```

`where` as a type variable, the one `where` position the language excludes
([`CLASS-2`](../tickets/class-2.md)):

```zel expect=ok
module Example exposing (Box)

type Box where
  = Box where
```

And `=>` as a user-defined infix operator ([`CLASS-1`](../tickets/class-1.md)):

```zel expect=ok
module Example exposing (both)

type Size
  = Small

infix left 5 (=>) = both

both : Size -> Size -> Size
both a b =
  a
```

## What the standard library declares

Four classes, and they are ordinary declarations in ordinary modules. A program may declare its
own alongside, and nothing about what a member means, or about which types have instances, is
built into the compiler.

No name is an exception. `Number` is as ordinary as the other three: the compiler does not know
it by name, knows no instance of it, and would behave identically if `std/core` declared it
under another name or not at all (*[Numeric literals](#numeric-literals)*, above).

| Class | Members, roughly | What it constrains a variable to |
|---|---|---|
| `Eq` | `eq`, `neq` | types that can be compared for equality |
| `Comparable` (superclass `Eq`) | `compare` | types that are ordered |
| `Number` | `add`, `sub`, `mul`, and the rest of the arithmetic | the numeric types |
| `Appendable` | `append` | types `++` joins |

`Comparable` has the one member, and the four ordering operators are ordinary functions
constrained by it — `lt : Comparable a => a -> a -> Bool`, and so on — rather than members of
it. That is not tidiness. A class is derivable only when *every* member is, and `lt` is not: a
lexicographic `lt` cannot be folded out of the `lt` of each pair of arguments, because it has to
know whether the pair before it was *equal*. `compare` can, so `Comparable` keeps `compare` and
builds the rest on top.

Two of the four carry [derivations](#a-class-says-how-it-is-derived): `Eq` and `Comparable`, in
the shape this chapter has already shown them. `Number` and `Appendable` carry none and could
not — `add` and `append` return the class variable, which a walk over two values has no way to
produce — and that is a fact about their signatures, not about their names. A program's own
class is derivable on exactly the same terms as either.

`Appendable` ranges over strings and lists. The compiler implements neither type — see the note
on brackets and quotes in [Lexical structure](lexical-structure.md#punctuation).

**Not implemented:** [`CLASS-6`](../tickets/class-6.md) is the pass that declares them. A
constrained function cannot be a single-line re-export of a JavaScript facade, which is what most
of these are in `std/core` — its body has to choose an instance.
