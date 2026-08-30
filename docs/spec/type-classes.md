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

An integer literal is usable where an `Int` or a `Float` is wanted, and a constraint is how the
language says so: a literal carries a `Number` constraint rather than a type, and whatever
determines it — an annotation, an argument position, an operator — discharges that constraint.

When nothing determines it, the literal is an `Int`.

```zel expect=ok
module Example exposing (x)

x =
  1
```

That default is the only one in the language. Every other constraint the compiler cannot
discharge is an error rather than a guess: there is no way to declare what a class falls back to.
The narrowness is the point — defaulting makes a program's meaning depend on a rule the reader
has to remember, and one such rule for the one case that comes up constantly is a different
proposition from a general facility.

How a literal is typed at all, and which types it may take, belong with the rest of literal
typing in the planned *Expressions* chapter (see [the chapter list](README.md#chapters)); that
chapter takes those paragraphs out of here when it is written. What stays is the pair above that
is a class rule rather than a literal one — the constraint a literal carries, and the one default
that discharges it.

**Known gap:** the type checker has a hard-coded ancestor of `Number` today — an internal type
given to every integer literal, which unifies with `Int` and `Float` and nothing else. It has no
source syntax, no instances behind it, and no way to fail; a literal that nothing determines
simply stays that type forever rather than defaulting. It is also rendered `number` in a
diagnostic, which [Types](types.md#type-variables) reads as an ordinary type variable — so `x : Char` with
a body of `1` reports *cannot match `Char` with `number`*, naming something the reader's source
does not contain. [`ERR-13`](../tickets/err-13.md) is the spelling;
[`CLASS-5`](../tickets/class-5.md) is what retires the type itself. No block here holds either to
account: the spec harness stops at canonicalization and never runs the type checker
([`TEST-2`](../tickets/test-2.md)).

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

**Known gap:** all four are ordinary today, and each of these blocks goes red when the ticket
naming it lands. `class` and `instance` as value names ([`CLASS-2`](../tickets/class-2.md)):

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

One name is the exception, and it is as narrow as it reads: an integer literal has to carry
*some* constraint, so the compiler knows `Number` by name and knows that an undetermined one
resolves to `Int` (*[Numeric literals](#numeric-literals)*, above). It knows no instance of
`Number`, and nothing whatever about the other three.

| Class | Members, roughly | What it constrains a variable to |
|---|---|---|
| `Eq` | `eq`, `neq` | types that can be compared for equality |
| `Comparable` (superclass `Eq`) | `compare`, and the four ordering operators | types that are ordered |
| `Number` | `add`, `sub`, `mul`, and the rest of the arithmetic | the numeric types |
| `Appendable` | `append` | types `++` joins |

`Appendable` ranges over strings and lists. The compiler implements neither type — see the note
on brackets and quotes in [Lexical structure](lexical-structure.md#punctuation).

**Not implemented:** [`CLASS-6`](../tickets/class-6.md) is the pass that declares them. A
constrained function cannot be a single-line re-export of a JavaScript facade, which is what most
of these are in `std/core` — its body has to choose an instance.
