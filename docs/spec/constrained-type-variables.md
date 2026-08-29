# Constrained type variables

Zelkova does not have them.

`number`, `comparable` and `appendable` are ordinary type variables. They are spelled the way
they are because the standard library was drafted against a language where those three
spellings are known to the compiler and restrict what a variable may stand for — and Zelkova
inherited the drafts along with the surface syntax. It did not inherit the mechanism, and it
has never had one. A lowercase name in a type position is a type variable, `number` exactly as
much as `a`, and no spelling of one carries a restriction.

This chapter exists because that is not what a reader assumes. The three names look like
vocabulary; every one of them is a variable, and reading one as a constraint is the specific
error the chapter is here to prevent. It then records what the language means to do instead —
the direction is settled, the mechanism is not — and what a program should do until that lands.

## The rule

There is only one, and [Types and type annotations](types.md#type-variables) already states it:
a lowercase-initial identifier in a type is a type variable, standing for a complete type
chosen by the caller. Nothing distinguishes one lowercase spelling from another.

`comparable` is therefore `a` under a longer name, and a function annotated with it accepts a
value of any type at all — including one that could not possibly be compared:

```zel expect=ok
module Example exposing (Colour, smaller)

type Colour
  = Red
  | Blue

min : comparable -> comparable -> comparable
min x y =
  x

smaller : Colour
smaller =
  min Red Blue
```

That block is not a known gap and its `expect=ok` tag is not a placeholder. `min`'s declared
type is `a -> a -> a`; `Colour` is a type; the application is well typed, and the language as
specified has no rule it breaks. The problem is one level up, in what the annotation led its
reader to expect — see [What the spellings were standing in for](#what-the-spellings-were-standing-in-for).

The same holds for the other two. Neither `number` nor `appendable` restricts anything, and
all three may appear in one signature, where they are simply three different variables:

```zel expect=ok
module Example exposing (Wrapper, keep)

type Wrapper
  = Wrap

keep : number -> comparable -> appendable -> Wrapper
keep a b c =
  Wrap
```

A digit suffix is not a mechanism either. `comparable2` is an identifier, so it is a variable,
and it is a *different* variable from `comparable` for the ordinary reason that the two names
are not the same — not because anything numbers them:

```zel expect=ok
module Example exposing (Colour, first)

type Colour
  = Red

first : comparable -> comparable2 -> comparable
first a b =
  a
```

## The spellings are not reserved

They are ordinary identifiers, and the language treats them as such everywhere. This is worth
stating explicitly rather than leaving to inference, because "reserved but inert" would have
been a defensible design and is not the one Zelkova has: a program may use all three names for
whatever it likes, and doing so is not borrowing against a future change.

A value may be named `number`. A declaration may use `number` as a type variable in its
annotation and as a parameter name in its body at the same time — the two live in different
namespaces and never meet, so the annotation's `number` and the body's `number` are unrelated
even on one line of source:

```zel expect=ok
module Example exposing (pick)

pick : number -> number -> number
pick number other =
  number
```

`std/core/src/Basics.zel` still names a parameter this way, in `clamp`, and nothing about that
is a problem: the annotation above it no longer spells any of the three, and even when it did
the two `number`s were never the same thing.

A `type` declaration's parameters are type variables like any other, so they may be spelled
this way too:

```zel expect=ok
module Example exposing (Box, wrap)

type Box comparable
  = Box comparable

wrap : comparable -> Box comparable
wrap x =
  Box x
```

The one thing none of the three may do is name a *type*, and that is not about these spellings
at all — no lowercase name may name a type, because case is what separates the two. See
[Lexical structure](lexical-structure.md#identifiers) for the rule and
[Types](types.md#type-names) for what it means in a type position. The grammar reads `type` and
then wants an uppercase identifier, so it rejects the declaration at the name:

```zel expect=parse-error:UnexpectedToken
module Example exposing (number)

type number
  = Red
```

## What the spellings were standing in for

Three distinct problems, none of which the language currently answers.

**Arithmetic over more than one numeric type.** `add` should accept two `Int`s or two
`Float`s, and reject a `Char`. Written `number -> number -> number` it accepts all three, which
is how `add 'x' 'y'` gets through.

**Ordering.** `min`, `max`, `compare` and the four comparison operators need a type that can be
ordered, and there is no way to say so. The example at the top of this chapter is the
consequence.

**Concatenation.** `append` needs a type that can be concatenated. Its intended domain is
strings and lists, neither of which the compiler implements yet — see the *Not implemented*
note on brackets and quotes in [Lexical structure](lexical-structure.md#punctuation) — so
`appendable` today ranges over everything while nothing it was meant to range over exists.

The second and third are not merely cosmetic, because the functions behind them are
[JavaScript facades](js-interop.md) and the JavaScript cannot honour the type it is declared
with. `Js.Utils.compare` compares non-objects with `<` and otherwise assumes its arguments are
tuples; handed a value of a user union type it reads three fields that are not there. So `min
Red Blue` above is not just permissive typing — it is a program the compiler accepts and the
runtime has no defined answer for. [`bug-20`](../tickets/bug-20.md) tracks that half.

What the standard library does about it in the meantime is nothing clever: since these are
ordinary variables, it now spells them `a`. `SPEC-11` rewrote `Basics`, `Js.Basics` and
`Js.Utils` off the three names, on the grounds that a signature should not describe a
restriction the language cannot express. The over-promise is unchanged — `min : a -> a -> a` is
the same type `min : comparable -> comparable -> comparable` always was — but it is now visible
in the signature instead of hidden behind a word that looked like it meant something.

**Known gap:** the compiler still uses one of the three spellings itself. An integer literal is
given an internal type that unifies with `Int` and `Float` and nothing else, and type errors
render that type as `number` — so `x : Char` with a body of `1` reports *cannot match `Char`
with `number`*, naming a type this chapter's rule says is an ordinary type variable and the
reader's source does not contain. [`err-13`](../tickets/err-13.md) is the ticket.
No block here can hold that to account: the spec harness stops at canonicalization and never
runs the type checker ([`test-2`](../tickets/test-2.md)), so this paragraph has to be deleted
by hand when `err-13` lands.

What that literal type *means* — whether an integer literal being usable as an `Int` or a
`Float` is a rule of the language or an implementation detail — is not settled here. It belongs
with the rest of literal typing, in the planned *Expressions* chapter (see
[the chapter list](README.md#chapters)), and this chapter deliberately hands it over rather
than answering half of it in passing.

## The intended answer: type classes

**Not implemented:** none of this parses. The direction below is the language owner's decision
and is recorded as settled; the syntax is not, and the sketches in this section are tagged
`expect=fragment` for that reason. A fragment is the one block the harness does not execute, so
nothing here is a claim about what the compiler accepts.

Zelkova will grow **type classes**, and they will be what these three names become. Not a fixed
set of compiler-known spellings — a class is declared, in the source, by whoever needs one, and
`Comparable` will be an ordinary class that happens to ship in `std/core` rather than a word
the compiler knows. The three inherited spellings do not survive as spellings; they survive as
three of the classes the standard library declares.

One restriction is settled with the direction: **no higher-kinded variables.** A type variable
stands for a complete type and is never applied — [Types](types.md#type-variables) already says
so, and a class mechanism does not relax it. A class is therefore always over a complete type,
which puts `Functor`- and `Monad`-shaped abstractions out of reach and is the price of not
needing a kind system.

Roughly, and with no commitment to any of the syntax:

```zel expect=fragment
class Comparable a where
  compare : a -> a -> Order

min : Comparable a => a -> a -> a
min x y =
  if lt x y then x else y
```

```zel expect=fragment
instance Comparable Colour where
  compare a b =
    Js.Utils.compare a b
```

### What is still open

The direction does not answer any of these, and each has to be settled before the mechanism can
be written down:

- **How a constraint is spelled in an annotation.** The sketch borrows `=>`, which today is not
  a token the type grammar has any use for. Whether a constrained annotation is one production
  or two, and how a constraint interacts with the two-space continuation rule that
  [Types](types.md#the-function-arrow) describes for a long signature, is undesigned.
- **Where an instance may be declared.** If any module may declare an instance for any class
  and any type, two packages can define conflicting instances for the same pair and the meaning
  of a call depends on what else was linked. The usual answer is an orphan rule tying an
  instance to the module declaring either the class or the type. That is a rule about package
  boundaries as much as about types, so it wants settling alongside the planned *Packages and
  source layout* chapter (see [the chapter list](README.md#chapters)).
- **Whether a class may require another.** `Ord` needing `Eq` is the standard case, and
  superclasses are a real increment in complexity for the checker.
- **How a class reaches JavaScript.** The obvious implementation passes a dictionary of the
  class's methods as a hidden argument. That collides with what
  [JS interop](js-interop.md) guarantees: a `module javascript` facade's companion `.mjs`
  export takes a plain parameter list, and a hidden dictionary argument is exactly the kind of
  calling convention that file is promised it will never have to know about. Either
  constrained functions may not be facades, or the dictionary is erased before codegen — a
  choice with consequences for both chapters.
- **Defaulting.** `1` on its own, with nothing else to constrain it, has to resolve to
  something. So does a call whose constraint the checker cannot discharge from context.
- **What happens to the standard library.** `Basics` currently declares `eq : a -> a -> Bool`,
  unconstrained, and the runtime behind it crashes on a function. Once classes exist, deciding
  which of `std/core`'s signatures gain a constraint is a pass over the whole package rather
  than a mechanical rename.

## Until then

A program that needs one of these restrictions has two options, both available today.

**Pass the operation.** Take the comparison, or the addition, as an argument. It is more
verbose at every call site and it is honest: the type says what the function needs.

```zel expect=ok
module Example exposing (Colour, Order, smaller)

type Colour
  = Red
  | Blue

type Order
  = LT
  | EQ
  | GT

pickSmaller : (a -> a -> Order) -> a -> a -> a
pickSmaller cmp x y =
  x

colourOrder : Colour -> Colour -> Order
colourOrder a b =
  EQ

smaller : Colour
smaller =
  pickSmaller colourOrder Red Blue
```

**Write one function per type.** A monomorphic `addInt` cannot be applied to the wrong thing,
because its type names the right one. This is what the standard library will have to fall back
on for anything that cannot wait.

Neither is a workaround for a missing feature so much as the state of the language: without a
constraint mechanism, a type that accepts everything and a type that names one thing are the
only two a signature can express, and the second is the one that tells the truth.
