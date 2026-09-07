# JS interoperability

A user can mark a Zelkova module as being a JavaScript interface. This is done by
using the `javascript` modifier after the `module` keyword. When declaring a JS
module, only signatures are accepted — no bodies, no infix declarations, no type
declarations. A signature may be a function's or, as [below](#facade-constants), a
constant's. Only [a subset of the Zelkova types](#which-types-may-cross-the-boundary)
may appear as a parameter or return type in one of those signatures.

This is the **only** way into JavaScript. Zelkova has no privileged internal escape
hatch — no module the standard library may use and a user's package may not — so a
standard-library module that needs a JavaScript primitive declares a facade in exactly
the syntax written here. Ordinary code and `std/core` reach the runtime the same way,
which is the property this design exists to preserve.

The idea is close to TypeScript's type definitions, with the difference that Zelkova
is less permissive in what types the functions can use: only things verifiable by the
runtime are let through.

A `javascript module` is only usable from inside the package that declares it. The
module name is not exposed to any other package.

Each facade module is paired with a companion `.mjs` file of the same base name
(`Js/Basics.zel` next to `Js/Basics.mjs`) holding the actual implementation. That file
is an ES module, and its exports take a **plain parameter list**: a Zelkova function
of two arguments is backed by a JavaScript function of two arguments, called directly.
There is no curried-wrapper convention to observe on the JavaScript side — currying is
the compiler's business.

Example — a reduced version of `std/core/src/Js/Basics.zel`, keeping only the
declarations its `exposing` list names so it stands alone as a compiling module (the
real file exposes many more):

```zel expect=ok
module javascript Js.Basics exposing
  ( fdiv
  , idiv
  )

fdiv : Float -> Float -> Float
idiv : Int -> Int -> Int
```

Two signatures over two types rather than one, because a facade names the
types its JavaScript really handles 
([which types those may be](#which-types-may-cross-the-boundary) is the next section) and it is
what makes the `.mjs` behind `fdiv` free to divide and the one behind `idiv` free to truncate.

## Which types may cross the boundary

A type may be named in a facade signature when the compiler can emit a **predicate**
for it: a piece of JavaScript that decides, from a value alone, whether that value belongs to
that type. Every value a companion `.mjs` hands back is run through the predicate of the type
its signature declares, and a value that fails one is an error at the boundary rather than a
wrong answer somewhere further on.

The predicate is what makes a facade signature mean anything. Everywhere else a declaration's
body is checked against its annotation; a facade has no body the compiler can read, so its
annotation is a claim about JavaScript that nothing verifies while compiling. Requiring a
predicate is what turns that claim into one the running program keeps, and it is a real
restriction, because two of the [six forms a type expression has](types.md#the-forms-of-a-type-expression)
have no predicate at all.

The forms that do:

| Type | What its predicate decides |
|---|---|
| `Int` | the value is a number, and a whole one the [32-bit range](evaluation-semantics.md#numbers) holds |
| `Float` | the value is a number |
| `Bool` | the value is a boolean |
| `Char` | the value is a string of one character |
| `String` | the value is a string |
| `()` | the value is the one value that type has |
| A tuple | the value is an array of the tuple's length, each element satisfying its component's predicate |
| A record | the value is an object with exactly the record's fields, each field satisfying its own predicate |
| A list | the value is an array, every element of which satisfies the element type's predicate |
| A union type, applied to admitted types | the value carries one of that type's constructor names, and arguments satisfying that constructor's predicates |

A signature may name any of those, in any position an argument or a result may take, and
nothing else.

```zel expect=ok
module javascript Js.Colour exposing
  ( rgb
  , luminance
  )

rgb : Int -> (Int, Int, Int)
luminance : (Int, Int, Int) -> Float
```

`Int` and `Float` are one type in JavaScript and two here, and that costs the rule nothing.
Whether a number is a whole one is a question about the value, which is all a predicate is ever
allowed to ask, so `Int` is admitted on exactly the terms `Bool` is and not as a tolerated
exception to them.

A predicate is a walk over the value, so what it costs is the size of the value: a facade taking
a list of a thousand tuples checks a thousand tuples on the way in. That cost is the price of
the boundary being checked at all, and it is paid once per crossing rather than once per use.

**Not implemented:** `String` has no [literal syntax](lexical-structure.md#strings) yet,
[`()` is not recognised](types.md#the-unit-type) in either position, and records and lists have
neither syntax nor a chapter, so four of those rows are about types a program cannot write
today. [`SPEC-21`](../tickets/spec-21.md) and
[`SPEC-22`](../tickets/spec-22.md) are the chapters that say what a record and a list are; what
either looks like to JavaScript belongs to them and to code generation, and this section commits
only to both crossing. Nothing runs a predicate either, because code generation has not started
— [`GEN-2`](../tickets/gen-2.md) is the ticket that emits them.

### A facade is monomorphic

A type variable has no predicate. `a` stands for a type the caller chooses, so there is no value
it excludes and nothing for a check to decide; a facade signature may not name one, and a facade
is therefore monomorphic.

That is the same rule as the one forbidding a
[class constraint](#a-facade-signature-may-not-carry-a-constraint) on a facade, reached from the
other side: a facade's signature has to name the types its JavaScript really handles, whether the
wider type it is tempting to write is `a` or `Comparable a => a`. Polymorphism lives in ordinary
Zelkova, which calls a monomorphic facade underneath it.

```zel expect=ok
module javascript Js.Utils exposing
  ( equal
  )

equal : a -> a -> Bool
```

**Known gap:** that signature names a type variable, which the language does not admit here, and
it is accepted. Nothing checks a facade's types at all — a facade annotation is resolved exactly
as any other annotation is, so every type a normal module may write, a facade may write.
[`LANG-43`](../tickets/lang-43.md) is the check, and this block goes red when it lands.

### A function does not cross

A function type has no predicate either, and it is rejected wherever it appears in a signature —
as an argument, as a result, or nested inside a tuple, list, record or union that is otherwise
admitted. `typeof x === 'function'` decides that a value is *some* function, which is not the
same as deciding it is a function of the type declared: nothing in a JavaScript value says how
many arguments it takes or what it does with them.

The plain-parameter-list rule rejects it a second time and independently. A companion's export
takes its arguments plainly because currying is the compiler's business; a Zelkova function
handed *into* JavaScript would have to be called from a `.mjs`, and calling it means knowing how
a Zelkova function of several arguments is applied — the one convention that promise keeps out
of a companion.

```zel expect=ok
module javascript Js.List exposing
  ( count
  )

count : (Int -> Bool) -> Int -> Int
```

**Known gap:** that signature takes a function as an argument, which the language does not admit,
and it is accepted — the same missing check as above, and the same ticket,
[`LANG-43`](../tickets/lang-43.md). This block goes red when it lands.

### A union crosses as a tagged object

A union type is admitted, which means a `.mjs` may receive one and construct one, which means the
representation of a union value is part of what a facade's two sides agree on. A union value is
an **object carrying the name of its constructor** in a `$` field and that constructor's
arguments, in declaration order, in fields named `a`, `b`, `c` and so on. `Red` is `{$: "Red"}`;
`Rgb 255 0 0` is `{$: "Rgb", a: 255, b: 0, c: 0}`. The predicate reads `$`, checks it against the
declaration's constructors, and checks each argument against the predicate of the type that
constructor declares for it.

```zel expect=ok package=union
module Palette exposing (Swatch(..))

type Swatch
  = Named Int
  | Rgb Int Int Int
```

```zel expect=ok package=union
module javascript Js.Palette exposing
  ( toHex
  )

import Palette exposing (Swatch(..))

toHex : Swatch -> Int
```

This is the one place a Zelkova value's runtime shape is a published interface, and it has a
price worth naming: a constructor's **name** is part of what a `.mjs` depends on, so renaming one
breaks every companion that mentions it. That price is smaller than it looks, because a facade
may only name a type its module can see, which is a type some module exposes — the constructor
names of an exposed union are already public API, and this makes them public to one more reader.

A union may be recursive, and the predicate follows it: the walk descends into each argument and
terminates because a Zelkova value is immutable and can hold no cycle. What it costs is the size
of the value, as above — a crossing checks the whole structure, not its outermost constructor.

**Not implemented:** no value is encoded and no predicate is run, because code generation has not
started; the encoding above is what [`GEN-2`](../tickets/gen-2.md) emits.

### A facade signature may not carry a constraint

**Not implemented:** a facade signature may never carry a class constraint. `Comparable a => a`
is still a signature over `a`, and the rule [above](#a-facade-is-monomorphic) is what rejects it:
a constrained function is specialised — one generated function per type it is used at — and a
facade has no body to generate one from, so a constrained facade would have to serve every
instance from a single hand-written export, telling the instances apart by inspecting arguments
whose type the signature never named. That is dispatch on a type variable, which is what a
predicate cannot do; a constrained function therefore lives in ordinary Zelkova and calls a
monomorphic facade underneath it. No table of the class's operations exists at runtime either:
specialisation is what discharges a constraint, before code is generated.
[Type classes](type-classes.md#a-constrained-function-may-not-be-a-javascript-facade) is the
chapter.

## Facade constants

A facade signature may also declare a constant — a type with no arrow, taking no arguments.
`std/core/src/Js/Basics` exposes two:

```zel expect=ok
module javascript Js.Basics exposing
  ( pi
  , e
  )

pi : Float
e : Float
```

The plain-parameter-list rule above has a zero-argument case, and this is it: the `.mjs`
export a constant names is the value itself, not a function that produces it. There is no
thunk to call and no parameter list to be plain about.

```js
export const pi = Math.PI;
export const e = Math.E;
```

is the whole of what `pi` and `e` require on the JavaScript side — an ordinary binding, read
directly rather than invoked.

Evaluation is [strict](evaluation-semantics.md#evaluation-is-strict), and an ordinary
parameterless binding is placed in an evaluation order that reads off which bindings it
mentions — see
[A binding with no parameters is evaluated once](evaluation-semantics.md#a-binding-with-no-parameters-is-evaluated-once).
A facade constant has no Zelkova body to place in that order: it names a JavaScript binding
directly, so it is evaluated whenever the `.mjs` module that exports it is, on whatever
schedule the host's module loading gives that — nothing here promises it happens once, lazily,
or at any particular point relative to the rest of the program.

## Open questions

- **WebAssembly modules.** `javascript` is the only interop modifier the language
  defines. What the equivalent declaration for a WebAssembly-backed module would look
  like — a second modifier, a different mechanism entirely — is undesigned
  ([`SPEC-19`](../tickets/spec-19.md)).
