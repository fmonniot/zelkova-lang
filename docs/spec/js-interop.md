# JS interoperability

A user marks a Zelkova module as a JavaScript interface with the `javascript`
modifier after the `module` keyword. When declaring a JS module, only signatures are
accepted — no bodies, no infix declarations, no type declarations. A signature may be
a function's or, as [below](#facade-constants), a constant's. Only
[a subset of the Zelkova types](#which-types-may-cross-the-boundary) may appear as a
parameter or return type in one of those signatures.

This is the **only** way into JavaScript. There is no privileged escape hatch the standard
library may use and a user's package may not: a standard-library module that needs a JavaScript
primitive declares a facade in the same syntax any user package would.

The idea is close to TypeScript's type definitions, but Zelkova is stricter about which
types the functions can use: only things verifiable by the runtime are let through.

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

Two signatures rather than one, because a facade names the types its JavaScript really handles
([which types those may be](#which-types-may-cross-the-boundary) is the next section): the `.mjs`
behind `fdiv` divides, the one behind `idiv` truncates.

## Which types may cross the boundary

A type may be named in a facade signature when the compiler can emit a **predicate**
for it: a piece of JavaScript that decides, from a value alone, whether that value belongs to
that type. Every value a companion `.mjs` hands back is run through the predicate of the type
its signature declares. A value that fails one becomes [`Err (Malformed
..)`](evaluation-semantics.md#an-effect-that-can-fail) out of an [effectful
facade](#an-effectful-facade), and [aborts the
program](evaluation-semantics.md#when-a-program-aborts) out of a pure one, which has no result
type to carry it.

A facade has no body the compiler can read, so its annotation is checked by the running program
rather than while compiling. Two of the
[six forms a type expression can take](types.md#the-forms-of-a-type-expression) have no predicate
at all.

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

A signature may name any of those, in any position an argument or a result may take.

```zel expect=ok
module javascript Js.Colour exposing
  ( rgb
  , luminance
  )

rgb : Int -> (Int, Int, Int)
luminance : (Int, Int, Int) -> Float
```

JavaScript has one number type where Zelkova has two, and the `Int` predicate is what separates
them.

A predicate walks the whole value, so a facade taking a list of a thousand tuples checks a
thousand tuples on the way in, once per crossing.

**Not implemented:** `String` has no [literal syntax](lexical-structure.md#strings) yet,
[`()` is not recognised](types.md#the-unit-type) in either position, records have no brace token
([`LANG-47`](../tickets/lang-47.md)) and lists no literal production
([`LANG-44`](../tickets/lang-44.md)) — so four of those rows are about types a program cannot
write today. Both constructs are specified, in [Records](records.md) and [Lists](lists.md), and
neither chapter publishes a JavaScript encoding: what a record and a list look like across the
boundary belongs to code generation. Nothing runs a predicate either, because code generation
has not started — [`GEN-2`](../tickets/gen-2.md) is the ticket that emits them.

### A union crosses as a tagged object

A `.mjs` may receive a union value and construct one, so the two sides have to agree on its
shape. A union value is an **object carrying the name of its constructor** in a `$` field, with
that constructor's arguments in declaration order in fields named `a`, `b`, `c` and so on. `Red`
is `{$: "Red"}`; `Rgb 255 0 0` is `{$: "Rgb", a: 255, b: 0, c: 0}`. The predicate reads `$`,
checks it against the declaration's constructors, and checks each argument against the predicate
of the type that constructor declares for it.

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

A constructor's **name** is part of what a `.mjs` depends on, so renaming one breaks every
companion that mentions it.

A union may be recursive, and the predicate follows it: the walk descends into each argument and
terminates because a Zelkova value is immutable and can hold no cycle.

**Not implemented:** no value is encoded and no predicate is run, because code generation has not
started; the encoding above is what [`GEN-2`](../tickets/gen-2.md) emits.

### What a facade signature may not name

Two of the six forms have no predicate, and a facade signature may name neither.

A **type variable** excludes no value, so there is nothing for a check to decide. A facade is
therefore monomorphic; polymorphism lives in ordinary Zelkova above it.

A **function type** is rejected wherever it appears. `typeof x === 'function'` decides that a
value is *some* function, not that it is the one declared.

```zel expect=ok
module javascript Js.Utils exposing
  ( equal
  )

equal : a -> a -> Bool
```

```zel expect=ok
module javascript Js.List exposing
  ( count
  )

count : (Int -> Bool) -> Int -> Int
```

**Known gap:** neither signature is admitted — `equal` names a type variable, `count` takes a
function — and both are accepted. Nothing checks a facade's types at all: a facade annotation is
resolved exactly as any other annotation is, so every type a normal module may write, a facade
may write. [`LANG-43`](../tickets/lang-43.md) is the check, and both blocks go red when it lands.

**Not implemented:** a class constraint is rejected on the same grounds, `Comparable a => a`
being a signature over `a`. A constrained function is specialised, and a facade has no body to
specialise; the constraint lives in ordinary Zelkova above the monomorphic facade.
[Type classes](type-classes.md#a-constrained-function-may-not-be-a-javascript-facade) is the
chapter.

## An effectful facade

Every signature above declares a function: the same arguments give the same result, and calling
it has no other consequence
([purity](evaluation-semantics.md#purity-and-the-javascript-boundary)). A clock, a file and a
socket are none of those, and the language reaches them through one variation on the same
declaration.

**A facade signature whose result type is a `Task` declares an effect**, and its companion is
released from the rule above: it may read, write, wait and fail.

**That result type must be `Task (Result Failure a)`.** A signature naming any other `Task` is
an error, so an effectful facade cannot declare its JavaScript to be infallible.
[`Failure`](evaluation-semantics.md#an-effect-that-can-fail) is the error type every one of them
names, `a` being the only part its author chooses. It is not one of
[the default imports](modules.md#the-default-imports), so a facade naming it imports it.

```zel expect=unimplemented
module javascript Js.File exposing (read)

import Task exposing (Failure)

read : String -> Task (Result Failure String)
```

A `Task` never crosses, and neither does the `Result`. The companion takes the arguments the
signature names and returns the payload — a string, for `read`:

```js
export async function read(path) {
  return await fs.promises.readFile(path, "utf8");
}
```

That companion throws when the file is missing, and it is a correct companion. The `Result` is
built on the Zelkova side, by the wrapper the compiler puts around the call: it catches what the
companion throws, runs `a`'s predicate over what the companion returns, and yields `Ok` for a
value that passes, `Err (Threw ..)` for a throw or a rejected promise, and `Err (Malformed ..)`
for a value that fails. A signature therefore declares a `Result` that no companion ever
produces.

The rules above are untouched. `a` is the type the companion really hands back, it is checked
by `a`'s predicate at the boundary like any other returned value, and it may be neither a type
variable nor a function type. An effectful facade is as monomorphic as a pure one.

A companion whose result is not ready at once returns a promise for it, and the predicate runs
on the value that promise resolves to. A promise that never settles is a `Task` that never
produces a value, which is the second of the [two
outcomes](evaluation-semantics.md#two-outcomes) rather than a failure.

A [facade constant](#facade-constants) may name a `Task` too, and it is the one constant whose
companion is a function:

```zel expect=unimplemented
module javascript Js.Time exposing (now)

import Task exposing (Failure)

now : Task (Result Failure Int)
```

`export const now = Date.now()` would read the clock once, when the host loaded the module. The
wrapper calls the export each time the `Task` is run, so `now` names
`export function now() { return Date.now(); }` — a function of no arguments, which is what a
plain parameter list comes to when there are none.

An effectful facade is a facade, so it is [not importable outside the package that declares
it](packages.md#what-a-package-exposes). A package offers an effect to its dependents the way it
offers any other JavaScript-backed value: an ordinary module imports the facade and re-declares
what it offers.

**`Task` may appear only as the whole of a result type.** Not as an argument, which would hand
JavaScript a value it has no predicate for; and not nested inside another type, since `(Task
Int, Task Int)` describes no single piece of work for the wrapper to build.

**A failure the caller is meant to tell apart from a broken companion goes in the payload.**
`Failure` reports that the JavaScript broke and says nothing about what the effect was for. A
`read` separating a missing file from a broken companion declares `String -> Task (Result Failure
(Result IoError String))` — two `Result`s, collapsed into one by the module that
[publishes `read`](packages.md#what-a-package-exposes) to other packages.

**Not implemented:** neither block above parses. A type argument must be a bare name today, so
the parentheses in `Task (Result Failure String)` are a syntax error
([`LANG-9`](../tickets/lang-9.md)) — the same gap that rejects `Maybe (Maybe Int)` and every
other nested type. Nothing declares `Task` or `Failure` either, no wrapper is generated, and no
predicate is run ([`GEN-1`](../tickets/gen-1.md), [`GEN-2`](../tickets/gen-2.md)); no check
holds a facade to the result type above, which is [`LANG-43`](../tickets/lang-43.md)'s.

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

The plain-parameter-list rule above has a zero-argument case: the `.mjs` export a constant names
is the value itself, not a function that produces it.

```js
export const pi = Math.PI;
export const e = Math.E;
```

is the whole of what `pi` and `e` require on the JavaScript side.

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
