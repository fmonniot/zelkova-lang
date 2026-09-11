# Foreign interoperability

A package reaches code the compiler did not compile through a **facade**: a module marked
`foreign` after the `module` keyword. In a facade only signatures are accepted — no bodies, no
infix declarations, no type declarations. A signature may be a function's or, as
[below](#facade-constants), a constant's. Only
[a subset of the Zelkova types](#which-types-may-cross-the-boundary) may appear as a parameter
or return type in one of those signatures.

This is the **only** way out of Zelkova. There is no privileged escape hatch the standard
library may use and a user's package may not: a standard-library module that needs a foreign
primitive declares a facade in the same syntax any user package would.

The idea is close to TypeScript's type definitions, with a stricter rule about which types a
signature may name.

A facade is only usable from inside the package that declares it. The module name is not
exposed to any other package.

A facade declares an **effect** unless it says otherwise. Its result type is
`Task (Result Failure a)`, its companion may do anything, and the compiler wraps the call
([An effectful facade](#an-effectful-facade)). Writing `unsafe` before a signature removes the
`Task` and leaves a plain function type, which its author promises is pure and total
([An `unsafe` facade](#an-unsafe-facade)).

```zel expect=unimplemented
module foreign Core.Prim exposing
  ( fdiv
  , idiv
  )

unsafe fdiv : Float -> Float -> Float
unsafe idiv : Int -> Int -> Int
```

Two signatures rather than one, because a facade names the types the code behind it really
handles: the division behind `fdiv` divides, the one behind `idiv` truncates.

**Not implemented:** `foreign` is not a word the grammar knows. The modifier is spelled
`javascript` today ([`LANG-54`](../tickets/lang-54.md)), so every block below that declares a
facade fails to parse on its header.

## A facade names a boundary, not a backend

Zelkova compiles to more than one target, and a facade is written once for all of them. What
varies is the **companion**: a file of the same base name, sitting beside the facade in the same
directory, holding the code the signatures describe. There is one companion per target.

```text
src/
  Core/
    Prim.zel     module foreign Core.Prim
    Prim.mjs     the JavaScript behind it
    Prim.wasm    the WebAssembly component behind it
```

A build reads the companion for the target it is building and ignores the others. Neither the
facade nor anything importing it names a target, so a module written above a facade — `Basics`,
say — is compiled for every target unchanged.

**One signature list serves every target.** Both companions above export `fdiv` and `idiv`, with
the types `Core.Prim` declares for them. A facade whose exports cannot be provided on some
target is not one facade: it splits into one facade per target, each shipping the companion it
has.

**A facade with no companion for the target being built is an error**, and the error names the
facade and the target. A facade that ships one companion is a package that builds for one
target — a facade over a browser API, say.

### The JavaScript companion

An ES module. Its exports take a **plain parameter list**: a Zelkova function of two arguments
is backed by a JavaScript function of two arguments, called directly. There is no
curried-wrapper convention to observe on the JavaScript side — currying is the compiler's
business.

### The WebAssembly companion

A [component](https://component-model.bytecodealliance.org/design/wit.html), whose exported
interface is written in WIT. Its exports take a plain parameter list on the same terms, and each
is declared with the WIT spelling of the Zelkova type in its signature.

## Which types may cross the boundary

A type may be named in a facade signature when **every target can hold the code behind the
facade to it**. Two targets means two mechanisms, and a type is admitted when it has both.

- **JavaScript needs a predicate**: a piece of JavaScript that decides, from a value alone,
  whether that value belongs to that type. A value the companion hands back is run through the
  predicate of the type its signature declares, on every crossing.
- **WebAssembly needs a spelling**: a WIT type that means what the Zelkova type means. The
  component's declared interface is checked against the facade once, when the component is
  loaded, and nothing is checked per call.

A value that fails a check becomes [`Err (Malformed
..)`](evaluation-semantics.md#an-effect-that-can-fail) out of an [effectful
facade](#an-effectful-facade), and [aborts the
program](evaluation-semantics.md#when-a-program-aborts) out of an [`unsafe`](#an-unsafe-facade)
one, which has no result type to carry it.

A facade has no body the compiler can read, so its annotation is enforced by the target rather
than while compiling. Four of the
[six forms a type expression can take](types.md#the-forms-of-a-type-expression) have both
mechanisms:

| Type | What its JavaScript predicate decides | What its WIT spelling is |
|---|---|---|
| `Int` | the value is a number, and a whole one the [32-bit range](evaluation-semantics.md#numbers) holds | `s32` |
| `Float` | the value is a number | `f64` |
| `Bool` | the value is a boolean | `bool` |
| `Char` | the value is a string of one character | `char` |
| `String` | the value is a string | `string` |
| `()` | the value is the one value that type has | nothing: no parameter, and no result |
| A tuple | the value is an array of the tuple's length, each element satisfying its component's predicate | `tuple` of its components' spellings |
| A record | the value is an object with exactly the record's fields, each field satisfying its own predicate | a `record` of the same fields |
| A list | the value is an array, every element of which satisfies the element type's predicate | `list` of the element's spelling |
| A union type, applied to admitted types | the value carries one of that type's constructor names, and arguments satisfying that constructor's predicates | a `variant`, one case per constructor |

A signature may name any of those, in any position an argument or a result may take.

```zel expect=unimplemented
module foreign Core.Colour exposing
  ( rgb
  , luminance
  )

rgb : Int -> (Int, Int, Int)
luminance : (Int, Int, Int) -> Float
```

JavaScript has one number type where Zelkova has two, and the `Int` predicate is what separates
them. WIT has both and needs no separating.

The two mechanisms cost different things. A predicate walks the whole value, so a facade taking
a list of a thousand tuples checks a thousand tuples on the way in, once per crossing; the same
facade on a WebAssembly target checks nothing at the call, because the interface was checked
when the component loaded.

**Not implemented:** `String` has no [literal syntax](lexical-structure.md#strings) yet,
[`()` is not recognised](types.md#the-unit-type) in either position, records have no brace token
([`LANG-47`](../tickets/lang-47.md)) and lists no literal production
([`LANG-44`](../tickets/lang-44.md)) — so four of those rows are about types a program cannot
write today. Both constructs are specified, in [Records](records.md) and [Lists](lists.md), and
neither chapter publishes an encoding: what a record and a list look like across either boundary
belongs to code generation. Nothing runs a predicate or reads a WIT interface either, because
code generation has not started — [`GEN-2`](../tickets/gen-2.md) is the ticket that emits them.

### A union crosses as a tagged value

A companion may receive a union value and construct one, so the two sides have to agree on its
shape.

In JavaScript a union value is an **object carrying the name of its constructor** in a `$` field,
with that constructor's arguments in declaration order in fields named `a`, `b`, `c` and so on.
`Red` is `{$: "Red"}`; `Rgb 255 0 0` is `{$: "Rgb", a: 255, b: 0, c: 0}`. The predicate reads
`$`, checks it against the declaration's constructors, and checks each argument against the
predicate of the type that constructor declares for it.

In WebAssembly it is a `variant` with one case per constructor, named after it, carrying that
constructor's arguments as its payload — a `tuple` of them where there is more than one.

```zel expect=ok
module Palette exposing (Swatch(..))

type Swatch
  = Named Int
  | Rgb Int Int Int
```

```zel expect=unimplemented
module foreign Core.Palette exposing
  ( toHex
  )

import Palette exposing (Swatch(..))

toHex : Swatch -> Int
```

A constructor's **name** is part of what a companion depends on, on either target, so renaming
one breaks every companion that mentions it.

A union may be recursive, and the JavaScript predicate follows it: the walk descends into each
argument and terminates because a Zelkova value is immutable and can hold no cycle.

**Not implemented:** no value is encoded, no predicate is run and no interface is read, because
code generation has not started; both encodings above are what
[`GEN-2`](../tickets/gen-2.md) emits.

### What a facade signature may not name

Two of the six forms can be enforced by neither target, and a facade signature may name neither.

A **type variable** excludes no value, so there is nothing for a predicate to decide, and WIT has
no variable to spell it with. A facade is therefore monomorphic; polymorphism lives in ordinary
Zelkova above it.

A **function type** is rejected wherever it appears. `typeof x === 'function'` decides that a
value is *some* function, not that it is the one declared, and a component's interface has no
function type to name either.

```zel expect=unimplemented
module foreign Core.Utils exposing
  ( equal
  )

equal : a -> a -> Bool
```

```zel expect=unimplemented
module foreign Core.List exposing
  ( count
  )

count : (Int -> Bool) -> Int -> Int
```

**Known gap:** neither signature is admitted — `equal` names a type variable, `count` takes a
function — and nothing rejects either. A facade annotation is resolved exactly as any other
annotation is, so every type a normal module may write, a facade may write.
[`LANG-43`](../tickets/lang-43.md) is the check. Both blocks fail on the modifier today rather
than on the type, so both go red when [`LANG-54`](../tickets/lang-54.md) lands and stay red until
`LANG-43` follows it.

**Not implemented:** a class constraint is rejected on the same grounds, `Comparable a => a`
being a signature over `a`. A constrained function is specialised, and a facade has no body to
specialise.
[Type classes](type-classes.md#a-constrained-function-may-not-be-a-foreign-facade) is the
chapter.

## An effectful facade

A clock, a file and a socket are what a program reaches foreign code for, and none of them is a
function of its arguments. So a facade declares an effect by default: its companion may read,
write, wait and fail.

**A facade signature declares an effect, and its result type must be `Task (Result Failure a)`.**
Any other result type is an error unless the signature is marked
[`unsafe`](#an-unsafe-facade), so a facade cannot declare its foreign code to be infallible by
saying less.
[`Failure`](evaluation-semantics.md#an-effect-that-can-fail) is the error type every one of them
names, `a` being the only part its author chooses. It is not one of
[the default imports](modules.md#the-default-imports), so a facade naming it imports it.

```zel expect=unimplemented
module foreign Core.File exposing (read)

import Task exposing (Failure)

read : String -> Task (Result Failure String)
```

A `Task` never crosses the boundary, and neither does the `Result`. The companion takes the
arguments the signature names and returns the payload — a string, for `read`:

```js
export async function read(path) {
  return await fs.promises.readFile(path, "utf8");
}
```

That companion throws when the file is missing, and it is a correct companion. The `Result` is
built on the Zelkova side, by the wrapper the compiler puts around the call: it catches what the
companion throws, checks what the companion returns against `a`, and yields `Ok` for a value that
passes, `Err (Threw ..)` for a failure the companion raised, and `Err (Malformed ..)` for a value
that does not match.

What counts as raising a failure is the target's own word for it: a thrown exception or a
rejected promise in JavaScript, [a trap](evaluation-semantics.md#when-a-program-aborts) in
WebAssembly. Both arrive as `Err (Threw ..)`.

`a` is the type the companion really hands back: it is checked at the boundary like any other
returned value, and it may be neither a type variable nor a function type.

A companion whose result is not ready at once returns it the way its target returns a value
later, and the check runs on the value that arrives. A result that never arrives is a `Task` that
never produces a value, which is the second of the [two
outcomes](evaluation-semantics.md#two-outcomes) rather than a failure.

A [facade constant](#facade-constants) may name a `Task` too, and it is the one constant whose
JavaScript companion is a function: the effect has to happen each time the `Task` is run, and a
value exported once happens once.

```zel expect=unimplemented
module foreign Core.Time exposing (now)

import Task exposing (Failure)

now : Task (Result Failure Int)
```

```js
export function now() {
  return Date.now();
}
```

An effectful facade is a facade, so it is [not importable outside the package that declares
it](packages.md#what-a-package-exposes). A package offers an effect to its dependents the way it
offers any other foreign-backed value: an ordinary module imports the facade and re-declares what
it offers.

**`Task` may appear only as the whole of a result type.** Not as an argument; and not nested
inside another type.

**A failure the caller is meant to tell apart from a broken companion goes in the payload.**
`Failure` reports that the foreign code broke and says nothing about what the effect was for. A
`read` separating a missing file from a broken companion declares `String -> Task (Result Failure
(Result IoError String))` — two `Result`s, collapsed into one by the module that
[publishes `read`](packages.md#what-a-package-exposes) to other packages.

**Not implemented:** neither block above parses, on two counts. A type argument must be a bare
name today, so the parentheses in `Task (Result Failure String)` are a syntax error
([`LANG-9`](../tickets/lang-9.md)) — the same gap that rejects `Maybe (Maybe Int)` and every
other nested type — and the modifier is [`LANG-54`](../tickets/lang-54.md)'s. Nothing declares
`Task` or `Failure` either, no wrapper is generated, and nothing is checked at either boundary
([`GEN-1`](../tickets/gen-1.md), [`GEN-2`](../tickets/gen-2.md)); no check holds a facade to the
result type above, which is [`LANG-43`](../tickets/lang-43.md)'s.

## An `unsafe` facade

**`unsafe` before a signature declares a function rather than an effect.** The result type is
then any [admitted type](#which-types-may-cross-the-boundary), the `Task` and its `Result` are
gone, and no wrapper stands between the caller and the companion.

```zel expect=unimplemented
module foreign Core.Basics exposing (idiv)

unsafe idiv : Int -> Int -> Int
```

The word is a promise its author makes to the compiler, covering the two things the compiler
cannot check and does not try to:

- **The companion is a function of its arguments.** The same arguments give the same result, and
  calling it reads nothing, writes nothing and sends nothing anywhere — the same
  [purity](evaluation-semantics.md#purity-and-the-foreign-boundary) an ordinary Zelkova
  expression keeps.
- **The companion returns.** It does not fail, and it hands back a value rather than something
  that produces one later: work still unfinished when the call returns cannot be declared this
  way.

A companion that breaks the first computes wrong answers and nothing reports it. One that breaks
the second [aborts the program](evaluation-semantics.md#when-a-program-aborts).

**`unsafe` removes the `Task`, not the check.** The return value is held to its declared type
exactly as any other crossing is, and a value that fails
[aborts](evaluation-semantics.md#when-a-program-aborts).

**Not implemented:** the block does not parse. `unsafe` is an ordinary identifier today, so
`unsafe idiv : Int -> Int -> Int` reads as two names where the grammar expects one
([`LANG-53`](../tickets/lang-53.md)), the modifier is [`LANG-54`](../tickets/lang-54.md)'s, and
nothing holds an unmarked facade to a `Task` result either.

## Facade constants

A facade signature may also declare a constant — a type with no arrow, taking no arguments.

```zel expect=unimplemented
module foreign Core.Basics exposing
  ( pi
  , e
  )

pi : Float
e : Float
```

The plain-parameter-list rule above has a zero-argument case, and the two targets spell it
differently. A JavaScript companion exports the value itself — unless the constant names a
`Task`, which is [the one case](#an-effectful-facade) where it exports a function:

```js
export const pi = Math.PI;
export const e = Math.E;
```

A component exports only functions, so a WebAssembly companion exports one of no arguments
returning the value, and its WIT declares that.

Evaluation is [strict](evaluation-semantics.md#evaluation-is-strict), and an ordinary
parameterless binding is placed in an evaluation order that reads off which bindings it
mentions — see
[A binding with no parameters is evaluated once](evaluation-semantics.md#a-binding-with-no-parameters-is-evaluated-once).
A facade constant has no Zelkova body to place in that order: it names a foreign binding
directly, so it is evaluated on whatever schedule the target gives that — when the `.mjs` module
exporting it is evaluated, or when the component's export is called — and nothing here promises
it happens once, lazily, or at any particular point relative to the rest of the program.

**Not implemented:** the block fails on its modifier ([`LANG-54`](../tickets/lang-54.md)), which
is the whole of what stands between it and compiling: a constant signature is an annotation with
no body, which a facade already accepts.

## Testing a companion

A companion is target code, and so is its test: a JavaScript assertion about a JavaScript
function. It reaches Zelkova the way any other foreign code does — as a facade — and sits under
[`tests/`](packages.md#tests), the root a dependent never compiles.

```text
src/
  Core/
    Prim.zel        module foreign Core.Prim
    Prim.mjs
    Prim.wasm
tests/
  Core/
    PrimChecks.zel  module foreign Core.PrimChecks
    PrimChecks.mjs
    PrimChecks.wasm
    PrimTest.zel    the Test values a runner finds
```

Everything [a facade is](#a-facade-names-a-boundary-not-a-backend) holds of one under `tests/`:
signatures and no bodies, one companion per target,
[the admitted types](#which-types-may-cross-the-boundary) in every position, and the same error
when the target being built has no companion. The
[two roots share one set of module names](packages.md#source-roots), so a facade that checks
`Core.Prim` takes a different name.

**A test facade declares an effect, and is never [`unsafe`](#an-unsafe-facade).** A failed
assertion is a failure the companion raised, and [the wrapper](#an-effectful-facade) around an
effectful call turns that into `Err (Threw ..)`, which the module above reports as a failing
test. The same assertion behind an `unsafe` signature
[aborts the program](evaluation-semantics.md#when-a-program-aborts) instead of failing one test.

```zel expect=unimplemented
module foreign Core.PrimChecks exposing
  ( idivTruncates
  , idivRefusesAFraction
  )

import Task exposing (Failure)

idivTruncates : Task (Result Failure ())
idivRefusesAFraction : Task (Result Failure ())
```

Each is a [facade constant naming a `Task`](#facade-constants), so its JavaScript companion
exports a function, and the target's own assertion library raises:

```js
import assert from "node:assert/strict";
import { idiv } from "../../src/Core/Prim.mjs";

export function idivTruncates() {
  assert.equal(idiv(7, 2), 3);
}

export function idivRefusesAFraction() {
  assert.throws(() => idiv(7.5, 2));
}
```

**A test companion may import the companion it checks as a module of the target**, which is what
the second export above does: `Core.Prim` is reached as a `.mjs` file, with no boundary between
them. A value crossing a boundary is checked against the type its signature declares, so a test
written *above* `Core.Prim` can hand `idiv` only what `Int` admits. What a companion does with a
value outside that set is a question only the target can ask it.

A runner finds [a value of type `Test` a module under `tests/`
exposes](packages.md#what-a-test-is), so `Core.PrimTest` imports the facade and exposes one per
check.

**Not implemented:** none of this runs. The `zel` block fails on its modifier
([`LANG-54`](../tickets/lang-54.md)) and on the parentheses in its result type
([`LANG-9`](../tickets/lang-9.md)), and [`()` is not recognised](types.md#the-unit-type) in
either position. A package has one source root and no notion of a test
([`LANG-15`](../tickets/lang-15.md)), nothing declares `Task` or `Failure`, no wrapper is
generated around an effectful call ([`GEN-1`](../tickets/gen-1.md),
[`GEN-2`](../tickets/gen-2.md)), and there is no runner to find a `Test`. Until there is, a
companion test under `tests/` is a `.mjs` file that the target's own test runner is pointed at
directly, and the facade half of the pair is not written yet.
