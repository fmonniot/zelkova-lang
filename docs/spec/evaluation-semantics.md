# Evaluation semantics

A program runs by evaluating an expression, and every form [Expressions](expressions.md) lists
is evaluated by a rule below. Two properties decide almost all of them. Evaluation is
**strict**: a subexpression is evaluated when it is reached, not when its value is demanded. And
it is **pure**: evaluating an expression produces a value and has no other consequence.

**Not implemented:** nothing in the compiler evaluates a Zelkova program. The pipeline ends at
type checking and code generation has not started, so every rule below is one the compiler
neither enforces nor implements. Without an evaluator the blocks are checked for syntax and
nothing else, so a block tagged `expect=ok` says nothing about what it computes.
[`GEN-1`](../tickets/gen-1.md) is the ticket.

## Two outcomes

Evaluating an expression either produces a value or does not terminate. There is no third
outcome: no exception, no crash, no undefined result. Nothing in the language throws, so
nothing in it catches.

That is a promise about a *well-typed* program, and it is kept by pushing every failure
somewhere earlier or somewhere else:

| What would otherwise fail | Where it goes instead |
|---|---|
| A `case` with no branch for the value | [Coverage](patterns.md#a-pattern-that-can-fail-and-one-that-cannot) is checked, so no such `case` compiles |
| A name that is not in scope | [Name resolution](name-resolution.md#unresolved-names) |
| Applying a value that is not a function | Type checking |
| Integer division by zero | [Defined below](#an-operation-with-no-answer) to be `0`, because no `Int` value means *no answer* |
| A lookup, a parse, a conversion that can fail | The type: `Maybe a`, `Result e a` |

**Known gap:** coverage is not checked — the exhaustiveness phase inspects nothing and accepts
every module — so a `case` missing a branch compiles today and would have no value to produce.
[`LANG-19`](../tickets/lang-19.md) is the ticket.

## Evaluation is strict

An argument is a value before the function it is passed to is entered, and it is evaluated
whether or not the body ever mentions it.

```zel expect=ok
module Example exposing (first, loop, stuck)

first a b =
  a

loop n =
  loop n

stuck =
  first 1 (loop 2)
```

`first` ignores its second argument, and `stuck` still has no value: `loop 2` is evaluated
before `first` is entered, and it does not terminate.

A subexpression therefore runs once each time it is reached, whether or not its value is used.

`if` and `case` are the exception; see [Conditional evaluation](#conditional-evaluation),
below.

## Order of evaluation

**Subexpressions are evaluated left to right, in the order they are written.**

An application evaluates the function expression first, then each argument in source order,
and applies last.

```zel expect=ok
module Example exposing ((+), add, g, h, f)

infix left 6 (+) = add

add a b =
  a

g x =
  x

h x =
  x

f a b =
  g a + h b
```

`g a + h b` is `add (g a) (h b)`, so `add` is evaluated, then `g a`, then `h b`, and then the
application happens. [Precedence and associativity](expressions.md#precedence-and-associativity)
decide how an expression *groups*; they never decide what runs first.

The same rule covers every other form that holds more than one subexpression: a tuple evaluates
its elements left to right, an `if` evaluates its condition before either arm, and a `case`
evaluates its scrutinee before any branch is tried.

Because evaluation is [pure](#purity-and-the-javascript-boundary), the order is observable only
through non-termination: it settles which of two diverging subexpressions hangs the program.

## Conditional evaluation

`if` evaluates its condition, then exactly one of its arms — never both. `case` evaluates its
scrutinee, then tries its branches in the order written and evaluates the body of the first one
that matches — never another.

These are the only two forms in the language that evaluate one subexpression and not another.

### Nothing short-circuits

`&&` and `||` are [names](expressions.md#an-operator-is-a-name), bound by an `infix`
declaration to ordinary functions.
**Both operands of `&&` are evaluated, always, and likewise for `||`.**

```zel expect=ok
module Example exposing (Bool, (&&), and, safe)

type Bool
  = True
  | False

infix right 3 (&&) = and

and a b =
  if a then b else False

safe a b =
  a && b
```

`and`'s body uses `if`, so the *body* looks at `b` only when `a` is `True`. Conditional
evaluation is a property of the call site, and `&&` is a call.

Skipping the right operand is written out:

```zel expect=ok
module Example exposing (Bool, expensive, careful)

type Bool
  = True
  | False

expensive n =
  expensive n

careful a n =
  if a then expensive n else False
```

`&&` gets no exception: an operator's meaning is its `infix` declaration and nothing else, and
no signature can defer a parameter.

**Known gap:** `std/core`'s `(&&)` and `(||)` are documented as short-circuiting.
[`LANG-36`](../tickets/lang-36.md) is the ticket. No block holds it to account: the claim is in
a doc comment, and the JavaScript behind those two operators already takes both operands
evaluated.

## Bindings

### A binding with no parameters is evaluated once

A top-level binding that names no parameters is evaluated once, before the program runs, and
every reference to it is that one value. Such bindings are evaluated in **dependency order**:
one is evaluated after everything it mentions.

```zel expect=ok
module Example exposing (Colour, base, shifted, other)

type Colour
  = Red
  | Green

base =
  Red

shifted =
  other base

other c =
  case c of
    Red ->
      Green

    Green ->
      Red
```

`shifted` mentions `base`, so `base` is evaluated first, whichever order the two are written in.
[Declarations are unordered](declarations.md#declarations-are-unordered) as text; this is the
one place the language puts an order on them, and it reads it off the references rather than off
the page.

A binding that *does* name parameters is not evaluated at initialisation at all. Its value is
the function, and its body runs when the function is applied.

### A binding may not depend on itself

Under strict evaluation a parameterless binding's value has to exist before the binding can be
used. **A cycle among parameterless bindings is an error**, whether it is one binding long or
runs through several.

```zel expect=ok
module Example exposing (x)

x =
  x
```

**Known gap:** that is accepted, and so is `a = b` beside `b = a`. Nothing computes an
initialisation order, so nothing notices there is no order to compute.
[`LANG-35`](../tickets/lang-35.md) is the ticket, and this block goes red when it lands.

The restriction is on parameterless bindings only. A function may call itself, and two functions
may call each other, because neither body runs until the function is applied:

```zel expect=ok
module Example exposing (Nat, Bool, isEven, isOdd)

type Bool
  = True
  | False

type Nat
  = Zero
  | Succ Nat

isEven n =
  case n of
    Zero ->
      True

    Succ m ->
      isOdd m

isOdd n =
  case n of
    Zero ->
      False

    Succ m ->
      isEven m
```

### `let` follows the same rule

```zel expect=unimplemented
module Example exposing (f)

f n =
  let
    doubled =
      add n n

    quadrupled =
      add doubled doubled
  in
  quadrupled
```

**Not implemented:** there is no `let` production
([`LANG-33`](../tickets/lang-33.md)). Its bindings are evaluated when the `let` is reached, in
dependency order, once each — `doubled` above is computed once and used twice. The bindings of
one `let` are [mutually recursive](expressions.md#let--in), and the same restriction applies
one level down: a `let` binding with parameters may take part in a cycle, one without may not.

## Function values

A function is a value like any other. It can be an argument, a result, an element of a tuple,
and the value of a binding.

Every function takes exactly one argument, and a function of several is one that returns a
function ([Application](expressions.md#application)), so **applying a function to fewer
arguments than its type has arrows produces a function value** rather than doing part of the
work:

```zel expect=ok
module Example exposing (Pair, pair, pairWithOne)

type Pair a
  = Pair a a

pair a b =
  Pair a b

pairWithOne =
  pair 1
```

`pairWithOne` is a value: the function `pair`, carrying the argument it has already been given.
Nothing of `pair`'s body has run.

A function value carries whatever its body needs from the scope it was built in, which is what
makes a partially applied function, a
[`let`-bound](expressions.md#let--in) function, and a [lambda](expressions.md#lambdas) work
away from where they were written.

**A function is not observable except by applying it.** It has no identity, no arity that can
be asked for, and no equality — see below. Two functions that compute the same results are
indistinguishable, and so are two occurrences of the same lambda.

## Equality

`==` is not built into the language. It is an [operator](expressions.md#an-operator-is-a-name),
bound to a function, and that function is a member of the `Eq` class
([Type classes](type-classes.md#what-the-standard-library-declares)). What equality *means* is
therefore a property of each instance rather than of the language.

```zel expect=ok
module Example exposing (Bool, (==), eq, alike)

type Bool
  = True
  | False

infix non 4 (==) = eq

eq a b =
  True

alike a b =
  a == b
```

### What structural equality computes

Every instance `std/core` declares is **structural**, and so is a
[derived instance](type-classes.md#an-instance-may-be-derived). Structural equality is defined
by the shape of the value:

- Two values of a union type are equal when they are the same constructor and their
  corresponding arguments are equal.
- Two tuples are equal when their elements are pairwise equal. They have the same
  [arity](types.md#tuple-types) by construction, since arity is part of the type.
- Two `Char`s are equal when they are the same character; two `String`s when they are the same
  sequence of characters; two `Int`s when they are the same number.
- Two `Float`s are compared as [IEEE 754](#numbers) does, which is the one place structural
  equality does not apply: `nan` is equal to nothing, itself included, and `0.0` is equal to
  `-0.0`.

An instance is free to define something else — equality up to a normal form, say, for a type
whose representation has more than one spelling of the same value.

```zel expect=unimplemented
module Example exposing (Colour, alike)

type Colour
  = Red
  | Green

alike : Eq a => a -> a -> Bool
alike a b =
  eq a b
```

**Not implemented:** no part of a class parses — `class`, `instance` and `=>` are ordinary
identifiers today ([`LANG-37`](../tickets/lang-37.md),
[`LANG-38`](../tickets/lang-38.md)), and `std/core` declares no classes
([`LANG-42`](../tickets/lang-42.md)). A type asks for the definition above rather than writing
it out by declaring an instance whose body is `derived` — and what that yields is the definition
[`Eq`'s own declaration supplies](type-classes.md#a-class-says-how-it-is-derived), not one the
compiler holds for a class it recognises.

### Functions are not comparable

There is no `Eq` instance for a function type, and there cannot usefully be one: deciding
whether two functions agree on every input is not something a program can do. So `f == g` is a
type error — an unsatisfied constraint, reported where every other unsatisfied constraint is
reported, rather than something that compiles and then misbehaves.

## Recursion and tail calls

Recursion is the only way to iterate. There is no loop form, and there is nothing to mutate
that a loop would use.

**A self tail call runs in constant stack.** A call to the enclosing declaration, in tail
position, is compiled as a jump back to the top of that declaration with new arguments — so a
recursion written this way is as deep as the compiler's stack allows, which is to say
unbounded.

```zel expect=ok
module Example exposing (Nat, count)

type Nat
  = Zero
  | Succ Nat

count acc n =
  case n of
    Zero ->
      acc

    Succ m ->
      count (Succ acc) m
```

An expression is in **tail position** when its value is the value of the whole declaration
body. The body itself is; both arms of an `if` in tail position are; every branch body of a
`case` in tail position is; and the expression after `in` of a `let` in tail position is.
Nothing else is — not an argument, not an operand, not a scrutinee, not an `if`'s condition.
`count (Succ acc) m` above is in tail position; `Succ (count acc m)` would not be, and would
use stack proportional to `n`.

The guarantee covers a call to the declaration the call is written in, and nothing wider.
Mutual tail recursion between two declarations carries no guarantee.

## Sharing

No rule above says how long an operation takes or how much memory it uses, and nothing here
gives one. The promise below is narrower, and needs only one notion of what a value is: a value
exists independently of any name bound to it, so binding a second name to one produces a second
way to reach it, not a second value.

**A value is not copied when it is passed as an argument, returned as a result, bound to a name,
or stored inside another value.** Binding a large structure to a second name costs nothing beyond
the binding itself. A backend that copies a value on assignment does not implement Zelkova.

**Not promised: anything about a function value.** Applying a function to fewer arguments than
its type has arrows produces a function value ([Function values](#function-values)), and whether
the code generator builds a new one on every such application or reuses one across calls made
with the same arguments is left to it entirely. A program whose performance depends on `f 1`,
written inside a loop where `f` is applied to a constant, not allocating depends on a particular
backend, never on the language.

Sharing does not widen [the tail-call rule](#recursion-and-tail-calls): mutual recursion between
two declarations still carries no stack guarantee. Nor does it name an allocation count for any
operation — only that reusing an existing value is free.

## Numbers

**`Int` is a 32-bit signed two's-complement integer.** Arithmetic wraps: `2147483647 + 1` is
`-2147483648`. The range and the wrapping are the same on every compilation target, so a
program computes the same answer wherever it is run.

**`Float` is an IEEE 754 binary64 number**, with IEEE's own answers throughout. `1.0 / 0.0` is
positive infinity, `0.0 / 0.0` is `nan`, and the ordering of a `nan` against anything is
`False`. Nothing about a `Float` operation is a failure; IEEE defines a result for every one of
them, and those results are the language's.

A float literal denotes the binary64 value nearest to the decimal number it spells, rounding
**to nearest, with ties going to the value whose final mantissa bit is even** — the rounding
IEEE 754 specifies for every decimal-to-binary conversion, and this language has no reason to
pick anything else. Rounding is total: every literal that [Lexical
structure](lexical-structure.md#floats) accepts denotes some binary64 value, and none is
rejected for the value it rounds to. A literal too large in magnitude for any finite binary64
value denotes positive infinity; one too small to be distinguished from zero denotes positive
zero. Both are the *positive* infinity and the *positive* zero, because a float literal's
grammar never places a `-` before it — a literal is always non-negative — so a negative literal,
a negative infinity and a negative zero are all reached the same way any other negative `Float`
is: by [prefix negation](lexical-structure.md#prefix-negation) applied to a non-negative one.

`nan` has no literal spelling at all — no run of digits denotes it — and is reached only
through an operation IEEE defines to produce it, such as the `0.0 / 0.0` above. Once reached,
`nan`, the infinities and the negative zero are ordinary `Float` values: every operation this
section defines accepts them and returns IEEE's answer, and [structural
equality](#what-structural-equality-computes) is the one place that answer is not the everyday
one.

### An operation with no answer

Some operations are handed arguments for which no answer exists: a divisor of zero, the square
root of a negative number, the logarithm of a negative one. A well-typed program has [only two
outcomes](#two-outcomes) and a crash is not one of them, so each of these still produces a
value — and which value it produces is decided by what the result type has room for.

**A `Float`-returning operation with no answer produces `nan`.** binary64 keeps a value meaning
*not a number*: `nan` is not any number, it propagates through every arithmetic operation it
reaches, and `isNaN` detects it. So a `Float` operation never invents a stand-in answer.
`sqrt (-1)` is `nan`, `logBase 0 0` is `nan`, `0.0 / 0.0` is `nan`, and a caller can ask
afterwards whether an answer was ever found.

Rounding is a different thing, and this rule does not reach it. An operation whose exact result
is too small for binary64 *has* an answer — the nearest representable value, which is a zero —
and returns it, so `1.0e-300 * 1.0e-300` is `0.0` rather than `nan`, and a literal too small to
be distinguished from zero denotes positive zero for the same reason. Losing precision, even all
of it, is not the same as having nothing to return.

`Int` has no such value. Every 32-bit two's-complement bit pattern is a number somebody might
have meant, so whatever an integer operation returns is indistinguishable from a real result,
and the language names one rather than leaving the operation partial:

```zel expect=ok
module Example exposing ((//), idiv, half)

infix left 7 (//) = idiv

idiv a b =
  a

half n =
  n // 2
```

**`n // 0` is `0`. `modBy 0 n` is `0`. `remainderBy 0 n` is `0`.** These are the values that
keep those three operations total. Nothing marks such a zero as invented, so a caller for whom a
zero divisor is a real case tests the divisor beforehand, where a `Float` caller can test the
result afterwards.

**Known gap:** `modBy 0` calls an undefined `__Debug_crash`, so it is a `ReferenceError` rather
than `0`, and `remainderBy 0` returns `nan` rather than `0`.
[`BUG-24`](../tickets/bug-24.md) is the ticket. No block holds either to account: both are in
the JavaScript companion files, which nothing in the test suite runs.

### Converting a `Float` to an `Int`

`round`, `floor`, `ceiling` and `truncate` each take a `Float` and produce an `Int`, and there
are `Float`s the `Int`s have no room for: `nan`, both infinities, and every finite value outside
the 32-bit range.

**A conversion to `Int` rounds as its name says and then wraps into 32 bits**, the way `Int`
arithmetic wraps, **and `nan` and both infinities convert to `0`.** That zero is the same
concession `//` makes, and for the same reason: the result type has no value meaning *no
answer*, so the conversion returns one that means something else. A program holding a `Float` it
is unsure of tests it with `isNaN` or `isInfinite` before converting, as a program holding a
divisor tests the divisor.

**Known gap:** `round`, `floor` and `ceiling` return their JavaScript `Math` result with no
wrap, so `round nan` is `nan` and `round 1.0e20` is `1.0e20` — neither of them an `Int`. Only
`truncate` wraps. [`BUG-25`](../tickets/bug-25.md) is the ticket. No block holds it to account:
all four conversions are in a JavaScript companion, which nothing in the test suite runs.

## Purity and the JavaScript boundary

**An expression's value depends only on the values of the names it mentions.** Evaluating it
twice gives the same value, and evaluating it does nothing else — nothing is written, read,
mutated or sent anywhere.

A [`module javascript` facade](js-interop.md) is where that guarantee meets code the compiler
did not produce and cannot inspect. **A facade's companion export must be a function of its
arguments**: the same arguments give the same result, and calling it has no other consequence.
A facade whose JavaScript reads a clock, keeps a counter, prints, or reaches the network breaks
that rule, and a program using it has no meaning the language defines.

One facade form is exempt, and it is how a clock is read: a signature whose result type is
[`Task`](#effects) declares an effect rather than a function, and its companion may do anything.
[JS interop](js-interop.md#an-effectful-facade) carries that form.

```zel expect=ok
module javascript Js.Math exposing (square)

square : Float -> Float
```

Nothing distinguishes that from a facade over an impure export:

```zel expect=ok
module javascript Js.Random exposing (next)

next : Int -> Int
```

Both compile. The second is a broken program, and the rule it breaks is one only its author can
keep.

Purity is not the only rule that crosses this boundary. **A companion also owes the answers
[Numbers](#an-operation-with-no-answer) defines**: a `Float`-returning companion with no answer
returns `nan`, an `Int`-returning one returns the value that section names, and a conversion
returns a value the `Int` type can hold. Neither returns a stand-in a caller cannot tell from a
real result. Most of the language's arithmetic is written as a facade, so a rule that stopped
here would be a rule the language did not have — and it is unenforceable in exactly the way
purity is, because a type annotation with no body is all the compiler ever sees.

How a program *does* reach the outside world is the next section's.

## Effects

Everything above describes a computation, and a computation on its own does nothing. A program
reaches the outside world by producing a **`Task`**: a value describing work, which the runtime
performs.

`Task` is a type `zelkova-core` declares and exposes without its constructors, so a `Task` is
opaque everywhere but in core — nothing else builds one out of parts or takes one apart. A
`Task a` describes work that produces an `a` when it is run.

**Building a `Task` performs nothing.** An expression whose value is a `Task` is as pure as any
other: evaluating it twice gives the same description twice, and neither evaluation reads a file
or writes a byte. The work happens when a `Task` is [run](#running-a-task), so the guarantee
above holds over a program with effects in it.

### Where a `Task` comes from

A primitive `Task` is declared by a [`module javascript` facade](js-interop.md) whose result
type is one.

```zel expect=ok
module javascript Js.File exposing (read)

read : String -> Task String
```

That is the only place an effect enters the language. What the companion behind such a signature
returns, and which types the signature may still name, is
[JS interop](js-interop.md#an-effectful-facade)'s. Every other `Task` is built from those, so a
package declares its own effects on the same terms `zelkova-core` declares its.

### Sequencing

`Task.andThen` runs one `Task` after another, and it is an ordinary function.

```zel expect=fragment
succeed : a -> Task a
map : (a -> b) -> Task a -> Task b
andThen : (a -> Task b) -> Task a -> Task b
```

`andThen f t` describes running `t`, handing its result to `f`, and running the `Task` `f`
returns. None of the three is a member of a class. A class over `Task` would need a variable
ranging over type constructors, which [a class variable is
not](type-classes.md#a-class-is-always-over-a-complete-type), so what the language offers is one
concrete type and ordinary functions over it.

Nothing about `Task` is built into the language beyond the name and
[running one](#running-a-task). Core writes its sequencing the way any module writes a function
over a type it declares.

```zel expect=fragment
type Task a
  = Task (() -> a)

andThen f t =
  Task (\_ ->
    case t of
      Task step ->
        case f (step ()) of
          Task next ->
            next ())
```

Which shape core picks is core's, and a program outside it cannot observe the difference.

### Running a `Task`

A program hands the runtime one `Task`, and running the program is running that `Task`. The
value it hands over is `main`, whose type [Packages](packages.md#programs) writes down. Nothing
else runs one: a `Task` a program builds and never gives to the runtime describes work that
never happens.

A `Task` is a value, so [sharing](#sharing) reaches it like anything else — binding one to two
names gives two ways to reach one description. Running that description twice performs its work
twice.

The effects of a `Task` happen in the order it sequences them: `andThen` runs the second only
once the first has produced a value.

### An effect that can fail

`Task` takes one type parameter and carries no channel for an error. An effect that can fail
produces a value saying so — `read` above, made honest, is `String -> Task (Result IoError
String)` — which is where [every other failure the language admits](#two-outcomes) goes.

Running a `Task` adds no third outcome to the two that section names. A companion that would
throw returns the failure in its result type instead, and one that throws anyway breaks its
contract the way any other broken facade does.

**Not implemented:** `zelkova-core` declares no `Task`, no facade wrapper is generated, and
nothing runs a program at all — the pipeline ends at type checking, so there is no runtime for a
`Task` to be handed to ([`GEN-1`](../tickets/gen-1.md)).

**Known gap:** the facade block above compiles, for the reason
[JS interop](js-interop.md#an-effectful-facade) gives: an unresolved type name is invented rather
than reported ([`BUG-16`](../tickets/bug-16.md)). It goes red when `BUG-16` lands.
