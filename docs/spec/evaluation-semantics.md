# Evaluation semantics

A program runs by evaluating an expression, and every form [Expressions](expressions.md) lists
is evaluated by a rule below. Two properties decide almost all of them. Evaluation is
**strict**: a subexpression is evaluated when it is reached, not when its value is demanded. And
it is **pure**: evaluating an expression produces a value and has no other consequence.

**Not implemented:** nothing in the compiler evaluates a Zelkova program. The pipeline ends at
type checking and code generation has not started, so every rule below is one the compiler
neither enforces nor implements. Without an evaluator the blocks are checked for syntax and
nothing else, so a block tagged `expect=ok` says nothing about what it computes.

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
| Integer division by zero | [Defined below](#numbers) to be `0` |
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

Strictness is what makes the cost of an expression readable off the page. Under call-by-need
the same text can run once, twice, or not at all depending on what its consumer demands, so the
question "does this line run" is answered somewhere other than where the line is written.

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
through non-termination. It is specified anyway: it settles which of two diverging
subexpressions hangs the program, so a hang is reproducible rather than a property of the
backend, and purity is a rule the compiler cannot check at the
[JavaScript boundary](#purity-and-the-javascript-boundary), which is exactly where an
unspecified order would be least recoverable.

## Conditional evaluation

`if` evaluates its condition, then exactly one of its arms — never both. `case` evaluates its
scrutinee, then tries its branches in the order written and evaluates the body of the first one
that matches — never another.

These are the only two forms in the language that evaluate one subexpression and not another.

### Nothing short-circuits

`&&` and `||` are [names](expressions.md#an-operator-is-a-name), bound by an `infix`
declaration to ordinary functions, and an ordinary function under call-by-value receives values.
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

`and`'s body uses `if`, so the *body* looks at `b` only when `a` is `True` — and that changes
nothing, because `safe` has already evaluated `b` in order to call `and` with it. Conditional
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

The alternative is to let `&&` skip an operand, and every way of arranging that costs more than
it buys. Recognising `&&` by name gives the language two kinds of operator — one whose meaning
is its `infix` declaration, one whose meaning is written into the compiler — so a reader can no
longer answer "what does this operator do" by finding its declaration. Making the *signature*
defer a parameter is honest but general: it puts a second evaluation strategy in the language,
and every function's type then has to be read for which of its arguments are values.

What is lost is smaller than it looks. In a pure language the operands differ only in cost and
in whether they terminate, and the form that expresses "only if" is `if`, which says so at the
point of use.

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
used, so one that depends on its own value describes nothing. **A cycle among parameterless
bindings is an error**, whether it is one binding long or runs through several.

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

Every instance `std/core` declares is **structural**, and so is a derived instance. Structural
equality is defined by the shape of the value:

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
whose representation has more than one spelling of the same value. That is the reason `Eq` is a
class rather than a primitive: a type that knows what its own values mean can say so.

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
identifiers today ([`CLASS-1`](../tickets/class-1.md),
[`CLASS-2`](../tickets/class-2.md)), and `std/core` declares no classes
([`CLASS-6`](../tickets/class-6.md)). How a structural instance is *derived*, rather than
written out, is an [open question](#open-questions).

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
Mutual tail recursion between two declarations carries no guarantee: a self tail call is a
rewrite of one declaration into a loop, needing no analysis beyond that declaration and costing
nothing at any other call site, while a general guarantee needs either a trampoline every call
in the program pays for or a target with proper tail calls, and neither JavaScript nor
WebAssembly offers one portably.

## Numbers

**`Int` is a 32-bit signed two's-complement integer.** Arithmetic wraps: `2147483647 + 1` is
`-2147483648`. The range and the wrapping are the same on every compilation target, so a
program computes the same answer wherever it is run.

**`Float` is an IEEE 754 binary64 number**, with IEEE's own answers throughout. `1.0 / 0.0` is
positive infinity, `0.0 / 0.0` is `nan`, and the ordering of a `nan` against anything is
`False`. Nothing about a `Float` operation is a failure; IEEE defines a result for every one of
them, and those results are the language's.

Integer division has to define a result for a zero divisor, because a well-typed program has
[only two outcomes](#two-outcomes) and a crash is not one of them:

```zel expect=ok
module Example exposing ((//), idiv, half)

infix left 7 (//) = idiv

idiv a b =
  a

half n =
  n // 2
```

**`n // 0` is `0`. `modBy 0 n` is `0`. `remainderBy 0 n` is `0`.** These are the values that
keep those three operations total, which is what lets `//` stay an `Int -> Int -> Int` rather
than becoming a `Maybe`-returning function every arithmetic expression has to unwrap. A caller
for whom a zero divisor is a real case tests the divisor.

**Known gap:** `modBy 0` calls an undefined `__Debug_crash`, so it is a `ReferenceError` rather
than `0`, and `remainderBy 0` returns `nan` rather than `0`.
[`BUG-24`](../tickets/bug-24.md) is the ticket. No block holds either to account: both are in
the JavaScript companion files, which nothing in the test suite runs.

## Purity and the JavaScript boundary

An expression's value depends only on the values of the names it mentions. Evaluating it twice
gives the same value, and evaluating it does nothing else — nothing is written, read, mutated
or sent anywhere.

Every rule above rests on it: evaluating a parameterless binding once and sharing the result,
rewriting a self tail call into a loop, and specifying an evaluation order without specifying
how many times anything runs are sound only because the answer does not depend on when or how
often the work happens.

A [`module javascript` facade](js-interop.md) is where that guarantee meets code the compiler
did not produce and cannot inspect. **A facade's companion export must be a function of its
arguments**: the same arguments give the same result, and calling it has no other consequence.
A facade whose JavaScript reads a clock, keeps a counter, prints, or reaches the network breaks
that rule, and a program using it has no meaning the language defines.

```zel expect=ok
module javascript Js.Math exposing (square)

square : a -> a
```

Nothing distinguishes that from a facade over an impure export:

```zel expect=ok
module javascript Js.Random exposing (next)

next : a -> a
```

Both compile. The second is a broken program, and the rule it breaks is one only its author can
keep — which is the price of having exactly one way into JavaScript and no privileged escape
hatch beside it.

How a program *does* reach the outside world is not this boundary's job and is undesigned; see
below.

## Open questions

- **How a program reaches the outside world.** Everything above describes a pure computation,
  and a program that only computes is not much of a program. The
  mechanism — what a value that describes an effect is, how one is run, how results come back —
  is undesigned, and it is the same design that
  [what type `main` must have](packages.md#open-questions) waits on.
- **How a structural instance is derived.** `Eq` is a class, so every type that wants `==` needs
  an instance, and writing a structural one out by hand for each is a tax the language should
  not charge. [Type classes](type-classes.md) specifies no way to ask for the structural
  definition — whether by a clause on the `type` declaration, an instance with no members, or
  something else. `Comparable`'s ordering over a union type has the same question and the same
  answer.
- **Whether the language promises anything else about space.** The tail-call rule is the one
  promise here about memory. Whether a program can rely on anything more — that a value is not
  copied, that a partially applied function is not rebuilt per call — is unanswered, and each
  answer constrains a code generator that does not exist.
- **What a `Float` literal denotes exactly.** `Float` is binary64 and most decimal literals are
  not, so a literal is rounded. Which rounding, and whether a literal that is not representable
  is an error rather than a rounding, is unspecified.
