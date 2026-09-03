# Expressions

An expression denotes a value. Every declaration's body is one, every argument to a function
is one, and every branch of a `case` is one — there is no statement form anywhere in the
language.

That is what makes `if` require its `else`, and a `case` cover its type: a form that produced
no value in some of its cases would not be an expression.

## The forms

| Form | Written |
|---|---|
| Literal | `42`, `1.5`, `'x'`, `"hello"` |
| Variable | `count`, `List.map` |
| Constructor | `Nothing`, `Maybe.Just` |
| Application | `f x y` |
| Operator application | `a + b` |
| Prefix negation | `-x` |
| Grouping | `(a)` |
| Tuple | `(a, b)`, `(a, b, c)` |
| Conditional | `if c then a else b` |
| Case analysis | `case v of …` |
| Local bindings | `let … in …` |
| Lambda | `\x -> e` |

Three more forms — list literals, records, and record field access — are part of the language
and are specified in their own chapters. None of them is implemented; see
[Forms the compiler does not have](#forms-the-compiler-does-not-have) at the foot of this
chapter.

## Literals

A literal is an expression whose value is written out in full. The spelling of each is
[Lexical structure](lexical-structure.md#literals)' subject; what a literal *means* is here.

### A literal's type is its spelling

**A numeric literal written without a point is an `Int`. One written with a point is a
`Float`.**

```zel expect=ok
module Example exposing (count, ratio, letter)

count = 1

ratio = 1.5

letter = 'x'
```

An annotation cannot change that: a declaration annotated `Float` whose body is `1` is an
error, and the fix is to write `1.0`. Arithmetic between the two is an error for the same
reason.

That strictness buys something: because a literal carries no constraint, nothing in the
language defaults. There is no fallback rule for a reader to remember, and no class the
compiler has to know by name. A literal that stood instead for a value in any type with a
`Number` instance would put a conversion member on every such instance, and a call to it under
every literal in every program — machinery, and invisible work at runtime, spread across the
whole language.

The cost lands in one place: inside a function constrained over a numeric class, a literal is
already concrete, so it cannot be used at the constrained type:

```zel expect=unimplemented
module Example exposing (double)

double : Number a => a -> a
double x =
  mul x 2
```

**Not implemented:** constraints do not parse ([`CLASS-1`](../tickets/class-1.md)). Once they
do, that declaration is an error: `2` is an `Int`, so `mul x 2` forces `a` to be `Int` and the
annotation promises more than the body supports. `double x = add x x` is the way to write it,
and a class that wants numeric constants declares them as members.

**Known gap:** the type checker gives an integer literal an internal type that unifies with
`Int` *and* `Float`, so a declaration annotated `Float` with a body of `1` is accepted today.
[`CLASS-5`](../tickets/class-5.md) is the ticket. No block here holds it to account: the spec
harness stops at canonicalization and never runs the type checker
([`TEST-2`](../tickets/test-2.md)).

### There is no boolean literal

`Bool` is an ordinary union type and `True` and `False` are its constructors, resolved and
shadowed like any other name — see [Reserved words](lexical-structure.md#reserved-words).

### String literals

```zel expect=unimplemented
module Example exposing (greeting)

greeting = "hello"
```

**Not implemented:** `"` is not a token the tokenizer knows.

## Names

A **variable** is a lowercase-initial name, optionally qualified by a module. A
**constructor** is an uppercase-initial one, likewise qualifiable. Which module a name is
looked up in, and what makes one ambiguous, is *Name resolution and scoping*' subject; this
chapter needs only that both are expressions.

```zel expect=ok
module Example exposing (Shape, area, corners)

type Shape
  = Square
  | Circle

area shape =
  shape

corners = Square
```

A constructor is an ordinary function of its arguments, so it applies and partially applies
like one:

```zel expect=ok
module Example exposing (Box, wrap, boxed)

type Box a
  = Box a

boxed = Box 1

wrap = Box
```

`wrap` here is the constructor itself, not applied to anything — a function from `a` to
`Box a`.

## Application

A function is applied to an argument by writing them next to each other. Application is
**left-associative**, so `f x y` is `(f x) y`: `f` is applied to `x`, and the result is
applied to `y`. Every function takes exactly one argument and a multi-argument function is one
that returns a function, which is why partial application needs no syntax of its own.

```zel expect=ok
module Example exposing (apply, twice)

apply f x =
  f x

twice f x =
  f (f x)
```

**Application binds tighter than every other form.** An argument is therefore an *atomic*
expression — a literal, a name, a parenthesised expression, or a tuple — and anything else has
to be parenthesised. `f (f x)` above is not decoration: `f f x` would be `(f f) x`.

The same rule is why `if` and `case` need parentheses in argument position. `g if c then a
else b` cannot be read as applying `g` to a conditional, because `g` claims `if` as its
argument before the conditional is assembled, and `if` is not an atomic expression:

```zel expect=parse-error:UnexpectedToken
module Example exposing (Flag, f)

type Flag
  = On
  | Off

f g c = g if c then On else Off
```

```zel expect=ok
module Example exposing (Flag, f)

type Flag
  = On
  | Off

f g c = g (if c then On else Off)
```

Nothing in the grammar restricts what may be applied. Applying a value that is not a function
is a type error rather than a syntax error:

```zel expect=ok
module Example exposing (f)

f = 1 2
```

## Grouping and tuples

Parentheses around a single expression **group**: `(a)` is `a`, and no node exists for the
parentheses. Parentheses around two or three comma-separated expressions build a **tuple**.

```zel expect=ok
module Example exposing (pair, triple, nested)

pair a b = (a, b)

triple a b c = (a, b, c)

nested a b = ((a, b), b)
```

A tuple has two or three elements only — see [Tuple types](types.md#tuple-types),
which carries the reasoning. Four elements is a *syntax* error, caught before
anything has to count them:

```zel expect=parse-error:UnexpectedToken
module Example exposing (f)

f a b = (a, b, a, b)
```

## Operators

### An operator is a name

An operator has no meaning of its own. It is a name, bound by an
[`infix` declaration](lexical-structure.md#operators) to an ordinary function, and `a + b` is
that function applied to `a` and `b`. Nothing about arithmetic, comparison or composition is
built into the language: `std/core` declares every operator it offers in exactly the way a
program declares its own.

```zel expect=ok
module Example exposing ((+), add, sum)

infix left 6 (+) = add

add a b =
  a

sum a b c =
  a + b + c
```

An operator with no `infix` declaration in scope is an unresolved name, and the diagnostic
says so under the operator itself:

```zel expect=canonical-error:VariableNotFound
module Example exposing (f)

f a b =
  a + b
```

### Naming an operator

An operator wrapped in parentheses is an expression denoting the function it is bound to. It
is the same spelling an `infix` declaration and an [`exposing`](modules.md) list use, so a
module that exports `(+)` exports something an expression can name.

```zel expect=unimplemented
module Example exposing ((+), add, plus)

infix left 6 (+) = add

add a b =
  a

plus = (+)
```

**Not implemented:** `(+)` in expression position is a parse error, so an exported operator
can be used infix and in no other way — it cannot be passed to a higher-order function at all.
[`LANG-23`](../tickets/lang-23.md) is the ticket.

### Precedence and associativity

Each `infix` declaration gives its operator a **precedence** and an **associativity**, and
those two decide how an expression holding more than one operator groups. A higher precedence
binds tighter, so `a * b + c` is `(a * b) + c` when `*` is declared above `+`. Among operators
of equal precedence, `left` groups leftward and `right` groups rightward: `a - b - c` is
`(a - b) - c`, and `a ++ b ++ c` is `a ++ (b ++ c)`.

Grouping is decided by these declarations alone. It does not depend on spacing, on the order
the declarations appear in, or on anything the operators are bound to.

```zel expect=ok
module Example exposing ((+), (*), add, mul, poly)

infix left 6 (+) = add

infix left 7 (*) = mul

add a b =
  a

mul a b =
  a

poly a b c =
  a * b + c
```

**Known gap:** precedence and associativity are recorded and then ignored. Every operator
application groups rightward regardless of what was declared, so `poly` above is compiled as
`a * (b + c)`. [`BUG-22`](../tickets/bug-22.md) is the ticket. That block passes either way —
grouping changes which value an expression has, and the spec harness stops before the phase
that could tell ([`TEST-2`](../tickets/test-2.md)) — so it pins the syntax only.

### Equal precedence, disagreeing associativity

Two operators of the same precedence whose associativities disagree have no unambiguous
grouping, and an expression that mixes them without parentheses is a syntax error. `non` is
the case worth naming: an operator declared `non` does not chain with itself at all.

```zel expect=ok
module Example exposing ((==), eq, chain)

infix non 4 (==) = eq

eq a b =
  a

chain a b c =
  a == b == c
```

**Known gap:** that declaration should be rejected, and today it is accepted as
`a == (b == c)`. It is the one consequence of [`BUG-22`](../tickets/bug-22.md) a block can
hold to account: fixing precedence makes this a parse error, and `expect=ok` goes red.

Requiring the parentheses is what makes `non` mean something. Falling back to left-grouping
would let `a == b == c` compile as `(a == b) == c` — comparing a boolean against `c` — which
is a reading nobody intends and which the spelling gives no hint of.

### Prefix negation

A `-` with no left operand available is **prefix negation** rather than subtraction. That is
one rule and it has no exceptions, which means it reads off the text alone and never off the
spacing.

At the start of an expression there is no left operand, so `-` negates:

```zel expect=ok
module Example exposing ((-), sub, opposite)

infix left 6 (-) = sub

sub a b =
  a

opposite n =
  -n
```

Directly after a binary operator there is no left operand either, so `-` negates there too:

```zel expect=parse-error:UnexpectedToken
module Example exposing ((-), (+), sub, add, f)

infix left 6 (-) = sub

infix left 6 (+) = add

sub a b =
  a

add a b =
  a

f a b =
  a - -b
```

**Known gap:** that should be `expect=ok` — `a - -b` is `a` minus the negation of `b`. The
right operand of an operator may not be a negation today, and `a - (-b)` is the only way to
write it. [`LANG-22`](../tickets/lang-22.md) is the ticket.

After an expression there *is* a left operand, so `-` is subtraction — including in what looks
like argument position. `g -n` is `g - n`, not `g` applied to the negation of `n`:

```zel expect=ok
module Example exposing ((-), sub, f)

infix left 6 (-) = sub

sub a b =
  a

f g n =
  g -n
```

The trap is real: reading `g -n` as an application would make meaning turn on spacing alone,
which the rule above never does. Write `g (-n)` when an argument is what is meant.

There is likewise no negative literal: `-1` is negation applied to the literal `1`. A
[pattern](patterns.md#literal-patterns) carries its sign on the literal instead, because
pattern syntax is closed and never looks an operator up — so the same three characters mean
two different things in the two positions, deliberately.

## `if … then … else`

`if c then a else b` evaluates `c`, which must be a `Bool`, and has the value of `a` or of
`b`. **The `else` is not optional** — an `if` with no value for one of its cases could not be
an expression.

```zel expect=parse-error:UnexpectedToken
module Example exposing (Flag, f)

type Flag
  = On
  | Off

f c = if c then On
```

Both arms are ordinary expressions, so an `else if` chain is nesting rather than a form of its
own, and needs no separate rule:

```zel expect=ok
module Example exposing (Flag, classify)

type Flag
  = On
  | Off

classify a b =
  if a then
    On
  else if b then
    Off
  else
    On
```

An `if` may appear wherever an expression may, subject to the
[application](#application) rule above: parenthesised in argument position, bare elsewhere.

```zel expect=ok
module Example exposing (Flag, f, g)

type Flag
  = On
  | Off

f c = (if c then On else Off, Off)

g c =
  case if c then On else Off of
    On ->
      Off

    Off ->
      On
```

Extending rightward is the whole of the rule. An `if` used as an operator's right operand
swallows everything after it, so `1 + if c then a else b` is `1 + (if c then a else b)` and
never anything else — and, by the same rule, an `if` on the *left* of an operator swallows the
operator too. `if c then 1 else 2 + 3` is `if c then 1 else (2 + 3)`, so parentheses are the
only way to make an `if` a left operand.

```zel expect=parse-error:UnexpectedToken
module Example exposing ((+), add, f)

infix left 6 (+) = add

add a b =
  a

f c =
  1 + if c then 2 else 3
```

**Known gap:** that should be `expect=ok`. An operator's right operand may not be an `if`
today. [`LANG-22`](../tickets/lang-22.md) is the ticket.

## `case … of`

`case v of` matches `v` against a list of branches, each a pattern, `->`, and an expression.
The branches are tried in the order written and the first match wins; between them they must
cover the type of `v`. Every branch has the same type, which is the type of the whole
expression.

What a pattern may be, and what binding one means, is [Patterns](patterns.md)' subject. Where
the branches must sit on the page is [Layout](layout.md#case--of)'s.

```zel expect=ok
module Example exposing (Flag, invert)

type Flag
  = On
  | Off

invert flag =
  case flag of
    On ->
      Off

    Off ->
      On
```

A `case` has **at least one branch**. A `case` with none could not have a value:

```zel expect=parse-error:UnexpectedToken
module Example exposing (f)

f v =
  case v of
```

A `case` is an expression like any other, so it nests in both positions — as the scrutinee of
another `case`, and as a branch body:

```zel expect=ok
module Example exposing (Flag, both)

type Flag
  = On
  | Off

both a b =
  case a of
    On ->
      case b of
        On ->
          On

        Off ->
          Off

    Off ->
      Off
```

The same is true of parenthesisation, so a `case` may also appear as an argument, a tuple
element, or an operand:

```zel expect=parse-error:UnexpectedToken
module Example exposing (Flag, f)

type Flag
  = On
  | Off

f g v =
  g (case v of
    On ->
      Off

    Off ->
      On)
```

**Known gap:** that should be `expect=ok`. A `case` cannot be parenthesised at all today, in
any position — the closing `)` arrives while the branch block is still open — so a `case` is
usable only as a whole declaration body, a whole `if` arm, or a whole branch body. It cannot be
an argument, a tuple element, an operator operand, or another `case`'s scrutinee.
[`LANG-21`](../tickets/lang-21.md) is the ticket.

A `case` in the `then` arm of an `if` ends where the `else` begins, the same as any other
block that a following keyword closes:

```zel expect=parse-error:LayoutError
module Example exposing (Flag, f)

type Flag
  = On
  | Off

f c v =
  if c then
    case v of
      On ->
        Off

      Off ->
        On
  else
    On
```

**Known gap:** that should be `expect=ok`. The `else` does not close the branch block, so a
`case` in a `then` arm is followed by a layout error whatever the `else` is indented to — which
makes the `then` arm the one position a `case` cannot occupy even though the `else` arm can.
[`BUG-23`](../tickets/bug-23.md) is the ticket.

## `let … in`

`let bindings in expression` introduces names visible in the bindings themselves and in the
expression after `in`. The value of the whole is the value of that expression.

```zel expect=unimplemented
module Example exposing (f)

f n =
  let
    double x =
      mul 2 x

    (lo, hi) =
      bounds n
  in
  double (add lo hi)
```

**Not implemented:** `let` and `in` are reserved words with no production behind them, and
everything below is design intent. Where the bindings sit on the page is
[Layout](layout.md#let--in)'s subject, with two open questions of its own. A `let` binding
takes any form a module's declarations take — a [type annotation](types.md#type-annotations),
a value binding, a function binding with parameters, and a
[destructuring pattern](patterns.md#where-a-pattern-may-appear), which must be irrefutable
since there is nowhere for a failed match to go. The bindings of one `let` are **mutually
recursive**: every one of them is in scope in every other's body, and in the expression after
`in`, so the order they are written in does not matter and two functions may call each other.

A `let` binds nothing outside itself. It is the only construct that introduces a name that is
not a declaration or a pattern.

## Lambdas

`\patterns -> expression` is an anonymous function. It takes one or more parameters, and
`\x y -> e` means the same as `\x -> \y -> e`, matching the way
[application](#application) and the [function arrow](types.md#the-function-arrow) already
associate.

```zel expect=unimplemented
module Example exposing (f)

f =
  \x y -> add x y
```

**Not implemented:** there is no lambda production — `\` is an ordinary operator character, so
`\x -> x` is rejected as an operator in a position where an expression was wanted. Its
parameters are meant to be the patterns a function declaration's parameters accept, for the
same reason: a lambda has one body, so a pattern that can fail has nowhere to fall through to.
Only [irrefutable patterns](patterns.md#a-pattern-that-can-fail-and-one-that-cannot) may
appear.

```zel expect=unimplemented
module Example exposing (Point, f)

type Point a
  = Point a a

f =
  \(Point x y) -> add x y
```

A lambda's body extends as far to the right as it can, so `\x -> f x y` is `\x -> (f x y)` and
a lambda used as an argument is parenthesised like any other non-atomic expression. A lambda can
not span multiple lines, and a dedicated function must be used instead.

## Forms the compiler does not have

Three expression forms are part of the language and are specified in their own chapters:

| Form | Where it is specified |
|---|---|
| List literals — `[1, 2]` | [Lexical structure](lexical-structure.md#punctuation) carries the brackets; the list chapter is unwritten |
| Records — `{ a = 1 }`, `{ r \| a = 2 }` | likewise, for the braces |
| Field access — `r.name`, `.name` | with records |

```zel expect=unimplemented
module Example exposing (f)

f r =
  r.name
```

**Not implemented:** `.` is punctuation for qualified names only, so `r.name` is rejected
rather than read as a projection.
