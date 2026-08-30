# Patterns

A pattern is a shape written where a value will arrive. Matching a value against a pattern
asks one question and answers two: does the value have this shape, and if it does, what are
the names for its parts. `Circle n` matches a value built by `Circle` and binds `n` to what it
was built from; `_` matches anything and binds nothing.

Pattern syntax is **closed**: the forms below are all of them, and no declaration adds
another. That is the deep difference between a pattern and an expression: an expression is
made of names, and an operator in one is a name someone bound with an `infix` declaration, so
what `a + b` means depends on what is in scope. A pattern looks up exactly one kind of name —
a type constructor — and everything else about it is fixed by the grammar. A reader can
therefore tell what a pattern matches by looking at it, without knowing which module it is in.

## The forms

| Form | Written | Matches |
|---|---|---|
| wildcard | `_` | any value, binding nothing |
| variable | `count` | any value, binding it under that name |
| literal | `0`, `1.5`, `'x'` | a value equal to that literal |
| tuple | `(a, b)`, `(a, b, c)` | a tuple of that arity, each element against its own pattern |
| constructor | `Dot`, `Circle n`, `S.Dot` | a value built by that constructor, each argument against its own pattern |

```zel expect=ok
module Example exposing (Count, Shape, describe)

type Count
  = One
  | Many

type Shape
  = Dot
  | Circle Count

describe : Shape -> Count
describe shape =
  case shape of
    Dot ->
      One

    Circle n ->
      n
```

## Where a pattern may appear

A pattern appears in two places: as a parameter in a function declaration's head, and as the
left-hand side of a `case` branch.

```zel expect=ok
module Example exposing (Flag, Pair, first, invert)

type Flag
  = On
  | Off

type Pair
  = Pair Flag Flag

first : Pair -> Flag
first (Pair a b) =
  a

invert : Flag -> Flag
invert flag =
  case flag of
    On ->
      Off

    Off ->
      On
```

**Not implemented:** a pattern will also stand on the left of a `let` binding and as a
lambda's parameter. Neither construct exists yet; see [Layout](layout.md#let--in).

## A pattern that can fail, and one that cannot

A pattern is **irrefutable** when every value of the type it is written against matches it,
and **refutable** otherwise. The wildcard and a variable are always irrefutable. A tuple
pattern is irrefutable exactly when all of its elements are. A literal pattern is always
refutable. A constructor pattern is irrefutable only when its type has that one constructor
and its arguments are themselves irrefutable.

Both positions accept both kinds. The language requires the patterns in a position to
**cover** the type between them — a `case`
covers it with its branches, and a function declaration covers it with its clauses:

```zel expect=canonical-error:MultipleBindingsUnsupported
module Example exposing (Flag, invert)

type Flag
  = On
  | Off

invert : Flag -> Flag
invert On = Off
invert Off = On
```

**Not implemented:** a declaration may be written as several clauses, one per line, each with
its own patterns and its own body. The compiler parses them and then reports that a name
declared over more than one binding is not supported, so today a declaration has exactly one
clause and any pattern that can fail has to go in a `case`.

Coverage is what makes this safe, and it is checked rather than assumed:

```zel expect=ok
module Example exposing (Flag, ignore)

type Flag
  = On
  | Off

ignore : Flag -> Flag
ignore flag =
  case flag of
    On ->
      Off
```

**Known gap:** that block should be rejected — the `case` has no branch for `Off`, so
`ignore Off` has no value to produce. Nothing checks coverage today: the exhaustiveness phase
inspects nothing and accepts every module.
[`docs/tickets/lang-16.md`](../tickets/lang-16.md) is the ticket.

## The wildcard

`_` matches any value and binds nothing. It is the way to say that a position is deliberately
ignored, and it may be written as many times in one pattern as there are positions to ignore —
it introduces no name, so two of them cannot collide.

```zel expect=ok
module Example exposing (Flag, Triple, middle)

type Flag
  = On
  | Off

type Triple
  = Triple Flag Flag Flag

middle : Triple -> Flag
middle (Triple _ b _) =
  b
```

`_` is a token in its own right and not a letter, so `_count` is *two* tokens — a wildcard
followed by a variable — rather than one name. There is no convention by which a leading
underscore marks a binding as unused; write `_`. See
[Lexical structure](lexical-structure.md#the-underscore-is-not-a-letter).

## Variable patterns

A lowercase-initial identifier matches any value and binds it under that name, for the body it
belongs to and no further. A `case` branch's bindings are visible in that branch's expression
alone:

```zel expect=canonical-error:VariableNotFound
module Example exposing (Flag, f, g)

type Flag
  = On
  | Off

f : Flag -> Flag
f flag =
  case flag of
    inner ->
      inner

g : Flag
g =
  inner
```

A pattern variable shadows anything of the same name from an enclosing scope, including a
top-level declaration in the same module:

```zel expect=ok
module Example exposing (Flag, count, f)

type Flag
  = On
  | Off

count : Flag
count =
  On

f : Flag -> Flag
f count =
  count
```

## A pattern binds each name once

Every variable in one pattern position is distinct. Repeating a name is an error rather than a
demand that the two values be equal — a pattern describes a shape, and equality is a question
about values that a body asks with an operator.

```zel expect=ok
module Example exposing (Flag, Pair, same)

type Flag
  = On
  | Off

type Pair
  = Pair Flag Flag

same : Pair -> Flag
same (Pair a a) =
  a
```

**Known gap:** that block should be rejected. Canonicalization inserts each of a pattern's
variables into the branch's scope in turn, so the second `a` silently replaces the first and
the body sees the second field. [`docs/tickets/lang-15.md`](../tickets/lang-15.md) is the
ticket.

The rule spans a whole clause rather than one pattern, so `f a a = a` is an error too: the two
parameters are one binding position, and a body naming `a` would have no way to say which it
meant.

## Literal patterns

An integer, float or character literal matches a value equal to it. The literal is written
exactly as it is in an expression; see
[Lexical structure](lexical-structure.md#literals) for what each may contain.

```zel expect=ok
module Example exposing (Flag)

type Flag
  = On
  | Off

isVowel c =
  case c of
    'a' ->
      On

    'e' ->
      On

    _ ->
      Off
```

A literal pattern is refutable, so it needs a branch after it that covers the rest of the
type — which for a literal type means a wildcard or a variable, since no finite list of
literals covers one.

A pattern may hold a **negative** number. The sign belongs to the literal rather than being an
operator applied to it: pattern syntax is closed, and `-` in an expression is a name bound by
an `infix` declaration, which a pattern never looks up.

```zel expect=unimplemented
module Example exposing (Flag)

type Flag
  = On
  | Off

sign n =
  case n of
    -1 ->
      On

    _ ->
      Off
```

**Not implemented:** the grammar reaches for a pattern after the branch's opening and finds
the `-`, so a signed literal is a syntax error.

There is no boolean literal, and none is needed: `True` and `False` are ordinary constructors
of an ordinary union type, so a boolean pattern is a constructor pattern like any other. See
[Lexical structure](lexical-structure.md#reserved-words).

There is no string literal pattern, because there are no string literals; see
[Lexical structure](lexical-structure.md#strings).

## Tuple patterns

A tuple pattern has two or three elements, matching the tuple types the language has and no
others. Each element is a pattern of its own.

```zel expect=ok
module Example exposing (Flag, swap)

type Flag
  = On
  | Off

swap : (Flag, Flag) -> (Flag, Flag)
swap (a, b) =
  (b, a)
```

A four-element tuple pattern is a syntax error, not a type error, for the same reason a
four-element tuple type is: there is no such tuple to describe. See
[Types](types.md#tuple-types).

```zel expect=parse-error:UnexpectedToken
module Example exposing (Flag)

type Flag
  = On
  | Off

first (a, b, c, d) =
  a
```

Parentheses with one pattern inside are grouping and not a tuple of one — `(p)` is `p`.

## Constructor patterns

A constructor pattern names a type constructor and gives one pattern per argument it was
declared with. It matches a value built by that constructor, and matches each argument
against the pattern in that position.

Whether the constructor's arguments may be written without parentheses depends on the
position, and the rule follows from what juxtaposition means there. In a `case` branch the
whole left-hand side is one pattern, so a constructor and its arguments read as one thing:

```zel expect=ok
module Example exposing (Count, Shape, width)

type Count
  = One
  | Many

type Shape
  = Dot
  | Rect Count Count

width : Shape -> Count
width shape =
  case shape of
    Rect w h ->
      w

    Dot ->
      One
```

In a function declaration's head, juxtaposition already separates one parameter from the
next, so an applied constructor must be parenthesised:

```zel expect=ok
module Example exposing (Count, Boxed, unbox)

type Count
  = One
  | Many

type Boxed
  = Boxed Count

unbox : Boxed -> Count
unbox (Boxed n) =
  n
```

A constructor with no arguments needs no parentheses there, because there is nothing for
juxtaposition to be ambiguous about:

```zel expect=ok
module Example exposing (Unit, identity)

type Unit
  = Unit

identity : Unit -> Unit
identity Unit =
  Unit
```

Dropping the parentheses is not a mistake the grammar can catch, and that is why the rule is
worth knowing rather than merely obeying. A head is a sequence of patterns, so `width Dot n`
below is a well-formed head of **two** parameters — a nullary constructor pattern and a
variable. What rejects it is the annotation, which promises one:

```zel expect=canonical-error:BindingPatternsInvalidLen
module Example exposing (Count, Shape, width)

type Count
  = One
  | Many

type Shape
  = Dot
  | Rect Count Count

width : Shape -> Count
width Dot n =
  One
```

See [Types](types.md#the-annotation-and-the-declarations-parameters). A declaration with no
annotation has nothing to disagree with, so it takes however many parameters its head names.

### The arguments must be the ones it was declared with

A constructor pattern must supplies exactly as many argument patterns as the `type` declaration
gave that constructor.

```zel expect=ok
module Example exposing (Count, Shape, width)

type Count
  = One
  | Many

type Shape
  = Dot
  | Rect Count Count

width : Shape -> Count
width shape =
  case shape of
    Rect w ->
      w

    Dot ->
      One
```

**Known gap:** that block should be rejected — `Rect` has two arguments and the pattern gives
one. No phase checks: canonicalization resolves the constructor without counting, and the type
checker pairs argument patterns with declared parameter types by zipping the two lists, which
stops at the shorter. A pattern with too many arguments is accepted the same way, and the
extra ones bind nothing. [`docs/tickets/lang-14.md`](../tickets/lang-14.md) is the ticket.

### Qualified constructors

A constructor pattern may name its constructor qualified, under the module's name or under
the alias the `import` gave it. The rule is the one every name follows; see
[Modules](modules.md#imports).

```zel expect=ok package=qualified
module Shape exposing (Count(..), Shape(..))

type Count
  = One
  | Many

type Shape
  = Dot
  | Circle Count
```

```zel expect=ok package=qualified
module Main exposing (describe)

import Shape as S

describe : S.Shape -> S.Count
describe shape =
  case shape of
    S.Circle n ->
      n

    S.Dot ->
      S.One
```

A constructor that is not in scope is reported as such, with the caret under the name:

```zel expect=canonical-error:VariantNotFound
module Example exposing (Flag, f)

type Flag
  = On
  | Off

f : Flag -> Flag
f flag =
  case flag of
    Onn ->
      Off

    _ ->
      On
```

## Patterns nest

Every pattern position takes a whole pattern, so patterns nest to any depth: a constructor
argument may be a tuple, a tuple element may be a constructor pattern, and a constructor
argument may be another constructor pattern. An applied constructor written as a sub-pattern
is parenthesised.

**Known gap:** a constructor pattern may not appear inside another pattern at all, and a
parenthesised one may not head a `case` branch. The grammar has one production for
sub-patterns and it has no constructor alternative, which also means the parenthesised form
does not admit one. [`docs/tickets/lang-13.md`](../tickets/lang-13.md) is the ticket, and each
of the three blocks below is an `expect=ok` once it lands.

A nullary constructor as a tuple element:

```zel expect=parse-error:UnexpectedToken
module Example exposing (Flag, both)

type Flag
  = On
  | Off

both : (Flag, Flag) -> Flag
both pair =
  case pair of
    (On, On) ->
      On

    _ ->
      Off
```

An applied constructor as a constructor's argument:

```zel expect=parse-error:UnexpectedToken
module Example exposing (Count, Shape, Wrapper, inner)

type Count
  = One
  | Many

type Shape
  = Dot
  | Circle Count

type Wrapper
  = Wrapper Shape

inner : Wrapper -> Count
inner w =
  case w of
    Wrapper (Circle n) ->
      n

    _ ->
      One
```

A parenthesised constructor pattern at the head of a `case` branch:

```zel expect=parse-error:UnexpectedToken
module Example exposing (Count, Shape, describe)

type Count
  = One
  | Many

type Shape
  = Dot
  | Circle Count

describe : Shape -> Count
describe shape =
  case shape of
    (Circle n) ->
      n

    Dot ->
      One
```

## As-patterns

A pattern may be given a name for the whole value it matched, written `pattern as name`. The
sub-patterns bind their parts as usual and the name binds what they were taken from, so a
branch can inspect a value and pass it on without rebuilding it.

`as` binds more loosely than everything else in a pattern: in `Rect w h as whole`, `whole`
names the entire `Rect`, not `h`. An as-pattern written as a sub-pattern is parenthesised,
which is what keeps that rule readable.

```zel expect=unimplemented
module Example exposing (Count, Shape, widen)

type Count
  = One
  | Many

type Shape
  = Dot
  | Rect Count Count

widen : Shape -> Shape
widen shape =
  case shape of
    Rect w h as whole ->
      whole

    Dot ->
      Dot
```

**Not implemented:** `as` is a reserved word but the grammar accepts it only in an `import`,
so a pattern using it is a syntax error.

## List patterns

A list pattern is a bracketed sequence of patterns and matches a list of exactly that length,
element by element. `[]` matches the empty list, `[a]` a list of one, `[a, b]` a list of two.

There is no head-and-tail pattern. Taking a list apart by its first element is an operation on
lists rather than a shape a list has, so it is a function that `List` provides and not syntax
that every pattern position has to carry.

A list pattern is therefore always refutable: no finite set of lengths covers a list, so a
`case` over one needs a variable or a wildcard branch to be complete.

```zel expect=unimplemented
module Example exposing (Flag)

type Flag
  = On
  | Off

firstOf xs =
  case xs of
    [] ->
      Off

    [only] ->
      only

    _ ->
      On
```

**Not implemented:** lists are not implemented in any position. Brackets are tokenized but no
construct consumes them; see [Lexical structure](lexical-structure.md#punctuation).

## The unit pattern

`()` matches the one value of the unit type, and binds nothing. It differs from `_` in the
same position by saying which type is being ignored.

```zel expect=unimplemented
module Example exposing (Flag, always)

type Flag
  = On
  | Off

always : () -> Flag
always () =
  On
```

**Not implemented:** `()` is not recognised in a pattern, a type or an expression; see
[Types](types.md#the-unit-type).

## Record patterns

A record pattern names fields rather than positions, and is specified together with the record
syntax it takes apart — see [Types](types.md#the-forms-of-a-type-expression), which draws the
same line for record types.
