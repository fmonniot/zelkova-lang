# Records

A record is a value made of **named fields**. A [tuple](types.md#tuple-types) says which part is
which by position; a record says it by label, so a value of several parts stays readable where it
is taken apart, and past the three elements a tuple stops at.

A record has a spelling in all three grammars — a [type](types.md), an
[expression](expressions.md) and a [pattern](patterns.md). A [list](lists.md) is an ordinary
union type behind bracket syntax; a record is a form of its own in each of the three, and no
`type` declaration anywhere describes one.

## The type

A record type is a brace-enclosed, comma-separated list of `label : Type` fields.

```zel expect=ok
module Example exposing (Celsius, reading)

type Celsius
  = Celsius

reading : { taken : Celsius, expected : Celsius }
reading =
  { taken = Celsius, expected = Celsius }
```

A label is a lowercase-initial identifier, spelled the way a
[value name](lexical-structure.md#identifiers) is. A field's type is any
[type expression](types.md#the-forms-of-a-type-expression), so a field may hold a function, a
tuple, a type variable or another record, and a record type may itself be written wherever a
type expression may.

Repeating a label in one record type is an error, reported at the repeated label:

```zel expect=canonical-error:RepeatedLabel
module Example exposing (Celsius, first)

type Celsius
  = Celsius

first : { taken : Celsius, taken : Celsius } -> Celsius
first reading =
  Celsius
```

A record has **at least one field**. `{}` is neither a type nor an expression: the
[unit type](types.md#the-unit-type) already names the type with a single value.

A record type is written out in full wherever it appears, so **a record type cannot contain
itself**. There is no name in `{ value : Int, next : … }` for the `next` field to be given. A
recursive shape goes through a `type` declaration, which introduces the name the recursion turns
on, and the record sits inside a variant:

```zel expect=ok
module Example exposing (Chain)

type Chain
  = End
  | Link { value : Chain, next : Chain }
```

## A record type is a set of fields

Two record types are the same type when they carry the **same labels with the same field types**.
Field order is not part of a record type, so the two annotations below name one type and either
declaration satisfies either annotation.

```zel expect=ok
module Example exposing (Celsius, swapped)

type Celsius
  = Celsius

taken : { low : Celsius, high : Celsius }
taken =
  { low = Celsius, high = Celsius }

swapped : { high : Celsius, low : Celsius }
swapped =
  taken
```

This makes a record type **structural**: it is described entirely by what it holds. Two modules
that separately annotate `{ x : Int, y : Int }` have written one type, and a value of it crosses
between them with no conversion and no shared declaration.

A [type alias](types.md#type-aliases) over a record therefore names a record type and introduces
nothing else — no new type, and no constructor function of its fields. `Point` below is
`{ x : Celsius, y : Celsius }` spelled shorter, in both directions.

```zel expect=unimplemented
module Example exposing (Celsius, Point, origin)

type Celsius
  = Celsius

type alias Point = { x : Celsius, y : Celsius }

origin : Point
origin =
  { x = Celsius, y = Celsius }
```

**Not implemented:** `type alias` is a syntax error before the braces are reached; see
[Types](types.md#type-aliases).

## Building a record

A record expression is a brace-enclosed, comma-separated list of `label = expression` fields, and
its type is the record type its labels and its values' types spell out. Every field of the type
is given a value.

```zel expect=ok
module Example exposing (Celsius)

type Celsius
  = Celsius

reading =
  { taken = Celsius, expected = Celsius }
```

Each field's value is an ordinary expression, so anything an expression may be a field may hold.
Repeating a label is an error here too, as it is in an [update](#updating-a-record):

```zel expect=canonical-error:RepeatedLabel
module Example exposing (Celsius)

type Celsius
  = Celsius

reading =
  { taken = Celsius, expected = Celsius, taken = Celsius }
```

A record may be written across several lines with the separator leading each line:

```zel expect=ok
module Example exposing (Celsius)

type Celsius
  = Celsius

reading =
  { taken = Celsius
  , expected = Celsius
  }
```

A trailing comma is an error, following the [variant list](types.md#a-variant-list-has-at-least-one-variant)
and the [list literal](lists.md#list-literals) rather than the
[`exposing` list](modules.md#the-exposing-list). The leading-comma layout above already buys what
a trailing comma would.

```zel expect=parse-error
module Example exposing (f)

f = { a = 1, }
```

## Reading a field

`r.name` is the value of `r`'s `name` field. The record's type must have that label, and the
expression's type is the field's.

```zel expect=ok
module Example exposing (Text, nameOf)

type Text
  = Text

nameOf : { name : Text } -> Text
nameOf person =
  person.name
```

Access binds tighter than [application](expressions.md#application), so `f r.name` is
`f (r.name)`. It chains left to right: `r.centre.x` is `(r.centre).x`.

## The accessor

`.name` on its own is a function that reads that field: `.name` is `\r -> r.name`, and it is an
expression wherever one may be written.

```zel expect=ok
module Example exposing (Text, nameOf)

type Text
  = Text

apply : ({ name : Text } -> Text) -> { name : Text } -> Text
apply f r =
  f r

nameOf : { name : Text } -> Text
nameOf person =
  apply .name person
```

`.name` names a field of *some* record, and
[a record type is written out in full](#records-are-closed), so there is no type an accessor can
stand for on its own. It is therefore typed **from where it is written**: the record type comes
from what the accessor is applied to, or from the annotation of the position it sits in. Where
nothing fixes that type, the accessor is an error naming itself, as
[every use of a record is](#a-use-does-not-decide-a-records-type).

### Whitespace before a `.` decides which form it is

A `.` written against the expression to its left is an **access** of that expression; a `.` with
whitespace before it, or one opening an expression, is an **accessor**, and it is written against
its label with no space after it.

The same rule governs the `.` of a [qualified name](name-resolution.md#namespaces): `Dict.get`
is one name, and `Dict .get` is `Dict` applied to an accessor. A qualified name takes no
whitespace on either side of its `.`, so `Widget . size` and `Widget. size` are not names at
all, and the block below is rejected with an error saying so. The module header and an
`import` read their name the same way, so `import Ui . Widget` is rejected as well.

```zel expect=parse-error:SpacedDot
module Example exposing ()

import Widget

f =
  Widget . size
```

## Updating a record

`{ r | label = expression, … }` is `r` with the named fields replaced. Every other field keeps
the value it had, and the result is a new record: [evaluation](evaluation-semantics.md) has no
mutation in it.

```zel expect=ok
module Example exposing (Celsius, correct)

type Celsius
  = Celsius

correct : { taken : Celsius, expected : Celsius } -> { taken : Celsius, expected : Celsius }
correct reading =
  { reading | taken = Celsius }
```

An update **has the type of the record it updates**. Each label it names must already be a field
of that type, and each value must have the type that field already has. An update can therefore
neither add a field, remove one, nor change one's type.

```zel expect=type-error:MissingField
module Example exposing (Celsius, added)

type Celsius
  = Celsius

added : { taken : Celsius } -> { taken : Celsius }
added reading =
  { reading | expected = Celsius }
```

The expression left of the `|` is an ordinary expression rather than a name, so
`{ f x | taken = Celsius }` updates whatever `f x` returns.

An update names each label once. Repeating one is an error, reported at the repeated label:

```zel expect=canonical-error:RepeatedLabel
module Example exposing (Celsius, correct)

type Celsius
  = Celsius

correct : { taken : Celsius } -> { taken : Celsius }
correct reading =
  { reading | taken = Celsius, taken = Celsius }
```

## Record patterns

A record pattern matches a record by naming fields. Each entry is `label = pattern`, and the
field's value is matched against that pattern:

```zel expect=ok
module Example exposing (Celsius, describe)

type Celsius
  = Celsius

describe : { taken : Celsius, expected : Celsius } -> Celsius
describe reading =
  case reading of
    { taken = Celsius, expected = e } ->
      e
```

`{ name }` is shorthand for `{ name = name }`: the field matched against a
[variable pattern](patterns.md#variable-patterns) of its own label.

A record pattern **names a subset**: the fields it does not mention are not matched and not
bound. It is therefore [refutable](patterns.md#a-pattern-that-can-fail-and-one-that-cannot)
exactly when one of its sub-patterns is, and a pattern of shorthand entries alone can never fail
— so one may be written where a pattern must not fail, as a parameter or in a `let` binding:

```zel expect=ok
module Example exposing (Text, nameOf)

type Text
  = Text

nameOf : { name : Text } -> Text
nameOf { name } =
  name
```

Sub-patterns are whole patterns, so record patterns [nest](patterns.md#patterns-nest) the way
every other form does, in both directions — a record pattern inside a constructor pattern, and a
constructor or record pattern inside a field.

```zel expect=ok
module Example exposing (Celsius, Reading, depth)

type Celsius
  = Celsius

type Reading
  = Reading { centre : { x : Celsius }, taken : Celsius }

depth : Reading -> Celsius
depth r =
  case r of
    Reading { centre = { x } } ->
      x
```

A record pattern has **one or more entries**, as a record has at least one field, and a trailing
comma is an error, as it is in a record expression. `{}` is therefore not a pattern, and neither
is `{ x, }`:

```zel expect=parse-error:UnexpectedToken
module Example exposing (f)

f {} = 1
```

```zel expect=parse-error:UnexpectedToken
module Example exposing (f)

f { x, } = 1
```

A record pattern names each label once. Repeating one is an error, whether either entry is
written `label = pattern` or as the shorthand:

```zel expect=canonical-error:RepeatedLabel
module Example exposing (Celsius, describe)

type Celsius
  = Celsius

describe : { taken : Celsius, expected : Celsius } -> Celsius
describe { taken, taken = t } =
  t
```

A record pattern says nothing about which record type it matches, since it names a subset of some
record's fields. The type comes from the value being matched, the same way an
[accessor's](#the-accessor) does, and a pattern naming a label the matched type does not have is
an error:

```zel expect=type-error:MissingField
module Example exposing (Celsius, describe)

type Celsius
  = Celsius

describe : { taken : Celsius } -> Celsius
describe { expected } =
  expected
```

So is a pattern matched against a value
[whose record type nothing supplies](#a-use-does-not-decide-a-records-type):

```zel expect=type-error:RecordTypeUnknown
module Example exposing (Text)

type Text
  = Text

nameOf { name } =
  name
```

## Labels are not values

A label is a name in a namespace of its own, the **sixth** of the ones
[name resolution](name-resolution.md#namespaces) lists, so a name in it never collides with a
name in another. Every record type that mentions a label puts it there: writing `{ x : Int }`
anywhere makes `x` a label, and two record types may both carry `x` without that being a clash.

Which record a label belongs to is decided by the type it is read against, never by what is in
scope. A label is never imported, exposed or shadowed — an `exposing` list holds declarations,
and no declaration ever introduced a label. A value and a field of one spelling have nothing to
do with each other:

```zel expect=ok
module Example exposing (Celsius, x)

type Celsius
  = Celsius

x : Celsius
x = Celsius

origin =
  { x = Celsius }
```

The shorthand pattern is where the two namespaces meet, in one direction only: `{ x }` reads the
label `x` and binds the **value** `x`, which then shadows an outer value of that name like any
other [pattern binding](patterns.md#variable-patterns).

## Records are closed

A record type names every field the record has. There is no way to write "any record with a
`name` field", and a function that wants one accepts the whole record type it is given.

```zel expect=ok
module Example exposing (Text, Number, greet)

type Text
  = Text

type Number
  = Number

greet : { name : Text, age : Number } -> Text
greet person =
  person.name
```

Opening records up means a variable standing for "the rest of the fields", a second kind of thing
a type variable may be: today [a type variable stands for a type](types.md#type-variables) and
nothing else, and solving for one would need the type checker to solve a second kind of equation
beside the one it solves for types.

A function loses by it the ability to be written once over records that differ. A function
wanting to serve many records takes the fields it needs as arguments.

## A use does not decide a record's type

A field access, an update, an accessor and a record pattern each name some of a record's fields,
and a record type names all of them. None of the four says which record type it is used at. The
type is supplied by something else in the declaration — its annotation, a record expression, the
result of a function whose type is known, a constructor's argument — and the form is checked
against it.

Where nothing supplies it, the form is an error. A record type is never worked out from the
fields a declaration happens to touch: that would read `person.name` as taking a record with
exactly one field.

```zel expect=type-error:RecordTypeUnknown
module Example exposing (Text)

type Text
  = Text

nameOf person =
  person.name
```

What supplies the type may be written anywhere in the declaration, before the form or after it.
A use of the declaration from another one does not: `nameOf` above is an error however it is
called.

## Records and derivation

A [derivation](type-classes.md#a-class-says-how-it-is-derived) folds the answers a value's parts
produce, and a record is walked with the bindings a class already supplies.

A record has exactly one shape, so the half of the walk that answers for a constructor is never
reached: there are no constructor positions to hand `differed` or `atConstructor`. What is left
is the argument half — each **field** in turn, through the instance belonging to that field's
type, folded with `combine`. A two-value derivation walks the fields in pairs and ends at
`matched`; a [one-value derivation](type-classes.md#a-derivation-over-one-value) walks them
singly and starts at the first field's answer, which every record has because it has
[at least one field](#the-type).

The fields are walked **in label order**, sorted by the label's characters, each compared by its
code point, and a label that another begins with comes first. A record type is a
set of fields with no order of its own, so the walk supplies one, and sorting is the only order
available that two spellings of one type agree on. Where
[reordering a union's variants changes what a derived member computes](type-classes.md#what-a-derived-instance-computes),
reordering a record's fields changes nothing — it did not produce a different type.

A record is walked because it has no instance to delegate to. A record type is
[not something an instance is declared for](type-classes.md#what-an-instance-is-declared-for),
so `instance Eq { x : Int }` is not writable. Each field's *value* still goes through its own
type's instance in the ordinary way.

[Structural equality](evaluation-semantics.md#what-structural-equality-computes) over two records
is that walk with `Eq`'s answers filled in: equal when every field is equal.
