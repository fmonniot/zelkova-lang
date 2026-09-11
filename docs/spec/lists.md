# Lists

A list is a sequence of values, all of one type, of any length. It has a spelling in all three of
the languages the earlier chapters describe — a [type](types.md), an
[expression](expressions.md) and a [pattern](patterns.md) — so those three chapters name the
forms and this one says what they mean.

Nothing about a list is built into the grammar as a new *kind* of thing. The type is an
ordinary application, the literal is sugar over two constructors, and prepending is an operator
someone declared. What the language adds is bracket syntax for building and matching a value
that could otherwise only be written constructor by constructor.

## The type

A list's type is written `List a`, where `a` is the type of its elements. `List` names a type
constructor taking one argument, so `List a` is a
[type application](types.md#applying-a-type-to-arguments) like `Maybe Colour`, covered by
[Types](types.md#the-forms-of-a-type-expression)' six forms. There is no bracket spelling for a
list *type*; `[a]` is not a type expression.

A list is **homogeneous**: `List a` is one type applied to one argument, so every element of a
list has the same type.

```zel expect=unimplemented
module Example exposing (mixed)

mixed =
  [1, 'a']
```

**Not implemented:** brackets are not consumed by any production, so that block is a syntax
error today rather than the type error it is ([`LANG-44`](../tickets/lang-44.md)). When lists
land it parses and canonicalizes, and the type checker is what rejects it, so this block
becomes `expect=type-error` rather than `expect=ok`.

## What a list is

A list is one of two things: **empty**, or a **first element and the rest**, where the rest is
itself a list. That is a two-variant union type and it is written as one:

```zel expect=unimplemented
module List exposing (List)

type List a
  = Nil
  | Cons a (List a)
```

**Not implemented:** a type argument must be a bare name, so the parenthesised `(List a)` is a
syntax error ([`LANG-9`](../tickets/lang-9.md)). That block is what goes green when it lands.

`Nil` and `Cons` are **not exposed**: a list is built and taken apart through the bracket forms
and the cons operator below and through nothing else, so no program names either constructor.
How a backend represents a list is therefore its own choice, and the only property the language
fixes is that **prepending an element to a list, and splitting a non-empty list into its first
element and the rest, each take constant time.** Every function written by recursion over a list
assumes exactly that.

`[`, `]` and the empty literal are read against `List`, `Nil` and `Cons` in the standard
library's `List` module, the way an integer literal is read against `Int`.

## List literals

A list literal is a bracketed, comma-separated sequence of expressions. `[]` is the empty list;
`[a]`, `[a, b]` and `[a, b, c]` are lists of one, two and three elements, and there is no upper
limit.

```zel expect=unimplemented
module Example exposing (empty, one, three)

empty =
  []

one =
  [1]

three =
  [1, 2, 3]
```

**Not implemented:** no production consumes a bracket ([`LANG-44`](../tickets/lang-44.md)).

Each element is an ordinary expression, so anything an expression may be an element may be. A
literal is exactly the nested `Cons` chain the elements spell out: `[1, 2]` is `1` prepended to
`2` prepended to the empty list, and `[]` is `Nil`.

`[]` has type `List a` for any `a`, and needs no annotation to have one. It is a polymorphic
value like any other, and the type it is used at is decided where it is used.

A literal may be written across several lines, with the separator leading each line — the
ordinary way to write a long one.

```zel expect=unimplemented
module Example exposing (primes)

primes =
  [ 2
  , 3
  , 5
  ]
```

**Not implemented:** the same missing production ([`LANG-44`](../tickets/lang-44.md)).

A trailing comma is an error: a list follows the same rule a
[variant list](types.md#a-variant-list-has-at-least-one-variant) does, and not the
[`exposing` list](modules.md#the-exposing-list)'s, whose trailing comma exists to make appending
a name touch one line. The leading-comma layout above already buys that here.

```zel expect=parse-error
module Example exposing (f)

f = [1, 2,]
```

**Not implemented:** that block is rejected today at the `[` rather than at the comma, because
no bracket is consumed at all, so it does not yet distinguish the rule it illustrates from the
missing production around it ([`LANG-44`](../tickets/lang-44.md)).

## The cons operator

`::` prepends one element to a list: `x :: xs` is the list whose first element is `x` and whose
rest is `xs`. It is an **ordinary operator** — a name, bound by an
[`infix` declaration](lexical-structure.md#operators) to an ordinary function, exactly like
`+` — and not punctuation. The standard library declares it `infix right 5`, so it is
right-associative and `a :: b :: xs` is `a :: (b :: xs)`, which is the only grouping that has a
type: the left operand of a `::` is an element and the right one is a list.

Being a name has three consequences. It is resolved, imported and shadowed like any other
operator ([Name resolution](name-resolution.md)). It can be written `(::)` to name the function
itself. And a module may bind the spelling to something else entirely, at which point `::` means
that instead:

```zel expect=ok
module Example exposing ((::), weird)

infix left 9 (::) = weird

weird a b =
  a
```

An operator with no `infix` declaration in scope is an unresolved name, and `::` is no different:

```zel expect=canonical-error:VariableNotFound
module Example exposing (f)

f x xs =
  x :: xs
```

`List` and `::` are an ordinary union and an ordinary operator over it, with the element type
left as a variable. Written out for one element type:

```zel expect=ok
module Example exposing (Flag, Chain, (::), link, chain)

type Flag
  = On
  | Off

type Chain
  = Empty
  | Link Flag Chain

infix right 5 (::) = link

link : Flag -> Chain -> Chain
link x xs =
  Link x xs

chain : Chain
chain =
  On :: Off :: Empty
```

## Lists in patterns

Both spellings have a pattern counterpart, and neither is the expression form reused.
[Pattern syntax is closed](patterns.md): a pattern never resolves an operator, so `::` in a
pattern cannot be the name the section above describes and is instead a production of the
**pattern grammar**, consumed there the way a leading `-` before a literal is. It separates a
first element from a rest the way `,` separates tuple elements, and it keeps that meaning
whatever a module has bound the operator to.

A **bracket pattern** matches a list of exactly its length, element by element: `[]` matches the
empty list, `[a, b]` a list of two. A **cons pattern**, `first :: rest`, matches a list of one
element or more. Both sides of a `::` are whole patterns, so `a :: b :: rest` matches a list of
two or more and `Circle n :: rest` matches on the first element's shape.

```zel expect=unimplemented
module Example exposing (f)

f xs =
  case xs of
    a :: b :: rest ->
      a

    [_] ->
      xs

    [] ->
      xs
```

**Not implemented:** neither production exists ([`LANG-45`](../tickets/lang-45.md)).

Each is a constructor pattern: `[]` is `Nil`, `first :: rest` is `Cons first rest`, and `[a, b]`
is `Cons a (Cons b Nil)`. So `[]` and a cons pattern **cover** the list type between them and a
`case` written out of the two needs no wildcard: coverage here is the ordinary
[constructor coverage](patterns.md#a-pattern-that-can-fail-and-one-that-cannot) every union type
gets. A bracket pattern of fixed length is a `Cons` chain ending in `Nil`, so it is refutable
however long it is, and no finite set of lengths covers a list.

[Patterns](patterns.md#list-patterns) has the worked examples of both.

## What the standard library provides

The type, the two literal forms and cons are the whole of the language's part. The functions over
a list — mapping, folding, filtering, sorting — are the standard library's `List` module and carry
no syntax.

Neither `++` nor `==` over two lists is built in. `++` is an
[`infix` declaration](lexical-structure.md#operators) like any other, and `==` is the ordinary
[structural equality](evaluation-semantics.md) two constructors and their arguments already
define.

## Lists and derivation

A [derivation](type-classes.md#a-class-says-how-it-is-derived) folds a class's `combine` pairwise
over the answers a constructor's arguments produced, because a class has no way to be handed all
of them at once. With a list type there is one: `combine : List R -> R` is a writable member
shape, and a class declared that way receives every answer in one call.

That reaches the one property a derivation is trusted to keep and cannot check — that
[`combine` is associative with `matched` as its identity](type-classes.md#what-a-derivation-is-trusted-to-keep)
— because an n-ary `combine` has no associativity to assume: the fold becomes the class's to
perform rather than the walk's. Which of the two shapes a derivation's `combine` takes is
[an open question](type-classes.md#open-questions) of the mechanism itself.
