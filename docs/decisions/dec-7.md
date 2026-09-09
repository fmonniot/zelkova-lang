# DEC-7 · Lists: six decisions

**Settled:** 2026-09-06, by the language owner (`SPEC-22`).
**Status:** live.
**Where the rule lives:** [Lists](../spec/lists.md), with the pattern half also stated in
[Patterns](../spec/patterns.md#list-patterns).

Lists were named by four chapters and specified by none. The six questions below are what a
chapter had to commit to before it could be written; each was open in the sense that the tree
answered it nowhere, and three of them had a tempting alternative that is not recoverable from
the rule that won.

## 1 — A list type is written `List a`, not `[a]`

An ordinary [type application](../spec/types.md#applying-a-type-to-arguments): an uppercase type
name and one argument, which [Types](../spec/types.md#the-forms-of-a-type-expression)' six forms
already cover.

`[a]` was the alternative, and it costs a seventh type form to save four characters. It also
leaves a question behind that `List a` does not have: whether the bare name `List` is still
writable, and what it means if it is — a type constructor a program can partially apply, or a
spelling that only ever appears applied. Bracket syntax stays where it earns its keep, on values
and patterns, where it replaces a constructor chain rather than a single application.

## 2 — `List` is an ordinary union type, exposed opaquely

`type List a = Nil | Cons a (List a)`, declared in `std/core`'s `List` module and exposed as
`List` rather than `List(..)`, so no program names either constructor.

Two alternatives lost. **Exposing the constructors** makes the bracket forms pure sugar with
nothing hidden, which is honest but gives every list two spellings and publishes the
representation as part of the interface. **A compiler-provided primitive** — the shape
`std/core` uses for `Int` and `Float` today, `type Int = Int -- NOTE: the compiler provides the
real implementation` — was the closest thing to a precedent in the tree, and it loses on
exhaustiveness. [Patterns](../spec/patterns.md#list-patterns) claims that `[]` and a cons pattern
cover the list type between them; with a real two-variant union that claim *is* ordinary
constructor coverage and the exhaustiveness phase needs to know nothing about lists, whereas a
primitive would need list coverage written as a special case beside it.

The cost is a real one and worth stating: the bracket forms are read against three names —
`List`, `Nil`, `Cons` — that the compiler holds. That is one step past holding `Int`, which it
already does ([`typer/mod.rs`](../../src/compiler/typer/mod.rs) matches the name literally to
type an integer literal), and it is the price of a literal meaning anything at all.

## 3 — `::` is an ordinary operator in an expression, and a pattern production in a pattern

`std/core` declares `infix right 5 (::) = cons` like any other operator, so `::` is a name that
is resolved, imported, shadowed, and writable as `(::)`. A module may bind the spelling to
something else. In a *pattern* it is a production of the pattern grammar and is never looked up.

This is [`DEC-5`](dec-5.md)'s answer to the same question one construct over, and `DEC-5` names
this case in its closing paragraph. The reserved-spelling alternative — `::` as punctuation
beside `->` and `|`, meaning cons everywhere regardless of scope — buys one thing, that the two
positions cannot disagree, and pays for it by removing `(::)` as a value that can be passed or
exposed. Pattern syntax is closed and has nowhere to look an operator up, so the split costs
nothing *inside* patterns; it is only visible to a program that deliberately rebinds the
spelling, and such a program has already said what it means.

The already-working half is what makes the asymmetry cheap rather than clever: `infix right 5
(::) = cons` parses and canonicalizes in the compiler as it stands, with no change at all.

## 4 — No trailing comma in a list literal

`[1, 2,]` is an error. Two rules already existed to follow and they disagree: the
[`exposing` list](../spec/modules.md#the-exposing-list) permits a trailing comma deliberately, so
that appending a name touches one line, and the
[variant list](../spec/types.md#a-variant-list-has-at-least-one-variant) declines it. Lists
follow the variant list, because the leading-comma layout a multi-line literal is written in
already buys what the `exposing` list's trailing comma buys.

## 5 — Prepending and splitting are constant time; the representation is otherwise unspecified

The language fixes one operational property of a list and nothing else about its runtime shape.
Every function written by recursion over a list assumes it, and `std/core`'s list API is written
that way throughout.

The alternative was considered specifically and rejected: opacity means no program can observe a
list's representation through constructors, so a backend could in principle compile `List` to a
JavaScript array. It would be a legal substitution *semantically* and a bad one, because `x ::
xs` and the cons pattern both become linear, and ordinary recursive list code becomes quadratic
with nothing in the language saying so. Zelkova uses the traditional cons/nil structure. An array
type, if one is ever wanted, is a separate type with its own guarantees rather than the same type
with different costs.

## 6 — The chapter specifies the type, the literal and cons; the functions are the library's

Mapping, folding, filtering and sorting are `std/core`'s `List` module and carry no syntax. This
is the split every chapter uses and it was not seriously in question, recorded here because the
chapter's brevity would otherwise read as an omission.

`combine : List R -> R` becoming writable is noted in
[Lists](../spec/lists.md#lists-and-derivation) and settles nothing: which shape a derivation's
`combine` takes stays [Type classes](../spec/type-classes.md#open-questions)' open question.
