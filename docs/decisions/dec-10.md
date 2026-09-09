# DEC-10 · The law on a derivation's `combine` is checked by nothing, permanently

**Settled:** 2026-09-08, by the language owner (`SPEC-27`).
**Status:** live.
**Where the rule lives:**
[Type classes — What a derivation is trusted to keep](../spec/type-classes.md#what-a-derivation-is-trusted-to-keep).

A class that carries a derivation supplies the bindings the compiler folds a constructor's
arguments with, and that fold means what the chapter says it means only under a law the chapter
states — [What a derivation is trusted to
keep](../spec/type-classes.md#what-a-derivation-is-trusted-to-keep) is the normative text and
nothing here is. What that section left open is whether the compiler could ever establish the law
itself. **It will not, and no work is filed towards it**: the law is prose, a derivation that
breaks it compiles, and the class author is the one who keeps it.

The question was worth asking rather than waving off, for two reasons peculiar to derivation.
The failure is silent and its cost lands on someone other than its author — a derivation is
written once, in a class declaration, and every `derived` instance inherits it, so a wrong answer
surfaces in code that did nothing but ask for the obvious definition. And a derivation is a
compile-time construct: the compiler is *writing* the fold, so this is one of the few places
where it could in principle inspect the thing it is trusting.

## What the compiler has to work with

Three facts about the tree bound every candidate below, and none of them is a matter of effort:

- **There is no evaluator.** `src/compiler/` ends at the typer, `exhaustiveness.rs` is a stub,
  and code generation has not started ([`GEN-1`](../tickets/gen-1.md)). Nothing runs a Zelkova
  expression, at compile time or at any other time.
- **There is no test root, and nothing runs a package's tests** ([`LANG-15`](../tickets/lang-15.md)).
- **The typer is Hindley–Milner** over `Term`/`Constraint`. It has a vocabulary for the type of a
  function and none for a property of its values.

## A shape restriction on the bindings

The one candidate needing no machinery: require `combine` to be a `case` on one of its arguments
whose branches return the other argument or a constant. `Eq`'s and `Comparable`'s derivations
pass, and the averaging derivation — the breach the chapter works through first — is rejected at
the class declaration.

It loses on a program the chapter itself offers. Counting rather than averaging (`matched = 0`,
`differed _ _ = 1`, `combine = add`) is a monoid, is the fix that section recommends to an author
who wrote the averaging one, and is not a `case` on anything. A syntactic rule that rejects the
correct program next to the incorrect one teaches an author to write the shape the checker
recognises instead of the law, which is worse than teaching nothing. The deeper objection is that
such a rule never establishes associativity at all — it recognises one syntactic family whose
members happen to have it, and the accuracy it appears to offer is an accident of which family
was picked.

## Exhaustive evaluation where the answer type is finite

The candidate that would actually decide the law, over the answer types where deciding it is
finite work. A two-value derivation's member is at `a -> a -> R` with the class variable absent
from `R` ([which signatures may carry
one](../spec/type-classes.md#a-class-says-how-it-is-derived)), so where `R` is a union of nullary
constructors its values enumerate — `Bool` has two, `Order` three — and the three equations
become a fixed, small number of closed evaluations. That is exactly the two classes `std/core`
derives ([`LANG-42`](../tickets/lang-42.md)), and it reaches nothing whose `R` is `Int`, `Float`,
or any type taking arguments.

Two things sink it. It needs an evaluator for whatever fragment of the language a derivation's
bindings may use, which is `GEN-1`'s first job pulled forward and then kept in step with the
backend forever after — two implementations of evaluation that have to agree, bought for three
equations. And the coverage is the wrong shape: the derivations whose authors most need the
warning are the numeric ones — a hash, a score, a proportion, the two breaches the chapter
works through — and every one of them has an `R` this check cannot see. A check that is silent
over `Int` and green over `Bool` reads as a guarantee about derivations in general while covering
none of the cases that motivated it, so the partial coverage is worse than none.

## A test obligation on the declaring package

Generate the equations as a test of the package declaring the class and let a test run fail:
later than compile time, earlier than the program, and in principle good for any `R` whose values
can be produced. It is gated on `LANG-15`, which does not exist, and on obtaining values of an
arbitrary `R` — which is the walk run backwards from a description of the type, the thing [the
chapter rules out for a member taking no
`a`](../spec/type-classes.md#a-class-says-how-it-is-derived) and does not intend to grow. Without
it the candidate reaches only the `R`s that enumerate, so it is the previous candidate arriving
later, through a runner that has to be built first.

## What is left to the class author

The baseline the other three had to beat is what every language with a `Monoid`-shaped
abstraction does, including the one this design is closest to: Haskell's derived `Ord` leans on
`Ordering`'s monoid without naming it, as [DEC-1](dec-1.md#what-a-lexicographic-fold-gets-wrong)
records. Zelkova states the law in the chapter, which is already more than most, and stops there.

The refusal is the same one the rest of the mechanism is built on. The compiler writes the fold
and knows nothing about what a class's answers mean — that is what keeps the three bindings
ordinary functions over `R`, what lets them stand for every deriving type at once, and what makes
the mechanism fit a language with no kinds ([DEC-1](dec-1.md#where-zelkova-sits)). A checker for
the law would be the first part of the compiler to read a derivation for meaning rather than for
type, and it would buy, at best, `Bool` and `Order`.

One open question in the chapter touches this and does not reopen it. An n-ary `combine :
List R -> R`, writable once lists exist, would hand the fold to the class and retire the law
rather than check it ([What lists add](../spec/type-classes.md#open-questions),
[DEC-7](dec-7.md) decision 6). That is a question about the shape of `combine`, and its answer
does not depend on this one.
