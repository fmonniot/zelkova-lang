# DEC-28 · How the scalar types are ordered: four decisions

**Settled:** 2026-10-06, by the language owner; decision 2 was made on the review of
[`LANG-42`](../tickets/README.md), which merged the day before, and is recorded here.
**Status:** live.
**Where the rule lives:**
[Evaluation semantics — Ordering](../spec/evaluation-semantics.md#ordering) for decisions 1 to
3, and [Types — Scalar types](../spec/types.md#scalar-types) with
[the `Char` and `String` rows of the admitted-type table](../spec/interop.md#which-types-may-cross-the-boundary)
for decision 4.

[`LANG-42`](../tickets/README.md) declared `Comparable` in `std/core` with instances for the
scalar types, and each instance had to answer something. [`SPEC-40`](../tickets/README.md) was
the ticket that had those answers ratified and written into a chapter. Decision 4 was not in
that ticket: it turned up while checking that "ordered by code point" was true of every value a
program can hold.

## 1 — A `Char` and a `String` are ordered by code point

A `String` is ordered lexicographically over its characters, a prefix first; nothing is
normalised and no locale takes part.

**Chosen over UTF-16 unit order**, which is what `<` on two JavaScript strings computes and so
what the JavaScript target gets for nothing. It places every character above U+FFFF (two units,
the first at most `0xDBFF`) before U+E000 to U+FFFF, so it disagrees with code point order
exactly there. Taking it would have made one target's string representation observable in the
language, and a WebAssembly target, whose strings are UTF-8, would have had to reproduce UTF-16
order on purpose. The lexical chapter already made a character literal one code point and
[*Records and derivation*](../spec/records.md#records-and-derivation) already sorted labels by
code point; this is the same rule reaching `compare`.

**Chosen over a collation** — locale-aware or the Unicode default one. A collation is data that
changes between Unicode versions and between hosts, so `compare` would stop being a function of
its two arguments. An ordering for people to read is a library's to offer under another name.

**Chosen over normalising first.** It would make `compare a b == EQ` for two strings `==` calls
different, or change `==` to match and make equality cost a normalisation.

## 2 — `compare` answers `GT` for a pair holding a `nan`

[*Numbers*](../spec/evaluation-semantics.md#numbers) had already made `<`, `<=`, `>` and `>=`
`False` against a `nan`, IEEE's answer. `Comparable` has one member, `compare`, with the four
operators written over it: `lt` and `le` read `compare a b`, `gt` and `ge` read `compare b a`.
`GT` is the only one of the three answers that all four read as `False`, so it is the only
answer that keeps both the one-member class and IEEE's operators. The price is that `compare`
is not antisymmetric on a `nan`.

**Chosen over a total order** — `nan` above every other `Float` and equal to itself, as Java's
`Double.compare` and Rust's `f64::total_cmp` have it. That makes sorting well behaved, and makes
`nan <= nan` and `1.5 < nan` `True`, against *Numbers*; or it keeps the operators and makes them
disagree with `compare`, which means `Float`'s operators can no longer be the ordinary functions
over `compare` every other type's are.

**Chosen over a partial `compare`** returning `Maybe Order`, Rust's `PartialOrd`. It is the
honest type, and every `Comparable` type pays for `Float`: each derivation, each `case` on a
comparison, each sort gains a branch that only one instance can reach.

**Chosen over leaving `Float` out of `Comparable`**, which makes `1.5 < 2.5` a type error.

## 3 — `min`, `max` and `clamp` are what `compare` makes them

`min x y` is `if lt x y then x else y` and `max` the same over `gt`, so each answers its second
argument when either is a `nan`; `clamp` passes a `nan` through. Ratified as the rule, and
written in the chapter as a consequence of decision 2.

**Chosen over propagating `nan`**, JavaScript's `Math.min` and IEEE 754-2019's `minimum`. That
cannot be written over `Comparable a` alone: it needs to ask whether a value is a `nan`, which
is a question about `Float`. `min` would become a class member, or `Float` would get its own
`min` and the constrained one would quietly differ from it. Elm's `min` behaves as this one
does.

## 4 — A `Char` is a Unicode scalar value, and a `String` a sequence of them

A surrogate, U+D800 to U+DFFF, is a code point and no `Char`, and no `String` holds one. On
JavaScript the boundary check for a `String` therefore asks for a well-formed string, and the one
for a `Char` refuses a lone surrogate.

The question arose because a JavaScript string may hold a surrogate that is half of no pair, a
companion may return one, and the ordering `std/core` implements places it above U+FFFF where
its code point is below U+E000 — so decision 1 was false of a value a program could hold.

**Chosen over admitting a lone surrogate as a `Char`** and ordering it by its code point. It
would have kept every JavaScript string a `String`. But WIT's `char` is a Unicode scalar value
and its `string` is well-formed, so on WebAssembly such a value cannot cross at all, and UTF-8
cannot encode it: the language would have a `Char` only one of its targets can represent. Rust
and Swift draw the line in the same place.

**Chosen over leaving a lone surrogate's order unspecified.** A program could then observe an
answer the chapter does not give, and the answer would differ by target.

The cost is a pass over every string that crosses the JavaScript boundary. The literal side was
already there: the tokenizer rejects `"\u{D800}"`, and [`LANG-80`](../tickets/lang-80.md) writes
that rejection into the lexical chapter. [`LANG-91`](../tickets/lang-91.md) is the boundary
check.
