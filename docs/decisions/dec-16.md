# DEC-16 · `Int` is 64 bits on every target: six decisions

**Settled:** 2026-09-15, by the language owner (`SPEC-28`).
**Status:** live.
**Where the rule lives:** [Numbers](../spec/evaluation-semantics.md#numbers) for the width and
the wrapping, [Integers](../spec/lexical-structure.md#integers) for what an out-of-range literal
does, and [the admitted-type table](../spec/interop.md#which-types-may-cross-the-boundary) for
what an `Int` is at a facade boundary.

Two chapters answered "how large may an integer be" differently.
[Integers](../spec/lexical-structure.md#integers) guaranteed `-2^31 .. 2^31 - 1` on every target
and left everything above it to the compilation target, naming two behaviours for two backends.
[Numbers](../spec/evaluation-semantics.md#numbers) said `Int` was 32-bit two's-complement
everywhere. Neither had an entry here, so the width was prose that had never been argued against
an alternative, and the disagreement could not be settled by asking which chapter was right.

Underneath sat a third bound nobody had written down: the tokenizer carries a literal's value in
an `i64` and rejects what does not fit.

## What the question turned out to be

Two questions wearing one. The first is **whether a width is fixed at all** — whether a program
computes the same answer on every target. The second is **which width**, and it only exists once
the first is answered.

Answering the first with *the target decides* dissolves the second and costs every rule that
names a width. Answering it with *the language decides* leaves the second standing, and the
five decisions below take that answer and then pay for it.

## 1 — A width is fixed by the language, not by the target

`Int` has one width, and a program computes the same answer wherever it runs.

Target-dependence was not a rule with a cost; it was a rule two other rules had already been
written against. [An operation with no answer](../spec/evaluation-semantics.md#an-operation-with-no-answer)
justifies `n // 0` being `0` by there being no bit pattern left over to mean *no answer* — an
argument with no subject once the pattern count is the backend's. [Converting a `Float` to an
`Int`](../spec/evaluation-semantics.md#converting-a-float-to-an-int) says `round`, `floor`,
`ceiling` and `truncate` wrap into a fixed width, and under target-dependence `truncate 1e18`
has as many answers as there are backends. A facade's boundary check is the third: the
admitted-type table gives `Int` one predicate and one WIT spelling, and a signature that means
different things on two targets is not the contract [a facade names a
boundary](dec-13.md) makes it.

The claim target-dependence trades for is that a backend may use its fastest integer. This
project does not buy that: `std/core/src/Js/Basics.mjs` opens by saying it is "not a language
designed for high-perf or high-reach applications".

"The JavaScript backend is exact to `2^53`" also understated what was being licensed. Above
`2^53` JavaScript rounds rather than wraps, so target-dependence was three behaviours and not
two — exact below `2^31`, rounding on one backend, wrapping on another.

## 2 — The width is 64

`2^31 - 1` is about 2.1 billion, and a Zelkova program meets that boundary in ordinary code:
millisecond timestamps, byte counts, identifiers, anything counting events.

[Foreign interoperability](../spec/interop.md#an-effectful-facade) shows the case in its own
canonical effect, a facade returning `Date.now()` as an `Int`. That value is around
`1.7 * 10^12`. Under a 32-bit `Int` the chapter's own example fails the chapter's own boundary
check on every call, and the repair available inside the language — widen it to `Float` — is
exact only to `2^53` and misdescribes what the value is.

64 bits carries every one of those quantities with room left, and is the native integer of the
eventual target: `i64` in WebAssembly, `s64` in WIT.

What it costs is the JavaScript representation, which is decision 5, and
`shiftRightZfBy`, which is decision 5's consequence.

## 3 — One integer type, and no `Long`

A second type — `Int` at 32 bits beside a 64-bit `Long`, as Java and Scala have — was weighed
and rejected. It is cheap to implement and expensive in the language, because two rules already
settled leave it with no way to spell a constant.

[A literal's type is its spelling](../spec/expressions.md#a-literals-type-is-its-spelling) makes
`1` an `Int`, and says an annotation cannot change that. [Numeric
literals](../spec/type-classes.md#numeric-literals) adds that nothing in the language defaults
and the compiler knows no class by name, "in every case and with no exception carved out for
arithmetic". So `ms : Long` bound to `1000` is an error, and the escape `Float` has — spell it
with a point — has no analogue, because there is one point to spend and it is spent.

The class mechanism cannot cover the gap either, and for a reason the same section states: a
literal inside a constrained function is already concrete, so `double x = mul x 2` under
`Number a => a -> a` forces `a` to be `Int`. Generic numeric code would reach `Long` only where
it contains no constant at all. That price bought something when `Number` had one integer
instance to serve; with two it buys a type most of the library cannot be written against.

Both rules could be reopened — a suffixed literal (`1000L`), or defaulting — and each is a
decision of [DEC-2](dec-2.md)'s weight, with the type-class program already queued behind it.
An explicit-width family (`Int8`, `Int32`, `Int64`) is the coherent version of the idea, and it
wants a literal rule of its own rather than a `Long` fitted beside an `Int`.

## 4 — An integer literal outside the range is rejected

A literal too large in magnitude for an `Int` is an error, rather than wrapping or saturating.

[Floats](../spec/evaluation-semantics.md#numbers) answers the same question the other way — a
float literal too large denotes positive infinity — and the difference is in the types rather
than in a preference. binary64 keeps a value meaning *larger than anything representable*, so a
`Float` literal has something to denote and a caller can ask afterwards. `Int` keeps no such
value at any width, which is the observation [An operation with no
answer](../spec/evaluation-semantics.md#an-operation-with-no-answer) already builds `//` on. A
wrapped literal denotes an ordinary number the program cannot tell from an intended one, and a
saturated one denotes the bound.

A literal is also the one place a program has written the exact number it means, with nothing
computed and nothing conditional, so the disagreement between what it says and what the type
holds is visible while compiling. Rejecting costs a program nothing it can want: the alternatives
are reached deliberately by writing the wrapped value.

That the tokenizer already does this, having carried literals in an `i64` since `BUG-12`, is not
the reason — it is why this decision closes a gap rather than opening a ticket.

## 5 — On JavaScript an `Int` is a `BigInt`

A JavaScript `number` is a binary64, exact on integers only to `2^53`, so it cannot carry a
64-bit `Int` and the language's answer cannot be the same on both targets while it is used. The
representation is therefore `BigInt`, and [the admitted-type
table](../spec/interop.md#which-types-may-cross-the-boundary) checks a crossing `Int` for one.

This lands on companions rather than on programs. A companion handing back a JavaScript number
converts — `BigInt(Date.now())` — and one taking an `Int` receives a `BigInt`.
`std/core`'s companions carry an `Int` as a `BigInt` and mask each result back into the range
with `BigInt.asIntN(64, ..)` (`LANG-56`).

`Bitwise` is where the cost is sharpest. `BigInt` supports `&`, `|`, `^`, `<<` and `>>`, and has
no `>>>`: an unsigned right shift has no meaning on a type with no width. Decision 6 gives it
one.

## 6 — A shift reads its operand as a fixed 64-bit pattern, and its count clamped into `0 .. 64`

`shiftRightZfBy` fills from the left with zeros, which needs a width to fill from. The width is
named by the operation rather than borrowed from the representation: the operand is read as a
64-bit two's-complement pattern, shifted, and the result read back as a signed `Int`. The count
is a number of positions, and it is read clamped into `0 .. 64` — the three things below follow,
and none of them held for the 32-bit version.

**Every result is an `Int`.** `Bitwise.zel` documents `shiftRightZfBy 1 -32` as `2147483632`, a
value deliberately outside `Int`'s 32-bit range and matching Elm — reachable only because a
JavaScript number had room above the range to hold it. At 64 bits the representation has no room
above, so the result is read back into the range, and it fits without anything being lost: a
shift by one or more leaves at most 63 significant bits. A shift by zero is the identity, which
is what reading the pattern back gives.

**A count of 64 or more leaves nothing of the pattern.** JavaScript's `>>>` masks its count to
five bits, so `1 >>> 32` is `1` and not `0`. `BigInt` does not mask, and naming the width does
not reintroduce it: a 64-bit pattern moved 64 positions has nothing of itself left. For
`shiftLeftBy` and `shiftRightZfBy`, whose fill is zero either way, that reads as `0` at any count
of 64 or more. `shiftRightBy` fills with the operand's own topmost bit instead, so at 64 or more
it reads as that bit copied across all 64 positions — `0` for a non-negative operand, `-1` for a
negative one: `shiftRightBy 64 -32` is `-1`, and so is `shiftRightBy 100 -32`. The pattern is
exhausted the same way for all three; only the fill rule differs in which value that leaves.

**A count below 0 reads as 0, which is the identity.** There is no such thing as a negative
number of positions, and the nearest count that does exist is none at all. Three other readings
were weighed and cost more: letting `BigInt`'s own `<<`/`>>` answer reverses the shift's
direction and costs each function its own name — `shiftLeftBy -1 8` becomes a right shift, and
`shiftRightZfBy -1 8` a zero-fill shift that answers negative, because the reversal happens after
the operand has already been read unsigned; throwing is closed off by
[Two outcomes](../spec/evaluation-semantics.md#two-outcomes), since nothing in the language
throws and a well-typed program produces a value or does not terminate; and shifting by the
count's magnitude answers a `-1` a caller wrote and a `-1` a caller never meant alike, for no
reason the language can state. Clamping gives the count one reading across its whole range, with
no jump at either boundary: for `shiftLeftBy`, counts `-2, -1, 0, 1, 2` answer `x, x, x, 2x, 4x`.
It also turns `std/core`'s `_Bitwise_boundOffset` guard — which exists to stop V8 materialising
an enormous intermediate `BigInt` before the outer mask can run — into the implementation of
this rule rather than a companion-local repair.
