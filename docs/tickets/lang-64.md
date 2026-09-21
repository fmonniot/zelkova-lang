# LANG-64 · A shift count is clamped into `0 .. 64`

**Sizing:** small — one helper changes; the rest is the prose and the examples that describe it.

**Decided:** 2026-09-20, by the language owner. This ticket was filed with three readings and no
pick, because picking changes what three `std/core` functions promise. It now carries the pick,
the argument for it, and the work that follows.

**Location:** `std/core/src/Js/Bitwise.mjs` — `_Bitwise_boundOffset` and the file header;
`std/core/src/Bitwise.zel` — the doc comments for `shiftLeftBy`, `shiftRightBy` and
`shiftRightZfBy`; `docs/decisions/dec-16.md` — decision 6;
`std/core/tests/Js/BitwiseChecks.mjs`.

**Depends on:** `LANG-56` (closed 2026-09-20), which carried `std/core`'s two JavaScript
companions to the 64-bit `BigInt` representation of `Int` that
[DEC-16](../decisions/dec-16.md) settled. Its own text said "ask before picking; nothing else in
either file depends on the answer," so it shipped the port and the masking without picking, and
left the question here.

## The rule

**A shift count is a number of positions, and it is read clamped into `0 .. 64`.**

- A count of **64 or more** reads as 64. This half is already
  [DEC-16 decision 6](../decisions/dec-16.md#6--a-shift-reads-its-operand-as-a-fixed-64-bit-pattern):
  a 64-bit pattern moved 64 positions has nothing of itself left, so a larger count has nothing
  further to do.
- A count **below 0** reads as 0, which is the identity. There is no such thing as a negative
  number of positions, and the nearest count that does exist is none at all.

```zel
shiftLeftBy    -1   5 ==  5
shiftRightBy   -1  32 == 32
shiftRightZfBy -1 -32 == -32
```

No positive count changes. A count of 0 was already the identity for all three, including
`shiftRightZfBy`, whose outer mask is what makes it so.

## Why this reading, and not the other three

**Reversal — do nothing, and let `BigInt`'s own `<<`/`>>` answer.** This is what the tree does
today, and it costs each of the three functions its own name. `shiftLeftBy -1 8` is `4` and
`shiftLeftBy -64 -32` is `-1`: a left shift that went right. `shiftRightBy -1 8` is `16`, a
right shift that filled from the right with zeros rather than with the topmost bit its doc
comment promises. `shiftRightZfBy -1 -32` is `-64` — a negative answer out of a zero-fill shift,
because the reversal happens after `BigInt.asUintN` has already read the operand as unsigned.
Three doc comments would have to stop saying what their functions do. The one place this reading
exists in the wild is Haskell's `Data.Bits`, and it attaches it to `shift`, whose name names no
direction, keeping `shiftL` and `shiftR` unidirectional.

**An error.** Closed by the language before this ticket was filed, not by this decision:
[Two outcomes](../spec/evaluation-semantics.md#two-outcomes) says a well-typed program produces a
value or does not terminate, and nothing in the language throws. A throwing shift would be a new
outcome for every program, and it would also need an answer for what a facade call does when the
JavaScript side throws, which nothing designs today.

**Ignoring the sign — shift by the count's magnitude.** The sign is something the caller wrote.
This is the only reading under which two counts a caller could compute answer alike for no reason
the language can state, and the caller who wrote `-1` by accident is given a confident answer to
a question they did not ask.

Clamping keeps every name true. It invents nothing at the top end — a count of 100 really does
leave nothing, exactly as 64 does — and at the bottom end it names the nearest count that exists
rather than naming a value the way `n // 0 == 0` has to. It gives the count one reading across
its whole range, with no jump at either boundary: for `shiftLeftBy`, counts `-2, -1, 0, 1, 2`
answer `x, x, x, 2x, 4x`. And it turns `_Bitwise_boundOffset` — which exists today only to stop
V8 materialising an enormous intermediate `BigInt` before the outer mask can run — into the
implementation of a language rule rather than a companion-local repair.

## A correction that comes with it

DEC-16 decision 6 says, in bold, "**A shift of 64 or more is `0`.**" That is true of
`shiftLeftBy` and `shiftRightZfBy` and **false of `shiftRightBy`**, which fills with the topmost
bit: `shiftRightBy 64 -32` is `-1`, and so is `shiftRightBy 100 -32`. The check that looks like
it pins the sentence — `PINS a shift of 64 or more is 0` in `BitwiseChecks.mjs` — passes only
because it tests the other two functions.

The clamp cannot be stated without fixing this, since it says a count above 64 reads as 64 and
the reader then needs the answer at 64. So the sentence becomes a claim about the *pattern* —
nothing of the original is left after 64 positions — and each function's own fill rule names the
value that leaves.

## What to change

- **`docs/decisions/dec-16.md`, decision 6.** State the clamp in both directions, with the
  argument above for it and against the three readings. Correct the "64 or more" sentence. Drop
  the closing paragraph that says the meaning is unsettled and cites this ticket — that citation
  is checked, see **Acceptance**. Nothing outside this file cites the section's own anchor
  (`#6--a-shift-reads-its-operand-as-a-fixed-64-bit-pattern`), and this file is deleted on close,
  so the heading may be reworded — but `decision_cross_references_resolve` checks anchors, so
  re-grep before renaming it.
- **`std/core/src/Js/Bitwise.mjs`.** `_Bitwise_boundOffset` becomes a clamp into `0 .. 64`:
  `if (offset < 0n) return 0n; if (offset > 64n) return 64n; return offset;`. Its comment stops
  describing the negative half as a placeholder for an undecided question and states the rule;
  the file header's closing paragraph ("What a *negative* shift count means is not settled")
  goes the same way. What the guard is still *for* — V8 throwing `RangeError: Maximum BigInt
  size exceeded` on the unmasked intermediate — stays, because it is why the clamp is written
  where it is rather than left to the mask.
- **`std/core/src/Bitwise.zel`.** Each of the three doc comments gains a negative-count line in
  its worked examples, matching the three above. None of them says anything about a negative
  count today.
- **`std/core/tests/Js/BitwiseChecks.mjs`.** The header note "nothing below passes one" goes.
  `PINS a large-magnitude offset does not throw and matches an offset of exactly 64 in the same
  direction` asserts `shift(-LARGE, a) === shift(-64n, a)`, which is the reversal reading — under
  the clamp every negative offset answers `shift(0n, a)`, which is `a`. Add a check per function
  pinning at least one negative count.

**Not in scope:** nothing goes into `docs/spec/`. No chapter states what `Bitwise`'s functions
do — the spec describes the language, and these three are `std/core`'s. The rule's user-facing
home is `Bitwise.zel`'s doc comments and its argued home is DEC-16, which is where decision 6
already put the rest of the shift semantics.

## Acceptance

- DEC-16 decision 6 states the clamp in both directions and argues it against reversal, an
  error, and discarding the sign.
- Decision 6 no longer claims a shift of 64 or more is `0` for `shiftRightBy`.
- `Bitwise.zel`'s doc comments for `shiftLeftBy`, `shiftRightBy` and `shiftRightZfBy` each show a
  negative count, matching the decision.
- `Js/Bitwise.mjs` clamps a count into `0 .. 64`, and no shift reverses direction for any count.
- `node --test 'std/core/tests/**/*.mjs'` passes, with a check per shift pinning a negative
  count. Each of those checks is **verified red** against the current companion first —
  `shiftLeftBy(-1n, 8n)` is `4n` today and `8n` after — and the `-LARGE` assertions in the
  large-magnitude check are updated rather than left asserting the old reading.
- `cargo test --test spec` is green. Deleting this file on close turns
  `decision_cross_references_resolve` red until decision 6's last paragraph, which links
  `../tickets/lang-64.md`, is rewritten; that rewrite is part of the same change.
- [LANG-65](lang-65.md) cites this ticket four times as an open question — including for its own
  negative-`Int`-exponent case in `pow`, which this decision does **not** settle (`2 ^ -1` has a
  real answer outside `Int`, where a negative shift count has none inside it). Those references
  are updated to cite the settled rule as a precedent, not a pending one.
