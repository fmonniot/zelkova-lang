# LANG-64 · What a negative shift count means is undecided

**Sizing:** small — one decision, then a one-line change to up to three functions.

**Location:** `std/core/src/Js/Bitwise.mjs` — `shiftLeftBy`, `shiftRightBy`, `shiftRightZfBy`.

**Depends on:** [LANG-56](lang-56.md), which carries `std/core`'s two JavaScript companions to
the 64-bit `BigInt` representation of `Int` [DEC-16](../decisions/dec-16.md) settled. This
ticket is the one question LANG-56 deliberately did not answer.

**Found:** while implementing LANG-56. Its own text said "ask before picking; nothing else in
either file depends on the answer," so LANG-56 shipped the port and masking without picking,
and this ticket is where the question now lives — [DEC-16 decision
6](../decisions/dec-16.md#6--a-shift-reads-its-operand-as-a-fixed-64-bit-pattern) records it in
prose but names no ticket, since none existed yet when it was written.

**Problem:** JavaScript's `<<`, `>>` and `>>>` mask their shift count to five bits and coerce
their operands to 32 bits, so a negative count on a 32-bit operand never had an unambiguous
reading — `8 << -1` is `8 << 31` after masking, an implementation artifact rather than a chosen
answer. Once the operands are `BigInt` (LANG-56), the mask is gone: JavaScript defines a
negative `BigInt` shift count as reversing the shift's direction, so `a << -1n` is `a >> 1n` and
vice versa. That is a real, working answer — not a throw — and it is silently the one you get by
doing nothing, unless the language decides otherwise.

`shiftRightZfBy` sharpens it further because it has no native operator at all (`BigInt` has no
`>>>`); LANG-56 built it from `BigInt.asUintN`/`asIntN` and `>>`, so its negative-count behaviour
is exactly whatever `>>`'s is, by construction rather than by any argument that it should be.

Concretely, today (post-LANG-56) `shiftRightZfBy -1 8` returns `16n` — a "zero-fill right shift"
that shifted left. Nothing checked this in either direction: LANG-56's test additions
deliberately pass no negative count.

**Options, none picked:**

1. **Keep JavaScript's reversal.** Cheapest — it's what fifth-column `>>`/`<<` on `BigInt`
   already does, so all three functions need no change once the rest of LANG-56 lands. Costs
   coherence: nothing in `docs/spec/` currently describes a shift as bidirectional, and a reader
   of `Bitwise.zel`'s docs has no reason to expect `shiftLeftBy -1 x` to shift right.
2. **Treat a negative count as an error.** Matches the "no answer" shape [An operation with no
   answer](../spec/evaluation-semantics.md#an-operation-with-no-answer) uses for division by
   zero — but that section's operations are total by returning `0`, not by raising, so this
   would be a new failure shape for `std/core` to justify, and it needs an answer for what a
   facade call does when the JavaScript side throws (nothing currently unwinds a JS exception
   back through a facade boundary — that is undesigned).
3. **Define it as shifting by the count's absolute value in the same direction** — i.e. a
   negative count behaves as if it were positive, and the sign is ignored rather than read as a
   direction. Closest to "there is no such thing as a negative shift," but silently discarding
   a sign a caller wrote is its own kind of surprising.

Picking is a language decision — it changes what three `std/core` functions promise — not an
implementation detail, so this ticket does not choose. Whichever is picked, name it in
[DEC-16 decision 6](../decisions/dec-16.md#6--a-shift-reads-its-operand-as-a-fixed-64-bit-pattern)
(the section already narrates the question) and in `Bitwise.zel`'s doc comments for the three
functions, none of which currently says anything about a negative count.

**Acceptance:** DEC-16 decision 6 states which of the above (or another option) was chosen, and
why. `Bitwise.zel`'s doc comments for `shiftLeftBy`, `shiftRightBy` and `shiftRightZfBy` each
gain a worked example with a negative count matching the decision. `Js/Bitwise.mjs` implements
it — a no-op if option 1 is picked, a guard if option 2 or 3 is. A `node --test` check in
`std/core/tests/Js/BitwiseChecks.mjs` pins the chosen behaviour for at least one negative count
per function.
