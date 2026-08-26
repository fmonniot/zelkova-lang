/*
Implementation of the `Js.Bitwise` facade. As with the other `.mjs` files in
this directory, exports take a plain parameter list rather than Elm's curried
`F2`/`F3` wrappers — see the note at the top of `Basics.mjs`.

JavaScript's bitwise operators coerce their operands to 32 bit integers, which
is exactly the range Zelkova's `Int` promises to be well defined over (see the
note in `Basics.zel`), so every operand here is in range by construction.

The six signed operators return a value in that range too. `>>>` is the
exception: its result is unsigned, in `0 .. 2^32 - 1`, so `shiftRightZfBy` can
hand back a number outside `Int`'s well-defined range. That is deliberate and
matches Elm, whose kernel is `a >>> offset` as well; `Bitwise.zel` documents
the behaviour on `shiftRightZfBy` itself.
*/

// BASIC OPERATIONS

export function and(a, b) { return a & b }
export function or(a, b) { return a | b }
export function xor(a, b) { return a ^ b }
export function complement(a) { return ~a }

// BIT SHIFTS

// The offset comes first so that the functions partially apply the way the
// `Bitwise` docs describe them: `shiftLeftBy 1 5 == 10` shifts 5 by one bit.

export function shiftLeftBy(offset, a) { return a << offset }
export function shiftRightBy(offset, a) { return a >> offset }
export function shiftRightZfBy(offset, a) { return a >>> offset }
