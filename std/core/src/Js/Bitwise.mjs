/*
Implementation of the `Js.Bitwise` facade. As with the other `.mjs` files in
this directory, exports take a plain parameter list rather than Elm's curried
`F2`/`F3` wrappers — see the note at the top of `Basics.mjs`.

Every operand and every result here is an `Int`, which reaches JavaScript as a
`BigInt` (docs/spec/interop.md#which-types-may-cross-the-boundary) holding a
64-bit signed two's-complement value
(docs/spec/evaluation-semantics.md#numbers). `BigInt`'s bitwise operators read
their operands as two's complement and are arbitrary precision, so the width
is not theirs to supply: this file supplies it, with `BigInt.asIntN(64, ..)`.

`and`, `or`, `xor` and `complement` need no mask. Combining two values that
fit in 64 bits bit by bit gives a value that fits, and `~a` is `-a - 1`, which
does too. The three shifts do need one, and `shiftRightZfBy` needs a second:
having no `>>>`, it reads its operand as an unsigned 64-bit pattern, shifts
zeros in from the left, and reads the result back as a signed `Int`
(DEC-16 decision 6). That outer mask is what makes a shift of `0` the identity
rather than `2^64 - 1`.

What a *negative* shift count means is not settled, for any of the three, and
`DEC-16` decision 6 is where the question is recorded. Under `BigInt` a
negative count reverses the shift's direction, so `shiftRightZfBy -1 8` is a
right shift that shifted left; the masks below keep the answer an `Int`
whatever the count turns out to mean.
*/

// BASIC OPERATIONS

export function and(a, b) { return a & b }
export function or(a, b) { return a | b }
export function xor(a, b) { return a ^ b }
export function complement(a) { return ~a }

// BIT SHIFTS

// The offset comes first so that the functions partially apply the way the
// `Bitwise` docs describe them: `shiftLeftBy 1 5 == 10` shifts 5 by one bit.

export function shiftLeftBy(offset, a) { return BigInt.asIntN(64, a << offset) }
export function shiftRightBy(offset, a) { return BigInt.asIntN(64, a >> offset) }
export function shiftRightZfBy(offset, a) {
  return BigInt.asIntN(64, BigInt.asUintN(64, a) >> offset);
}
