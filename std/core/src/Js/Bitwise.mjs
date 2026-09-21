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

A shift count is a number of positions, and it is read clamped into `0 .. 64`
(DEC-16 decision 6, LANG-64): a count of 64 or more leaves nothing of the
64-bit pattern, and a count below 0 reads as 0, the identity, there being no
such thing as a negative number of positions. Under `BigInt` a negative count
would otherwise reverse the shift's direction, so `shiftRightZfBy -1 8` would
be a right shift that shifted left; `_Bitwise_boundOffset` below is what
keeps every count read this way before any shift runs.
*/

// BASIC OPERATIONS

export function and(a, b) { return a & b }
export function or(a, b) { return a | b }
export function xor(a, b) { return a ^ b }
export function complement(a) { return ~a }

// BIT SHIFTS

// The offset comes first so that the functions partially apply the way the
// `Bitwise` docs describe them: `shiftLeftBy 1 5 == 10` shifts 5 by one bit.

// `<<` and `>>` on a `BigInt` do not mask their count the way JavaScript's
// 32-bit bitwise operators do, so the outer `BigInt.asIntN(64, ..)` mask below
// cannot help: V8 has to materialize the shifted value, at its full unmasked
// magnitude, before that mask ever runs. An offset with a large enough
// magnitude — reachable with an entirely ordinary in-range `Int`, positive or
// negative — makes that intermediate allocation itself throw `RangeError:
// Maximum BigInt size exceeded`.
//
// DEC-16 decision 6 (LANG-64) is the rule this implements, not just guards
// against a crash: a shift count is a number of positions, read clamped into
// `0 .. 64`. An offset of 64 or more leaves nothing of the 64-bit pattern, so
// it reads as 64; an offset below 0 names no such thing as a negative number
// of positions, so it reads as 0, the identity. Clamping here, before the
// native operator sees it, also removes the unbounded allocation above — the
// same shape as `idiv`/`remainderBy`'s zero-divisor guard — without changing
// any answer in `0 .. 64`.
function _Bitwise_boundOffset(offset) {
  if (offset < 0n) return 0n;
  if (offset > 64n) return 64n;
  return offset;
}

export function shiftLeftBy(offset, a) {
  return BigInt.asIntN(64, a << _Bitwise_boundOffset(offset));
}
export function shiftRightBy(offset, a) {
  return BigInt.asIntN(64, a >> _Bitwise_boundOffset(offset));
}
export function shiftRightZfBy(offset, a) {
  return BigInt.asIntN(64, BigInt.asUintN(64, a) >> _Bitwise_boundOffset(offset));
}
