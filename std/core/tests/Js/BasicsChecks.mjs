// The JavaScript checks over std/core/src/Js/Basics.mjs, the companion behind
// the Js.Basics facade (Js/Basics.zel).
//
// This is the companion of the test facade Js.BasicsChecks
// (tests/Js/BasicsChecks.zel), laid out as docs/spec/interop.md's "Testing a
// companion" describes. Each export is one check, declared there as
// `Task (Result Failure ())`: it returns nothing when the check holds, and a
// failed assertion throws, which the facade's wrapper turns into
// `Err (Threw ..)`. Js.BasicsTests (tests/Js/BasicsTests.zel) exposes one
// `Test` per check, so `zelkova test std/core` runs them.
//
// Three rules are checked below.
//
// An `Int` is a `BigInt` on this target
// (docs/spec/interop.md#which-types-may-cross-the-boundary), holding a 64-bit
// signed two's-complement value that wraps on overflow
// (docs/spec/evaluation-semantics.md#numbers). Every `Int` below is therefore
// written `0n` and not `0`.
//
// docs/spec/evaluation-semantics.md#an-operation-with-no-answer defines
// `n // 0`, `modBy 0 n` and `remainderBy 0 n` to each be `0`, which keeps
// those three operations total.
//
// docs/spec/evaluation-semantics.md#converting-a-float-to-an-int defines a
// conversion to `Int` as rounding and then wrapping into 64 bits, with `nan`
// and both infinities landing on 0.
//
// Two kinds of check live below, following the PINS/GUARD convention
// UtilsChecks.mjs documents and uses:
//
//   PINS  — verified red against the file as it stood before the relevant
//           fix. These are the fix.
//   GUARD — passes with and without the fix. These pin that the fix did not
//           disturb already-correct behaviour; they prove nothing about the
//           fix itself, so do not read a green one as a pinned new behaviour.
//
// LANG-56 moved every `Int` here from a number to a `BigInt`, and most of the
// assertions below are red against the pre-LANG-56 companion for that reason
// alone, whatever the answer used to be. A GUARD below is one of the few
// whose JavaScript already worked on a `BigInt` operand and already answered
// what it answers now.

import assert from 'node:assert/strict';
// `Basics.mjs` no longer exports bare `add`/`sub`/`mul`/`pow`: `LANG-43`
// split each into a monomorphic `*Int`/`*Float` pair (`Js/Basics.zel`), and
// `Basics.zel` itself picks the `Int` one for its own `a -> a -> a`
// re-export. The two aliases share one underlying function (see
// `Basics.mjs`), so importing the `Int` name under its old bare spelling
// below still exercises exactly what these tests exercised before — Float
// operands included.
import {
    addInt as add, subInt as sub, mulInt as mul, idiv, modBy, remainderBy,
    round, floor, ceiling, truncate, toFloat, powInt as pow,
} from '../../src/Js/Basics.mjs';

// INT ARITHMETIC (LANG-56)

const INT_MAX = 9223372036854775807n;
const INT_MIN = -9223372036854775808n;

// PINS add wraps at 64 bits
export function addWrapsAt64Bits() {
    assert.equal(add(INT_MAX, 1n), INT_MIN);
    assert.equal(add(1n, 2n), 3n);
}

// PINS sub wraps at 64 bits
export function subWrapsAt64Bits() {
    assert.equal(sub(INT_MIN, 1n), INT_MAX);
    assert.equal(sub(3n, 1n), 2n);
}

// PINS mul wraps at 64 bits and stays exact past 2^53
export function mulWrapsAt64Bits() {
    assert.equal(mul(INT_MAX, 2n), -2n);
    assert.equal(mul(4294967296n, 4294967296n), 0n);
    assert.equal(mul(123456789n, 987654321n), 121932631112635269n);
}

// `add`, `sub` and `mul` back `Float` arithmetic as well as `Int`: Basics.zel
// declares each of them `a -> a -> a`. A number operand keeps IEEE's answer.
// GUARD add, sub and mul still compute on Floats
export function addSubAndMulComputeOnFloats() {
    assert.equal(add(3.14, 3.14), 6.28);
    assert.equal(sub(1.5, 0.25), 1.25);
    assert.equal(mul(1.5, 2.0), 3.0);
    assert.equal(add(1.0 / 0.0, 1.0), Infinity);
}

// `BigInt` division and `%` both throw on a zero divisor, so the three zeros
// below are the explicit guards in Basics.mjs rather than anything falling out
// of the arithmetic.
// PINS idiv divides by zero to 0
export function idivByZeroIsZero() {
    assert.equal(idiv(1n, 0n), 0n);
    assert.equal(idiv(-1n, 0n), 0n);
    assert.equal(idiv(0n, 0n), 0n);
}

// The answer here was already correct before LANG-56; only its type moved.
// PINS idiv truncates toward zero on a non-zero divisor
export function idivTruncatesTowardZero() {
    assert.equal(idiv(7n, 2n), 3n);
    assert.equal(idiv(-7n, 2n), -3n);
}

// PINS idiv wraps the one division that leaves the range
export function idivWrapsTheOneDivisionThatLeavesTheRange() {
    assert.equal(idiv(INT_MIN, -1n), INT_MIN);
}

// PINS modBy 0 n is 0
export function modByZeroIsZero() {
    assert.equal(modBy(0n, 5n), 0n);
    assert.equal(modBy(0n, -5n), 0n);
    assert.equal(modBy(0n, 0n), 0n);
}

// `%` and the sign comparisons read a `BigInt` operand the way they read a
// number, so this arithmetic answered the same before LANG-56.
// GUARD modBy keeps its sign-correcting arithmetic for a non-zero modulus
export function modByCorrectsTheSign() {
    assert.equal(modBy(3n, 5n), 2n);
    assert.equal(modBy(-3n, 5n), -1n);
    assert.equal(modBy(3n, -5n), 1n);
    assert.equal(modBy(-3n, -5n), -2n);
}

// PINS remainderBy 0 n is 0
export function remainderByZeroIsZero() {
    assert.equal(remainderBy(0n, 5n), 0n);
    assert.equal(remainderBy(0n, -5n), 0n);
    assert.equal(remainderBy(0n, 0n), 0n);
}

// GUARD remainderBy keeps the sign of the dividend on a non-zero divisor
export function remainderByKeepsTheSignOfTheDividend() {
    assert.equal(remainderBy(3n, 5n), 2n);
    assert.equal(remainderBy(-3n, 5n), 2n);
    assert.equal(remainderBy(3n, -5n), -2n);
    assert.equal(remainderBy(-3n, -5n), -2n);
}

// FLOAT -> INT CONVERSIONS (BUG-25, LANG-56)

// PINS round wraps nan and both infinities to 0
export function roundWrapsNanAndInfinitiesToZero() {
    assert.equal(round(NaN), 0n);
    assert.equal(round(Infinity), 0n);
    assert.equal(round(-Infinity), 0n);
}

// PINS floor wraps nan and both infinities to 0
export function floorWrapsNanAndInfinitiesToZero() {
    assert.equal(floor(NaN), 0n);
    assert.equal(floor(Infinity), 0n);
    assert.equal(floor(-Infinity), 0n);
}

// PINS ceiling wraps nan and both infinities to 0
export function ceilingWrapsNanAndInfinitiesToZero() {
    assert.equal(ceiling(NaN), 0n);
    assert.equal(ceiling(Infinity), 0n);
    assert.equal(ceiling(-Infinity), 0n);
}

// PINS truncate wraps nan and both infinities to 0
export function truncateWrapsNanAndInfinitiesToZero() {
    assert.equal(truncate(NaN), 0n);
    assert.equal(truncate(Infinity), 0n);
    assert.equal(truncate(-Infinity), 0n);
}

// `1.0e20` is an integer value outside the 64-bit range, so
// Math.round/Math.floor/Math.ceil/Math.trunc all return it unchanged and the
// wrap is the only thing that brings any of the four into `Int`'s range.
const WRAPPED_1E20 = BigInt.asIntN(64, BigInt(1.0e20));

// PINS round wraps a finite value outside the 64-bit range into it
export function roundWrapsAnOutOfRangeValue() {
    assert.equal(round(1.0e20), WRAPPED_1E20);
    assert.equal(typeof round(1.0e20), 'bigint');
}

// PINS floor wraps a finite value outside the 64-bit range into it
export function floorWrapsAnOutOfRangeValue() {
    assert.equal(floor(1.0e20), WRAPPED_1E20);
    assert.equal(typeof floor(1.0e20), 'bigint');
}

// PINS ceiling wraps a finite value outside the 64-bit range into it
export function ceilingWrapsAnOutOfRangeValue() {
    assert.equal(ceiling(1.0e20), WRAPPED_1E20);
    assert.equal(typeof ceiling(1.0e20), 'bigint');
}

// PINS truncate wraps a finite value outside the 64-bit range into it
export function truncateWrapsAnOutOfRangeValue() {
    assert.equal(truncate(1.0e20), WRAPPED_1E20);
    assert.equal(typeof truncate(1.0e20), 'bigint');
}

// `2^53` is where a JavaScript number stops being exact on integers, and the
// four conversions carry a value past it because the result is a `BigInt`.
// PINS a value past 2^53 converts exactly
export function aValuePast2To53ConvertsExactly() {
    assert.equal(round(1.0e18), 1000000000000000000n);
    assert.equal(floor(1.0e18), 1000000000000000000n);
    assert.equal(ceiling(1.0e18), 1000000000000000000n);
    assert.equal(truncate(1.0e18), 1000000000000000000n);
}

// The rounding directions were already correct before LANG-56; only the
// result type moved.
// PINS round, floor and ceiling keep their rounding direction in range
export function roundFloorAndCeilingKeepTheirDirection() {
    assert.equal(round(1.5), 2n);
    assert.equal(round(-1.5), -1n);
    assert.equal(floor(1.9), 1n);
    assert.equal(floor(-1.1), -2n);
    assert.equal(ceiling(1.1), 2n);
    assert.equal(ceiling(-1.9), -1n);
}

// PINS truncate keeps rounding toward zero
export function truncateRoundsTowardZero() {
    assert.equal(truncate(1.9), 1n);
    assert.equal(truncate(-1.9), -1n);
}

// TO FLOAT AND POW (LANG-65)
//
// `toFloat` used to be `return x`, so it handed back the `BigInt` argument
// unchanged rather than the `Float` (a JavaScript number) its signature
// promises. `pow` used to be `Math.pow`, which throws on two `BigInt`
// operands ("Cannot convert a BigInt to a number").

// PINS toFloat returns a Number, not the BigInt it was handed
export function toFloatReturnsANumber() {
    assert.equal(typeof toFloat(10n), 'number');
    // A BigInt and a Number are never `===`, so this only holds once toFloat
    // has actually converted rather than returned its `bigint` argument
    // unchanged.
    assert.equal(toFloat(10n) === 10, true);
}

// PINS toFloat converts a large BigInt to the double a Number literal of the
// same value would be
export function toFloatConvertsALargeIntToTheNearestDouble() {
    // `10^18` factors as `5^18 * 2^18`; `5^18` fits in 42 bits, so this
    // particular value happens to survive the conversion exactly. That is a
    // property of this one value, not of `toFloat` in general — see the next
    // test.
    assert.equal(toFloat(1000000000000000000n), 1e18);
}

// PINS toFloat loses precision above 2^53, as DEC-16 decision 2 accepts
export function toFloatLosesPrecisionAbove2To53() {
    // INT_MAX (2^63 - 1) is not exactly representable as a double: the
    // nearest one is 2^63, one past INT_MAX. A `toFloat` that preserved
    // 64-bit precision (e.g. by clamping to the nearest representable value
    // below INT_MAX, or by throwing) would fail this assertion.
    assert.equal(toFloat(INT_MAX), 9223372036854775808);
}

// PINS pow computes on two Ints instead of throwing
export function powComputesOnTwoInts() {
    assert.equal(pow(3n, 2n), 9n);
    assert.equal(pow(3n, 0n), 1n);
    assert.equal(typeof pow(3n, 2n), 'bigint');
}

// PINS pow wraps an Int result at 64 bits
export function powWrapsAt64Bits() {
    assert.equal(pow(2n, 64n), 0n);
    assert.equal(pow(2n, 63n), INT_MIN);
}

// `pow` backed both Int and Float arithmetic even before this fix, since
// `Math.pow` already handled two numbers — this only pins that dispatching
// on `typeof a` did not disturb it.
// GUARD pow still computes on Floats
export function powComputesOnFloats() {
    assert.equal(pow(2.0, 0.5), Math.SQRT2);
    assert.equal(pow(3.0, 3.0), 27.0);
}

// A negative Int exponent has no committed answer yet (LANG-66): `**` throws
// on a negative BigInt exponent, and pow does not catch it. This pins that
// today's behaviour is "throws", not some silently invented value, so a
// later fix for LANG-66 is what has to touch this test, not an accident.
// It asserts the error's type, not V8's wording, which differs between Node
// versions.
// PINS pow still throws on a negative Int exponent, pending LANG-66
export function powThrowsOnANegativeIntExponent() {
    assert.throws(() => pow(2n, -1n), RangeError);
}
