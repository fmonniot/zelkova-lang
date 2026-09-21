// The JavaScript checks over std/core/src/Js/Basics.mjs, the companion behind
// the Js.Basics facade (Js/Basics.zel).
//
// Where this file sits is docs/spec/interop.md's "Testing a companion": a
// companion's test is a facade under the package's own tests/ root, carrying
// one companion per target. The facade half — tests/Js/BasicsChecks.zel,
// declaring each check as `Task (Result Failure ())` — is not written, for
// the same reason UtilsChecks.mjs gives: `foreign` does not parse and there
// is no runner to find a `Test`. The checks below are registered with Node's
// own test runner rather than exported from this module, and Node is pointed
// at them directly:
//
//   node --test 'std/core/tests/**/*.mjs'
//
// Rewriting them as plain exports is what lands with the facade half.
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
// Two kinds of test live below, following the PINS/GUARD convention
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
import { test } from 'node:test';
import {
    add, sub, mul, idiv, modBy, remainderBy, round, floor, ceiling, truncate,
} from '../../src/Js/Basics.mjs';

// INT ARITHMETIC (LANG-56)

const INT_MAX = 9223372036854775807n;
const INT_MIN = -9223372036854775808n;

test('PINS add wraps at 64 bits', () => {
    assert.equal(add(INT_MAX, 1n), INT_MIN);
    assert.equal(add(1n, 2n), 3n);
});

test('PINS sub wraps at 64 bits', () => {
    assert.equal(sub(INT_MIN, 1n), INT_MAX);
    assert.equal(sub(3n, 1n), 2n);
});

test('PINS mul wraps at 64 bits and stays exact past 2^53', () => {
    assert.equal(mul(INT_MAX, 2n), -2n);
    assert.equal(mul(4294967296n, 4294967296n), 0n);
    assert.equal(mul(123456789n, 987654321n), 121932631112635269n);
});

// `add`, `sub` and `mul` back `Float` arithmetic as well as `Int`: Basics.zel
// declares each of them `a -> a -> a`. A number operand keeps IEEE's answer.
test('GUARD add, sub and mul still compute on Floats', () => {
    assert.equal(add(3.14, 3.14), 6.28);
    assert.equal(sub(1.5, 0.25), 1.25);
    assert.equal(mul(1.5, 2.0), 3.0);
    assert.equal(add(1.0 / 0.0, 1.0), Infinity);
});

// `BigInt` division and `%` both throw on a zero divisor, so the three zeros
// below are the explicit guards in Basics.mjs rather than anything falling out
// of the arithmetic.
test('PINS idiv divides by zero to 0', () => {
    assert.equal(idiv(1n, 0n), 0n);
    assert.equal(idiv(-1n, 0n), 0n);
    assert.equal(idiv(0n, 0n), 0n);
});

// The answer here was already correct before LANG-56; only its type moved.
test('PINS idiv truncates toward zero on a non-zero divisor', () => {
    assert.equal(idiv(7n, 2n), 3n);
    assert.equal(idiv(-7n, 2n), -3n);
});

test('PINS idiv wraps the one division that leaves the range', () => {
    assert.equal(idiv(INT_MIN, -1n), INT_MIN);
});

test('PINS modBy 0 n is 0', () => {
    assert.equal(modBy(0n, 5n), 0n);
    assert.equal(modBy(0n, -5n), 0n);
    assert.equal(modBy(0n, 0n), 0n);
});

// `%` and the sign comparisons read a `BigInt` operand the way they read a
// number, so this arithmetic answered the same before LANG-56.
test('GUARD modBy keeps its sign-correcting arithmetic for a non-zero modulus', () => {
    assert.equal(modBy(3n, 5n), 2n);
    assert.equal(modBy(-3n, 5n), -1n);
    assert.equal(modBy(3n, -5n), 1n);
    assert.equal(modBy(-3n, -5n), -2n);
});

test('PINS remainderBy 0 n is 0', () => {
    assert.equal(remainderBy(0n, 5n), 0n);
    assert.equal(remainderBy(0n, -5n), 0n);
    assert.equal(remainderBy(0n, 0n), 0n);
});

test('GUARD remainderBy keeps the sign of the dividend on a non-zero divisor', () => {
    assert.equal(remainderBy(3n, 5n), 2n);
    assert.equal(remainderBy(-3n, 5n), 2n);
    assert.equal(remainderBy(3n, -5n), -2n);
    assert.equal(remainderBy(-3n, -5n), -2n);
});

// FLOAT -> INT CONVERSIONS (BUG-25, LANG-56)

test('PINS round wraps nan and both infinities to 0', () => {
    assert.equal(round(NaN), 0n);
    assert.equal(round(Infinity), 0n);
    assert.equal(round(-Infinity), 0n);
});

test('PINS floor wraps nan and both infinities to 0', () => {
    assert.equal(floor(NaN), 0n);
    assert.equal(floor(Infinity), 0n);
    assert.equal(floor(-Infinity), 0n);
});

test('PINS ceiling wraps nan and both infinities to 0', () => {
    assert.equal(ceiling(NaN), 0n);
    assert.equal(ceiling(Infinity), 0n);
    assert.equal(ceiling(-Infinity), 0n);
});

test('PINS truncate wraps nan and both infinities to 0', () => {
    assert.equal(truncate(NaN), 0n);
    assert.equal(truncate(Infinity), 0n);
    assert.equal(truncate(-Infinity), 0n);
});

// `1.0e20` is an integer value outside the 64-bit range, so
// Math.round/Math.floor/Math.ceil/Math.trunc all return it unchanged and the
// wrap is the only thing that brings any of the four into `Int`'s range.
const WRAPPED_1E20 = BigInt.asIntN(64, BigInt(1.0e20));

test('PINS round wraps a finite value outside the 64-bit range into it', () => {
    assert.equal(round(1.0e20), WRAPPED_1E20);
    assert.equal(typeof round(1.0e20), 'bigint');
});

test('PINS floor wraps a finite value outside the 64-bit range into it', () => {
    assert.equal(floor(1.0e20), WRAPPED_1E20);
    assert.equal(typeof floor(1.0e20), 'bigint');
});

test('PINS ceiling wraps a finite value outside the 64-bit range into it', () => {
    assert.equal(ceiling(1.0e20), WRAPPED_1E20);
    assert.equal(typeof ceiling(1.0e20), 'bigint');
});

test('PINS truncate wraps a finite value outside the 64-bit range into it', () => {
    assert.equal(truncate(1.0e20), WRAPPED_1E20);
    assert.equal(typeof truncate(1.0e20), 'bigint');
});

// `2^53` is where a JavaScript number stops being exact on integers, and the
// four conversions carry a value past it because the result is a `BigInt`.
test('PINS a value past 2^53 converts exactly', () => {
    assert.equal(round(1.0e18), 1000000000000000000n);
    assert.equal(floor(1.0e18), 1000000000000000000n);
    assert.equal(ceiling(1.0e18), 1000000000000000000n);
    assert.equal(truncate(1.0e18), 1000000000000000000n);
});

// The rounding directions were already correct before LANG-56; only the
// result type moved.
test('PINS round, floor and ceiling keep their rounding direction in range', () => {
    assert.equal(round(1.5), 2n);
    assert.equal(round(-1.5), -1n);
    assert.equal(floor(1.9), 1n);
    assert.equal(floor(-1.1), -2n);
    assert.equal(ceiling(1.1), 2n);
    assert.equal(ceiling(-1.9), -1n);
});

test('PINS truncate keeps rounding toward zero', () => {
    assert.equal(truncate(1.9), 1n);
    assert.equal(truncate(-1.9), -1n);
});
