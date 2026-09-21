// The JavaScript checks over std/core/src/Js/Bitwise.mjs, the companion behind
// the Js.Bitwise facade (Js/Bitwise.zel).
//
// Where this file sits is docs/spec/interop.md's "Testing a companion": a
// companion's test is a facade under the package's own tests/ root, carrying
// one companion per target. The facade half — tests/Js/BitwiseChecks.zel,
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
// Every operand and every result here is an `Int`, a `BigInt` holding a 64-bit
// signed two's-complement value
// (docs/spec/interop.md#which-types-may-cross-the-boundary). `BigInt` has no
// `>>>` and no width of its own, so `Bitwise.mjs` supplies one: `DEC-16`
// decision 6 has `shiftRightZfBy` read its operand as an unsigned 64-bit
// pattern, shift zeros in from the left, and read the result back as a signed
// `Int`.
//
// Two kinds of test live below, following the PINS/GUARD convention
// UtilsChecks.mjs documents and uses:
//
//   PINS  — verified red against the pre-LANG-56 companion, which used a
//           JavaScript bitwise operator and so answered on the low 32 bits.
//           These are the fix.
//   GUARD — passes with and without the fix, because `&`, `|`, `^`, `~`, `<<`
//           and `>>` read a `BigInt` operand as a two's-complement pattern
//           already and agree with the 64-bit answer wherever nothing leaves
//           the range. These prove nothing about the fix itself, so do not
//           read a green one as a pinned new behaviour.
//
// What a *negative* shift count means is unsettled — `DEC-16` decision 6
// records the question — so nothing below passes one.

import assert from 'node:assert/strict';
import { test } from 'node:test';
import {
    and, or, xor, complement, shiftLeftBy, shiftRightBy, shiftRightZfBy,
} from '../../src/Js/Bitwise.mjs';

const INT_MAX = 9223372036854775807n;
const INT_MIN = -9223372036854775808n;

// BASIC OPERATIONS

test('GUARD and, or and xor combine all 64 bits', () => {
    assert.equal(and(12n, 10n), 8n);
    assert.equal(or(12n, 10n), 14n);
    assert.equal(xor(12n, 10n), 6n);

    assert.equal(and(INT_MAX, INT_MIN), 0n);
    assert.equal(or(INT_MAX, INT_MIN), -1n);
    assert.equal(xor(INT_MAX, INT_MIN), -1n);
});

test('GUARD complement flips all 64 bits', () => {
    assert.equal(complement(0n), -1n);
    assert.equal(complement(INT_MAX), INT_MIN);
    assert.equal(complement(INT_MIN), INT_MAX);
});

// BIT SHIFTS

test('GUARD shiftLeftBy multiplies by a power of two', () => {
    assert.equal(shiftLeftBy(1n, 5n), 10n);
    assert.equal(shiftLeftBy(5n, 1n), 32n);
    assert.equal(shiftLeftBy(40n, 1n), 1099511627776n);
});

test('PINS shiftLeftBy wraps at 64 bits', () => {
    assert.equal(shiftLeftBy(1n, INT_MAX), -2n);
    assert.equal(shiftLeftBy(63n, 1n), INT_MIN);
});

// docs/decisions/dec-16.md decision 6: a count is a number of positions, and a
// 64-bit pattern moved 64 of them has nothing left. JavaScript's operators
// masked the count to five bits and answered `1` to `1 >>> 32`.
test('PINS a shift of 64 or more is 0', () => {
    assert.equal(shiftLeftBy(64n, 1n), 0n);
    assert.equal(shiftRightZfBy(64n, -1n), 0n);
    assert.equal(shiftRightZfBy(32n, 1n), 0n);
});

test('GUARD shiftRightBy fills with the topmost bit', () => {
    assert.equal(shiftRightBy(1n, 32n), 16n);
    assert.equal(shiftRightBy(2n, 32n), 8n);
    assert.equal(shiftRightBy(1n, -32n), -16n);
    assert.equal(shiftRightBy(62n, INT_MIN), -2n);
});

test('PINS shiftRightZfBy fills with zeros from bit 63', () => {
    assert.equal(shiftRightZfBy(1n, 32n), 16n);
    assert.equal(shiftRightZfBy(2n, 32n), 8n);
    assert.equal(shiftRightZfBy(1n, -32n), 9223372036854775792n);
});

// The outer mask is a no-op for every offset of 1 or more — a zero-filled
// shift leaves at most 63 significant bits — and it is what makes an offset of
// 0 the identity rather than `2^64 - 1`.
test('PINS shiftRightZfBy by 0 is the identity', () => {
    assert.equal(shiftRightZfBy(0n, -1n), -1n);
    assert.equal(shiftRightZfBy(0n, INT_MIN), INT_MIN);
    assert.equal(shiftRightZfBy(0n, 7n), 7n);
});

// Every result is an `Int`, which the 32-bit `>>>` could not promise: its
// result was unsigned, so `shiftRightZfBy 1 -32` used to land outside the
// range the type held.
test('PINS every shift lands back in the Int range', () => {
    for (const offset of [0n, 1n, 31n, 32n, 63n, 64n]) {
        for (const a of [INT_MIN, -32n, -1n, 0n, 1n, INT_MAX]) {
            for (const shift of [shiftLeftBy, shiftRightBy, shiftRightZfBy]) {
                const result = shift(offset, a);
                assert.equal(typeof result, 'bigint');
                assert.ok(result >= INT_MIN && result <= INT_MAX);
            }
        }
    }
});

// Review finding on the PR that introduced these companions: `<<`/`>>` build
// their unmasked result before the outer `BigInt.asIntN(64, ..)` mask can
// bound anything, so an offset large enough (an ordinary in-range `Int`, well
// short of `Int`'s own bound) makes that intermediate allocation throw
// `RangeError: Maximum BigInt size exceeded` before the mask ever runs. Every
// shift below used to throw; now it must not, and DEC-16 decision 6 already
// predicts the answer: an offset whose magnitude is 64 or more behaves like
// an offset of exactly 64 in the same direction (a negative offset still
// reverses direction — LANG-64 owns what that means, this only bounds it).
test('PINS a large-magnitude offset does not throw and matches an offset of exactly 64 in the same direction', () => {
    const LARGE = 2_000_000_000n;

    for (const a of [INT_MIN, -1n, 0n, 1n, INT_MAX]) {
        assert.doesNotThrow(() => shiftLeftBy(LARGE, a));
        assert.doesNotThrow(() => shiftLeftBy(-LARGE, a));
        assert.doesNotThrow(() => shiftRightBy(LARGE, a));
        assert.doesNotThrow(() => shiftRightBy(-LARGE, a));
        assert.doesNotThrow(() => shiftRightZfBy(LARGE, a));
        assert.doesNotThrow(() => shiftRightZfBy(-LARGE, a));

        assert.equal(shiftLeftBy(LARGE, a), shiftLeftBy(64n, a));
        assert.equal(shiftLeftBy(-LARGE, a), shiftLeftBy(-64n, a));

        assert.equal(shiftRightBy(LARGE, a), shiftRightBy(64n, a));
        assert.equal(shiftRightBy(-LARGE, a), shiftRightBy(-64n, a));

        assert.equal(shiftRightZfBy(LARGE, a), shiftRightZfBy(64n, a));
        assert.equal(shiftRightZfBy(-LARGE, a), shiftRightZfBy(-64n, a));
    }
});
