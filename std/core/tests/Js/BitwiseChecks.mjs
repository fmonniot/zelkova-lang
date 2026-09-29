// The JavaScript checks over std/core/src/Js/Bitwise.mjs, the companion behind
// the Js.Bitwise facade (Js/Bitwise.zel).
//
// This is the companion of the test facade Js.BitwiseChecks
// (tests/Js/BitwiseChecks.zel), laid out as docs/spec/interop.md's "Testing a
// companion" describes. Each export is one check, declared there as
// `Task (Result Failure ())`: it returns nothing when the check holds, and a
// failed assertion throws, which the facade's wrapper turns into
// `Err (Threw ..)`. Js.BitwiseTests (tests/Js/BitwiseTests.zel) exposes one
// `Test` per check, so `zelkova test std/core` runs them.
//
// Every operand and every result here is an `Int`, a `BigInt` holding a 64-bit
// signed two's-complement value
// (docs/spec/interop.md#which-types-may-cross-the-boundary). `BigInt` has no
// `>>>` and no width of its own, so `Bitwise.mjs` supplies one: `DEC-16`
// decision 6 has `shiftRightZfBy` read its operand as an unsigned 64-bit
// pattern, shift zeros in from the left, and read the result back as a signed
// `Int`.
//
// Two kinds of check live below, following the PINS/GUARD convention
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
// A shift count is read clamped into `0 .. 64` (`DEC-16` decision 6,
// `LANG-64`), so a negative count reads as 0, the identity.

import assert from 'node:assert/strict';
import {
    and, or, xor, complement, shiftLeftBy, shiftRightBy, shiftRightZfBy,
} from '../../src/Js/Bitwise.mjs';

const INT_MAX = 9223372036854775807n;
const INT_MIN = -9223372036854775808n;

// BASIC OPERATIONS

// GUARD and, or and xor combine all 64 bits
export function andOrXorCombineAll64Bits() {
    assert.equal(and(12n, 10n), 8n);
    assert.equal(or(12n, 10n), 14n);
    assert.equal(xor(12n, 10n), 6n);

    assert.equal(and(INT_MAX, INT_MIN), 0n);
    assert.equal(or(INT_MAX, INT_MIN), -1n);
    assert.equal(xor(INT_MAX, INT_MIN), -1n);
}

// GUARD complement flips all 64 bits
export function complementFlipsAll64Bits() {
    assert.equal(complement(0n), -1n);
    assert.equal(complement(INT_MAX), INT_MIN);
    assert.equal(complement(INT_MIN), INT_MAX);
}

// BIT SHIFTS

// GUARD shiftLeftBy multiplies by a power of two
export function shiftLeftByMultipliesByAPowerOfTwo() {
    assert.equal(shiftLeftBy(1n, 5n), 10n);
    assert.equal(shiftLeftBy(5n, 1n), 32n);
    assert.equal(shiftLeftBy(40n, 1n), 1099511627776n);
}

// PINS shiftLeftBy wraps at 64 bits
export function shiftLeftByWrapsAt64Bits() {
    assert.equal(shiftLeftBy(1n, INT_MAX), -2n);
    assert.equal(shiftLeftBy(63n, 1n), INT_MIN);
}

// docs/decisions/dec-16.md decision 6: a count is a number of positions, and a
// 64-bit pattern moved 64 of them has nothing left. JavaScript's operators
// masked the count to five bits and answered `1` to `1 >>> 32`. Both of
// these fill with zero, so "nothing left" reads as `0`; `shiftRightBy`'s own
// version of the same claim is the GUARD just above, since it fills with the
// topmost bit instead.
// PINS a shift of 64 or more is 0, for shiftLeftBy and shiftRightZfBy
export function aShiftOf64OrMoreIsZero() {
    assert.equal(shiftLeftBy(64n, 1n), 0n);
    assert.equal(shiftRightZfBy(64n, -1n), 0n);
}

// docs/decisions/dec-16.md decision 6 (LANG-64): a shift count is read
// clamped into `0 .. 64`, so a count below 0 reads as 0, the identity.
// Verified red against the pre-LANG-64 companion, where a negative count
// instead reversed the shift's direction: `shiftLeftBy(-1n, 8n)` was `4n`
// (a right shift), `shiftRightBy(-1n, 32n)` was `64n` (a left shift), and
// `shiftRightZfBy(-1n, -32n)` was `-64n` (a negative answer out of a
// zero-fill shift, the reversal happening after the operand was already
// read unsigned).
// PINS a negative count reads as 0, the identity
export function aNegativeCountReadsAsZero() {
    assert.equal(shiftLeftBy(-1n, 8n), 8n);
    assert.equal(shiftRightBy(-1n, 32n), 32n);
    assert.equal(shiftRightZfBy(-1n, -32n), -32n);
}

// GUARD shiftRightBy fills with the topmost bit
export function shiftRightByFillsWithTheTopmostBit() {
    assert.equal(shiftRightBy(1n, 32n), 16n);
    assert.equal(shiftRightBy(2n, 32n), 8n);
    assert.equal(shiftRightBy(1n, -32n), -16n);
    assert.equal(shiftRightBy(62n, INT_MIN), -2n);
}

// DEC-16 decision 6 corrected: a shift of 64 or more leaves nothing of the
// pattern, but `shiftRightBy` fills with the operand's own topmost bit
// rather than zero, so that reads as the bit copied across all 64
// positions — `-1` for a negative operand — not `0` the way the other two
// read it. Already true of the pre-LANG-64 bound (it clamped the positive
// side the same way); this GUARDs the corrected claim rather than pinning a
// behaviour change.
// GUARD shiftRightBy at 64 or more fills from the topmost bit, not 0
export function shiftRightByAt64OrMoreFillsFromTheTopmostBit() {
    assert.equal(shiftRightBy(64n, -32n), -1n);
    assert.equal(shiftRightBy(100n, -32n), -1n);
    assert.equal(shiftRightBy(64n, 32n), 0n);
}

// PINS shiftRightZfBy fills with zeros from bit 63
export function shiftRightZfByFillsWithZeros() {
    assert.equal(shiftRightZfBy(1n, 32n), 16n);
    assert.equal(shiftRightZfBy(2n, 32n), 8n);
    assert.equal(shiftRightZfBy(1n, -32n), 9223372036854775792n);
    // A 1-bit value has nothing left once it has been shifted past its own
    // width, the same claim decision 6 makes for a full 64-bit pattern, just
    // reached at a smaller offset because there was less to shift out.
    assert.equal(shiftRightZfBy(32n, 1n), 0n);
}

// The outer mask is a no-op for every offset of 1 or more — a zero-filled
// shift leaves at most 63 significant bits — and it is what makes an offset of
// 0 the identity rather than `2^64 - 1`.
// PINS shiftRightZfBy by 0 is the identity
export function shiftRightZfByZeroIsTheIdentity() {
    assert.equal(shiftRightZfBy(0n, -1n), -1n);
    assert.equal(shiftRightZfBy(0n, INT_MIN), INT_MIN);
    assert.equal(shiftRightZfBy(0n, 7n), 7n);
}

// Every result is an `Int`, which the 32-bit `>>>` could not promise: its
// result was unsigned, so `shiftRightZfBy 1 -32` used to land outside the
// range the type held.
// PINS every shift lands back in the Int range
export function everyShiftLandsInTheIntRange() {
    for (const offset of [0n, 1n, 31n, 32n, 63n, 64n]) {
        for (const a of [INT_MIN, -32n, -1n, 0n, 1n, INT_MAX]) {
            for (const shift of [shiftLeftBy, shiftRightBy, shiftRightZfBy]) {
                const result = shift(offset, a);
                assert.equal(typeof result, 'bigint');
                assert.ok(result >= INT_MIN && result <= INT_MAX);
            }
        }
    }
}

// Review finding on the PR that introduced these companions: `<<`/`>>` build
// their unmasked result before the outer `BigInt.asIntN(64, ..)` mask can
// bound anything, so an offset large enough (an ordinary in-range `Int`, well
// short of `Int`'s own bound) makes that intermediate allocation throw
// `RangeError: Maximum BigInt size exceeded` before the mask ever runs. Every
// shift below used to throw; now it must not. A positive large-magnitude
// offset clamps to 64, the same as an offset of exactly 64 (DEC-16 decision
// 6); a negative one clamps to 0 (LANG-64), the identity, not to an offset
// of `-64` the way the pre-LANG-64 reversal reading would have answered.
// PINS a large-magnitude offset does not throw, clamps to 64 when positive and
// to the identity when negative
export function aLargeOffsetDoesNotThrowAndClamps() {
    const LARGE = 2_000_000_000n;

    for (const a of [INT_MIN, -1n, 0n, 1n, INT_MAX]) {
        assert.doesNotThrow(() => shiftLeftBy(LARGE, a));
        assert.doesNotThrow(() => shiftLeftBy(-LARGE, a));
        assert.doesNotThrow(() => shiftRightBy(LARGE, a));
        assert.doesNotThrow(() => shiftRightBy(-LARGE, a));
        assert.doesNotThrow(() => shiftRightZfBy(LARGE, a));
        assert.doesNotThrow(() => shiftRightZfBy(-LARGE, a));

        assert.equal(shiftLeftBy(LARGE, a), shiftLeftBy(64n, a));
        assert.equal(shiftLeftBy(-LARGE, a), a);

        assert.equal(shiftRightBy(LARGE, a), shiftRightBy(64n, a));
        assert.equal(shiftRightBy(-LARGE, a), a);

        assert.equal(shiftRightZfBy(LARGE, a), shiftRightZfBy(64n, a));
        assert.equal(shiftRightZfBy(-LARGE, a), a);
    }
}
