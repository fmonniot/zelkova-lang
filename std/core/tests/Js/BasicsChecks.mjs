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
// docs/spec/evaluation-semantics.md#an-operation-with-no-answer defines
// `n // 0`, `modBy 0 n` and `remainderBy 0 n` to each be `0`, which keeps
// those three operations total. Before BUG-24's fix, `modBy 0 n` called an
// undefined crash helper (a `ReferenceError`) and `remainderBy 0 n` returned
// `nan`. `idiv` (`//`) was already correct; it is checked here too as a
// regression test, per the ticket's acceptance wording.
//
// docs/spec/evaluation-semantics.md#converting-a-float-to-an-int defines a
// conversion to `Int` as rounding and then wrapping into 32 bits, with `nan`
// and both infinities landing on 0. Before BUG-25's fix, `round`, `floor`
// and `ceiling` were bare aliases for `Math.round`/`Math.floor`/`Math.ceil`
// and returned that JavaScript number unwrapped; `truncate` (`n | 0`) was
// already correct and is checked here too as a regression test.
//
// Two kinds of test live below, following the PINS/GUARD convention
// UtilsChecks.mjs documents and uses:
//
//   PINS  — verified red against the file as it stood before the relevant
//           fix (BUG-24 for idiv/modBy/remainderBy, BUG-25 for
//           round/floor/ceiling/truncate). These are the fix.
//   GUARD — passes with and without the fix. These pin that the fix did not
//           disturb already-correct behaviour; they prove nothing about the
//           fix itself, so do not read a green one as a pinned new behaviour.

import assert from 'node:assert/strict';
import { test } from 'node:test';
import {
    idiv, modBy, remainderBy, round, floor, ceiling, truncate,
} from '../../src/Js/Basics.mjs';

test('GUARD idiv divides by zero to 0', () => {
    assert.equal(idiv(1, 0), 0);
    assert.equal(idiv(-1, 0), 0);
    assert.equal(idiv(0, 0), 0);
});

test('GUARD idiv truncates toward zero on a non-zero divisor', () => {
    assert.equal(idiv(7, 2), 3);
    assert.equal(idiv(-7, 2), -3);
});

test('PINS modBy 0 n is 0', () => {
    assert.equal(modBy(0, 5), 0);
    assert.equal(modBy(0, -5), 0);
    assert.equal(modBy(0, 0), 0);
});

test('GUARD modBy keeps its sign-correcting arithmetic for a non-zero modulus', () => {
    assert.equal(modBy(3, 5), 2);
    assert.equal(modBy(-3, 5), -1);
    assert.equal(modBy(3, -5), 1);
    assert.equal(modBy(-3, -5), -2);
});

test('PINS remainderBy 0 n is 0', () => {
    assert.equal(remainderBy(0, 5), 0);
    assert.equal(remainderBy(0, -5), 0);
    assert.equal(remainderBy(0, 0), 0);
});

test('GUARD remainderBy keeps JavaScript % for a non-zero divisor', () => {
    assert.equal(remainderBy(3, 5), 2);
    assert.equal(remainderBy(-3, 5), 2);
    assert.equal(remainderBy(3, -5), -2);
    assert.equal(remainderBy(-3, -5), -2);
});

// FLOAT -> INT CONVERSIONS (BUG-25)

test('PINS round wraps nan and both infinities to 0', () => {
    assert.equal(round(NaN), 0);
    assert.equal(round(Infinity), 0);
    assert.equal(round(-Infinity), 0);
});

test('PINS floor wraps nan and both infinities to 0', () => {
    assert.equal(floor(NaN), 0);
    assert.equal(floor(Infinity), 0);
    assert.equal(floor(-Infinity), 0);
});

test('PINS ceiling wraps nan and both infinities to 0', () => {
    assert.equal(ceiling(NaN), 0);
    assert.equal(ceiling(Infinity), 0);
    assert.equal(ceiling(-Infinity), 0);
});

test('GUARD truncate already wraps nan and both infinities to 0', () => {
    assert.equal(truncate(NaN), 0);
    assert.equal(truncate(Infinity), 0);
    assert.equal(truncate(-Infinity), 0);
});

// `1.0e20 | 0` is `1661992960`: the low 32 bits of `1.0e20`, as a signed
// integer, per docs/spec/evaluation-semantics.md and the ticket's own table.
// Math.round/Math.floor/Math.ceil applied to 1.0e20 return 1.0e20 unchanged
// (it is already an integer value, just outside 32-bit range), so the wrap
// is the only thing that brings any of the three into `Int`'s range here.
const WRAPPED_1E20 = 1.0e20 | 0;

test('PINS round wraps a finite value outside the 32-bit range into it', () => {
    assert.equal(round(1.0e20), WRAPPED_1E20);
    assert.equal(Number.isInteger(round(1.0e20)), true);
});

test('PINS floor wraps a finite value outside the 32-bit range into it', () => {
    assert.equal(floor(1.0e20), WRAPPED_1E20);
    assert.equal(Number.isInteger(floor(1.0e20)), true);
});

test('PINS ceiling wraps a finite value outside the 32-bit range into it', () => {
    assert.equal(ceiling(1.0e20), WRAPPED_1E20);
    assert.equal(Number.isInteger(ceiling(1.0e20)), true);
});

test('GUARD truncate already wraps a finite value outside the 32-bit range', () => {
    assert.equal(truncate(1.0e20), WRAPPED_1E20);
});

test('GUARD round, floor and ceiling keep their rounding direction in range', () => {
    assert.equal(round(1.5), 2);
    assert.equal(round(-1.5), -1);
    assert.equal(floor(1.9), 1);
    assert.equal(floor(-1.1), -2);
    assert.equal(ceiling(1.1), 2);
    assert.equal(ceiling(-1.9), -1);
});

test('GUARD truncate keeps rounding toward zero', () => {
    assert.equal(truncate(1.9), 1);
    assert.equal(truncate(-1.9), -1);
});
