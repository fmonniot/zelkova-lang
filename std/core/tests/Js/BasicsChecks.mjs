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

import assert from 'node:assert/strict';
import { test } from 'node:test';
import { idiv, modBy, remainderBy } from '../../src/Js/Basics.mjs';

test('idiv divides by zero to 0', () => {
    assert.equal(idiv(1, 0), 0);
    assert.equal(idiv(-1, 0), 0);
    assert.equal(idiv(0, 0), 0);
});

test('idiv truncates toward zero on a non-zero divisor', () => {
    assert.equal(idiv(7, 2), 3);
    assert.equal(idiv(-7, 2), -3);
});

test('modBy 0 n is 0', () => {
    assert.equal(modBy(0, 5), 0);
    assert.equal(modBy(0, -5), 0);
    assert.equal(modBy(0, 0), 0);
});

test('modBy keeps its sign-correcting arithmetic for a non-zero modulus', () => {
    assert.equal(modBy(3, 5), 2);
    assert.equal(modBy(-3, 5), -1);
    assert.equal(modBy(3, -5), 1);
    assert.equal(modBy(-3, -5), -2);
});

test('remainderBy 0 n is 0', () => {
    assert.equal(remainderBy(0, 5), 0);
    assert.equal(remainderBy(0, -5), 0);
    assert.equal(remainderBy(0, 0), 0);
});

test('remainderBy keeps JavaScript % for a non-zero divisor', () => {
    assert.equal(remainderBy(3, 5), 2);
    assert.equal(remainderBy(-3, 5), 2);
    assert.equal(remainderBy(3, -5), -2);
    assert.equal(remainderBy(-3, -5), -2);
});
