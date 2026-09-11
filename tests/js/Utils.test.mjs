// Tests for std/core/src/Js/Utils.mjs, the JavaScript companion behind the
// Js.Utils facade (Js/Utils.zel). There is no other harness covering the .mjs
// companions — this repo's tests are otherwise all Rust, exercised through
// `cargo test`, which never loads a .mjs — so this one runs on Node's own
// built-in test runner and needs no dependency:
//
//   node --test 'tests/js/*.test.mjs'
//
// `lt`/`le`/`gt`/`ge`/`compare` and `append` are declared `a -> a -> ...`
// (BUG-20), a type their JavaScript cannot honour: handed a value of a
// user-defined union type, `_Utils_cmp` and `append` read undefined tuple and
// list fields off it and returned a nonsense answer instead of failing. The
// fix is a guard that admits only the shapes this file can actually read and
// throws on everything else.
//
// Two kinds of test live below, and they are labelled where they sit:
//
//   PINS  — verified red against the file as it stood before the guard: either
//           the unguarded original, or a guard keyed on the presence of a `$`
//           field alone. These are the fix.
//   GUARD — passes with and without the guard. These pin that the fix did not
//           narrow what the file used to accept; they prove nothing about the
//           fix itself, so do not read a green one as a pinned new behaviour.
//
// Values are shaped the way docs/spec/interop.md says they cross to
// JavaScript, or the way this file's own constructors build them, since code
// generation does not exist yet to produce either from real Zelkova source.

import assert from 'node:assert/strict';
import { test } from 'node:test';
import {
    compare, lt, le, gt, ge, append,
} from '../../std/core/src/Js/Utils.mjs';

// A stand-in for `Colour = Red | Blue`, encoded as
// docs/spec/interop.md#a-union-crosses-as-a-tagged-value specifies.
const Red = { $: 'Red' };
const Blue = { $: 'Blue' };
const Rgb = (r, g, b) => ({ $: 'Rgb', a: r, b: g, c: b });

// The two encodings `_Utils_Tuple2`/`_Utils_Tuple3` build in this file.
const prodPair = (a, b) => ({ a, b });
const debugPair = (a, b) => ({ $: '#2', a, b });
const prodTriple = (a, b, c) => ({ a, b, c });
const debugTriple = (a, b, c) => ({ $: '#3', a, b, c });

// A cons list in the encoding `__List_Cons` used to build, which `append`'s
// deleted walk was written for.
const nil = { $: 0 };
const cons = (h, t) => ({ $: 1, a: h, b: t });

const cmpError = /compare: can only compare /;
const appendError = /append: can only append two Strings/;

// COMPARE — what it accepts

test('GUARD compare orders numbers', () => {
    assert.equal(compare(1, 2), -1);
    assert.equal(compare(2, 2), 0);
    assert.equal(compare(3, 2), 1);
});

test('GUARD compare orders strings', () => {
    assert.equal(compare('a', 'b'), -1);
    assert.equal(compare('b', 'b'), 0);
});

test('GUARD compare orders tuples in the PROD encoding', () => {
    assert.equal(compare(prodPair(1, 2), prodPair(1, 3)), -1);
    assert.equal(compare(prodPair(1, 2), prodPair(1, 2)), 0);
    assert.equal(compare(prodTriple(1, 2, 3), prodTriple(1, 2, 2)), 1);
});

test('PINS compare orders tuples in the DEBUG encoding', () => {
    // _Utils_Tuple2__DEBUG stamps `$: '#2'` on a perfectly legitimate tuple.
    // A guard keyed on the mere presence of `$` rejects it.
    assert.equal(compare(debugPair(1, 2), debugPair(1, 3)), -1);
    assert.equal(compare(debugTriple(1, 2, 3), debugTriple(1, 2, 3)), 0);
});

test('GUARD compare stops at a 2-tuple instead of reading a third field', () => {
    // The walk now stops at the arity rather than recursing into `x.c`/`y.c`,
    // which are both `undefined` on a pair. The old walk reached the same
    // answer the long way round, so this only pins that it still does.
    assert.equal(compare(prodPair(1, 2), prodPair(1, 2)), 0);
    assert.equal(compare(prodPair(2, 1), prodPair(1, 1)), 1);
});

// COMPARE — what it refuses

test('PINS compare refuses a union value', () => {
    assert.throws(() => compare(Red, Blue), cmpError);
    assert.throws(() => compare(Rgb(1, 2, 3), Rgb(1, 2, 4)), cmpError);
});

test('PINS compare refuses a union against a primitive, either way round', () => {
    // The old guard sat behind the primitive branch, so only one order threw.
    assert.throws(() => compare(Red, 1), cmpError);
    assert.throws(() => compare(1, Red), cmpError);
});

test('PINS compare refuses an array', () => {
    // docs/spec/interop.md says a tuple and a list each cross as an array.
    // This file reads the object encoding, so an array is unreadable here —
    // and untagged, so a `$`-keyed guard waves it through and answers EQ.
    assert.throws(() => compare([1, 2], [1, 3]), cmpError);
    assert.throws(() => compare([1, 2, 3], [1, 2, 4]), cmpError);
});

test('PINS compare refuses a record', () => {
    // _Utils_update builds plain untagged objects of the record's own fields.
    assert.throws(() => compare({ x: 1, y: 2 }, { x: 1, y: 3 }), cmpError);
});

test('PINS compare refuses a function', () => {
    assert.throws(() => compare(() => 1, () => 2), cmpError);
});

test('PINS compare refuses null and undefined', () => {
    assert.throws(() => compare(null, null), cmpError);
    assert.throws(() => compare(undefined, undefined), cmpError);
});

test('PINS compare refuses tuples of different sizes', () => {
    assert.throws(() => compare(prodPair(1, 2), prodTriple(1, 2, 3)), cmpError);
});

test('PINS compare refuses a union nested inside a tuple', () => {
    // Here it is the recursion that has to catch it, not the entry point.
    assert.throws(() => compare(prodPair(1, Red), prodPair(1, Blue)), cmpError);
    assert.throws(
        () => compare(prodTriple(1, 2, Red), prodTriple(1, 2, Blue)),
        cmpError,
    );
});

test('PINS the comparison operators refuse what compare refuses', () => {
    for (const op of [lt, le, gt, ge]) {
        assert.throws(() => op(Red, Blue), cmpError);
        assert.throws(() => op([1, 2], [1, 3]), cmpError);
    }
});

test('GUARD the comparison operators still answer on numbers', () => {
    assert.equal(lt(1, 2), true);
    assert.equal(le(2, 2), true);
    assert.equal(gt(1, 2), false);
    assert.equal(ge(2, 2), true);
});

// APPEND

test('GUARD append concatenates two strings', () => {
    assert.equal(append('ab', 'cd'), 'abcd');
    assert.equal(append('', ''), '');
});

test('PINS append refuses a string and a non-string', () => {
    assert.throws(() => append('ab', 1), appendError);
    assert.throws(() => append(1, 'ab'), appendError);
    assert.throws(() => append('ab', Red), appendError);
});

test('PINS append refuses a union value', () => {
    assert.throws(() => append(Red, Blue), appendError);
});

test('PINS append refuses a cons list, and says lists are not implemented', () => {
    // The deleted walk was written for exactly this encoding and called an
    // undefined `__List_Cons` (BUG-24). Whatever it did, it never concatenated
    // two lists — so the error has to name lists as absent rather than claim
    // they are supported.
    assert.throws(() => append(cons(1, nil), cons(2, nil)), appendError);
    assert.throws(() => append(nil, nil), appendError);
});

test('PINS append refuses arrays rather than returning one of them', () => {
    // The array encoding docs/spec/interop.md gives a list. Untagged, so a
    // `$`-keyed guard passes it to the walk, which returned `ys` unchanged.
    assert.throws(() => append([1, 2], [3, 4]), appendError);
});

test('PINS append refuses a record and a function', () => {
    assert.throws(() => append({ x: 1 }, { x: 2 }), appendError);
    assert.throws(() => append(() => 1, () => 2), appendError);
});
