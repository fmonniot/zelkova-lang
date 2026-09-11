// Tests for Utils.mjs, the JavaScript companion behind the Js.Utils facade
// (Js/Utils.zel). There is no other harness covering the .mjs files yet —
// this repo's tests are otherwise all Rust, exercised through `cargo test`
// — so this one runs on Node's own built-in test runner and needs no
// dependency:
//
//   node --test std/core/src/Js/Utils.test.mjs
//
// `lt`/`le`/`gt`/`ge`/`compare` and `append` are declared `a -> a -> ...`
// (BUG-20), a type their JavaScript cannot actually honour: handed a value
// of a user-defined union type, `_Utils_cmp` and `append` used to read
// undefined tuple/list fields off it and return a nonsense answer instead
// of failing. These tests pin the fix — a thrown Error instead — using
// object literals shaped the way docs/spec/interop.md#a-union-crosses-as-a-tagged-value
// says a union value crosses to JavaScript, since code generation does not
// exist yet to produce one from real Zelkova source.

import assert from 'node:assert/strict';
import { test } from 'node:test';
import { compare, lt, le, gt, ge, append } from './Utils.mjs';

// A stand-in for `Colour = Red | Blue`, encoded the way a union value is
// specified to cross the JS boundary.
const Red = { $: 'Red' };
const Blue = { $: 'Blue' };

test('compare accepts numbers', () => {
  assert.equal(compare(1, 2), -1);
  assert.equal(compare(2, 2), 0);
  assert.equal(compare(3, 2), 1);
});

test('compare accepts tuples of comparables', () => {
  // { a: .., b: .. } is how _Utils_Tuple2 encodes a 2-tuple.
  assert.equal(compare({ a: 1, b: 2 }, { a: 1, b: 3 }), -1);
});

test('compare rejects a union-typed value instead of comparing its fields', () => {
  assert.throws(() => compare(Red, Blue), /user-defined type/);
});

test('lt/le/gt/ge reject a union-typed value', () => {
  assert.throws(() => lt(Red, Blue), /user-defined type/);
  assert.throws(() => le(Red, Blue), /user-defined type/);
  assert.throws(() => gt(Red, Blue), /user-defined type/);
  assert.throws(() => ge(Red, Blue), /user-defined type/);
});

test('append concatenates two strings', () => {
  assert.equal(append('foo', 'bar'), 'foobar');
});

test('append rejects a string paired with a non-string', () => {
  assert.throws(() => append('foo', Red), /String/);
});

test('append rejects a union-typed value instead of appending its fields', () => {
  assert.throws(() => append(Red, Blue), /user-defined type/);
});
