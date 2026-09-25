// A runtime check on the shape `javascript::emit` (src/compiler/javascript.rs) produces
// for a `case`, since GEN-10 is what makes the JavaScript backend emit one at all.
//
// This is not a companion check the way std/core/tests/Js/*.mjs are: it is not
// testing hand-written JavaScript behind a `module foreign` facade, it is testing the
// compiler's own generated code. Nothing writes that code to disk yet — GEN-13 is the
// build, GEN-14 the harness that would run its real output under node — so `label`
// below is a hand-copied instance of what `javascript::emit` answers today for the
// fixture in its own comment, kept in sync by hand with
// tests/javascript.rs's `a_case_on_a_three_constructor_union_is_nested_ifs_naming_each_tag`
// (which pins the same text `assert_eq!`) until GEN-13/14 lands and this can import real
// build output instead. GEN-10's acceptance asks for this much: a `case` over a
// three-constructor union returning each branch's value, and a value no branch matches
// aborting rather than returning `undefined`.
//
//   type Colour
//     = Red
//     | Green
//     | Blue
//
//   label : Colour -> Int
//   label c =
//     case c of
//       Red ->
//         1
//
//       Green ->
//         2
//
//       Blue ->
//         3
//
// node --test 'std/core/tests/**/*.mjs'

import assert from 'node:assert/strict';
import { test } from 'node:test';
import { $abort } from '../../../runtime/js/zelkova.mjs';

const Red = { $: "Red" };
const Green = { $: "Green" };
const Blue = { $: "Blue" };

function label(c) {
  return (() => {
  const $scrutinee = c;
  if ($scrutinee.$ === "Red") {
    return 1n;
  } else {
    if ($scrutinee.$ === "Green") {
      return 2n;
    } else {
      if ($scrutinee.$ === "Blue") {
        return 3n;
      } else {
        return $abort("`label`'s case matched no branch");
      }
    }
  }
})();
}

test('a case over a three-constructor union returns each matching branch\'s value', () => {
  assert.equal(label(Red), 1n);
  assert.equal(label(Green), 2n);
  assert.equal(label(Blue), 3n);
});

// Coverage is not checked yet (LANG-19), so nothing in the compiler stops a `Colour`
// value naming a fourth, undeclared tag from reaching `label` — this is what such a
// value hits at runtime: the `Fail` leaf's `$abort` call, never `undefined`.
test('a value no branch matches aborts rather than returning undefined', () => {
  assert.throws(
    () => label({ $: "Purple" }),
    /`label`'s case matched no branch/
  );
});
