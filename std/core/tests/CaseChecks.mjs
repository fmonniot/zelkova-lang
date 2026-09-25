// A runtime check on the shape `javascript::emit` (src/compiler/javascript.rs) produces
// for a `case`, since GEN-10 is what makes the JavaScript backend emit one at all.
//
// This is not a companion check the way std/core/tests/Js/*.mjs are: it is not testing
// hand-written JavaScript behind a `module foreign` facade, it is testing the
// compiler's own generated code, and it sits in std/core's own tests/ root on
// sufferance — that root is otherwise reserved for a package's own companion checks
// (see CLAUDE.md). Nothing writes that generated code to disk yet — GEN-13 is the
// build, GEN-14 the harness that would run its real output under node — so `label` and
// `partial` below are hand-copied instances of what `javascript::emit` answers today
// for the fixture in this comment: literal JavaScript, checked in by hand, that runs
// neither `javascript::emit` nor an import of any emitted module. Nothing here proves
// this text still matches what the compiler produces beyond the human who copied it
// reading both sides; `tests/javascript.rs`'s
// `a_case_on_a_three_constructor_union_is_nested_ifs_naming_each_tag` is that same
// shape's Rust-side pin (a `contains` assertion against the same nested block, not an
// `assert_eq!` on the whole module), and a change to either side has to be carried to
// the other by hand until GEN-13/14 land and this file can import real build output
// instead. GEN-10's acceptance asks for this much: a `case` over a three-constructor
// union returning each branch's value, and a value no branch matches aborting rather
// than returning `undefined`.
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
//   partial : Colour -> Int
//   partial c =
//     case c of
//       Red ->
//         1
//
//       Green ->
//         2
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
    {
      return 1n;
    }
  } else {
    if ($scrutinee.$ === "Green") {
      {
        return 2n;
      }
    } else {
      if ($scrutinee.$ === "Blue") {
        {
          return 3n;
        }
      } else {
        return $abort("`label`'s case matched no branch");
      }
    }
  }
})();
}

function partial(c) {
  return (() => {
  const $scrutinee = c;
  if ($scrutinee.$ === "Red") {
    {
      return 1n;
    }
  } else {
    if ($scrutinee.$ === "Green") {
      {
        return 2n;
      }
    } else {
      return $abort("`partial`'s case matched no branch");
    }
  }
})();
}

test('a case over a three-constructor union returns each matching branch\'s value', () => {
  assert.equal(label(Red), 1n);
  assert.equal(label(Green), 2n);
  assert.equal(label(Blue), 3n);
});

// Coverage is not checked yet (LANG-19), so a `case` may omit a constructor its
// scrutinee's union declares — `partial` above names only `Red` and `Green` — and a
// `Colour` naming the constructor it omits is an entirely ordinary, typer-accepted
// value (unlike an out-of-union tag such as `{ $: "Purple" }`, which nothing in this
// language can actually construct). Calling `partial` with that value is the real path
// that reaches the `Fail` leaf's `$abort` call today, never `undefined`.
test('a case omitting a constructor aborts on a value naming it, rather than returning undefined', () => {
  assert.throws(
    () => partial(Blue),
    /`partial`'s case matched no branch/
  );
});
