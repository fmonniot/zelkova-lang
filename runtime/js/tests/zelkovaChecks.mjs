// The JavaScript checks over runtime/js/zelkova.mjs, the hand-written runtime `GEN-8` added.
//
// Where this file sits follows docs/spec/interop.md's "Testing a companion", the pattern
// std/core/tests/Js/UtilsChecks.mjs already uses: a companion's test is JavaScript, not
// Zelkova, and sits under the package's own tests/ root. `zelkova.mjs` is not a facade
// companion — it backs no `.zel` module — but it is still hand-written JavaScript with no
// runner able to find a Zelkova `Test` yet, so it is checked the same interim way, registered
// with Node's own test runner and pointed at directly:
//
//   node --test 'runtime/js/tests/**/*.mjs'
//
// Unlike UtilsChecks.mjs and its neighbours, there is no earlier, buggy version of this file to
// pin a fix against, so the PINS/GUARD convention those files use does not apply here — every
// check below is read as an ordinary specification of `$curry` and `$abort`'s behaviour.
//
// CLAUDE.md's "A green test proves nothing until you have seen it fail" was applied to each
// check below by temporarily breaking the one behaviour it names — discarding the surplus in
// `$curry`'s over-application branch, and only ever accumulating one argument per call — and
// confirming the relevant check went red before restoring the fix. The comments on the
// over-application and several-arguments-at-once checks say what a broken implementation would
// have to do to still pass them.

import assert from 'node:assert/strict';
import { test } from 'node:test';
import { $curry, $abort } from '../zelkova.mjs';

// Plain n-ary functions, the shape a declaration of that many parameters is emitted as
// (docs/decisions/dec-18.md#3--a-function-emits-as-a-plain-n-ary-function-and-currying-is-a-runtime-helper).
function add2(a, b) { return a + b; }
function add3(a, b, c) { return a + b + c; }

// CURRYING — one argument at a time

test('an arity-2 function applied one argument at a time', () => {
  const curried = $curry(add2, 2);
  assert.equal(curried(1)(2), 3);
});

test('an arity-3 function applied 1 then 2 arguments', () => {
  const curried = $curry(add3, 3);
  assert.equal(curried(1)(2, 3), 6);
});

test('an arity-3 function applied 2 then 1 argument', () => {
  const curried = $curry(add3, 3);
  assert.equal(curried(1, 2)(3), 6);
});

// CURRYING — several arguments at once

test('an arity-2 function applied both arguments at once gives the same answer as one at a time', () => {
  const curried = $curry(add2, 2);
  assert.equal(curried(1, 2), curried(1)(2));
});

test('an arity-3 function applied all 3 arguments in one call', () => {
  // Would fail if $curry only ever accumulated one argument per call: `curried(1, 2, 3)`
  // would then still be a function waiting for two more arguments (args.length === 1 as far
  // as such an implementation is concerned), not the number 6.
  const curried = $curry(add3, 3);
  assert.equal(curried(1, 2, 3), 6);
});

test('an arity-3 function applied 3 at once gives the same answer as 1+2 and 2+1', () => {
  const c = $curry(add3, 3);
  assert.equal(c(1, 2, 3), c(1)(2, 3));
  assert.equal(c(1, 2, 3), c(1, 2)(3));
});

// OVER-APPLICATION

test('a function of arity 1 returning a function, applied to two arguments in one call', () => {
  // `f` takes one argument and returns a function — the shape the ticket's own example
  // (`f 1 2`) describes, and the induction this module's header comment states: a function
  // value returned from a declaration is itself curry-safe, here because it too was built by
  // `$curry`, matching what the backend emits for a nested partial application.
  const f = (a) => $curry((b) => a - b, 1);
  const curried = $curry(f, 1);

  // Would fail if $curry's over-application branch discarded the surplus argument instead of
  // applying it — `curried(1, 2)` would then be the function `f(1)` returned rather than a
  // number, and this equality would fail on type alone — or if it threw instead of applying it.
  assert.equal(curried(1, 2), -1);

  // The same computation, one call at a time, to confirm curried(1, 2) reaches the same place
  // rather than the assertion above passing for an unrelated reason.
  assert.equal(curried(1, 2), curried(1)(2));
});

test('over-application where the surplus itself is only a partial application', () => {
  // `f` returns the arity-3 `add3` untouched; the single surplus argument from `curried(10)`'s
  // over-application is not enough to saturate it, so the result is itself still a function
  // accumulating two more, not a number.
  const f = () => $curry(add3, 3);
  const curried = $curry(f, 0);

  const partial = curried(10);
  assert.equal(typeof partial, 'function');
  assert.equal(partial(1, 2), 13);
  assert.equal(partial(1)(2), 13);
});

// ABORTING

test('$abort stops by throwing, and the failure carries the description it was given', () => {
  assert.throws(
    () => $abort('missing companion for Js.Utils on target js'),
    { message: /missing companion for Js\.Utils on target js/ },
  );
});

test('$abort never returns a value', () => {
  let returned = 'not yet called';
  try {
    returned = $abort('unreachable');
  } catch {
    // expected — $abort always throws
  }
  assert.equal(returned, 'not yet called');
});
