// The JavaScript checks over runtime/js/zelkova.mjs, the hand-written runtime `GEN-8` added.
//
// `zelkova.mjs` is hand-written JavaScript but not a package's companion: it backs no `.zel`
// module and belongs to no package, so there is no test facade to declare these checks in
// (docs/spec/interop.md's "Testing a companion" is how a package's companion is checked).
// They are registered with Node's own test runner instead, and Node is pointed at them
// directly:
//
//   node --test 'runtime/js/tests/**/*.mjs'
//
// Unlike UtilsChecks.mjs and its neighbours, there is no earlier, buggy version of this file to
// pin a fix against, so the PINS/GUARD convention those files use does not apply here — every
// check below is read as an ordinary specification of `$curry` and `$abort`'s behaviour.
//
// CLAUDE.md's "A green test proves nothing until you have seen it fail" was applied to each
// check below by temporarily breaking the one behaviour it names and confirming the check went
// red before restoring the fix: discarding the surplus in `$curry`'s over-application branch,
// and only ever accumulating one argument per call. The `$runTask` checks whose comments begin
// "Mutation checked by" say which mutation turns them red; the two ordering checks, `andThen`
// and `Suspend`-resume, carry no such note. The comments on the over-application and
// several-arguments-at-once checks say what a broken implementation would have to do to still
// pass them.

import assert from 'node:assert/strict';
import { test } from 'node:test';
import { $curry, $abort, $effect, $runTask } from '../zelkova.mjs';

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

// RUNNING A TASK

// A `Task` built by hand in docs/decisions/dec-22.md's representation, the way `Task.zel`
// writes it: a `Task` holds a run function, and every handoff returns a `Bounce` of the call
// instead of making it. `callWith` is curried by `$curry` because the emitted code is: a step
// is a partial application, which is why the loop has to call it as `step(undefined)`.
const callWith = $curry((k, a, _unit) => k(a), 3);
const bounce = (step) => ({ $: 'Bounce', a: step });
const halt = { $: 'Halt' };
const task = (run) => ({ $: 'Task', a: run });

const succeed = (a) => task($curry((a, k) => bounce(callWith(k, a)), 2)(a));

// `andThen f (Task run)`: `f` is applied once `run` produces a value.
const andThen = (f, { a: run }) => task((k) => bounce(callWith(run, continueWith(f, k))));
const continueWith = (f, k) => $curry((a) => bounce(callWith(f(a).a, k)), 1);

test('$runTask of succeed resolves with its value', async () => {
  // Mutation checked by calling `step()` in the loop: a curried step then returns the partial
  // function, which is not a `Done`, and this rejects.
  assert.equal(await $runTask(succeed(42)), 42);
});

test('andThen runs the second task only after the first has produced its value', async () => {
  const events = [];
  const first = task((k) => {
    events.push('first run');
    return bounce(callWith(k, 1));
  });
  const chained = andThen((a) => {
    events.push(`f got ${a}`);
    return succeed(a + 1);
  }, first);
  assert.deepEqual(events, [], 'building a Task performs nothing');
  assert.equal(await $runTask(chained), 2);
  assert.deepEqual(events, ['first run', 'f got 1']);
});

const LINKS = 100_000;

test('a chain of andThen links over succeed completes without exhausting the stack', async () => {
  // `andThen inc (andThen inc (… (succeed 0)))`: running the outer link calls the inner run
  // function, so a loop that made that call directly would nest LINKS frames. Mutation
  // checked by having the helpers call `run` directly instead of bouncing: a RangeError.
  let chain = succeed(0);
  for (let i = 0; i < LINKS; i++) chain = andThen((a) => succeed(a + 1), chain);
  assert.equal(await $runTask(chain), LINKS);
});

test('a Task that builds the next link itself completes without exhausting the stack', async () => {
  // The other nesting: each `f` returns `andThen f (succeed …)`, so the chain is built while
  // it runs.
  const count = (a) => (a < LINKS ? andThen(count, succeed(a + 1)) : succeed(a));
  assert.equal(await $runTask(andThen(count, succeed(0))), LINKS);
});

test('a timer set before a chain longer than the yield interval fires before the chain finishes', async () => {
  // Mutation checked by removing the yield (following every `Bounce` at once): the chain
  // finishes in one synchronous run, before any timer.
  let fired = false;
  setTimeout(() => { fired = true; }, 0);
  let chain = succeed(0);
  for (let i = 0; i < LINKS; i++) chain = andThen((a) => succeed(a + 1), chain);
  const firedWhenFinished = await $runTask(chain).then(() => fired);
  assert.equal(firedWhenFinished, true);
});

// A `Task` that suspends: it hands the loop a `Suspend` whose function calls `resume` from a
// timer with the step that carries `value` on.
const later = (value) => task((k) => ({
  $: 'Suspend',
  a: (resume) => { setTimeout(() => resume(callWith(k, value)), 0); },
}));

test('a Suspend resumes the loop with the step it is handed', async () => {
  assert.equal(await $runTask(andThen((a) => succeed(a * 2), later(21))), 42);
});

test('a Suspend that resumes at once continues from the loop, not from inside its own function', async () => {
  // `register` calls `resume` before it returns. The loop picks the step up after `register`
  // has returned, so it does not nest a second loop in `register`'s frame. Mutation checked by
  // making `resume` always call `enter`: the continuation then runs inside `register`, and
  // 'continued' comes before 'register returned'.
  const events = [];
  const at_once = task((k) => ({
    $: 'Suspend',
    a: (resume) => {
      resume($curry((_unit) => { events.push('continued'); return k(7); }, 1));
      events.push('register returned');
    },
  }));
  assert.equal(await $runTask(at_once), 7);
  assert.deepEqual(events, ['register returned', 'continued']);
});

test('a continuation that throws after a Suspend resumed still rejects the promise', async () => {
  // Mutation checked by making `resume` call `drive` instead of `enter`: `drive` has no `try`,
  // so the throw is then an uncaught exception in a timer and the promise never settles.
  const boom = new Error('boom after resume');
  const chained = andThen(() => { throw boom; }, later(1));
  await assert.rejects($runTask(chained), (error) => error === boom);
});

test('a continuation that throws rejects the promise', async () => {
  // Mutation checked by dropping `reject(error)` from `enter`'s `catch`: the promise then
  // never settles. (Removing the `try` outright is not a mutation this check can see: a throw
  // on the first entry, before any yield, escapes into the Promise executor, which rejects.)
  const boom = new Error('boom');
  const chained = andThen(() => { throw boom; }, succeed(1));
  await assert.rejects($runTask(chained), (error) => error === boom);
});

test('a continuation that throws after the loop has yielded to the host still rejects the promise', async () => {
  // The throw comes after more than `YIELD_EVERY` links, so it runs from the macrotask the
  // loop yielded to. Mutation checked by making the yield call `drive` instead of `enter`: the
  // throw is then an uncaught exception in the `setImmediate` callback.
  const boom = new Error('boom after yield');
  let chain = succeed(0);
  for (let i = 0; i < 3000; i++) chain = andThen((a) => succeed(a + 1), chain);
  await assert.rejects($runTask(andThen(() => { throw boom; }, chain)), (error) => error === boom);
});

test('nothing runs after the promise has rejected', async () => {
  // Two `resume`s from one timer: the first continues into a throw, the second would run a
  // step. Mutation checked by deleting `if (failed) return;` from `enter`: the second runs.
  const boom = new Error('boom');
  let ranAfter = false;
  const suspended = task(() => ({
    $: 'Suspend',
    a: (resume) => {
      setTimeout(() => {
        resume(() => { throw boom; });
        resume(() => { ranAfter = true; return halt; });
      }, 0);
    },
  }));
  await assert.rejects($runTask(suspended), (error) => error === boom);
  assert.equal(ranAfter, false);
});

test('a Done the loop does not know rejects the promise', async () => {
  // Mutation checked by disabling the `$abort` branch for an unknown tag: the loop then stops
  // quietly and the promise never settles.
  const odd = task(() => ({ $: 'Sideways' }));
  await assert.rejects($runTask(odd), { message: /does not know: Sideways/ });
});

// EFFECTS

// `$effect` is what an effectful facade's `Task` calls when it is run. These checks hand it a
// `call` and a `check` by hand and drive the `Done` it returns through `$runTask`, or, for the
// two properties no emitted program can reach, by hand. The end-to-end behaviour over emitted
// code — `Ok`, `Threw` and `Malformed` from a real companion — is tests/js/EffectChecks.mjs.
const isBigInt = (v) => typeof v === 'bigint';
const effect = (call, check = isBigInt) => task((k) => $effect(call, check, 'Mod.export', k));
// Whatever the `Task` yields, unwrapped from its `Result`: an `Ok` payload, or the `Err`'s.
const ranWith = (result) => $runTask(andThen((r) => succeed(r), result));

test('$effect yields Ok with a synchronous value that passes the check', async () => {
  assert.deepEqual(await ranWith(effect(() => 3n)), { $: 'Ok', a: 3n });
});

test('$effect yields Ok with a promise that resolves to a value that passes the check', async () => {
  assert.deepEqual(await ranWith(effect(async () => 3n)), { $: 'Ok', a: 3n });
});

test('$effect yields Threw carrying the host\'s description for a synchronous throw', async () => {
  const result = await ranWith(effect(() => { throw new TypeError('no'); }));
  assert.deepEqual(result, { $: 'Err', a: { $: 'Threw', a: 'TypeError: no' } });
});

test('$effect yields Threw for a thrown value with no string conversion', async () => {
  const result = await ranWith(effect(() => { throw Object.create(null); }));
  assert.equal(result.a.$, 'Threw');
  assert.equal(typeof result.a.a, 'string');
});

test('$effect yields Threw for a promise that rejects, with anything', async () => {
  const result = await ranWith(effect(() => Promise.reject('plain string')));
  assert.deepEqual(result, { $: 'Err', a: { $: 'Threw', a: 'plain string' } });
});

test('$effect yields Malformed naming the export for a value that fails the check, either way', async () => {
  const malformed = {
    $: 'Err',
    a: { $: 'Malformed', a: '`Mod.export`\'s companion returned a value its declared type does not admit' },
  };
  assert.deepEqual(await ranWith(effect(() => 'three')), malformed);
  assert.deepEqual(await ranWith(effect(async () => 'three')), malformed);
});

test('$effect treats a thenable that is not a Promise as a plain value, which fails the check', async () => {
  const result = await ranWith(effect(() => ({ then() {} })));
  assert.equal(result.a.$, 'Malformed');
});

test('$effect with no check discards the value, and still catches', async () => {
  assert.deepEqual(await ranWith(effect(() => 4, null)), { $: 'Ok', a: undefined });
  assert.deepEqual(await ranWith(effect(async () => 4, null)), { $: 'Ok', a: undefined });
  const result = await ranWith(effect(async () => { throw new Error('x'); }, null));
  assert.equal(result.a.$, 'Threw');
});

test('$effect does not call the companion until the Task is run', async () => {
  let calls = 0;
  const built = effect(() => { calls += 1; return 1n; });
  assert.equal(calls, 0);
  await ranWith(built);
  await ranWith(built);
  assert.equal(calls, 2);
});

test('$effect returns a Bounce for a synchronous result and a Suspend for a promise', () => {
  const k = () => halt;
  assert.equal($effect(() => 1n, isBigInt, 'Mod.export', k).$, 'Bounce');
  assert.equal($effect(() => { throw new Error('x'); }, isBigInt, 'Mod.export', k).$, 'Bounce');
  assert.equal($effect(async () => 1n, isBigInt, 'Mod.export', k).$, 'Suspend');
});

test('a continuation that throws after a synchronous companion returned rejects, and is not Threw', async () => {
  // Mutation checked by running `k` inside `$effect`'s `try` (`try { return bounce(...).a(); }`):
  // the throw is then caught as `Threw` and the continuation runs a second time with it.
  const boom = new Error('boom after a value');
  const chained = andThen(() => { throw boom; }, effect(() => 1n));
  await assert.rejects($runTask(chained), (error) => error === boom);
});

test('a continuation that throws after a companion\'s promise resolved rejects, and is not Threw', async () => {
  // `$runTask`'s `resume` catches what a continuation throws, so this holds under either
  // spelling of the `then`, and stays green under a handler that catches. The check after the
  // next one is what fails then. This one pins the outcome a program can see.
  const boom = new Error('boom after a promise');
  const chained = andThen(() => { throw boom; }, effect(async () => 1n));
  await assert.rejects($runTask(chained), (error) => error === boom);
});

test('a predicate that throws rejects $runTask, for a synchronous companion', async () => {
  // Mutation checked by wrapping the predicate call in a `try` that returns a `Threw`: the
  // rejection becomes an `Err` and this goes red.
  const boom = new Error('predicate blew up');
  const throwing = () => { throw boom; };
  await assert.rejects($runTask(effect(() => 1n, throwing)), (error) => error === boom);
});

test('a predicate that throws rejects $runTask, for a companion\'s promise', async () => {
  // Mutation checked by computing `returned(settled)` in the `onValue` handler, outside the step
  // (`resume(stepFor(returned(settled)))`): the handler's derived promise rejects unhandled and
  // `$runTask` never settles, which the timeout below reports.
  const boom = new Error('predicate blew up');
  const throwing = () => { throw boom; };
  const settled = await Promise.race([
    $runTask(effect(async () => 1n, throwing)).then(() => 'resolved', (error) => error),
    new Promise((resolve) => setTimeout(() => resolve('hung'), 200)),
  ]);
  assert.equal(settled, boom);
});

test('a step handed to resume twice aborts, naming the export', () => {
  // Mutation checked by dropping the `used` test in `stepFor`: the second call then runs `k`
  // again and does not throw.
  const step = $effect(() => 1n, isBigInt, 'Mod.export', () => halt).a;
  step();
  assert.throws(step, { message: 'the continuation of `Mod.export`\'s effect was run twice' });
});

test('a step a promise\'s handler resumes with aborts when run twice', async () => {
  let resumed;
  const done = $effect(async () => 1n, isBigInt, 'Mod.export', () => halt);
  done.a((step) => { resumed = step; });
  await new Promise((resolve) => setImmediate(resolve));
  resumed();
  assert.throws(resumed, { message: 'the continuation of `Mod.export`\'s effect was run twice' });
});

test('a throw from resume after a companion\'s promise settled is not handed to resume again as Threw', async () => {
  // `resume` never throws when it is `$runTask`'s own, since `enter` catches; a `resume` that
  // does is what tells `then(onValue, onReject)` from `then(onValue).catch(onReject)`. Mutation
  // checked by that replacement: the `catch` then calls `resume` a second time, with `Threw`.
  // The promise is a `Promise` whose `then` swallows the rejection of the promise it returns,
  // so the throw is not also reported as unhandled.
  class Quiet extends Promise {
    static get [Symbol.species]() { return Promise; }
    then(onValue, onReject) {
      const derived = super.then(onValue, onReject);
      derived.catch(() => {});
      return derived;
    }
  }
  const resumed = [];
  const done = $effect(() => new Quiet((resolve) => resolve(1n)), isBigInt, 'Mod.export', () => halt);
  done.a((step) => { resumed.push(step); throw new Error('boom from resume'); });
  await new Promise((resolve) => setImmediate(resolve));
  assert.equal(resumed.length, 1);
});
