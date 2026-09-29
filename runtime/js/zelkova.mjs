/*
The JavaScript runtime: what emitted code needs and cannot repeat at every call site without
duplicating itself in every module, and the loop that runs a `Task` (`GEN-8`;
docs/decisions/dec-18.md#3--a-function-emits-as-a-plain-n-ary-function-and-currying-is-a-runtime-helper).

A Zelkova declaration is emitted as a plain n-ary JavaScript function, and a saturated call at a
known callee is a direct call — the fast path, and this module is not on it. Everything else
goes through `$curry`:

  - an application that supplies fewer arguments than the callee's arity produces a function
    value (docs/spec/evaluation-semantics.md#function-values), so it accumulates what it has
    until there is enough;
  - an application that supplies more — over-application: `f 1 2` where `f` takes one argument
    and returns a function is two applications the emitted call site may not know apart — hands
    the surplus to whatever the first `arity` arguments produced, with a plain JavaScript call.
    That plain call is always correct by induction: every function value this runtime hands
    around is either a reference to a declaration, called elsewhere only when the call site
    knows it is saturated, or something `$curry` itself produced, which already knows how to
    accumulate or finish. Nothing a `$curry` result is applied to is a bare, arity-less closure.

`$abort` is the other piece the backend does not generate.
docs/spec/evaluation-semantics.md#when-a-program-aborts is when the runtime can no longer keep
the language's guarantees: it stops without producing a value and without running any more of
itself. A thrown JavaScript error is the obvious way to do that on Node, and a throw can never
be returned or bound the way a sentinel value could, so it cannot be mistaken for one. Nothing
in Zelkova catches (docs/spec/evaluation-semantics.md#two-outcomes) and nothing in emitted code
does; `$runTask`, below, is the one place an abort is caught, and it hands the abort to its
caller. The description says what caused the abort and is carried on the thrown error's
`message`.

`$runTask(task)` is the one place a `Task` is run
(docs/spec/evaluation-semantics.md#running-a-task, docs/decisions/dec-22.md#5--runtask-returns-a-promise-and-owns-every-abort-raised-while-it-runs).
It returns a `Promise` of the value the `Task` produces, and is called only by generated entry
points, never by emitted module code. It reads `Done` by its `$` tag and its `a` field, the union
encoding (docs/spec/interop.md#a-union-crosses-as-a-tagged-value), so the constructors' names
and order in `std/core/src/Task.zel` have to match the ones it reads. An exception raised while
a continuation runs is an abort: the promise rejects with it, after a `Suspend` as well as before.

Equality, comparison and arithmetic are deliberately absent from this file: those are ordinary
functions behind the `Js/Basics` and `Js/Utils` facades (`std/core/src/Js/`), and a second copy
here is how the two drift apart.
*/

// CURRYING

// `$curry(fn, arity)` wraps a plain n-ary function `fn` — the shape a declaration of `arity`
// parameters is emitted as — so that a call is total regardless of how many arguments it is
// given:
//
//   - fewer than `arity`: returns a function accumulating the rest. Any number of calls, each
//     supplying any number of arguments, works — `curried(1)(2, 3)`, `curried(1, 2)(3)` and
//     `curried(1, 2, 3)` all reach `fn` the same way.
//   - `arity` or more: calls `fn` with the first `arity` of them. Anything left over is
//     over-application and is applied to whatever `fn` returned, with a plain call — see this
//     module's header comment for why that call is always safe.
export function $curry(fn, arity) {
  return function accumulated(...args) {
    if (args.length < arity) {
      return $curry((...rest) => fn(...args, ...rest), arity - args.length);
    }

    const result = fn(...args.slice(0, arity));
    const surplus = args.slice(arity);
    return surplus.length === 0 ? result : result(...surplus);
  };
}

// ABORTING

// `$abort(description)` stops the program: it throws, and nothing in generated code — or in
// the language itself — catches. `description` says what caused the abort
// (docs/spec/evaluation-semantics.md#when-a-program-aborts) and is the thrown error's message.
export function $abort(description) {
  throw new Error(description);
}

// RUNNING A TASK

// How many `Bounce`s the loop follows before it yields to the host (docs/decisions/dec-22.md
// decision 3). cats-effect's default, which the decision names as the place to start.
const YIELD_EVERY = 1024;

// Hands `callback` to the host as a macrotask, so timers and I/O callbacks that are due run
// before it does. A microtask would not do: the microtask queue drains before the host looks
// at either. `setImmediate` is Node's; `setTimeout` is the fallback for a host without it.
function yieldToHost(callback) {
  if (typeof setImmediate === 'function') {
    setImmediate(callback);
  } else {
    setTimeout(callback, 0);
  }
}

// `$runTask(task)` runs a `Task` and returns a `Promise` of the value it produces.
//
// `task` is a `Task` value, `{$: "Task", a: run}`, and `run` is handed one continuation: a
// function that resolves the promise and returns `Halt`. What `run` returns, and what each
// step returns after it, is a `Done`:
//
//   - `Bounce`: the loop calls its step with `undefined`, the `()` a step is applied to. A step
//     is a curried partial application, so `step()` would return the partial function and not
//     a `Done`.
//   - `Suspend`: the loop calls its function with a `resume` and stops. Calling `resume(step)`
//     continues the loop from `step`, immediately if `Suspend`'s function is still running and
//     from a later turn of the host's event loop otherwise, so a `Suspend` that resumes
//     at once cannot nest one loop inside another.
//   - `Halt`: nothing more to do on this stack.
//
// Every `YIELD_EVERY`th `Bounce` is followed from a macrotask instead of at once.
//
// The loop is entered from `run`, from each such macrotask and from each `resume`, and every
// entry is inside one `try` that rejects the promise: an abort belongs to the `$runTask` whose
// continuation raised it, wherever it was raised. Once the promise has rejected, no entry
// runs any further. A continuation that is never called leaves the promise pending.
export function $runTask(task) {
  return new Promise((resolve, reject) => {
    const halt = { $: 'Halt' };
    const finish = (value) => {
      resolve(value);
      return halt;
    };

    let bounces = 0;
    let running = false;
    let failed = false;
    let pending = null;

    function drive(first) {
      running = true;
      try {
        let step = first;
        while (step !== null) {
          const done = step(undefined);
          step = null;
          if (done.$ === 'Bounce') {
            if (++bounces >= YIELD_EVERY) {
              bounces = 0;
              const next = done.a;
              yieldToHost(() => enter(next));
            } else {
              step = done.a;
            }
          } else if (done.$ === 'Suspend') {
            done.a(resume);
            step = pending;
            pending = null;
          } else if (done.$ !== 'Halt') {
            $abort(`$runTask was handed a Done it does not know: ${String(done.$)}`);
          }
        }
      } finally {
        running = false;
      }
    }

    function enter(step) {
      if (failed) return;
      try {
        drive(step);
      } catch (error) {
        failed = true;
        reject(error);
      }
    }

    function resume(step) {
      if (running) {
        pending = step;
      } else {
        enter(step);
      }
    }

    enter(() => task.a(finish));
  });
}
