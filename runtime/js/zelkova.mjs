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

`$effect(call, check, exported, k)` is the wrapper an effectful facade's `Task` is built over
(docs/decisions/dec-22.md#4--the-wrapper-is-one-runtime-helper-and-a-synchronous-companion-continues-synchronously).
Only it and `$runTask` know what `Done` looks like: emitted code hands it the companion call, the
payload's predicate and the continuation, and never builds a `Bounce` or a `Suspend`.

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
//     continues the loop from `step`. If that call happens while the loop is still inside
//     this `Suspend`'s function, `step` is held and the loop takes it up when the function
//     returns, so a `Suspend` that resumes at once cannot nest one loop inside another.
//     Otherwise `resume` enters the loop at once, from whatever turn of the host's event loop
//     called it.
//   - `Halt`: nothing more to do on this stack.
//
// Every `YIELD_EVERY`th `Bounce` is followed from a macrotask instead of at once.
//
// The loop is entered from `run`, from each such macrotask and from each `resume`, and every
// entry is inside one `try` that rejects the promise: an abort belongs to the `$runTask` whose
// continuation raised it, wherever it was raised. Once the promise has rejected, no entry
// runs any further. A continuation that is never called leaves the promise pending.
//
// `resume` is not guarded beyond that. The held step is a single slot, so a second `resume`
// inside the same `Suspend`'s function replaces the first. A `resume` called while the loop is
// running for another reason, or after `Halt`, is not held and not ignored: it enters the loop.
// Calling `resume` once is the `Suspend`'s function's job; the one-shot guard of DEC-22
// decision 5 belongs to `$effect`, below, not to this loop.
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

// EFFECTS

// What a host says about a value a companion threw or rejected with: its `String`
// conversion, which for an `Error` is its name and message. A value with no conversion
// (`Object.create(null)`) still has to produce a description, since the wrapper's job is to
// keep a broken companion from ending the program.
function describeThrown(thrown) {
  try {
    return String(thrown);
  } catch {
    return 'a value that cannot be described';
  }
}

// `$effect(call, check, exported, k)` is the wrapper an effectful facade's call site gets
// (docs/spec/interop.md#an-effectful-facade, docs/decisions/dec-22.md decisions 4 and 5). The
// emitted `Task`'s run function calls it with the continuation it was handed:
//
//   - `call` calls the companion with the facade's arguments and returns what it returns;
//   - `check` is the predicate of the payload type, or `null` for a `()` payload, which is
//     discarded, not checked, so the companion's return value is ignored and the result is
//     `Ok` of `undefined`;
//   - `exported` names the export, module and value, and is what `Malformed` carries;
//   - `k` is the continuation, which is handed one `Result Failure a`.
//
// It returns a `Done`. `call` is called inside a `try` that covers that call and nothing else:
// the continuation and the predicate run outside it, so an abort raised further down the chain
// is never turned into `Threw`. A value that is not a `Promise` — recognised by `instanceof`,
// which is what an `async function` returns, so a thenable of any other kind is a plain value
// and fails the predicate — gives a `Bounce`; a throw gives a `Bounce` of `Threw`; a `Promise`
// gives a `Suspend` whose function hands the two handlers to `promise.then(onValue, onReject)`,
// never a `.catch` after a `.then`, for the same reason. Each handler resumes the loop with the
// step the synchronous cases would have bounced.
//
// The step is one-shot: run a second time it aborts, naming the export. A promise settles once
// and a companion returns once, so only a defect in this function or in the loop reaches that.
export function $effect(call, check, exported, k) {
  let used = false;

  const stepFor = (result) => () => {
    if (used) {
      $abort(`the continuation of \`${exported}\`'s effect was run twice`);
    }
    used = true;
    return k(result);
  };
  const bounce = (result) => ({ $: 'Bounce', a: stepFor(result) });

  const returned = (value) => {
    if (check === null) return { $: 'Ok', a: undefined };
    if (check(value)) return { $: 'Ok', a: value };
    return {
      $: 'Err',
      a: {
        $: 'Malformed',
        a: `\`${exported}\`'s companion returned a value its declared type does not admit`,
      },
    };
  };
  const threw = (thrown) => ({ $: 'Err', a: { $: 'Threw', a: describeThrown(thrown) } });

  let value;
  try {
    value = call();
  } catch (thrown) {
    return bounce(threw(thrown));
  }

  if (value instanceof Promise) {
    return {
      $: 'Suspend',
      a: (resume) => {
        value.then(
          (settled) => resume(stepFor(returned(settled))),
          (rejected) => resume(stepFor(threw(rejected))),
        );
      },
    };
  }
  return bounce(returned(value));
}
