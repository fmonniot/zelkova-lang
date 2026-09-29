# GEN-21 · The JavaScript runtime cannot run a `Task`

**Sizing:** medium. It adds one exported function to `runtime/js/zelkova.mjs`, and the loop
behind it, plus the node-side checks. Most of the size is the loop: three kinds of `Done`, a
yield every N bounces, and one `try` around every entry, each needing a check of its own.

**Part of:** [Active work: effects](README.md#active-work-effects). This is the one place a `Task`
is run. [`GEN-22`](gen-22.md) (`zelkova run`) and [`LANG-76`](lang-76.md) (a `Test` that holds a
`Task`) both call it, and nothing else may: [*Running a `Task`*](../spec/evaluation-semantics.md#running-a-task)
says nothing else runs one.

**Depends on:** [`LANG-74`](README.md), for a `Task` to run. The checks below build their `Task`s
by hand, so the runtime half can be written first.

**Location:** `runtime/js/zelkova.mjs` — beside `$curry` and `$abort`, and its header comment, which
lists what the runtime holds; `runtime/js/tests/zelkovaChecks.mjs`, where the runtime's own checks
live; `src/compiler/javascript.rs` — `RUNTIME`, which embeds the file.

**Problem:** building a `Task` [performs nothing](../spec/evaluation-semantics.md#effects). Once
[`LANG-74`](README.md) landed, a program can build any number of them and none can happen, because
nothing hands a `Task` its final continuation and waits for it.

**Approach:** apply [`DEC-22`](../decisions/dec-22.md) decisions [2](../decisions/dec-22.md#2--done-is-a-union-core-declares-and-does-not-export-and-the-runtime-reads-it),
[3](../decisions/dec-22.md#3--every-handoff-bounces-the-loop-yields-every-n-bounces) and
[5](../decisions/dec-22.md#5--runtask-returns-a-promise-and-owns-every-abort-raised-while-it-runs). They are not
repeated here, so that the two do not drift. In short: one exported function, `$runTask(task)`,
returns a JavaScript `Promise` of the `Task`'s final value. It hands the `Task` a final
continuation that resolves the promise and returns `Halt`, and drives a loop over `Done` —
`Bounce` calls its step, `Suspend` calls its function with a `resume` that re-enters the loop,
`Halt` stops. The emitted `Bounce` thunk is a curried partial application, so the loop calls
it as `step(undefined)`; `step()` would return the partial function, not a `Done`. Every N
bounces the loop yields to the host with a macrotask; choosing N and the primitive is this
ticket's. Every entry into the loop, the first and each resumption, is wrapped
in one `try` that rejects the promise. Its callers are generated entry points (`run.mjs` and the
program entry `GEN-22` writes), never emitted module code.

An exception raised while a continuation runs is an [abort](../spec/evaluation-semantics.md#when-a-program-aborts),
which `$abort` already expresses as a throw. `$runTask` rejects its promise with it, and does not
turn it into a value. The loop reads `Done` by its `$` tag, the
[union encoding](../spec/interop.md#a-union-crosses-as-a-tagged-value), so its header comment
names `std/core/src/Task.zel` as the declaration it has to match.

**Tests:** `runtime/js/tests/zelkovaChecks.mjs`, with `Task`s built by hand in `DEC-22`'s
representation:

- `succeed` resolves with its value;
- `andThen` runs the second task only after the first has produced its value, and passes that value
  along;
- a chain of at least 100 000 `andThen` links over `succeed` completes without a `RangeError`,
  and so does a chain nested the other way, `andThen f (andThen g (…))`;
- a timer set before running a chain longer than N fires before the chain finishes;
- a `Suspend` resumes the loop, and a continuation that throws after that resumption still rejects
  `$runTask`'s promise;
- a continuation that throws rejects the promise.

Then, once the build can emit `std/core`'s `Task`, add the same checks as Zelkova tests. That is
[`LANG-76`](lang-76.md)'s acceptance, not this ticket's. The `andThen` checks among them wait
for [`LANG-78`](lang-78.md), since `Task.zel` does not export `andThen` yet.

**Acceptance:** `node --test runtime/js/tests/` passes the six checks above, and each has been
seen to fail with the behaviour it pins neutralised. `$runTask` is described in the runtime's
header comment beside `$curry` and `$abort`.
