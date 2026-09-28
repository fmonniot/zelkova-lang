# GEN-21 · The JavaScript runtime cannot run a `Task`

**Sizing:** small-to-medium. It adds one exported function to `runtime/js/zelkova.mjs`, and
[`Done`](spec-37.md) if [`SPEC-37`](spec-37.md) puts it there, plus the node-side checks. The
stack-depth strategy is what could make it medium: a trampoline is more code, and more to test,
than a plain call.

**Part of:** [Active work: effects](README.md#active-work-effects). This is the one place a `Task`
is run. [`GEN-22`](gen-22.md) (`zelkova run`) and [`LANG-76`](lang-76.md) (a `Test` that holds a
`Task`) both call it, and nothing else may: [*Running a `Task`*](../spec/evaluation-semantics.md#running-a-task)
says nothing else runs one.

**Depends on:** [`SPEC-37`](spec-37.md), for the entry point's shape, `Done`, and the stack-depth
strategy; [`LANG-74`](lang-74.md), for a `Task` to run.

**Location:** `runtime/js/zelkova.mjs` — beside `$curry` and `$abort`, and its header comment, which
lists what the runtime holds; `runtime/js/tests/zelkovaChecks.mjs`, where the runtime's own checks
live; `src/compiler/javascript.rs` — `RUNTIME`, which embeds the file.

**Problem:** building a `Task` [performs nothing](../spec/evaluation-semantics.md#effects). Once
[`LANG-74`](lang-74.md) lands, a program can build any number of them and none can happen, because
nothing hands a `Task` its final continuation and waits for it.

**Approach:** apply [`SPEC-37`](spec-37.md)'s decisions. They are not repeated here, so that the
two do not drift. The shape the ticket expects is one exported function, `$runTask(task)`, that
returns a JavaScript `Promise` of the `Task`'s final value. Its callers are generated entry points
(`run.mjs` and the program entry `GEN-22` writes), never emitted module code. If
`SPEC-37` chose trampolining, the loop lives here, and this function is the only place that knows
what `Done` holds.

An exception raised while a continuation runs is an [abort](../spec/evaluation-semantics.md#when-a-program-aborts),
which `$abort` already expresses as a throw. `$runTask` rejects its promise with it, and does not
turn it into a value.

**Tests:** `runtime/js/tests/zelkovaChecks.mjs`, with a `Task` built by hand in the representation
`SPEC-37` settles:

- `succeed` resolves with its value;
- `andThen` runs the second task only after the first has produced its value, and passes that value
  along;
- a chain of at least 100 000 `andThen` links over `succeed` completes without a `RangeError`,
  or reaches the depth `SPEC-37` specifies and no less;
- a continuation that throws rejects the promise.

Then, once the build can emit `std/core`'s `Task`, add the same checks as Zelkova tests. That is
[`LANG-76`](lang-76.md)'s acceptance, not this ticket's.

**Acceptance:** `node --test runtime/js/tests/` passes the four checks above, and each has been
seen to fail with the behaviour it pins neutralised. `$runTask` is described in the runtime's
header comment beside `$curry` and `$abort`.
