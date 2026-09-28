# GEN-16 · The wrapper an effectful facade's call site gets

**Sizing:** medium. The wrapper itself is small: the call site emits a `Task` over one runtime
helper, `$effect`, which this ticket also writes. Most of the size is in the tests, which need a
companion that throws, one that rejects, one that returns the wrong shape and one that behaves.

**Part of:** [Active work: effects](README.md#active-work-effects). It is also
[`GEN-1`](gen-1.md)'s in subject, and deliberately outside that program: the original GEN-1 text
carried it as inherited work, and it could not be written until the tickets below existed.

**Depends on:** [`LANG-74`](README.md), for `Task` and `Failure`; [`GEN-21`](gen-21.md), for
something that runs what the wrapper builds; [`LANG-68`](README.md) (closed), which holds an
unmarked facade to the result type this wrapper assumes; [`GEN-2`](README.md) (closed), for the
predicate whose failure is `Err (Malformed ..)`. Sequenced after [`GEN-12`](README.md), which
emits the `unsafe` half of the same call site. `GEN-2` emitted the predicates and the `unsafe`
half's check only, since an effectful facade was still refused, so `Malformed` lands here.

**Location:** `src/compiler/javascript.rs`, at the facade call site
[`GEN-12`](README.md) emits. `src/compiler/canonical/mod.rs` — `Value::TypedValue`'s
`marked_unsafe`, which is the flag that decides which of the two shapes a call gets.
`std/core/src/Task.zel`, which [`LANG-74`](README.md) wrote, declares the types the wrapper builds.

**Decided ([`docs/spec/interop.md`](../spec/interop.md#an-effectful-facade) and
[`DEC-12`](../decisions/dec-12.md)):**

- **A facade declares an effect unless it says otherwise**, and its result type must be
  `Task (Result Failure a)`. The companion takes the arguments the signature names and **returns
  the bare payload** — never a `Result`, and never a `Task`. The two sides disagree about the
  type on purpose.
- **The wrapper is where they are reconciled.** It catches what the companion throws, checks
  what the companion returns against `a`, and yields `Ok` for a value that passes,
  [`Err (Threw ..)`](../spec/evaluation-semantics.md#an-effect-that-can-fail) for a failure the
  companion raised, and `Err (Malformed ..)` for a value that does not match. A thrown exception
  and a rejected promise are both `Threw`.
- **A `()` payload is discarded, not checked** ([`GEN-20`](README.md)). For
  `Task (Result Failure ())` the wrapper ignores whatever the companion returns, or its promise
  resolves to, and yields `Ok ()` with `()` as `undefined`. A companion ending on a call it does
  not mean to return — `a.push(x)`, `map.set(k, v)` — is therefore `Ok`, never `Malformed`. It
  still catches: a throw or a rejection is `Threw` as for any other payload.
- **A result that never arrives is not a failure.** It is a `Task` that never produces a value,
  which is the second of the [two outcomes](../spec/evaluation-semantics.md#two-outcomes).
- An [`unsafe`](../spec/interop.md#an-unsafe-facade) facade gets **no wrapper** — its companion
  is called directly, and one that throws
  [aborts the program](../spec/evaluation-semantics.md#when-a-program-aborts). That half is
  [`GEN-12`](README.md)'s.
- A [facade constant naming a `Task`](../spec/interop.md#facade-constants) is the one constant
  whose JavaScript companion exports a **function** rather than a value: the effect has to happen
  each time the `Task` is run.

**Problem:** the wrapper is the whole of what keeps a throwing `.mjs` from ending the program,
and nothing emits one. `javascript::emit` refuses an effectful facade signature outright today.

**Approach:** apply [`DEC-22`](../decisions/dec-22.md) decisions
[4](../decisions/dec-22.md#4--the-wrapper-is-one-runtime-helper-and-a-synchronous-companion-continues-synchronously),
[5](../decisions/dec-22.md#5--runtask-returns-a-promise-and-owns-every-abort-raised-while-it-runs), for the one-shot
guard, and [7](../decisions/dec-22.md#7--a-facade-constant-naming-a-task-gets-the-same-wrapper-with-no-arguments).
None of that is restated here beyond its outline. The call site emits a `Task` whose run function
calls `$effect` in `runtime/js/zelkova.mjs` with the companion call, the predicate and the
continuation. `$effect` calls the companion inside a `try` that covers that call and nothing else,
tells a promise apart with `instanceof Promise`, and returns a `Bounce` for a synchronous value or
throw and a `Suspend` for a promise, whose two handlers go to `promise.then(onValue, onReject)` —
never a `.catch` after `.then`, which would turn a later abort into `Threw`. A facade constant
naming a `Task` gets the same `$effect` call with no arguments, as one module-level `Task`.

[Building a `Task` performs nothing](../spec/evaluation-semantics.md#effects), so the companion is
called, and caught, when the `Task` is *run*, never when it is built. A wrapper that caught at
build time would catch nothing.

The predicate that decides whether the returned value matches `a` already exists:
`Predicates::test` in `src/compiler/javascript.rs` builds it for an `unsafe` facade's result, as
that module's *The boundary check* describes. This ticket calls the same predicate and routes a
failure to `Err (Malformed ..)` where an `unsafe` facade's check calls `$abort`
([Which types may cross the boundary](../spec/interop.md#which-types-may-cross-the-boundary)).
`$abort`'s description names the export whose companion returned the value, and `Malformed`'s
string should too.

**Acceptance:** five companions, each behind an unmarked facade under `std/core/tests/`, asserted
as Zelkova tests that `zelkova test` runs through [`LANG-76`](lang-76.md)'s `Test` over a `Task`:
one that throws synchronously yields `Err (Threw _)`; one whose promise rejects yields
`Err (Threw _)`; one that returns a value of the wrong shape yields `Err (Malformed _)`; and one
that returns correctly, after an `await`, yields `Ok` with the value. A fifth, behind a
`Task (Result Failure ())` facade, returns a number and yields `Ok ()`. With no string literals, the
tests match the constructor and do not compare the `String`. That `Threw` carries the host's
description and `Malformed` names the export is asserted on the emitted wrapper's behaviour in a
`node --test` check beside `runtime/js/tests/`, as are two properties of `$effect` no Zelkova
test can see: a continuation that throws after a companion returned, synchronously or through a
promise, rejects `$runTask`'s promise and is not turned into `Threw`; and a step handed to `resume`
twice aborts.
