# GEN-16 · The wrapper an effectful facade's call site gets

**Sizing:** medium. The wrapper itself is small. Most of the size is in the tests, which need a
companion that throws, one that rejects, one that returns the wrong shape and one that behaves.

**Part of:** [Active work: effects](README.md#active-work-effects). It is also
[`GEN-1`](gen-1.md)'s in subject, and deliberately outside that program: the original GEN-1 text
carried it as inherited work, and it could not be written until the tickets below existed.

**Depends on:** [`SPEC-37`](spec-37.md), for what the wrapper builds and when it calls the
continuation; [`LANG-74`](lang-74.md), for `Task` and `Failure`; [`GEN-21`](gen-21.md), for
something that runs what the wrapper builds; [`LANG-68`](lang-68.md), which holds an unmarked
facade to the result type this wrapper assumes; [`GEN-2`](gen-2.md), for the predicate whose
failure is `Err (Malformed ..)`. Sequenced after [`GEN-12`](README.md), which emits the `unsafe`
half of the same call site. If `GEN-2` is the last of these still open, this ticket may land first
with `Threw` and `Ok` only. The `Malformed` case then lands with `GEN-2`, and each PR says which
half it holds.

**Location:** `src/compiler/javascript.rs`, at the facade call site
[`GEN-12`](README.md) emits. `src/compiler/canonical/mod.rs` — `Value::TypedValue`'s
`marked_unsafe`, which is the flag that decides which of the two shapes a call gets.
`std/core/src/Task.zel`, once [`LANG-74`](lang-74.md) writes it, declares the types the wrapper builds.

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

**Approach:** apply [`SPEC-37`](spec-37.md)'s decisions. It settles how a `Task` is
represented, which continuation the wrapper calls and when, and what a facade constant naming a
`Task` exports. None of that is restated here. One constraint was fixed before `SPEC-37` and holds
whatever it decides: [building a `Task` performs nothing](../spec/evaluation-semantics.md#effects).
The companion is therefore called, and caught, when the `Task` is *run*, never when it is built. A
wrapper that caught at build time would catch nothing.

The predicate that decides whether the returned value matches `a` is [`GEN-2`](gen-2.md)'s. This
ticket calls one and routes its answer. The routing is the part that differs by facade kind, and
it is stated in [`GEN-2`](gen-2.md)'s first point.

**Acceptance:** four companions, each behind an unmarked facade under `std/core/tests/`, asserted
as Zelkova tests that `zelkova test` runs through [`LANG-76`](lang-76.md)'s `Test` over a `Task`:
one that throws synchronously yields `Err (Threw _)`; one whose promise rejects yields
`Err (Threw _)`; one that returns a value of the wrong shape yields `Err (Malformed _)`; and one
that returns correctly, after an `await`, yields `Ok` with the value. With no string literals, the
tests match the constructor and do not compare the `String`. That `Threw` carries the host's
description and `Malformed` names the export is asserted on the emitted wrapper's behaviour in a
`node --test` check beside `runtime/js/tests/`. `Malformed` may land with [`GEN-2`](gen-2.md), as
**Depends on** says.
