# GEN-16 · The wrapper an effectful facade's call site gets

**Sizing:** medium, and **blocked** — see below. The wrapper itself is small; what it builds
does not exist.

**Blocked on:** `Task` and `Failure` existing at all; [`LANG-9`](lang-9.md), since
`Task (Result Failure String)` does not parse while a type argument must be a bare name; and
[`LANG-43`](lang-43.md), which is what holds an unmarked facade to the result type this wrapper
assumes. Sequence after [`GEN-12`](README.md), which emits the `unsafe` half of the same call
site.

**Part of:** [`GEN-1`](gen-1.md) in subject, and deliberately outside its program: the original
GEN-1 text carried this as inherited work, and it cannot be written until the three blockers
above clear.

**Location:** `src/compiler/javascript.rs`, at the facade call site
[`GEN-12`](README.md) emits. `src/compiler/canonical/mod.rs` — `Value::TypedValue`'s
`marked_unsafe`, which is the flag that decides which of the two shapes a call gets.
`std/core/src/Task.ignored` is the module that would declare the types, and it does not compile.

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
and nothing emits one. Today it is also unreachable: every facade in the tree is marked
`unsafe`, nothing declares `Task` or `Failure`, and nothing holds an unmarked facade to a `Task`
result — so an unmarked facade is read as if it carried `unsafe`
([`LANG-43`](lang-43.md)).

**Approach:** not settled, and it depends on decisions `Task` itself has not made. What this
ticket needs from whatever declares `Task`: how a `Task` is represented at runtime, since the
wrapper builds one; and what running one does, since the catch has to be around the *running*
rather than around the building — [building a `Task` performs
nothing](../spec/evaluation-semantics.md#effects), so a wrapper that caught at build time would
catch nothing.

The predicate that decides whether the returned value matches `a` is [`GEN-2`](gen-2.md)'s; this
ticket calls one and routes its answer. The routing is the part that differs by facade kind and
is stated in [`GEN-2`](gen-2.md)'s first point.

**Acceptance:** not written while the blockers stand. What it will have to show: a companion
that throws yields `Err (Threw ..)` carrying the host's description, a companion that returns a
value of the wrong shape yields `Err (Malformed ..)` naming the export, and a companion that
returns correctly yields `Ok` with the value — all three asserted by running the emitted output
under `node` through [`GEN-14`](gen-14.md)'s harness.
