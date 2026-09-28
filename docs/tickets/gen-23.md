# GEN-23 · An `unsafe` facade's forwarding code does not catch what its companion throws

**Sizing:** small-to-medium. The generated forwarding code is the whole of the change; the size
is in deciding the abort message's wording and in the fixture the acceptance check needs.

**Part of:** [`GEN-2`](gen-2.md) (closed), whose boundary check this extends. Not
[`GEN-16`](gen-16.md): that ticket's wrapper is for an *effectful* facade's `Task`/`Result`
forwarding, a separate call site — an `unsafe` facade gets no wrapper by design
(`docs/spec/interop.md#an-unsafe-facade`, and GEN-16's own text: "An unsafe facade gets no
wrapper — its companion is called directly, and one that throws aborts the program. That half is
GEN-12's.").

**Location:** `src/compiler/javascript.rs` — `Emitter::facade_declaration`, the forwarding code
it builds for a non-unit-result `unsafe` facade:

```
function name(params) {
  const $returned = companion(params);
  return test ? $returned : $abort(description);
}
```

and the constant (arity-0) form, an IIFE built the same way. `docs/spec/evaluation-semantics.md`
— *When a program aborts* — and `docs/spec/interop.md` — *An unsafe facade* — both currently
describe a throwing companion as aborting the program and naming the broken export; both carry a
**Not implemented:** note pointing here.

**Problem:** neither forwarding form wraps the call to `companion(params)` in a `try`. A
companion that returns a value the predicate rejects reaches the runtime's `$abort`, with a
description naming the module and the export
(`` `{module}.{name}`'s companion returned a value its declared type, `{type}`, does not admit ``).
A companion that **throws** instead has its exception propagate unchanged — an ordinary
uncaught JavaScript exception, naming no export, going nowhere near `$abort`. Confirmed against
the emitted `Emitter::facade_declaration` output: nothing between the call and the return
statement could catch anything.

`docs/spec/evaluation-semantics.md#when-a-program-aborts` states "one caused by a facade names
the export whose companion broke" without qualifying it to the returned-value case, and
`docs/spec/interop.md#an-unsafe-facade` states "[a companion] that breaks the [return] promise
aborts the program" the same way — both true of the `$abort` case only.

**Approach:** wrap the call to the companion in a `try`, and on `catch` call `$abort` with a
description naming the export the same way the failed-predicate branch does, presumably folding
in the caught value's message. Two things this ticket does not decide and whoever picks it up
should settle rather than guess:

- **The abort message's exact wording** — whether and how the caught JavaScript error's own
  message is included, and what it reads as for a thrown non-`Error` value (companions are not
  obliged to throw an `Error`).
- **How the arity-0 constant's IIFE is restructured** to add a `catch` without changing its
  arity-N sibling's shape gratuitously — the two forms share `checked` today
  (`Emitter::facade_declaration`'s `checked` binding) and a good fix keeps sharing what it can.

**Acceptance:** a fixture (`tests/js/` or a `std/core/tests/` companion, whichever
`docs/spec/interop.md#testing-a-companion` fits better for an `unsafe` facade) with a companion
that throws synchronously reaches the runtime's `$abort`, naming that export, rather than
surfacing as an uncaught exception — mutation-checked by removing the `try`/`catch` and
confirming the test goes back to catching a raw exception instead. `docs/spec/evaluation-semantics.md`'s
"an abort ... names the export whose companion broke" and `docs/spec/interop.md`'s "[a companion
that breaks the return promise] aborts the program" become true without qualification for the
throwing case too, and both chapters' **Not implemented:** notes citing this ticket are removed.

**Found:** while addressing round-1 review comments on [`GEN-2`](gen-2.md) (PR #269) — the
review flagged that both chapters' **Not implemented:** paragraphs claimed more than
`Emitter::facade_declaration` (as GEN-2 left it) actually does.
