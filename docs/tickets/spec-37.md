# SPEC-37 · How a `Task` is represented and run is undesigned, on either target

**Sizing:** large. It is a research-and-decide ticket: a survey of how effect runtimes are built on
JavaScript and on WebAssembly, a decision entry, and the chapter sentences that follow from it. No
compiler change. What could make it larger is the WebAssembly half, where the component model's
async story is still moving.

**Part of:** [Active work: effects](README.md#active-work-effects), as its first step. Every
ticket there that builds, runs or wraps a `Task` reads its answer.

**Location:** [`docs/spec/evaluation-semantics.md`](../spec/evaluation-semantics.md) — *Effects*,
*Sequencing* and *Running a `Task`*; [`docs/spec/interop.md`](../spec/interop.md) — *An effectful
facade*; [`DEC-11`](../decisions/dec-11.md), decisions 1 and 4, which this builds on.
`std/core/src/Task.ignored` is Elm's `Task` over `Elm.Kernel.Scheduler` and is no guide: that
kernel is exactly what [interop](../spec/interop.md) rules out.

**Decided (by the language owner, 2026-09-27):** **a `Task` is continuation-passing.** A
`Task a` holds a function that is handed a continuation, `a -> Done`, and calls it once the
result exists — now, or later. `zelkova-core` declares the type and writes `succeed`, `map` and
`andThen` in Zelkova, over that one constructor, the way [*Sequencing*](../spec/evaluation-semantics.md#sequencing)
says any module writes functions over a type it declares. No lambda is needed to do it: partial
application of a top-level helper is the continuation.

```zel
type Task a = Task ((a -> Done) -> Done)

andThen : (a -> Task b) -> Task a -> Task b
andThen f (Task run) = Task (andThenRun f run)

andThenRun f run k = run (continueWith f k)

continueWith f k a =
  case f a of
    Task next -> next k
```

(The block is illustrative. The CPS shape is what was decided. The names, and whether the helpers
look like this, were not.)

It was chosen for two reasons. A data representation (`Succeed a | AndThen …`, interpreted by
the runtime) needs an existential type for `AndThen`'s intermediate result, and Zelkova cannot
declare one. And CPS never asks Zelkova code to suspend its own stack: on JavaScript a
continuation is called from a promise's `then`, and on WebAssembly from the host's event loop.
That keeps WebAssembly off stack switching (JSPI, or the stack-switching proposal).

**Problem:** the decision above names a shape and settles none of what a backend has to emit.
Four tickets in the effects section ([`LANG-74`](lang-74.md), [`GEN-21`](gen-21.md),
[`GEN-16`](gen-16.md), [`LANG-76`](lang-76.md)) need answers that nobody has written down, and
each would otherwise invent its own:

1. **What `Done` is.** It is the result of every continuation and of running a `Task`, and no
   Zelkova program should be able to make or read one. Is it a type `Task` declares without
   constructors? What is it at run time on each target? What is the runtime allowed to put in
   it?
2. **Stack depth.** A chain of `succeed` and `andThen` that never waits nests one call per link,
   and so does a loop written as a recursive `Task`. The candidates are: trampolining in the
   runtime (`Done` carries "the next step", and the runner drives a loop), an async hop every
   N steps, or an async hop on every effect. They differ in cost per effect, in whether a
   synchronous companion stays synchronous, and in what the spec can promise. The
   [tail-call rule](../spec/evaluation-semantics.md#recursion-and-tail-calls) and
   [`GEN-11`](gen-11.md) are adjacent and do not cover it, because the recursion goes through
   continuations rather than self-calls. Whatever is chosen, the chapter should say what depth a
   program may rely on, the way it already does for tail calls.
3. **What the wrapper emits on JavaScript.** Which of these call `k` synchronously and which go
   through the microtask queue: a companion that returns a plain value, one that returns a
   promise, one that throws synchronously, one that rejects. How that interacts with (2). Where
   [`GEN-2`](gen-2.md)'s predicate sits: on the value that arrives, per
   [*An effectful facade*](../spec/interop.md#an-effectful-facade).
4. **Running one.** What the runtime's entry point is (a function that returns a promise of the
   final value is the obvious candidate), and what it does with an
   [abort](../spec/evaluation-semantics.md#when-a-program-aborts) raised inside a continuation.
   What happens when a continuation is called twice by a broken wrapper, or never called.
5. **WebAssembly.** How a continuation is represented (a closure, the same one partial
   application needs). How a synchronous component export and an async one (the component
   model's `future<T>`, WASI 0.3) each reach `k`. What that asks of a host. Of the five
   questions, this is the one that may honestly end as "the direction, with open questions" and
   not a full design, since [`GEN-15`](gen-15.md) is unscheduled. Say which it is.
6. **Facade constants.** A [facade constant naming a `Task`](../spec/interop.md#facade-constants)
   exports a function. Say how its wrapper differs from a function facade's, if at all.

**Approach:**

1. **Survey the state of the art before picking.** At least: how Elm's scheduler, PureScript's
   `Aff`, Scala's cats-effect/ZIO fibers and Koka's effect handlers compile to JavaScript, and
   what each does about stack depth; what OCaml 5, Koka and Wasmer/Wasmtime-hosted languages do
   on WebAssembly; the component model's async ABI and JSPI's status. Write down the sources, the
   way [`DEC-12`](../decisions/dec-12.md) does.
2. **Record the decisions** as a new entry, `docs/decisions/dec-21.md`, one numbered decision per
   question above, each with the alternatives it was chosen over. Include the CPS choice itself
   and the two reasons above, so the record does not depend on this ticket, which is deleted when
   it closes.
3. **State the rules in the chapters.** Anything a program can observe goes in
   [Evaluation semantics](../spec/evaluation-semantics.md): the depth a `Task` chain may reach,
   and when a synchronous effect's result is available. Representation details a program cannot
   observe stay out of the spec and go to the tickets that implement them. Per
   [the conventions](../spec/conventions.md), this ticket writes no compiler code.
4. **Update the implementing tickets** — [`LANG-74`](lang-74.md), [`GEN-21`](gen-21.md),
   [`GEN-16`](gen-16.md) — so that each one's **Approach** cites the decision it applies in place
   of the open question it carries today.

**Acceptance:** `docs/decisions/dec-21.md` exists, is listed in the decisions index, and answers
questions 1–6 above or says for each one why it stays open. The chapter sentences in step 3
exist and `cargo test --test spec` is green. [`LANG-74`](lang-74.md), [`GEN-21`](gen-21.md) and
[`GEN-16`](gen-16.md) no longer name an undecided representation question.
