# DEC-22 · How a `Task` is represented and run: a survey and eight decisions

**Settled:** 2026-09-28, by the language owner (`SPEC-37`). Decision 1 was taken on 2026-09-27.
**Status:** live.
**Where the rule lives:**
[Evaluation semantics — Running a `Task`](../spec/evaluation-semantics.md#running-a-task), for the
one rule a program can observe: a `Task` sequences any number of steps without exhausting the
stack. Everything else here is representation a program cannot observe, so it lives in the code
that implements it — `std/core/src/Task.zel` ([`LANG-74`](../tickets/README.md)),
`runtime/js/zelkova.mjs` ([`GEN-21`](../tickets/README.md), closed) and the facade call site
([`GEN-16`](../tickets/README.md)) — and in no chapter.

[DEC-11](dec-11.md) made a `Task` a value of an ordinary type, and sequencing ordinary functions
over it (decisions [1](dec-11.md#1--an-effect-is-a-value-of-an-ordinary-type-not-a-construct-the-language-knows)
and [4](dec-11.md#4--sequencing-is-ordinary-functions-over-one-concrete-type)).
[DEC-12](dec-12.md) decided what a broken companion produces. Neither said what a `Task` *is* at
run time, what the runtime does with one, or what the code around a facade call builds — and four
tickets needed all three, each of which would otherwise have invented its own.

## The survey

Every effect runtime has to answer the same three questions: how a chain of binds avoids nesting
one call per link, how an asynchronous result gets back into the chain, and whether a long
synchronous chain ever lets anything else run.

**A data structure and an interpreter loop** is the common answer on JavaScript. Elm's scheduler
represents a `Task` as tagged nodes — `SUCCEED`, `AND_THEN`, `BINDING` and the rest — and
`_Scheduler_step` walks them in a loop, keeping the pending callbacks as an explicit linked stack
on the process. A `BINDING` hands its callback a function that re-enqueues the process, and a
`_Scheduler_working` flag keeps a resumption from re-entering the loop it was called from.
PureScript's `Aff` has the same shape: `Pure`, `Bind`, `Sync` and `Async` nodes, a run loop that
keeps binds on a `bhead`/`btail` cons list, and a `Sync` step that continues inside the loop while
an `Async` one returns and is resumed by callback. Both are stack safe for a chain of any length,
and both keep a synchronous effect synchronous. Neither is available to Zelkova as written, for
the reason decision 1 gives.

**The same loop, yielding on a budget.** cats-effect's `IO` interprets a data structure too, and
adds an auto-yield: every `autoYieldThreshold` steps — 1024 by default — the fiber gives up its
thread. The documentation frames this as fairness, not stack: a fiber in a long `flatMap` loop
otherwise starves the others. ZIO's runtime has had the same knob, as `yieldOpCount`. Stack
safety comes from the loop; the yield only decides who runs next.

**A compiled translation, with a trampoline behind it.** Koka compiles effect handlers to
JavaScript by a monadic translation into plain lambda calculus — an effectful function returns
either its value or a `Yield`, and a type-directed pass leaves total functions in direct style.
js_of_ocaml supports OCaml 5's effects by a selective CPS transformation, guided by a static
analysis of which functions can perform one. Its generated code returns to a trampoline after a
fixed number of calls (`tc_depth`, 50 by default) rather than on every one.

**On WebAssembly, the question is whether the guest's stack can be suspended.** wasm_of_ocaml
offers three answers side by side: the CPS transformation, JSPI, and the stack-switching
proposal's native continuations. It relies on JSPI by default, which costs little until effects
are used heavily; the stack-switching path is opt-in, because no stable browser ships it. JSPI reached phase 4 in April 2025 and ships in Chrome and
Firefox. The component model's async ABI (WASI 0.3, released 2026-06-11; Wasmtime 46 implements
it) offers both a stackful lift and a stackless *callback* lift. Under the second, a core export
returns what it wants the runtime to do next — finished, yield and resume, or wait on a set of
waitables — and the runtime calls the component's callback each time something it waits on
happens, until the callback reports that it is finished.

## What the survey settles

The data-structure-and-loop design is the one that works everywhere, but its bind node needs a
type Zelkova cannot declare. Continuation passing gets the same loop without that node, at the
cost of the loop's steps being written in Zelkova rather than in the runtime. What every
JavaScript runtime above has in common is kept: one loop, owned by the runtime, that every
handoff returns to.

On WebAssembly, the stackless callback lift is continuation passing spelled as an ABI. Its three
return codes are the three things a step of the loop below can report.

## 1 — A `Task` is continuation-passing

A `Task a` holds a function that is handed a continuation, `a -> Done`, and calls it once the
result exists — now, or later. `zelkova-core` declares the type and writes `succeed`, `map` and
`andThen` in Zelkova over that one constructor. Partial application of a top-level helper is the
continuation, so no lambda is needed.

```zel
type Task a = Task ((a -> Done) -> Done)
```

It was chosen over **a data representation** — `Succeed a | AndThen …`, interpreted by the
runtime, as Elm and `Aff` do — for two reasons. `AndThen` has to hold a `Task x` and an
`x -> Task a` for some `x` that the outer type does not mention, which is an existential type,
and Zelkova cannot declare one. And CPS never asks Zelkova code to suspend its own stack: on
JavaScript a continuation is called from a promise's `then`, and on WebAssembly from the host's
event loop. That keeps the WebAssembly backend off stack switching, JSPI included, which the
survey shows is still the part of the platform that differs from host to host.

## 2 — `Done` is a union core declares and does not export, and the runtime reads it

```zel
type Done
  = Bounce (() -> Done)
  | Suspend ((() -> Done) -> ())
  | Halt
```

- **`Bounce step`** — keep going: the loop calls `step ()`.
- **`Suspend register`** — waiting: the loop calls `register resume`, and `register` arranges
  for `resume` to be called with the next step once the result is in.
- **`Halt`** — nothing more to do on this stack.

A step is a suspended call, written with partial application — `callWith k a` where
`callWith k a () = k a`. The thunk closes over `a`, so no existential is needed.

**No hand-written code builds a `Done`.** `Task.zel` builds `Bounce`. The runtime builds
`Suspend`, in the helper decision 4 names, and `Halt`, in the final continuation it hands the
`Task` it runs. A program cannot name `Done`, since `Task` does not export it, and cannot reach
it through `Task`, whose constructor is not exported either. A companion returns a bare payload,
as [An effectful facade](../spec/interop.md#an-effectful-facade) says, and never sees a
continuation. So the three constructors can change without changing the language: `Task.zel`
and the runtime move together, and nothing else reads them.

`register` is an impure function behind a type that reads as pure — calling it schedules work.
Only the runtime calls it, and only the runtime builds one. `Task.zel`'s doc comment on `Done`
says so.

Two alternatives were rejected. **A `Done` owned entirely by the runtime**, opaque to Zelkova,
with `Bounce` reached through something the compiler knows, would keep the representation out of
Zelkova — and needs a new kind of built-in, since a facade cannot name a function type. That is
the line [DEC-11](dec-11.md#1--an-effect-is-a-value-of-an-ordinary-type-not-a-construct-the-language-knows)
draws around `Task`. **A `Suspend` holding a JavaScript promise** needs no built-in either, since
only JavaScript builds it — but the declaration would list two constructors and the runtime
handle three, and a promise is JavaScript's alone. `register` is typed in Zelkova and is the
component model's waitable on WebAssembly (decision 6).

Zero constructors was never an option: a union declaration has at least one.

## 3 — Every handoff bounces; the loop yields every N bounces

Core's helpers never call a run function or a continuation directly: each returns a `Bounce` of
the call instead.

```zel
succeedRun a k = Bounce (callWith k a)

andThenRun f run k = Bounce (callWith run (continueWith f k))

continueWith f k a =
  case f a of
    Task next -> Bounce (callWith next k)
```

(Illustrative. `LANG-74` chose the names for `succeed` and `map`; `andThen`'s are
[`LANG-78`](../tickets/lang-78.md)'s.)

Bouncing on the continuation alone is not enough. Running `andThen f (andThen g t)` calls the
outer run function, which calls the inner one, which calls `t`'s — one frame per link, before
any continuation is reached. Bouncing on both handoffs puts every link back at the bottom of the
loop. The only stack a step uses is the stack the pure function inside it uses, as any other call
does. That is what [Running a `Task`](../spec/evaluation-semantics.md#running-a-task) promises: a
chain of any length, and a `Task` that builds itself recursively, run in constant stack.

**The loop yields to the host every N bounces**, for fairness: a long synchronous chain lets
timers and I/O callbacks run instead of holding the event loop until it finishes. N is a runtime
constant, which the spec does not name; cats-effect's 1024 is where to start. The yield is a
macrotask, not a microtask — the microtask queue drains before the host looks at timers or I/O,
so a microtask hop would yield to nothing. Which primitive (`setImmediate`, `setTimeout`, a
`MessageChannel`) is [`GEN-21`](../tickets/README.md)'s.

The cost is an allocation per handoff, two per `andThen` link, and a loop iteration each. A
synchronous chain longer than N stops being synchronous.

Three alternatives were rejected:

- **An async hop every N steps, with no `Bounce`.** Counting steps needs state, and Zelkova has
  none, so the counter has to live in the runtime and core has to reach it at every step. Core
  reaches the runtime only by returning a `Done` — or by calling a compiler-known `$step`, which
  is decision 2's rejected built-in. And once each step returns to the loop, the stack is already
  at the bottom: the hop adds fairness and nothing for depth. This design is that hop, placed
  where it can be counted.
- **A synchronous return to the loop every N calls**, as js_of_ocaml does. It needs the same
  counter, in the same place.
- **No depth guarantee.** Free, and a 100 000-element sequence of `succeed`s would overflow.

## 4 — The wrapper is one runtime helper, and a synchronous companion continues synchronously

The code emitted at an effectful facade's call site builds a `Task` whose run function calls one
runtime helper, `$effect`, with the companion call, the facade's predicate
([`GEN-2`](../tickets/README.md)) and the continuation. Only `$effect` and the loop know what
`Done` looks like, so the emitted code does not change when it does.

When the `Task` is run, `$effect` calls the companion inside a `try` that covers the companion
call and nothing else, and then:

- **a value that is not a `Promise`** is checked, and `$effect` returns
  `Bounce (callWith k (Ok v))`, or `Err (Malformed ..)` in place of `Ok v` if the check fails;
- **a synchronous throw** returns `Bounce (callWith k (Err (Threw ..)))`;
- **a `Promise`** returns `Suspend register`, where `register resume` calls
  `promise.then(onValue, onReject)` and each of the two hands `resume` the step the two cases
  above would have bounced.

A `()` payload is discarded, not checked ([DEC-21](dec-21.md)).

The `try` and the `then` are narrow on purpose. If the continuation ran inside the `try`, or
behind a `.catch` after `.then`, an abort raised by the program *after* the effect — an `unsafe`
facade breaking its promise, further down the chain — would be caught and turned into
`Err (Threw ..)`. An abort is never caught
([When a program aborts](../spec/evaluation-semantics.md#when-a-program-aborts)).

**A promise is recognised by `instanceof Promise`.** It is exactly what an `async function`
returns. A custom thenable is then a plain value, which fails the predicate and is
`Err (Malformed ..)` — visible, not silently misread. A duck-typed test
(`typeof r?.then === "function"`) accepts every thenable, and would mistake a legitimate value
with a `then` field for one once records exist.

**When a synchronous result is available is not in the spec.** No program can observe it today,
since the language has no concurrency, and keeping it unwritten leaves the other candidate open:
**always go through `Suspend`**, one code path and one hop per effect. Emitting the wrapper
inline at each call site was the other alternative, and was rejected because it spreads `Done`'s
shape over every package's output.

## 5 — `$runTask` returns a promise, and owns every abort raised while it runs

The runtime's one entry point is `$runTask(task)`, which returns a `Promise` of the `Task`'s
final value. It hands the `Task` a final continuation that resolves that promise and returns
`Halt`, and drives the loop: call each `Bounce`'s step, stop at `Halt`, and on `Suspend` call
`register` with a `resume` that re-enters the same loop.

**Every entry into the loop is wrapped in one `try` that rejects `$runTask`'s promise** — the
first entry and every resumption. So an abort raised in a continuation reaches the caller of the
`$runTask` it belongs to, even after a `Suspend`. That matters to
[`LANG-76`](../tickets/README.md), where several tests' `Task`s may be in flight at once and
each abort belongs to one test. The alternative — the resumption running outside any `try`, and
an abort there becoming an unhandled rejection — is abort semantics for `zelkova run`, where Node
exits non-zero, and loses the attribution for a test.

**A continuation called twice is an abort, naming the export.** `$effect` guards the step it
hands `resume` with a one-shot flag. A promise settles once and a synchronous companion returns
once, so only a defect in the wrapper or the runtime reaches the guard. It turns that defect into
a stop instead of an effect's continuation running twice.

**A continuation never called** is a `Task` that never produces a value, the second of the
[two outcomes](../spec/evaluation-semantics.md#two-outcomes), and `$runTask`'s promise never
settles.

## 6 — WebAssembly: the direction, with two open questions

[`GEN-15`](../tickets/README.md) is unscheduled, so this is a direction, not a design.

- **A continuation is a closure**: a function reference and its environment, as a WasmGC struct.
  Partial application needs the same representation, so a `Task` asks for nothing new.
- **`Done` and the loop carry over unchanged.**
- **A synchronous component import** is called, checked, and returns `Bounce`, as the JavaScript
  synchronous path does.
- **An asynchronous import uses the component model's callback lift.** `Bounce`, `Suspend` and
  `Halt` correspond to its yield, wait and exit codes. `Suspend`'s `register` adds the subtask's
  waitable to the set the export returns, and the callback the host calls resumes the loop.
- **Stack switching is not needed,** JSPI included — decision 1's second reason.

Open, for `GEN-15` to settle:

1. **How much of the host support can be relied on.** Wasmtime 46 implements WASI 0.3 by
   default; jco's support was announced with the release but was not yet on by default.
2. **Where a trap in a continuation goes.** On JavaScript an abort rejects `$runTask`'s promise.
   The component model's equivalent — whether a trap in the callback fails only the task the
   export was running — has not been worked through.

## 7 — A facade constant naming a `Task` gets the same wrapper, with no arguments

A [facade constant naming a `Task`](../spec/interop.md#facade-constants) is a facade function of
zero arguments as far as the wrapper is concerned. `$effect` calls the exported function each
time the `Task` is run. The one difference is where the `Task` is built: with no arguments to
close over, it is one module-level value, [shared](../spec/evaluation-semantics.md#sharing) like
any constant, and running it twice performs the effect twice.

## 8 — An effect produces one result, and what does not fit is out of scope

A companion returns a value or a promise, and a promise settles once. That covers most effects,
and a callback-style API fits with a `new Promise` in the companion. Two kinds of JavaScript API
do not fit:

- **repeated results** — WebSocket messages, `setInterval`, DOM events, a stream read chunk by
  chunk;
- **cancellation** — an `AbortController` on a `fetch`. A `Task` has no way to say *stop*.

Neither is in scope. Either one would push towards letting a companion see a continuation, or
towards subscriptions and cancellation as a design of their own, and a ticket taking that up
starts here. Letting a companion see `resume` would break decision 2's rule that no hand-written
code builds a `Done`, and with it the freedom to change `Done`.

## Sources

- [elm/core: `Elm/Kernel/Scheduler.js`](https://github.com/elm/core/blob/master/src/Elm/Kernel/Scheduler.js)
- [purescript-aff: `Effect/Aff.js`](https://github.com/purescript-contrib/purescript-aff/blob/main/src/Effect/Aff.js)
- [Cats Effect: Starvation and Tuning](https://typelevel.org/cats-effect/docs/core/starvation-and-tuning)
- [typelevel/cats-effect#1126: Auto-yielding semantics](https://github.com/typelevel/cats-effect/issues/1126)
- [ZIO: Runtime](https://zio.dev/reference/core/runtime/)
- [Generalized Evidence Passing for Effect Handlers](https://www.microsoft.com/en-us/research/publication/generalized-evidence-passing-for-effect-handlers-or-efficient-compilation-of-effect-handlers-to-c/)
- [js_of_ocaml: Tail call optimization](https://ocsigen.org/js_of_ocaml/latest/manual/tailcall)
- [ocsigen/js_of_ocaml#1384: Effects: partial CPS transform](https://github.com/ocsigen/js_of_ocaml/pull/1384)
- [js_of_ocaml: `README_wasm_of_ocaml.md`](https://github.com/ocsigen/js_of_ocaml/blob/master/README_wasm_of_ocaml.md)
- [OCaml Backstage: Wasm_of_ocaml, what changed since 6.1](https://ocaml.org/backstage/2026-04-16-wasm-of-ocaml-what-changed-since-6-1)
- [V8: Introducing the WebAssembly JavaScript Promise Integration API](https://v8.dev/blog/jspi)
- [Bytecode Alliance: WASI 0.3 Launched](https://bytecodealliance.org/articles/WASI-0.3)
- [WebAssembly Component Model: `Concurrency.md`](https://github.com/WebAssembly/component-model/blob/main/design/mvp/Concurrency.md)
