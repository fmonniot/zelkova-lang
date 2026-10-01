# LANG-78 · `std/core`'s `Task` has no `andThen`

**Sizing:** small. It adds one exported function and its two helpers to `std/core/src/Task.zel`,
plus a signature test, an emit test and removing the spec paragraph. It could grow if a lambda turns out to
change how `andThen` should be written: the shape in `DEC-22` decision 3 uses named helpers and
partial application, and this ticket does not settle whether lambdas make it simpler.

**Part of:** [Active work: effects](README.md#active-work-effects).

**Depends on:** [`LANG-74`](README.md), closed, which wrote `Task.zel` with `Task`, `Failure`,
`succeed` and `map`; and [`LANG-34`](lang-34.md), lambdas. `andThen` was taken out of
`LANG-74` on purpose and deferred until lambdas are in the language.

**Location:** `std/core/src/Task.zel` — the module header's `exposing` list, and the helpers
`succeedRun`, `mapRun` and `mapContinue` beside which `andThen`'s belong.
`docs/spec/evaluation-semantics.md` — [*Sequencing*](../spec/evaluation-semantics.md#sequencing),
whose **Not implemented:** paragraph cites this ticket.

**Problem:** [*Sequencing*](../spec/evaluation-semantics.md#sequencing) says core gives `Task` an
`andThen : (a -> Task b) -> Task a -> Task b`. `Task.zel` exports `succeed` and `map` and no
`andThen`, so a program cannot run one `Task` after another, and the runtime's sequencing
check has nothing of core's to call.

**Approach:**

1. Write `andThen` in `Task.zel` over the continuation-passing constructor
   ([`DEC-22` decision 1](../decisions/dec-22.md#1--a-task-is-continuation-passing)), and export it.
   Both handoffs return a `Bounce`, as
   [decision 3](../decisions/dec-22.md#3--every-handoff-bounces-the-loop-yields-every-n-bounces)
   explains: the outer run function is reached by a `Bounce`, and so is the `Task` that `f`
   returns. The decision's illustration is
   `andThenRun f run k = Bounce (callWith run (continueWith f k))` with `continueWith f k a = case f a of Task next -> Bounce (callWith next k)`.
2. No helper builds `Suspend` or `Halt`: those are the runtime's.
3. Whether lambdas are used at all is this ticket's to decide once it is worked. The illustration
   in `DEC-22` needs none, and `succeed` and `map` in `Task.zel` are written without any.
4. Retag any spec block that needed `andThen` and could not compile without it. As of this
   ticket's filing none exists: the only block naming `andThen` is an `expect=fragment`.
   Remove the **Not implemented:** paragraph under *Sequencing*.
5. `std/core/src/Task.ignored` still holds Elm's `andThen` as a reference. Leave it as it is,
   or trim it, as [`LANG-74`](README.md) left the rest of the file.

**Tests:** `crates/zelkova-compiler/tests/typer.rs`, beside the `succeed` and `map` signature test, for `andThen`'s
signature and for the case that a function returning a `Task Bool` handed a `Task Int` is a type
error. `crates/zelkova-js/tests/javascript.rs`, beside the `Task` emit test, asserting that both of `andThen`'s
helpers return a `Bounce`. Whether the emitted chain actually *sequences* is checked once
`$runTask` can run a `Task`, not here.

**Acceptance:** `Task.andThen` is exported from `zelkova-core`'s `Task` module with type
`(a -> Task b) -> Task a -> Task b`, checked by a test that annotates it so. `cargo run --
compile std/core` still checks every module, `cargo test --workspace` is green, and the
**Not implemented:** paragraph under *Sequencing* is gone.

**Where this came from:** filed while working [`LANG-74`](README.md). The ticket had
`Task.zel` declare `andThen` beside `succeed` and `map`; the project owner asked for it to be
split out and to wait on lambdas, so `LANG-74` shipped without it.
