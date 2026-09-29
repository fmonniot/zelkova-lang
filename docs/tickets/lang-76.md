# LANG-76 · A `Test` cannot hold a `Task`, so no effectful check can be a test

**Sizing:** medium. `zelkova-test` gains a constructor and one or two functions, and `run.mjs`
learns to run a `Task` and wait for it. Picking the surface is the part to think about: it is
the first time `Test` grows since the bootstrap fixed it at `Pass | Fail`.

**Part of:** [Active work: effects](README.md#active-work-effects).

**Depends on:** [`LANG-74`](README.md), for `Task` and `Failure`; [`GEN-21`](gen-21.md), for
`$runTask`; [`LANG-72`](README.md) (closed) and [`GEN-20`](README.md) (closed), if the surface
names `Task (Result Failure ())`, which the leaning option below does.

**Location:** `std/test/src/Test.zel` — `Test`, `equal`, `check`; `src/compiler/test_runner.rs` —
the `TAIL` of the generated `run.mjs`, which counts a test as passing when its value's `$` is
`"Pass"`, and the module comment that describes that; `std/core/tests/`, where the tests of this
go.

**Problem:** [*Testing a companion*](../spec/interop.md#testing-a-companion) specifies each check
over a companion as a test facade constant of type `Task (Result Failure ())`. It specifies a
module under `tests/` that exposes one `Test` per check, where a check that raised,
`Err (Threw ..)`, is "reported as a failing test". The chapter's **Not implemented:** paragraph
says why none of it can be written: "`zelkova test` runs a `Test` that is `Pass` or `Fail`; it
cannot run a `Task`." `Test` has no way to hold one, and `run.mjs` reads the verdict
synchronously from the value it imports.

**Approach:**

1. **Choose the surface, and say why.** [What a `Test` holds](../spec/packages.md#what-a-test-is)
   is `zelkova-test`'s decision, not the language's. The ticket leans to one constructor
   carrying a `Task Test`, exposed through two functions:
   - `Test.task : Task Test -> Test`, the general form: run the `Task` and use the verdict it
     produces;
   - `Test.succeeds : Task (Result Failure ()) -> Test`, the form a test facade's constant
     plugs into directly. `Ok ()` passes and `Err _` fails. This is what `std/core`'s ported
     checks ([`TEST-7`](test-7.md)) use, one line per check.

   Without lambdas ([`LANG-34`](lang-34.md)), `succeeds` is written with `Task.map` and a named
   helper. Both functions stay opaque over the constructor, as `equal` and `check` do.
2. **Whether a failure says why** is open. `Err (Threw message)` carries the host's description,
   and printing it beside `FAIL` is what makes a failing companion check debuggable. That means
   a `Fail` which carries a `Maybe String`, or a third constructor. Neither needs a string
   literal, since the `String` comes from the wrapper. If this is deferred, say so, and leave
   `FAIL` with no message as today.
3. **Teach `run.mjs` to wait.** When an export's value holds a `Task`, hand it to `$runTask`,
   await it, and judge the `Test` it produces the same way, which may be another `Task`. A
   rejection is an abort and counts as `ERROR`, the way a module that failed to load does
   today. Update `test_runner`'s module comment, which says the verdict is decided by reading
   `$`.
4. **Order.** Tests keep running one at a time, in the order they are collected. Running
   effectful tests concurrently is not this ticket's.

**Tests:** in `std/test`'s own `tests/` root if it has one by then, otherwise in `std/core/tests/`.
A `Test.task (Task.succeed (Test.check True))` passes. A `Test.task` that produces a `Fail` fails.
A `Test.succeeds` over `Task.succeed (Ok ())` passes, and over `Task.succeed (Err (Threw …))`
fails. The last needs a `String` value, which a facade can produce while
[string literals](../spec/lexical-structure.md#strings) cannot, so it may have to wait for
[`GEN-16`](gen-16.md). Say so if it does. Unit-test the new branch of `run.mjs`'s text in
`test_runner`'s `tests` module.

**Acceptance:** `zelkova test std/core` reports a `Test` built from a `Task` as passing or failing
by the verdict the `Task` produces, and exits `1` when any of them fails. The **Not implemented:**
paragraph under [*Testing a companion*](../spec/interop.md#testing-a-companion) no longer says
that `zelkova test` cannot run a `Task`.
