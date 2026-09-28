# LANG-74 · `std/core` declares no `Task` and no `Failure`

**Sizing:** medium. It adds one module of Zelkova, `std/core/src/Task.zel`, written against a
representation [`SPEC-37`](spec-37.md) settles, plus its tests. It could grow if writing
`andThen` without lambdas or `let` turns up a typer gap: higher-order partial application
through a union constructor has not been exercised this hard before.

**Part of:** [Active work: effects](README.md#active-work-effects).

**Depends on:** [`SPEC-37`](spec-37.md), for `Done` and for the helper shape; [`LANG-73`](README.md),
for the `String` that `Failure`'s constructors carry — closed, so `std/core` now declares it.

**Location:** `std/core/src/Task.ignored` — Elm's `effect module Task`, over
`Elm.Kernel.Scheduler`, `Platform` and `Task x a`. None of that carries over: it is two type
parameters, a kernel import, and a declaration form Zelkova does not have.
`src/compiler/default_imports.rs` — the `Task` entry, `import Task exposing (Task)`, inert while
the module is missing. `tests/pipeline.rs::stdlib_package_compiles`.

**Problem:** [Effects](../spec/evaluation-semantics.md#effects) says `zelkova-core` declares
`Task`, exposes it without its constructors, and gives it `succeed`, `map` and `andThen` as
ordinary functions ([*Sequencing*](../spec/evaluation-semantics.md#sequencing)).
[*An effect that can fail*](../spec/evaluation-semantics.md#an-effect-that-can-fail) says `Task`
also declares `Failure = Threw String | Malformed String` and exposes it with its constructors.
None of it exists. Every block in the spec that imports `Task` is `expect=unimplemented` and fails
at that import, and every later ticket in the effects section needs the type.

**Approach:**

1. Write `std/core/src/Task.zel`: `module Task exposing (Task, Failure(..), succeed, map, andThen)`.
   Declare `Task` with the continuation-passing constructor, and `Done` however
   [`SPEC-37`](spec-37.md) decides. Write the three functions in Zelkova with top-level helpers
   and partial application, since [`LANG-34`](lang-34.md) (lambdas) and [`LANG-33`](lang-33.md)
   (`let`) are not on this path. `Task` is exposed opaquely. That is what keeps building one out of
   parts inside core, per the chapter.
2. Declare `Failure` with both constructors exposed.
3. Decide what happens to `Task.ignored`, as [`LANG-73`](README.md) did for `String.ignored`.
   Elm's `map2`…`map5`, `sequence`, `onError`, `mapError`, `perform` and `attempt` are not in the
   chapter and are out of scope. `Task (Result e a)` makes most of them a different function
   anyway, and a later ticket can decide which ones Zelkova wants.
4. Once the module compiles, the default import `Task exposing (Task)` starts working by itself.
   Update [modules.md's *Known gap*](../spec/modules.md#the-default-imports) and
   `default_imports.rs`'s module comment accordingly.
5. Retag the blocks this unblocks and no more. The `Failure` block in
   [*An effect that can fail*](../spec/evaluation-semantics.md#an-effect-that-can-fail) already
   compiles. The `read` and `now` facade blocks in [interop](../spec/interop.md#an-effectful-facade)
   and [evaluation-semantics](../spec/evaluation-semantics.md#where-a-task-comes-from) may start
   to compile. If they do, the harness says so, and they become `expect=ok` with their
   **Not implemented:** sentences trimmed to what is still missing, which is the wrapper and the
   shape check.

**Tests:** `tests/typer.rs` for the three signatures, plus a `Task Int` handed where a
`Task Bool` is expected, which is a type error. Code generation already covers unions and
partial application, so `javascript::emit` should emit the module as it stands. Assert that in
`tests/javascript.rs`. Whether the emitted `andThen` actually *sequences* is checked once
[`GEN-21`](gen-21.md) can run a `Task`, not here.

**Acceptance:** `cargo run -- compile std/core` parses and checks `Task` along with the rest, and
`stdlib_package_compiles` pins the new count. A module with no `import` can annotate
`t : Task Int` and write `t = Task.succeed 1`. Outside `zelkova-core`, writing the `Task`
constructor is a canonicalization error. `cargo test --workspace` is green.
