# TEST-3 · CI runs neither a package's Zelkova tests nor a `.mjs` companion's checks

**Sizing:** small. It adds a `.github/workflows/rust.yml` job and moves no compiler code. It
could grow to small-to-medium if pinning a Node version with `actions/setup-node` turns out to
matter (see **Approach**).

**Part of:** the [bootstrap](README.md#active-work-bootstrap) section, as its last step.
Re-scoped on 2026-09-27. This ticket used to be only about `node --test` over the companion
checks. Once [`GEN-14`](gen-14.md) lands, `std/core` also holds Zelkova tests run by
`zelkova test`, and both kinds of check belong in one job.

**Depends on:** [`GEN-14`](gen-14.md), which gives `zelkova test std/core` something to run.
The `node --test` half has no dependency and could land first.

**Location:** `.github/workflows/rust.yml`, whose `test`, `fmt` and `clippy` jobs all run only
`cargo`. `CLAUDE.md`'s *Commands* section. `std/core/tests/Js/*.mjs`, the companion checks in
the place [*Testing a companion*](../spec/interop.md#testing-a-companion) gives them.

**Found:** while working [`BUG-20`](bug-20.md), which added the first companion check because
no harness existed. That fix ran the check with Node's built-in test runner and stopped there.
Wiring it into CI was out of its scope.

**Problem:** CI never runs anything that executes the JavaScript the compiler emits or ships.
`cargo test` executes only Rust. So a regression in a companion `.mjs`, or in the emitter's
runtime behaviour, is invisible on a pull request. [`BUG-24`](README.md) and
[`BUG-25`](README.md) both landed fixes to `Basics.mjs` that CI never loaded.

**Approach:** one job, two steps, in this order:

1. `cargo run -- test std/core`, which runs `std/core`'s Zelkova tests ([`LANG-69`](lang-69.md),
   [`GEN-14`](gen-14.md)).
2. `node --test 'std/core/tests/**/*.mjs'`, which runs the companion checks that are not
   Zelkova tests yet.

There are two choices this ticket does not make:

- **The Node version.** `ubuntu-latest` runners ship with a preinstalled Node. The other option
  is to pin one with `actions/setup-node`, the way the `test` job pins its Rust toolchain.
- **Whether the job gates the build**, or is marked `continue-on-error: true` like `fmt` and
  `clippy`. These checks pin runtime behaviour, which argues for gating. The repository does
  have a precedent for advisory checks. Make the choice on purpose rather than by default.

In `CLAUDE.md`'s *Commands*, the paragraph that says `cargo test` never loads a `.mjs` stays
true. Its note that the checks are "not wired into CI" goes.

**Acceptance:**

- A pull request that regresses a companion shows a failing check (or, if advisory was chosen,
  a visibly red but non-blocking one), without anyone running a command by hand. Reverting
  BUG-20's thrown-error guard in `Utils.mjs` is one such regression.
- A pull request that breaks a test in `std/core/tests/*.zel` shows the same.
- `CLAUDE.md`'s *Commands* names both local commands.
