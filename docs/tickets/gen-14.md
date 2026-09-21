# GEN-14 · The end-to-end check under node

**Sizing:** small. A fixture package, a step that emits it, and the JavaScript assertions. It is
small only because every ticket before it has its own tests; this one proves the whole thing
runs.

**Depends on:** [`GEN-13`](gen-13.md), and through it everything else in the program.

**Part of:** [`GEN-1`](gen-1.md) — this is the acceptance the original ticket named.

**Location:** a fixture package in the repository, emitted into a build directory; JavaScript
assertions beside it, run by `node --test`. `.github/workflows/rust.yml` is where the job goes,
and [`TEST-3`](test-3.md) is either the ticket that created that job or the ticket this one
completes — check which has landed.

**Decided ([`GEN-1`](gen-1.md) decision 6):** emission is checked in two halves. Everything
testable without running JavaScript is a Rust test, and those are already written by the tickets
that added each piece. The behavioural half — that the emitted program computes the right value
— is a JavaScript test under `node --test`, in the shape
[*Testing a companion*](../spec/interop.md#testing-a-companion) already uses. **`cargo test`
does not shell out to `node`.**

The reason is not squeamishness about the dependency: it is that a Rust test which skips when
`node` is absent is a green test proving nothing, which is the failure mode `CLAUDE.md` names as
the most common review finding there is.

**Problem:** every ticket in the program is graded by eye until this lands. The Rust tests
assert emitted *text*, which pins what the emitter writes and says nothing about whether the
writing runs. [`LANG-56`](lang-56.md) is the standing example of what that costs — its own
**Note** says it has no red test behind it, because nothing loads a `.mjs`.

**Approach:** a fixture package with a `zelkova.toml` and a handful of modules, compiled by a
step that emits it into a build directory, followed by `node --test` over assertions that import
the emitted modules and check values.

**What the fixture has to cover**, because these are the two rules nothing else can check:

- **A self-recursive function in tail position, deep enough that a non-tail emission exhausts
  the stack.** Pick the depth deliberately and say in a comment why that number: too shallow and
  the test passes without the rewrite, which makes it exactly the test that passes both ways.
  Verify it by reverting [`GEN-11`](gen-11.md)'s loop emission and watching it overflow.
- **A call through a `module foreign` facade into its companion `.mjs`**, which is the only
  thing that exercises the boundary at all.

Beyond those two, cover one value of each representation the emitter decides — an `Int` coming
back as a `BigInt`, a union value's `$` field, a tuple as an array, a `Bool` as a boolean — so
that a change to the encoding is caught here and not only in the text assertions.

**Wire it into CI.** One job, two steps: emit the fixture, then `node --test`. It shares that
job with the `.mjs` companion checks rather than standing up a second harness, so the glob
widens to reach both. Whether the job gates the build or is advisory the way `fmt` and `clippy`
are is [`TEST-3`](test-3.md)'s question and gets one answer for both — but note that this one
pins runtime behaviour, which is the argument that ticket already records for gating.

`CLAUDE.md`'s *Commands* section names the local command, and its sentence saying `cargo test`
never loads a `.mjs` stays true and gains this file's command beside it.

**Not in this ticket:** a `zelkova` binary that compiles and runs ([`GEN-17`](gen-17.md)). The
emitting step here is whatever `cargo run` already does; giving it arguments is that ticket's.

**Acceptance:** the command in `CLAUDE.md` runs green locally over the fixture, and the same
command runs in CI on a pull request. Reverting [`GEN-11`](gen-11.md)'s loop emission turns the
depth test red; reverting [`GEN-12`](gen-12.md)'s companion placement turns the facade test red.
`cargo test --workspace` is unchanged and does not invoke `node`. `cargo run` still prints
`parsed 8 modules`, lists all eight and exits 0.
