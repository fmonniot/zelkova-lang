# GEN-14 · Nothing checks that an emitted program computes the right value

**Sizing:** small-to-medium. It is a handful of Zelkova test modules, one manifest entry, and a
file deleted. It is small only because every ticket before it has its own tests. This one proves
the whole thing runs. What could make it bigger is a test that exposes a codegen bug, which is
the point of writing it. File that bug as its own ticket rather than fixing it here.

**Part of:** the [bootstrap](README.md#active-work-bootstrap) section, as its capstone.
Re-scoped on 2026-09-27. This ticket used to be the last step of [`GEN-1`](gen-1.md)'s program,
as a fixture package checked by hand-written `node --test` assertions. It is now the first real
use of `zelkova test`. The behavioural checks it always asked for are written in Zelkova, in
`std/core`'s own `tests/` root.

**Depends on:** [`LANG-69`](lang-69.md) (`zelkova test` exists) and [`SPEC-35`](README.md)
(`zelkova-core` may test-depend on `zelkova-test`).

**Location:** `std/core/zelkova.toml`, whose `test-dependencies` is empty, and
`std/core/tests/`. `std/core/tests/CaseChecks.mjs` is the file to delete: it is a hand-copied
stand-in for emitted output and imports no real build. `tests/javascript.rs` holds the
text-level pins, which stay.

**Decided ([`DEC-18` decision 6](../decisions/dec-18.md#6--the-generated-code-is-checked-in-two-halves-and-cargo-test-does-not-run-node)):**
emission is checked in two halves. Everything that can be tested without running JavaScript is a
Rust test, and those tests already exist. The behavioural half is checked by running the emitted
program. **`cargo test` does not shell out to `node`.** The reason is not squeamishness about the
dependency. A Rust test that skips when `node` is absent is a green test that proves nothing.
`zelkova test` is now the thing that runs it, instead of a bespoke `node --test` harness.

**Problem:** every ticket in the program is graded by eye. The Rust tests assert emitted
*text*, which pins what the emitter writes and says nothing about whether it runs.
`CaseChecks.mjs` is the standing example. It checks JavaScript that a human copied from
`javascript::emit`'s output, so it can pass while the real output is broken.

**Approach:**

1. `std/core/zelkova.toml` names `zelkova-test` in `test-dependencies`, as
   `path = "../test"` and `wrapped = false`.
2. Write test modules under `std/core/tests/`, one per concern, each exposing `Test` values.
   They must cover:
   - **A `case`** over a three-constructor union, returning each branch's value. This is what
     `CaseChecks.mjs` covered. The half that checks a value no branch matches *aborts* stays
     with the Rust text pin. An abort cannot be asserted from inside Zelkova, and
     [`LANG-69`](lang-69.md) would report it as an errored module rather than as a failing
     test.
   - **A call through a `module foreign` facade into its companion.** `Basics` arithmetic
     reaches `Js.Basics`, so something as small as `Test.equal (7 // 2) 3` exercises the
     boundary, the placement of the companion, and the `BigInt` representation of an `Int`.
   - One value per representation the emitter decides: an `Int` (including one that wraps at
     64 bits), a `Float`, a `Bool` from a comparison, a `Char`, a tuple taken apart by a
     pattern, a union with arguments, and a hoisted nullary constructor compared with `==`.
3. Delete `CaseChecks.mjs`. Replace the comment in `tests/javascript.rs` that points at it with
   a pointer to the new test module.
4. **The self-tail-call depth check is not here.** That behaviour is [`GEN-11`](gen-11.md)'s.
   Its acceptance asks for a Zelkova test in this same root, written when it lands. Until then,
   a deep recursion would overflow the stack and turn the test red for a reason no one could
   fix under this ticket.

**Acceptance:**

- `cargo run -- test std/core` runs every test above, reports them all passing, and exits 0.
- For each of the following, make the change, confirm that at least one test goes red, then
  restore:
  - Revert [`GEN-12`](README.md)'s companion placement.
  - Swap two branches in `Emitter::case_expression`'s output.
  - Emit an `Int` literal without its `n` suffix.
  Record which test caught each.
- `CaseChecks.mjs` is gone and nothing references it (`grep -rn CaseChecks`).
- `cargo test --workspace` is unchanged and does not invoke `node`.
- `cargo run -- compile std/core` still prints `parsed 8 modules`, lists all eight, and exits 0.
  A test-dependency is not compiled by a plain build.

Wiring `zelkova test std/core` into CI is [`TEST-3`](test-3.md), which lands after this.
