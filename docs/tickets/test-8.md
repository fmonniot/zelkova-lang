# TEST-8 · CI never runs the runtime's own checks, `runtime/js/tests/zelkovaChecks.mjs`

**Sizing:** small. One more `node --test` glob in a workflow step, and the matching line in
`CLAUDE.md`'s *Commands* section. What could make it bigger: a check that has drifted red since
nothing has run it, though all ten pass today.

**Location:** `.github/workflows/rust.yml` — the `javascript` job, whose *Zelkova tests* step
runs `cargo run -- test std/core` and whose *Compiler checks* step runs
`node --test 'tests/js/**/*.mjs'`. `runtime/js/tests/zelkovaChecks.mjs`, the checks over
`$curry` and `$abort` in `runtime/js/zelkova.mjs`. `CLAUDE.md` — *Commands*.

**Problem:** the runtime is hand-written JavaScript that every emitted module imports, and its
own checks are a `node --test` file whose header says to run it with
`node --test 'runtime/js/tests/**/*.mjs'`. The one glob in CI does not match that path, and
`CLAUDE.md`'s *Commands* section does not name it, so a change to `zelkova.mjs` that breaks
`$curry` is caught only if the Zelkova tests happen to exercise the broken case.
[`GEN-21`](README.md) added the `Task` loop's checks to the same file, and
[`GEN-16`](README.md) added a check beside it, which makes the gap bigger rather than smaller.

**Approach:** run `runtime/js/tests/**/*.mjs` in the `javascript` job — its own step, or one more
glob on the *Compiler checks* step, since both are checks of what the compiler ships rather than
of a package's companions — and guard it with the same `find … | grep .` the *Compiler checks*
step uses, so a moved file fails the step instead of passing on an empty match. Add the command
to `CLAUDE.md`'s *Commands* section beside the other `node --test` line.

**Acceptance:** breaking `$curry`'s over-application branch in `runtime/js/zelkova.mjs` turns the
step that runs these checks red on a PR, and restoring it turns that step green.

**Found:** by the first attempt at [`GEN-2`](README.md), whose own `node --test` file under
`tests/js/` needed a CI step of its own. Left unfixed there because it is not that ticket's
file.
