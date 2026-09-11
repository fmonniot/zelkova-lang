# TEST-3 · A `.mjs` companion's test file has no harness in CI, and no documented way to discover it

**Sizing:** small (a `.github/workflows/rust.yml` job plus a `CLAUDE.md` command update; no
compiler code moves). Could grow to small-to-medium if pinning a Node version or adding
`actions/setup-node` turns out to matter — see **Approach**.

**Location:** `.github/workflows/rust.yml` — the `test`/`fmt`/`clippy` jobs, all three
`cargo`-only. `CLAUDE.md`'s *Commands* section, which lists `cargo test` as "full suite: unit
tests + tests/" with nothing about `.mjs` files. `std/core/src/Js/Utils.test.mjs` — the first
(and, as of this writing, only) file matching the convention this ticket is about.

**Found:** while working [`BUG-20`](bug-20.md), which added `Utils.test.mjs` because no harness
for the `.mjs` companion files existed. That ticket's fix runs it with Node's built-in test
runner (`node --test std/core/src/Js/Utils.test.mjs`) and says so in `CLAUDE.md`, but stops
there — wiring it into CI was out of that ticket's scope, and `CLAUDE.md` says outright "Not
wired into CI yet."

**Problem:** `.github/workflows/rust.yml` runs `cargo test`, `cargo fmt --all --check` and
`cargo clippy --all-features` and nothing else. None of them load a `.mjs` file — `cargo test`
only ever executes Rust. So `Utils.test.mjs` runs only when someone remembers to run it by hand,
and a regression in `Utils.mjs` (or in a future sibling file) is invisible to both local
`cargo test` runs and to CI on a pull request.

This also leaves the convention undiscoverable from the one place a session is told to look:
`CLAUDE.md`'s *Commands* section lists every way to check the Rust side of the tree and nothing
about the JavaScript side. A reader who does not already know `Utils.test.mjs` exists has no
prompt to look for it — `cargo test` passing looks like "everything is checked."

[`BUG-24`](bug-24.md) and [`BUG-25`](bug-25.md) both fix `Js/Basics.mjs`, and both say explicitly
that they have no red test until a sibling `Basics.test.mjs` exists — so this gap is not only
about the one file `BUG-20` covered; it is about what any test file for a `.mjs` companion is
checked by, once written.

**Approach:** two independent pieces, and the ticket does not pick between the options within
each:

- **A CI job that runs every `.mjs` test file.** `ubuntu-latest` GitHub Actions runners carry a
  preinstalled Node, so the smallest version is a job with a bare `run: node --test
  std/core/src/**/*.test.mjs` step (`node --test` accepts a glob and runs every match). Whether
  that job needs `actions/setup-node` to pin a specific Node version — matching how the `test`
  job pins a Rust toolchain via `dtolnay/rust-toolchain@stable` — or can rely on the runner's
  preinstalled version is a real choice, not a detail to guess at while filing this.
- **Whether the job gates the build**, or is marked `continue-on-error: true` the way `fmt` and
  `clippy` are. `Utils.test.mjs` pins real runtime behaviour (BUG-20's thrown-error fix), unlike
  formatting — an argument for gating outright — but `CLAUDE.md`'s own words about `fmt`/`clippy`
  ("CI does not actually gate on them... a red clippy will not be caught for you") show this
  repository has an existing precedent for advisory-only checks, so this is worth a deliberate
  choice either way, not a default.

Either way, `CLAUDE.md`'s *Commands* section should say how to run the `.mjs` tests locally,
next to `cargo test`, so a session that only reads `CLAUDE.md` knows a JavaScript-side check
exists at all.

**Acceptance:** opening a pull request that regresses `Utils.test.mjs` (e.g. reverting BUG-20's
thrown-error guard) shows a failing (or, if advisory was chosen, a visibly-red-but-non-blocking)
check in the PR's CI run, without anyone running `node --test` by hand. `CLAUDE.md`'s *Commands*
section names the command that runs the `.mjs` tests locally.
