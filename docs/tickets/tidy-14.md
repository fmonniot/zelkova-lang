# TIDY-14 · CI's clippy job never lints test code, and test code already fails it

**Sizing:** small for the fixes, and the CI change is one line of workflow once the decision
below is made. What could make it bigger is that `-D warnings` on a floating `stable` toolchain
turns every new clippy lint into a red `main`; see *Approach*, step 2.

**Location:** `.github/workflows/rust.yml` — the `clippy` job's `args: --workspace
--all-features`; `crates/zelkova/tests/pipeline.rs` — the nine `match unwrap_in_file(&errors[0])`
and `let bare = unwrap_in_file(&errors[0])` sites; `crates/zelkova-js/tests/javascript.rs` — the
`.err().expect("expected the facade to be refused")` call; `CLAUDE.md` — *Commands* and the
paragraph on what `rust.yml` gates.

**Found while** closing the `TOOL-8` through `TOOL-12` stack (`TOOL-8`'s and `TOOL-12`'s
implementer agents both reported it). Left unfixed there because it is a different diff: the
sites are in test files those PRs only touch for their own tests.

**Problem:** the `clippy` job lints `cargo clippy --workspace --all-features`, which checks the
library and binary targets and none of the `tests/` binaries. `CLAUDE.md` also tells a
contributor to run `cargo clippy --workspace --all-features -- -D warnings` locally, which
likewise skips them. So clippy has no view of the largest part of the tree, and it has drifted:

```
$ cargo clippy --workspace --all-features --all-targets -- -D warnings
error: this expression creates a reference which is immediately dereferenced by the compiler
    --> crates/zelkova/tests/pipeline.rs   (nine sites, all `unwrap_in_file(&errors[0])`)
error: called `.err().expect()` on a `Result` value
    --> crates/zelkova-js/tests/javascript.rs
error: could not compile `zelkova` (test "pipeline") due to 9 previous errors
error: could not compile `zelkova-js` (test "javascript") due to 1 previous error
```

That is the complete list: ten findings in two files, on `origin/main`. `errors` in those nine
tests is a `Vec<&CompilationError>` (it comes from `many(&error)`), so `errors[0]` is already a
reference and the extra `&` is the finding. The other crates' test targets, and `tools/`, pass.

The job does not fail on any of this even if it were widened, because it passes no
`-D warnings`: `actions-rs-plus/clippy-check` fails the job on a clippy *error* and only
annotates a warning, and every one of these lints is a warning by default. `CLAUDE.md` says so
already. So there are two separate gaps, and closing one does not close the other.

**Approach:** the two fixes are settled; the CI change is not.

1. **Fix the ten findings.** Drop the `&` in the nine `pipeline.rs` sites. Replace `.err()
   .expect(msg)` in `javascript.rs` with `.expect_err(msg)`, which needs the `Ok` type to be
   `Debug`; if it is not, `let Err(e) = … else { panic!(msg) }` says the same thing. Test code
   may panic: the no-`unwrap` invariant is for non-test paths.

2. **Decide how much of CI changes.** Three choices, which this ticket does not pick:
   - *Lint all targets only.* Add `--all-targets` to the job's `args`. Test code is now
     visible to clippy and a lint *error* in it fails the PR, but a warning is still only an
     annotation, which is the state the non-test code is in today.
   - *Lint all targets and deny warnings.* `args: --workspace --all-features --all-targets --
     -D warnings`. A warning now fails the PR, and `CLAUDE.md` can stop telling contributors to
     run the stricter command by hand. The cost is the floating toolchain: the job uses
     `dtolnay/rust-toolchain@stable`, so a new stable release that adds a lint turns `main`
     red without any commit. Pinning the toolchain in `rust.yml` and in a `rust-toolchain.toml`
     removes that and adds a bump to maintain. Whether `clippy-check` accepts trailing `--
     -D warnings` in `args` has to be confirmed against the action's README before relying on it.
   - *Leave CI alone* and only update `CLAUDE.md` so the local command includes
     `--all-targets`. Cheapest; the drift this ticket found can recur unseen.

3. **Make `CLAUDE.md` and the sibling tickets say the same thing.** *Commands* lists `cargo
   clippy --workspace --all-features`, and the paragraph on what `rust.yml` gates names the
   `-D warnings` caveat; both change with whichever choice is made. [`TIDY-12`](tidy-12.md)'s
   step 3 and Acceptance, and every skill under `.claude/skills/` that tells an agent to run
   `cargo clippy --workspace --all-features -- -D warnings`, name the same command and want the
   same edit. `grep -rn "clippy" CLAUDE.md .claude docs/tickets` finds them.

**Acceptance:**

- `cargo clippy --workspace --all-features --all-targets -- -D warnings` exits 0 on the branch.
  This is the check for step 1 alone, and it fails today.
- Whichever option step 2 settles on is the one `.github/workflows/rust.yml` runs: if it is not
  *Leave CI alone*, a throwaway commit that puts one of the ten findings back (the extra `&` in
  one `unwrap_in_file(&errors[0])`) turns the `Clippy` check red, and reverting it turns it
  green. Under *Lint all targets only* the lint is a warning, so the check stays green and
  annotates the line instead; say which was seen. State the command and the result in the PR
  body.
- `cargo test --workspace` is unchanged, and `cargo run -- compile std/core` and `cargo run --
  test std/core` report what `CLAUDE.md` records.
