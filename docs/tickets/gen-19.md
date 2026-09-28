# GEN-19 · Reorganize build output so prod and test live at the same folder level

**Sizing:** small-to-medium

**Location:** `src/compiler/mod.rs` — `compile_package`, `output::write`; `src/compiler/output.rs` — module doc comment, `File::path`; `src/compiler/javascript.rs` — *Paths* section

**Problem:** Today a build writes to two places:

- Production code to `build/js/`
- Tests to `build/test/js/`

This nests the test output under `test/`, when both should be siblings beneath `build/`. The asymmetry matters because build tools often expect code and tests at the same depth, and scripts that consume the build need to handle two different path shapes.

**Approach:**

Reorganize to:

- Production code to `build/out/js/` (the `out/` folder name is open to bikeshedding within this ticket)
- Tests remain at `build/test/js/`

Both are now direct children of `build/`, with the runtime and code files under each.

This is a path-only change — no logic changes. The steps are:

1. Update `src/compiler/mod.rs:1100` to write to `build_dir.join("out").join("js")` instead of `build_dir.join("js")`
2. Update the *Paths* section in `src/compiler/javascript.rs` to show `build/out/js/` as the root
3. Update the module doc comments in `src/compiler/output.rs` and `src/compiler/mod.rs` wherever they cite `build/js/` or describe the structure
4. A test build's copy-from-prod step copies from `build/out/js/`, and documentation reflects that

**Acceptance:** `cargo run -- compile std/core` produces files at `build/out/js/`, `cargo test --test pipeline` checks that the smoke test and compiler tests both pass, and `cargo test --workspace` succeeds.
