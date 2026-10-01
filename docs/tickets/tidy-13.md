# TIDY-13 · `CompilationError::Many` is never constructed

**Sizing:** small. One variant and one match arm, no behaviour change.

**Location:** `crates/zelkova-compiler/src/lib.rs` — the `CompilationError::Many` variant and its arm in
`CompilationError::as_diagnostic_in`; `crates/zelkova/src/lib.rs` — `check_errors`, which unwraps one.

**Found while:** addressing review of [`TOOL-7`](README.md)'s PR. That ticket moved the
accumulation of a build's errors into `driver::BuildError::Many` and said nothing else about
`CompilationError` changing, so removing the variant was left out of it.

**Problem:** `grep CompilationError::Many` finds the variant, its render arm, and the one arm
of `driver::check_errors` that flattens it. No code builds one: `check_package` hands its
errors back as `PackageCheck::errors`, and `driver::BuildError::Many` is the group a build
accumulates and renders. The variant's render arm still carries the reasoning for not
flattening a group's labels across files, which now belongs to `BuildError::Many`, and the
variant's doc comment has to explain that nothing builds it.

**Approach:** delete the variant, its arm, and the `CompilationError::Many` arm of
`check_errors` together with the unit test that pins it
(`driver::tests::a_many_of_the_check_is_flattened_into_its_members`). Check first that
nothing outside this crate (`tools/`, `tests/`) names it. Whether anything is to be built on
it, such as the language server of [`TOOL-6`](tool-6.md), is not decided here; if so, keep it.

**Acceptance:** `grep -rn "CompilationError::Many" src tests tools` finds nothing, and
`cargo test --workspace` and `cargo clippy --workspace --all-features -- -D warnings` pass.
