# TIDY-12 · The compiler's crates are on edition 2018

**Sizing:** small. `cargo fix --edition` does the source changes. It grows only if the 2024
style edition's reformatting is large enough to want reviewing by crate.

**Depends on:** [`TOOL-5`](README.md), which creates the manifests this ticket edits. Doing it
first would put a repository-wide reformat underneath a repository-wide file move.

**Location:** `Cargo.toml` — `[workspace.package]`; the `Cargo.toml` of each of the five
crates under `crates/`, and of `tools/spec-doc` and `tools/spec-site`.

**Found while** settling [`TOOL-5`](README.md)'s open decisions, which left the edition alone
so that its diff stays a move.

**Problem:** the compiler is on `edition = "2018"`, and the two crates under `tools/` are on
`2021`. Nothing depends on 2018. The split is only an accident of when each manifest was
written, and it means the workspace's crates are formatted and linted under two sets of rules.

**Approach:** move every member to `2024`, one edition for the workspace.

1. `cargo fix --edition --workspace --all-targets`, then set `edition = "2024"` under
   `[workspace.package]` and `edition.workspace = true` in all seven manifests. This is one
   commit, and it holds no formatting change.
2. `cargo fmt --all` in a second commit that holds nothing else. The 2024 style edition sorts
   imports differently, so this one touches most files.
3. Fix what `cargo clippy --workspace --all-features -- -D warnings` newly reports, in a third
   commit if there is anything.

`grammar.lalrpop` is not covered by `cargo fix`. The parser LALRPOP generates from it is
compiled under the crate's edition, so a build failure inside the generated file after step 1
is fixed in the grammar's own Rust blocks.

**Acceptance:**

- `grep -rn "edition" --include=Cargo.toml .` shows `2024` once, under `[workspace.package]`,
  and `edition.workspace = true` in every member.
- `cargo test --workspace`, `cargo clippy --workspace --all-features -- -D warnings`,
  `cargo fmt --all --check` and the local rustdoc command in `CLAUDE.md` are green.
- `cargo run -- compile std/core` prints `parsed 10 modules`, lists all ten as checked, and
  exits 0. `cargo run -- test std/core` reports the count `CLAUDE.md` records.
- The commit from step 2 is reproduced by running `cargo fmt --all` on its parent.
