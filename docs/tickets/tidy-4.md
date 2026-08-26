# TIDY-4 · Test-module doc comments still describe the type checker as a stub

**Sizing:** trivial — two module-level doc comments.

**Location:** `tests/typer.rs` — the `//!` header; `tests/pipeline.rs` — the `//!` header.

**Problem:** both files open by telling the reader that `typer::type_check` is a stub. It is
not. `type_check` performs real Hindley–Milner inference and is called by `check_module`, and
all fifteen tests in `tests/typer.rs` are un-ignored and passing.

`tests/typer.rs` says:

> These tests describe the expected behaviour **once the type checker is integrated** into the
> main pipeline. They are all marked `#[ignore]` and will be un-ignored (or replaced) when
> `type_check` is no longer a stub.

followed by a "How to activate" section explaining how to remove `#[ignore]` attributes that no
longer exist. `tests/pipeline.rs` says:

> Currently `type_check` and `exhaustiveness::check` are stubs returning `Ok(())`, so these
> tests primarily validate that the canonicalization phase succeeds (or fails) as expected
> end-to-end.

Half of that is still true — `exhaustiveness::check` really does return `Ok(())` — which is
what makes it worth correcting rather than deleting: the sentence is now the wrong shape for a
reader deciding whether a pipeline test failure implicates the type checker.

This is the class of defect `CLAUDE.md`'s standing invariant about doc comments exists for. A
comment that describes the tree as of the day it was written is what the next reader trusts,
and here it would send them looking for `#[ignore]` attributes that were removed months ago.

**Approach:** rewrite both headers to describe what the files do now. For `tests/typer.rs`:
these are type-checker expectation tests running the full `check_module` path and asserting on
type-level properties; drop the "How to activate" section entirely. For `tests/pipeline.rs`:
`check_module` runs canonicalization *and* type checking; `exhaustiveness::check` is still a
stub, so a pipeline test cannot fail for exhaustiveness reasons yet.

Say only what you have verified. If you are unsure whether a claim in the rewritten comment
holds, delete the claim rather than softening it.

While in `tests/typer.rs`, drop the unused `parser` from
`use zelkova_lang::compiler::{check_module, parser};` — it is the only compiler warning the
build currently emits, and it is left over from the same period as the stale header. Removing
it makes a clean `cargo build` the baseline, which is worth more than the one line it costs.

**Acceptance:** neither header describes `type_check` as a stub or refers to `#[ignore]`
attributes, and both accurately state which phases a failure in that file can implicate.
`cargo test` passes and `cargo build` emits no warnings — no test code changes beyond the
unused import.
