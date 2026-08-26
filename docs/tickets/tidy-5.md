# TIDY-5 · Fix all outstanding `cargo clippy` warnings

**Sizing:** medium — around 40 individual warning sites, but they collapse into six
mechanical categories plus one structural fix; no single change is large.

**Location:** `cargo clippy --all-features --all-targets` on current `main` reports:

- `src/compiler/canonical/environment.rs` — 35× `clippy::bool_assert_comparison`, all in
  `#[cfg(test)]` code, all `assert_eq!(expr, true, msg)`.
- `src/compiler/parser/layout.rs:262` — `clippy::collapsible_match`.
- `src/compiler/parser/layout.rs:408` and `:443` — `clippy::clone_on_copy` (`Position`
  implements `Copy`).
- `src/compiler/parser/layout.rs:405` — `clippy::into_iter_on_ref`.
- `src/compiler/source/files.rs:195` — `clippy::needless_borrow`.
- `src/compiler/typer/mod.rs:817` — `clippy::wrong_self_convention`, on the private
  `#[cfg(test)]` helper `Signature::from_type(&mut self, …)`.
- `tests/compiler_tests.rs:1` — `clippy::duplicate_mod`: this file's top-level `mod support;`
  loads `tests/support/mod.rs` a second time, redundant with the `#[path = "../support/mod.rs"]
  mod support;` already declared in `tests/compiler/canonical.rs:12`. Nothing in
  `compiler_tests.rs`'s own tree reaches the top-level copy via `crate::support` or
  `super::support` — grepping the repo confirms it (the only `super::support` uses are in
  `tests/compiler/parser/*.rs`, resolving to the unrelated
  `tests/compiler/parser/support/mod.rs`).
- `tests/support/mod.rs:10,14,19,25,35` — `dead_code`, five functions ("never used") inside
  the `compiler_tests` binary specifically: `test_package`, `parse_source`,
  `canonicalize_standalone`, `canonicalize_with_interfaces`, `maybe_interface`. This is a
  symptom of the `duplicate_mod` finding above, not a separate root cause — see Approach.
- `tests/compiler/canonical.rs:26` — `dead_code`, `fn bool_t()`, unrelated to the above.
- `tests/typer.rs:22` — `unused_imports`, the `parser` import. **This one may already be gone
  by the time this ticket is picked up** — [TIDY-4](tidy-4.md) removes it and was open with a
  PR up (not yet merged into `main`) when this ticket was filed. Check before doing anything
  here; if `TIDY-4` has landed, there is nothing left to fix at this site.

**Problem:** none of these are individually worth their own ticket — most are one clippy
pattern repeated across a file, not a design question — but the count means they never all
get fixed in the same sitting as whatever change surfaces one of them, so they accumulate.
Three separate `work-ticket` agents (working `TIDY-1`, `TIDY-2`, and `TIDY-3` in parallel on
2026-08-25) each independently hit the `layout.rs:262` warning, confirmed it pre-existed on
`main`, and left it alone to keep their diffs scoped — which is the correct call per-ticket,
but means it just sits there. This ticket exists to clear the backlog in one pass instead of
one line at a time.

Note the scope is broader than pure `clippy::*` lints: `cargo clippy` also surfaces plain
`rustc` warnings (`dead_code`, `unused_imports`) on the targets it compiles, and those are
included here too — the ask is "no warnings from running `cargo clippy`", not "no
clippy-namespaced lints specifically".

Also note `.github/workflows/rust.yml`'s clippy job runs `cargo clippy --all-features` **without**
`--all-targets`, so it only compiles the lib target and currently only ever sees the single
`collapsible_match` warning — the other ~39 are invisible to CI today regardless of this
ticket. Fixing them is still worth doing (they're real, and `cargo clippy --all-features
--all-targets` is what a contributor sees locally per `CLAUDE.md`'s "run both locally"
instruction), but don't expect CI's clippy job to visibly change.

**Approach:**

1. **`bool_assert_comparison` (35×, `environment.rs`):** mechanical, one shape throughout —
   `assert_eq!(expr, true, msg)` → `assert!(expr, msg)`. `cargo clippy --fix` (or
   `--fix --lib -p zelkova-lang --tests`) should apply all of these unattended; review the
   diff rather than trusting it blindly, but no manual per-site judgment is expected.
2. **`collapsible_match` (`layout.rs:262`):** clippy's own suggestion (shown in its output)
   collapses the nested `if` into the outer `match` guard. Apply it, then re-run the layout
   tests specifically — this function is the offside-rule state machine and worth double
   checking by eye, not just by clippy's autofix.
3. **`clone_on_copy` (`layout.rs:408,443`)** and **`into_iter_on_ref` (`layout.rs:405`):**
   drop the `.clone()` calls (`Position` is `Copy`) and change `.into_iter()` to `.iter()`
   (the receiver is a reference, so `.into_iter()` doesn't consume the `Vec` — it was never
   doing what the name implies).
4. **`needless_borrow` (`files.rs:195`):** drop the `&` clippy points at; `root_path` is
   already the reference type the callee wants.
5. **`wrong_self_convention` (`typer/mod.rs:817`):** rename `Signature::from_type` to
   something that doesn't read as a `from_*` constructor — e.g. `type_signature` or
   `signature_of_type` — and update its one call site (`Signature::of_type`, three lines
   above). Test-only code; no public API to worry about.
6. **`duplicate_mod` + five `dead_code` warnings in `tests/support/mod.rs`:** delete the
   top-level `mod support;` at `tests/compiler_tests.rs:1`. Verify first — with
   `git grep -n 'support::' tests/compiler_tests.rs` or equivalent — that nothing in that
   file's own top level (as opposed to inside its nested `mod compiler { … }`) actually uses
   it; the investigation for this ticket found no such use, but confirm again against
   whatever `main` looks like when this is picked up. Removing the line should make both the
   `duplicate_mod` warning and the five `dead_code` warnings on `tests/support/mod.rs`'s
   functions disappear together, since `tests/compiler/canonical.rs`'s own
   `#[path = "../support/mod.rs"] mod support;` remains as the one copy actually in use.
7. **`dead_code` on `tests/compiler/canonical.rs:26` (`bool_t`):** unrelated to the above —
   check whether it's genuinely unused (a leftover from a removed or rewritten test) or
   whether some test *should* be calling it and isn't. If it's genuinely dead, delete it
   rather than `#[allow]`-ing it.
8. **`tests/typer.rs:22` unused `parser` import:** only touch this if `TIDY-4` has not landed
   by the time this is picked up (see Location note above).

After each category, re-run `cargo clippy --all-features --all-targets` to confirm that
category's warnings are gone and no new ones appeared — clippy's autofix has been known to
introduce its own follow-on lint in adjacent code.

**Acceptance:** `cargo clippy --all-features --all-targets` produces zero warnings, `cargo
clippy --all-features` (CI's actual command) also produces zero, `cargo test` passes, `cargo
build` emits no warnings, `cargo fmt --all` produces no diff, and `cargo run` still reports
`parsed 7 modules` plus the one documented `Bitwise` warning. No behaviour change is expected
anywhere in this ticket — if any single fix looks like it would change behaviour rather than
just silence a lint, stop and flag that site instead of applying it, since that would no
longer be a same-ticket-scope change.
