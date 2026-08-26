# BUG-2 · One failing module discards every module that checked successfully

**Severity:** medium (all successful work in a compilation pass is thrown away because one
unrelated module failed)

**Location:** `src/compiler/dependencies.rs` — `ModuleWalker::check_in_order`, whose return
type is `Result<Vec<canonical::Module>, Vec<E>>`, and the `collect_accumulate` helper it ends
on; `src/compiler/mod.rs` — the `unwrap_or_else(|errors| { …; vec![] })` in `compile_package`
that consumes it.

**Problem:** `check_in_order` maps `check` over the modules in dependency order and collects
with `collect_accumulate`, which accumulates every error but keeps the `Ok` values only when
there are no errors at all. One failure anywhere in the package therefore yields
`Err(Vec<E>)`, and `compile_package`'s `unwrap_or_else` substitutes an empty vector for the
modules that did check.

This used to be visible in `cargo run`, where `Bitwise` was the one module that failed
(`BUG-3`, closed) and nothing else imported it:

```
success parsed 7 modules
success checked modules: []
```

Six modules — `Basics`, `Maybe`, `Result`, `Tuple`, `Js.Basics`, `Js.Utils` — canonicalized
successfully and were reported as nothing. Since `BUG-3` closed, every module under
`std/core/src` checks, so `cargo run` no longer reproduces it. Reproduce it instead on
`tests/fixtures/package_canonicalize_fails`, which holds one deliberately broken module
(`Broken.zel`) *and* one that checks cleanly (`Fine.zel`) — the passing module is what gets
discarded, so a fixture with only the broken one would show nothing either way. On that
fixture `compile_package` reports only `1 modules failed to check` and never mentions `Fine`,
because `check_in_order` returned `Err` and there is no list of successes to print; after this
is fixed it should report `Fine` as checked *and* still fail. The `dependencies.rs` unit test
described below pins the same thing at the unit level. The bug itself is unchanged.

The work is not entirely lost: `check_in_order`
inserts each successful module's `Interface` into the `&mut interfaces` map as it goes, so
later modules still resolve against earlier ones. It is only the returned vector, the one
codegen will eventually consume, that is emptied. That makes this a bug that will get much
more expensive the moment a code generator exists, and cheap to fix now.

There is already a TODO on `check_in_order` saying as much — "We might want to have a less
strict approach if we want to make some progress in dependent modules even if the current one
doesn't pass all checks". This ticket is the narrower half of it: not partial progress
*within* a failing module, just not throwing away the modules that wholly succeeded.

**Fix:** change `check_in_order` to return both halves — `(Vec<canonical::Module>, Vec<E>)`,
or `Result<Vec<Module>, (Vec<Module>, Vec<E>)>` if the caller should still be forced to
acknowledge failure. `compile_package` then extends its accumulated `errors` with the errors
*and* keeps the modules. Do not silence the failure in the process: since `BUG-1` closed, a
non-empty accumulation is exactly what makes `compile_package` return `Err`, and this fix must
not make it easier to lose one.

Leave the wider TODO in place, rephrased so it no longer describes this part as unfixed.

**Acceptance:** with one module in the package failing to check, `check_in_order` still hands
back every module that succeeded, and `compile_package` reports them rather than an empty
list. It must still exit non-zero — that half is `BUG-1`, closed. A unit test in
`dependencies.rs` — where `dummy_check` already exists — should pin it directly: one failing
module among several, assert both the successes and the error are returned.
