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

Visible in `cargo run` today, where only `Bitwise` fails (`BUG-3`) and nothing else imports
it:

```
success parsed 7 modules
success checked modules: []
```

Six modules — `Basics`, `Maybe`, `Result`, `Tuple`, `Js.Basics`, `Js.Utils` — canonicalized
successfully and are reported as nothing. The work is not entirely lost: `check_in_order`
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
acknowledge failure. `compile_package` then extends `diagnostics` with the errors *and* keeps
the modules. Do not silence the failure in the process: `BUG-1` is what makes a non-empty
error list actually fail the compilation, and this fix must not make it easier to forget.

Leave the wider TODO in place, rephrased so it no longer describes this part as unfixed.

**Acceptance:** with one module in the package failing to check, `check_in_order` still hands
back every module that succeeded, and `cargo run` on `std/core/src` lists the six modules that
check rather than `[]`. A unit test in `dependencies.rs` — where `dummy_check` already exists
— should pin it directly: one failing module among several, assert both the successes and the
error are returned.
