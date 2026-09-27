# BUG-42 · A module-name collision with a test-dependency's module is found only after `src/` checks

**Severity:** low (the collision is still reported once `src/` and every test-dependency
compile; until then the user sees other errors first, and never a wrong build).

**Location:** `src/compiler/mod.rs` — `compile`'s second stage (the `if let Some((root,
environment)) = root_tests` block), `compile_tests`'s `visible_modules` call, and
`compile_in_build`'s step 3.d, whose `visible_modules` call is handed the plain
`dependencies` alone.

**Problem:** [*Two modules under one name is an
error*](../spec/packages.md#two-modules-under-one-name-is-an-error) requires a collision to be
reported before any module of the package is compiled. `compile_in_build` does that for every
collision among `src/`, `tests/` and the plain dependencies. A collision that needs a module of
a test-dependency is different: `visible_modules` keys a dependency's modules on what that
package *publishes* (`published`, filled in only once the package has compiled), and since
`SPEC-35` a test-dependency may depend on the package being tested, so every test-only package
is compiled after the root's `src/`. The collision is therefore only seen by `compile_tests`,
which runs after `src/` has been checked, and not at all when `src/` or a test-only package
fails, since `root_tests` is then `None` (or `direct_dependencies` returns early).

Found while addressing review on `SPEC-35`'s PR, which moved the `src/`↔`tests/` half of the
check back ahead of `src/` and recorded this half as a **Known gap:** in the chapter. Left
unfixed there because each fix below is a change to how the build learns a package's exports,
not to the ordering that PR was about.

**Fix:** undecided; two options, neither picked here.

1. Take a test-dependency's exposed module names from its parse rather than from `published`:
   its `src/` modules minus its `private-modules` and minus every `module foreign` facade —
   the same filter `compile_in_build`'s step 6 applies. That means loading and parsing each
   test-only package before the root's `src/` is checked, and either parsing it a second time
   when it is compiled or carrying the parse forward.
2. Compile the test-only packages that do not reach the root first, as the plain packages are,
   and defer only those that depend on it. That closes the gap for the common case and leaves
   it open for exactly the arrangement `SPEC-35` added, so the **Known gap:** paragraph would
   narrow rather than go.

**Acceptance:** a fixture whose root has a failing `src/` module and an unwrapped
test-dependency exposing a module of the same name as one of the root's own; in
`tests/pipeline.rs`, `compile_package_with_tests` on it returns the `ModuleNameCollision`
alone, with no type error. The **Known gap:** paragraph in
[`docs/spec/packages.md`](../spec/packages.md#two-modules-under-one-name-is-an-error) is
removed (or, under option 2, narrowed to what remains) in the same diff.
