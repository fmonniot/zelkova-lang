# GEN-18 · A build that compiles the tests writes none of them, so nothing can run one

**Sizing:** small. `compile` already checks every module a test run needs. This ticket writes
them, to a directory of their own. What could make it bigger is the output layout if a test
module's name can collide with a `src/` module's in one package directory. It cannot today,
because [a name two modules both answer to](../spec/packages.md#two-modules-under-one-name-is-an-error)
is reported across both roots. Confirm that before relying on it.

**Part of:** the [bootstrap](README.md#active-work-bootstrap) section.

**Location:** `src/compiler/mod.rs` has `compile`, whose second loop compiles each test-only
package without extending `checked`, and `compile_tests`, which checks the root package's
`tests/` root and hands nothing back. Its doc comment and `compile_package_with_tests`'s both
say the test modules are not written. `emit_build` and
`output::write` produce and write the tree.

**Decided ([`DEC-18` decision 5](../decisions/dec-18.md#5--output-is-written-per-package-beside-the-root-manifest)):**
output is `build/js/<package-name>/<module path>.mjs` beside the root manifest, and a build
that emitted any error writes nothing. `compile_package_with_tests`'s doc comment promises that
a test build leaves `build/js/` exactly as a plain build would. So a test module never turns up
in a plain build's output, and this ticket keeps that promise.

**Problem:** a test can only be run from emitted JavaScript, and a test build emits none. The
root package's `tests/` modules and every test-only package are checked and then dropped. That
was the right call before anything could run a test, and it is now what stands between
[`LANG-63`](lang-63.md)'s collected tests and [`LANG-69`](lang-69.md)'s runner.

**Approach:** this ticket picks the layout, since the decision above does not reach tests.
**A test build writes a complete, separate tree at `build/test/js/`**, laid out exactly like
`build/js/`: the runtime at its root, then one directory per package. It holds every package of
the build (test-only packages included) and the root package's `tests/` modules beside its
`src/` modules. A separate tree keeps the promise above without any filtering. It can also be
cleared and rewritten on each run without touching a plain build's output, and the runner
imports from one root.

1. `compile` takes the output directory from its caller, as it already does through
   `build_dir`. `compile_package_with_tests` passes `build/test` and keeps the test-only
   packages' modules and the root's test modules in `checked`.
2. `compile_tests` returns the checked test modules, so `emit_build` sees them beside the
   `src/` ones. A facade under `tests/` needs its companion placed like any other
   facade's; [*Testing a companion*](../spec/interop.md#testing-a-companion) is where that
   shape comes from.
3. Update both doc comments to describe the new tree.

**Acceptance:**

- A `tests/pipeline.rs` test runs `compile_package_with_tests` over
  `tests/fixtures/package_test_dependency`, writing into a temp directory. Afterwards
  `test/js/package-test-dependency/AppTest.mjs` and the test-only package's `Expect.mjs` both
  exist, and `js/` holds neither. Match whatever helper the existing `compile_package_into`
  tests use for the temp directory.
- Neutralise-check it by restoring the `checked.extend` guard. The test goes red.
- A plain `compile_package` over the same fixture still writes no test module and no test-only
  package.
- `cargo run -- compile std/core` still prints `parsed 8 modules`, lists all eight as checked,
  and exits 0.
