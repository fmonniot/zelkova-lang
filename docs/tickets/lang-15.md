# LANG-15 · A package has no test root, and nothing runs a package's tests

**Sizing:** medium for the root and the manifest field; the runner itself is blocked and is
not part of this ticket. Sequence it after `LANG-13`, which is what gives a package a manifest
for `test-dependencies` to be a field of, and after `LANG-14`, which is what makes a
dependency resolvable at all.

**Location:** `src/compiler/source/mod.rs` — `load_package_sources` takes one `root` and walks
it, so a package is exactly one directory of `.zel` files; `SourceFile::load` derives a
module's name relative to that same single root. `src/compiler/mod.rs` — `compile_package`,
which calls it once. `src/main.rs`, which passes `std/core/src`.

**Decided ([`docs/spec/packages.md`](../spec/packages.md)):**

- A package has **two** source roots, `src/` and `tests/`, neither configurable and no third.
  `src/` is what the package ships; `tests/` is compiled when this package's own tests are run
  and never when it is depended on.
- The two roots share one set of module names. `src/Model.zel` and `tests/Model.zel` are both
  `Model`, and that is the same error as declaring one name twice under one root.
- A test module may import any module of its own package, the private ones included. Nothing
  may import a test module — not `src/`, not another package, and it is not listed in
  `private-modules` to achieve that.
- `test-dependencies` is a sixth manifest field, required and possibly empty, taking entries
  of exactly the shape `dependencies` takes. A package listed there is available to `tests/`
  and to nothing else, is not resolved by anyone depending on this package, and does not
  constrain their versions. A package name appears in at most one of the two maps.
- The remaining resolution rules apply to the union of the two maps: acyclic, one version of
  each package, only direct dependencies usable, both recorded in `zelkova.lock`.

**Problem:** none of it exists. `load_package_sources` is handed a single directory and
`compile_package` calls it once, so there is one root and it is whatever the caller passed —
today `std/core/src`, which is a source root standing in for a package. There is no
`test-dependencies` field because there is no manifest (`LANG-13`), no way to mark a module as
test-only, and no runner. A package that wants to test itself today has no place to put the
test and no way to depend on a library for it.

**Approach:** after `LANG-13` and `LANG-14`.

1. `load_package_sources` takes the package root and walks `src/` and `tests/` separately,
   deriving each module's name relative to *its own* root, and tags every `SourceFile` with
   which root it came from. A missing `tests/` is not an error; a missing `src/` is.
2. One name declared under both roots is reported where a duplicate under one root is, naming
   both files.
3. `compile_package` compiles `src/` as it does today. Test modules are compiled only when the
   caller asks for them, and the environment they canonicalize against is the package's own
   modules — private ones included — plus the dependencies of both maps. A `src/` module
   importing a `tests/` module is an unresolved module, not a refused one, exactly as a private
   module of another package is.
4. Read and validate `test-dependencies` alongside `dependencies`, rejecting a name that
   appears in both, and resolve the union.

**Not in this ticket: the runner.** What makes a declaration under `tests/` something a runner
runs is an [open question in the chapter](../spec/packages.md#open-questions), owned by
[SPEC-15](spec-15.md) — it waits on the
same design `main`'s type waits on, since both are a value the outside world picks up and acts
on. This ticket gets test modules compiled with the right things in scope, which is everything
that does not depend on that answer, and
[the toolchain appendix](../spec/toolchain.md#running-a-packages-tests) says the same from the
other side.

**Acceptance:** `tests/pipeline.rs` tests over `tests/fixtures/` packages — a package whose
`tests/` module imports a module listed in its own `private-modules` compiles; a package whose
`src/` module imports a `tests/` module fails on an unresolved module; a package declaring one
module name under both roots fails naming both files; and a manifest listing one package name
in both dependency maps is rejected. `cargo run` must still print `parsed 8 modules`, list all
eight as checked, and exit 0 — `std/core` has no `tests/` directory, and its absence must not
be an error.

**No block in `docs/spec/packages.md` goes red when this lands.** A source root is not source
text and neither is a manifest field, so nothing the harness can run observes either. The
chapter's *Tests* section and
[the toolchain appendix](../spec/toolchain.md#running-a-packages-tests) each carry a **Not
implemented:** paragraph naming this ticket, and both have to be deleted by hand as part of it.
