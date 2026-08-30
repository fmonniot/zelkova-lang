# LANG-13 · A package has no manifest, and its name is hardcoded

**Sizing:** medium. Reading and validating a small JSON document is small; changing what
`compile_package` is handed, and reshaping `PackageName`, touches every caller and every test
that builds one.

**Location:** `src/compiler/mod.rs` — `compile_package`'s `package_path` parameter and the
`// TODO Ultimately we will pass a manifest content instead of a raw path` comment above it;
`PackageName`, whose fields are `author` and `project`; the
`// TODO Load those information from somewhere` above `PackageName::new("zelkova", "core")`;
`ModuleName::as_human_string`, which renders `author/project:Module`. `src/main.rs` passes
`"std/core/src"`. `src/compiler/source/mod.rs` — `load_package_sources` takes the source root
directly.

**Decided ([`docs/spec/packages.md`](../spec/packages.md)):** a package is a directory holding
a `zelkova.json` manifest, with `src/` beside it as its one source root, not configurable. A
package name is a single flat identifier — lowercase ASCII letters, digits and hyphens,
starting with a letter — and not an author/project pair. The manifest carries `name`,
`version`, `exposed-modules`, `dependencies`, and an optional `main` naming the module that
holds a program's entry point.

**Problem:** none of it exists. `compile_package` is handed a source directory and never looks
for a manifest, so a package has no declared name, no version, no stated public surface and no
dependencies. The one name it does have is invented at the call site: every module compiled by
this compiler, from any directory, belongs to `zelkova/core`, which is what
`ModuleName::as_human_string` prints in the checked-modules list and in cross-module
diagnostics. `PackageName` also carries the wrong shape — two segments where the language has
one.

**Approach:**

1. Write `std/core/zelkova.json`, naming the package `zelkova-core` and listing the eight
   modules that compile today under `exposed-modules`. The three `Js.*` facades are not listed:
   a `module javascript` facade is package-internal ([`docs/spec/js-interop.md`](../spec/js-interop.md)).
2. `compile_package` takes the *package* directory, reads and validates the manifest, and
   derives the source root as `<package>/src` before calling `load_package_sources`. A
   directory with no `zelkova.json`, a malformed one, or a name that is not a legal package
   name is a `CompilationError` — and it is raised before the file database exists, so it goes
   back to the caller unrendered, the way loading errors already do.
3. `PackageName` becomes one `String` with a validating constructor;
   `as_human_string` becomes `name:Module`.
4. `src/main.rs` passes `std/core`.

`exposed-modules` is read and validated here — every entry naming a module the package
actually holds — but nothing consults it yet, because no second package exists to be kept out.
Enforcing it is `LANG-14`.

**Acceptance:** `cargo run` compiles `std/core` rather than `std/core/src`, still prints
`parsed 8 modules`, lists all eight as checked, and exits 0. Two `tests/pipeline.rs` tests
over `tests/fixtures/` packages: a directory with no `zelkova.json` is an `Err` naming the
missing manifest, and a manifest whose `name` is not a legal package name is an `Err` naming
the field.

**No block in `docs/spec/packages.md` goes red when this lands.** A manifest has no in-source
counterpart, so nothing the harness can run is able to observe it. The chapter's *What a
package is*, *The source root* and *The manifest* sections each carry a **Not implemented:**
paragraph that has to be deleted by hand as part of this ticket, and the chapter says so.
