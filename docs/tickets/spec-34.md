# SPEC-34 · Only `zelkova-core` may declare a module the default imports name, and the exemption is keyed on the package rather than on module names

**Sizing:** medium. One new resolution error, one parameter removed from five signatures, a
decision entry, two spec paragraphs, and a handful of fixtures and test call sites re-pointed.
No decision is left open: the language owner settled the approach on 2026-09-26 (below), and a
session implementing this should not reopen it.

**Location:**

- `src/compiler/default_imports.rs` — `declares_a_default` (~line 230), the name test the
  exemption is keyed on today; `implicit_imports` (~line 247), which takes it as a `bool`.
- `src/compiler/canonical/environment.rs` — `new_environment` (~line 338), whose scalar-seeding
  block (~line 359) and `implicit_imports` call are both gated on that `bool`.
- `src/compiler/canonical/mod.rs` — `canonicalize` (~line 1656); `src/compiler/mod.rs` —
  `check_module` (~line 1571) and the `package_declares_a_default` computed in the per-package
  step of the build (~line 1387); `src/compiler/dependencies.rs` — `add_default_import_edges`
  (~line 349), `ModuleWalker::new` / `new_for_root` (~lines 396, 422) and the `check` fn pointer
  of `check_in_order` (~line 520). All of these thread the one `bool`.
- `src/compiler/resolve.rs` — `visible_modules` (~line 588) and `Error` (~line 174), where the
  new rule is reported.
- `tests/` — `tests/support/mod.rs` (`canonicalize_core_standalone`,
  `canonicalize_exempt_package`), `tests/spec.rs` (`package_of` ~line 164, the `canonicalize`
  wrapper ~line 172, the group runner ~line 583), every `check_module(.., true|false)` call in
  `tests/pipeline.rs`, `tests/typer.rs`, `tests/ir.rs`, `tests/javascript.rs`,
  `tests/compiler/canonical.rs:2513`; fixtures `dep_rival_basics`, `package_wrapped_rival_basics`,
  `package_core_basics_collision`, `dep_core`.
- `docs/decisions/dec-17.md`, `docs/spec/packages.md` (*`zelkova-core` is a dependency of every
  package*), `docs/spec/modules.md` (*The default imports*, the **`zelkova-core` is the
  exception** paragraph), `docs/tickets/lang-62.md` (its **Acceptance**).

**Problem:** the default-imports exemption ([`DEC-17`](../decisions/dec-17.md) decision 1) and
the scalar seeding that replaces it (decision 3) are both *about* `zelkova-core`, but the
compiler decides them with `declares_a_default`: "does this package hold a module named
`Basics`, `List`, `Maybe`, `Result`, `Task`, `Char`, `String` or `Tuple`". DEC-17 argued the two
coincide because no other package can declare one of those names without colliding with core.
That premise holds under [the spec](../spec/packages.md#zelkova-core-is-a-dependency-of-every-package)
but not under the compiler, for two reasons that are both still true today:

1. The compiler does not supply core ([`LANG-62`](lang-62.md)), so a package that does not list
   it has nothing to collide with. `tests/fixtures/dep_rival_basics` (`acme-basics`) is such a
   package: it holds its own `Basics`, is taken for core-shaped, receives no default imports,
   and has core's five scalars seeded into every module even though core is not in its build.
   Since [`BUG-37`](README.md) put the package in every `QualName`, that seeded `Int` and
   `acme-basics`' own `Basics.Int` are two distinct types inside one package.
2. Core compiles only `Basics`, `Maybe`, `Result` and `Tuple`; `List`, `Task`, `Char` and
   `String` are `.ignored`. So an **ordinary application** that depends on core and has its own
   `src/List.zel` collides with nothing and is also taken for core-shaped. Verified on
   2026-09-26: a package depending on `std/core` with `src/List.zel` and a `src/Main.zel`
   writing `y : Maybe Int` / `y = Nothing` fails on `Nothing` (`VariantNotFound`), because
   every module of the package lost all eight default imports. The user made no choice that
   should cost them that.

**Decided (2026-09-26, by the language owner):** the two questions are made one question by
construction, and it is asked of the package's name.

1. **A package other than `zelkova-core` may not declare a module named after one of the eight
   default imports** — the names in `DEFAULT_IMPORTS`, as `default_imports::is_default` tests
   them. This is reported at resolution, whether or not core is in the build and whether or not
   core publishes that module yet. It is the spec's existing rule (core's module names are
   taken in every package) enforced ahead of `LANG-62` for these eight names, and it is what
   makes DEC-17's premise true rather than assumed. It applies to both source roots:
   `tests/List.zel` is rejected as well.
2. **Everything the flag decided is keyed on `PackageName == zelkova-core`**: default-import
   suppression, the implicit graph edges, and scalar seeding. The `bool` parameter is removed
   from every signature listed under **Location**; the answer is derived from the package the
   callee already receives.
3. Rejected alternatives, for the decision entry: *leave it* (the `List.zel` application above
   is the counter-example); *split the flag* (seed scalars by package, suppress defaults by
   name — fixes `acme-basics` and leaves the `List.zel` application broken); *key both on
   package without the reservation* (a non-core package holding `Basics` would then receive
   `import Basics` inside `Basics.zel` itself — the self-cycle DEC-17 exists to avoid).

**Approach:**

1. **The error.** Add `resolve::Error::ReservedModuleName { package: PackageName, module:
   ModuleOrigin }` (the local module's origin, which carries its file). In `visible_modules`,
   when `package.name` is not `CORE_PACKAGE`, a local module for which `is_default` holds is
   pushed as this error and **not claimed** — so a build that does contain core does not also
   report a `ModuleNameCollision` for the same module. Every such module is reported, not only
   the first, as for collisions. `PhaseError`: message in the user's vocabulary, e.g.
   ``"`List` is reserved for `zelkova-core`, and `app` declares a module of that name"``; a
   note saying the name is one of the modules every package imports by default; labels exactly
   as `ModuleNameCollision` gives a `Local` origin (mirror whatever it does, including when it
   gives none). Because resolution errors stop the package before any module is compiled, a
   dependent of the rejected package gets `DependencyNotCompiled`, as for any failed
   dependency.
2. **The key.** Add `PackageName::is_core(&self) -> bool` beside `PackageName::core()`. Remove
   the `bool` from `check_module`, `canonicalize`, `new_environment` (read it off
   `module_name.package()`), `implicit_imports` (take `is_core: bool` from its caller, or the
   `ModuleName` — either, as long as nothing computes it from module names), the `check` fn
   pointer of `check_in_order`, and the `package_declares_a_default` local in the build step.
   `ModuleWalker::new` and `new_for_root` take `package: &PackageName` in its place and hand
   `package.is_core()` to `add_default_import_edges`. Delete `declares_a_default` and its unit
   test `declares_a_default_asks_about_the_whole_package`; `is_default` stays (the reservation
   and `add_default_import_edges` both use it). Rewrite the doc comments that explain the old
   key — the one on `declares_a_default`'s callers in `mod.rs` (~line 1373, the "asked of
   `src/` alone" argument no longer applies: the answer cannot differ between builds because it
   depends on no module list at all), `new_environment`'s, `canonicalize`'s, `check_module`'s,
   `new_for_root`'s, `check_in_order`'s, `add_default_import_edges`', and the module doc of
   `default_imports.rs` (~lines 34 and 52).
3. **Tests that passed a mismatched pair.** The `bool` let a test say `(test_package(), true)`
   or `(core, false)`; neither is representable now.
   - `canonicalize_exempt_package` (`test_package`, `true`) becomes a `PackageName::core()`
     module. Keep the function (its callers want the exempt shape) and update its doc comment.
   - `canonicalize_core_standalone` (`core`, `false`) was a core module that *received* the
     defaults — with an empty interface map, so nothing arrived either way. It becomes an
     exempt core module; check each caller still says what it meant, and merge the two helpers
     if they are now the same function.
   - Every `check_module(.., true)` in `tests/pipeline.rs` already passes a core-shaped package
     (`std_package()` or a `pkg` of std modules): confirm the package is `PackageName::core()`
     and drop the argument. Every `.., false)` passes `test_package()` or a non-core `pkg`:
     drop the argument. Same for `tests/typer.rs`, `tests/ir.rs`, `tests/javascript.rs` and
     `tests/compiler/canonical.rs:2513`.
   - `tests/spec.rs`: `package_of` stays — a chapter block declaring `module Basics` is still
     compiled as `zelkova-core` — but its doc comment stops citing the flag, and the
     `canonicalize` wrapper and group runner stop passing it. The spec harness does not go
     through `visible_modules`, so the reservation never fires there.
   - The `dependencies.rs` unit tests that build a walker or call `add_default_import_edges`
     pass a package: `PackageName::core()` where the modules are core-shaped, any other valid
     name where they are not. The mutation-checked test at ~line 977 keeps its mutation note.
4. **Fixtures.**
   - `a_wrapped_dependencys_basics_declares_no_scalar` / `package_wrapped_rival_basics` becomes
     the pipeline test of the reservation: compiling it reports exactly one
     `ReservedModuleName` naming `acme-basics` and `Basics`, plus the root's
     `DependencyNotCompiled`, and no type error. Rename the test to say so and rewrite the
     comments in the fixture's manifest and `App.zel`. `Scalar::declares`' package check stays
     pinned by `scalars.rs`' `another_packages_basics_int_is_not_the_scalar`, which is now its
     only pin — that is expected: the check becomes defence in depth, not a rule a build can
     reach.
   - `a_dependencys_basics_collides_with_cores` / `package_core_basics_collision` can no longer
     reach a collision (`acme-basics` is rejected first). Retarget it to a core module that is
     **not** one of the eight: add a minimal `tests/fixtures/dep_core/src/Bitwise.zel`, a new
     `tests/fixtures/dep_rival_bitwise` package declaring its own `Bitwise`, and point the
     fixture at it unwrapped. The test then asserts a `ModuleNameCollision` on `Bitwise`
     between `zelkova-core` and the rival, which is the collision-with-core rule `LANG-62`
     relies on. Check that adding a module to `dep_core` changes no other test's expectations.
   - Add one test for the case that motivated this: a root package holding its own
     `src/List.zel` gets `ReservedModuleName` for `List` (with and without core in its
     dependencies), and one showing `tests/List.zel` is rejected too when tests are compiled.
   - Add a `resolve.rs` unit test beside the existing `visible_modules` tests: a package named
     `zelkova-core` holding `Basics` gets no error; any other package holding it does.
5. **Docs.**
   - `DEC-17`: add **decision 4**, *Only `zelkova-core` may declare the eight, so the exception
     is asked of the package's name*, carrying the two reasons under **Problem** and the three
     rejected alternatives under **Decided** (into *What it was chosen over*). Update the
     **Status** line, and the third bullet of *What the question turned out to be*, which
     states as a fact what was until now only an assumption. [`DEC-15`](../decisions/dec-15.md)
     decision 3's *It scopes itself* paragraph needs no edit; check it still reads true.
   - `packages.md`, *`zelkova-core` is a dependency of every package*: state that the eight are
     reserved outside core even in a build that does not contain core, and narrow the **Not
     implemented** paragraph accordingly — `LANG-62` still owns supplying core and the collision
     for core's other module names. Hold the prose to
     [`conventions.md`](../spec/conventions.md).
   - `modules.md`, the **`zelkova-core` is the exception** paragraph: one clause linking to the
     reservation, so a reader does not wonder what happens to a package that names a module
     `Maybe`.
   - `lang-62.md` **Acceptance**: `package_core_basics_collision` now collides on `Bitwise`;
     update the sentence that names it.

**Acceptance:**

- A package other than `zelkova-core` declaring a module named after one of the eight, under
  `src/` or `tests/`, fails resolution with `ReservedModuleName`, whether or not core is among
  its dependencies; a package named `zelkova-core` declaring them does not.
- No code path decides the exemption or the scalar seeding from module names:
  `declares_a_default` is gone, and `grep -rn 'declares_a_default' src tests tools` finds
  nothing.
- Each new test was seen to fail with its behaviour neutralised (drop the reservation check in
  `visible_modules`; make `is_core` return `false`), per `CLAUDE.md`'s testing notes, and its
  doc comment says what was mutated.
- `DEC-17` carries decision 4; `packages.md`, `modules.md` and `lang-62.md` are updated as
  above; `cargo test --test spec` is green.
- `cargo test --workspace` is green, `cargo clippy --workspace --all-features` and `cargo fmt
  --all --check` are clean, and `cargo run` still prints `parsed 8 modules`, lists all eight as
  checked, and exits 0.

**Related:** found in review of [PR #245](https://github.com/fmonniot/zelkova-lang/pull/245)
(`BUG-37`), which made the two `Int`s in `acme-basics` distinguishable. [`LANG-62`](lang-62.md)
is the ticket that makes the rest of core's names taken everywhere; this one does it early for
the eight, because the exemption depends on them.
