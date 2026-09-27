# SPEC-35 · A package cannot be tested with a library that depends on it, so `zelkova-core` cannot use `zelkova-test`

**Sizing:** medium. It covers the rule, its decision entry, and the implementation, following
the precedent of [`SPEC-34`](README.md), which landed spec and compiler together. The rule is a
paragraph. The implementation is what makes it medium, because the build loop compiles a
package's two roots as one unit today, and this rule needs something to sit between them.

**Part of:** the [bootstrap](README.md#active-work-bootstrap) section.

**Location:** [*One version of each*](../spec/packages.md#one-version-of-each) and
[*`test-dependencies`*](../spec/packages.md#test-dependencies) in `docs/spec/packages.md`.
`src/compiler/resolve.rs` has `Resolver::visit`, which reports `Error::Cycle` the moment a chain
meets a package already on its stack, and `test_only_packages`. `src/compiler/mod.rs` has
`compile`, whose loop calls `compile_in_build` once per package with both of that package's
roots.

**Decided by the language owner on 2026-09-27:** a `test-dependency` may depend on the package
being tested. The edge that closes the loop resolves to that package's own `src/`, which is
already in the build, rather than being a cycle. The test library is compiled against the
package's `src/`, and the package's `tests/` is compiled against both. This is the arrangement
Cargo gives a `dev-dependency` that depends on its dependent. Elm's `elm/core` needs the same
thing to test itself with `elm-explorations/test`.

The other option was a separate package holding core's tests, depending on both. That needs no
rule change. It was rejected because it can see only core's public modules, and the
[*Tests*](../spec/packages.md#tests) section argues that a package's tests must be able to reach
its private ones.

**Problem:** `zelkova-core` is the first package that should run Zelkova tests. Its behaviour is
checked today by hand-written `.mjs` files under `std/core/tests/`. But `zelkova-test` depends
on `zelkova-core`, so writing it in core's `test-dependencies` gives the chain
`zelkova-core → zelkova-test → zelkova-core`. The chapter calls that chain a cycle, and
`Resolver::visit` rejects it as `Error::Cycle`. The graph really is acyclic once the two roots
are counted separately: core's `src/` comes first, then `zelkova-test`, then core's `tests/`.
But nothing in the spec or in the compiler counts them separately.

**Approach:**

1. **Spec.** In [*`test-dependencies`*](../spec/packages.md#test-dependencies), change "the
   graph stays acyclic" to state the exception. A `test-dependency`'s dependency on the package
   being tested is that package's `src/`, and the acyclicity rule applies to the graph in which
   `src/` and `tests/` are separate nodes. Say that this holds for the root package only, since
   no other package's `test-dependencies` are resolved. Record the decision, with the
   alternative above and the reason it lost, as a new `DEC-` entry under `docs/decisions/`.
2. **Resolution.** In `Resolver::visit`, when a chain that started from the root's
   `test-dependencies` meets the root package at the root's own directory, it is not a cycle.
   The walk stops there without an error, because the root is already being resolved. A chain
   through the root's plain `dependencies` that meets the root is still `Error::Cycle`, and so
   is one that meets the root at a *different* directory, which stays `ConflictingSources`. The
   order `resolve` returns has to put the test library after the root's `src/`. Decide whether
   to do that with the order carrying the root twice, as a `src` step and a `tests` step, or
   with `compile` splitting the root itself. The second option keeps `ResolvedPackage` as it is.
3. **The build loop.** `compile` compiles the root's `src/` and publishes its interfaces, then
   compiles each test-only package, then the root's `tests/`. That split can be local to
   `compile`, because `compile_in_build` already takes a `TestRoot`. A test library reached this
   way sees the root through `published` exactly as it would see any other dependency, so the
   `seen_unwrapped` handling of `zelkova-core` applies unchanged.
4. `test_only_packages` must still classify the test library as test-only, so a plain build does
   not compile it.

**Acceptance:**

- A new fixture pair under `tests/fixtures/`: a package `acme-lib` whose `test-dependencies`
  names `acme-check`, and `acme-check` whose `dependencies` names `acme-lib` by path.
  `compile_package_with_tests` over `acme-lib` returns `Ok`. A test module in `acme-lib` that
  imports both an `acme-lib` private module and `acme-check` checks cleanly.
- Neutralise-check it by restoring the unconditional `Error::Cycle` branch. The test goes red.
- The same pair built with `compile_package` (no tests) returns `Ok` and never compiles
  `acme-check`.
- `tests/fixtures/package_dependency_cycle` and `package_cycle_a`/`package_cycle_b` still fail
  with `Error::Cycle`. A plain `dependencies` cycle through the root is unchanged.
- `cargo test --test spec` is green with the new chapter paragraph and decision entry.
- `cargo run -- compile std/core` is unaffected: it prints `parsed 8 modules`, lists all eight,
  and exits 0.

Once [`LANG-63`](lang-63.md) has landed, `std/core/zelkova.toml` can name `zelkova-test` in
`test-dependencies` with `wrapped = false`. Doing so is [`GEN-14`](gen-14.md)'s work, not this
ticket's.
