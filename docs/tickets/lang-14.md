# LANG-14 · Nothing implements a package boundary

**Sizing:** large, and the largest single piece of the packages design. Sequence it after
`LANG-13`: a dependency cannot be named until a package can be.

**Location:** `src/compiler/mod.rs` — `compile_package`, whose `interfaces` map is built
entirely from the modules it just walked, so nothing in the compiler represents a package
other than the one being compiled; `src/compiler/canonical/environment.rs`, which resolves an
`import` by looking its bare module name up in that one flat map;
`src/compiler/parser/grammar.lalrpop`, whose `import` production accepts a module name and
nothing before it.

**Decided ([`docs/spec/packages.md`](../spec/packages.md)):**

- A package's `exposed-modules` is its public surface. Every other module is package-internal
  and unimportable from outside, a `module javascript` facade always among them.
- Only the packages listed in a package's own `dependencies` are importable. A transitive
  dependency is not.
- Module names are one flat namespace across the package under compilation and the exposed
  modules of its direct dependencies, so `import Basics` is the ordinary spelling and names
  a module without naming a package.
- Two importable packages exposing one module name is an error **at the `import` line** that
  names both, not at the point the dependency is declared. `import <package>:<Module>` picks
  one, and an `as` alias is required when both are imported into one file.
- At most one version of a package is in a build, and the resolved set is recorded in a
  generated `zelkova.lock`.

**Problem:** the compiler compiles exactly one package and has no representation of a second
one, so every rule above is unimplemented rather than merely unenforced. `exposed-modules` has
nothing to filter. A facade's package-internal rule is unenforceable. And the package-qualified
import does not parse at all: `import zelkova-core:Maybe` is rejected with `UnexpectedToken`
on `zelkova-core`, the grammar expecting an `up_ident` where the package name is written.

**Approach:** after `LANG-13`, in roughly this order.

1. Resolve the dependency set — reading `zelkova.lock`, generating it when absent — and give
   `compile_package` a way to obtain each dependency's `Interface`s. Where those come from on
   disk is a toolchain question the spec leaves open; compiling each dependency from source is
   the simplest first answer.
2. Load only the `exposed-modules` of each direct dependency into the environment, keyed by
   bare module name alongside the local modules. A name present twice is not an error here —
   it is recorded as ambiguous and reported when an `import` names it.
3. Extend the grammar: an `import` may be preceded by a package name and a `:`. This needs a
   token the tokenizer does not produce today — a package name is lowercase with hyphens, and
   `acme-widgets` currently lexes as three tokens.
4. Report the collision at the `import` line, and report a second import introducing an
   existing prefix, which is the `LANG-7` alias rule extended to the qualified form.

**Acceptance:** the two `expect=unimplemented` blocks in *Imports across a package boundary*
in [`docs/spec/packages.md`](../spec/packages.md) go **red** — they are tagged for a feature
that does not exist, and both must compile once it does. Beyond that: a `tests/pipeline.rs`
test over two `tests/fixtures/` packages showing a dependent importing an exposed module and
failing on a package-internal one, and one showing the collision error naming both packages.
`cargo run` must still print `parsed 8 modules` and list all eight.
