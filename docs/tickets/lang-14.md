# LANG-14 · Nothing implements a package boundary

**Sizing:** large, and the largest single piece of the packages design. Sequence it after
`LANG-13`: a dependency cannot be named until a package can be.

**Location:** `src/compiler/mod.rs` — `compile_package`, whose `interfaces` map is built
entirely from the modules it just walked, so nothing in the compiler represents a package
other than the one being compiled; `src/compiler/canonical/environment.rs`, which resolves an
`import` by looking its module name up in that one flat map.

**Decided ([`docs/spec/packages.md`](../spec/packages.md)):**

- Every module of a package is importable from outside it except the ones `private-modules`
  names, and a `module javascript` facade is never importable whatever the manifest says.
- Only the packages listed in a package's own `dependencies` are importable. A transitive
  dependency is not.
- A dependency's modules are named through its **namespace**, derived from its package name by
  splitting at the hyphens, uppercasing each piece and joining: `acme-widgets` is
  `AcmeWidgets`. A package never writes its own namespace — inside `acme-widgets`, the module
  is `Size`.
- A depending package may **unwrap** one dependency by writing `"wrapped": false` in that
  dependency's entry, and then that package's modules are named by their own names throughout
  the depending package. The choice belongs to the depending package and is invisible to
  anyone else. Either way a module has exactly one spelling in a file.
- `zelkova-core` is unwrapped in every package and is not written in `dependencies`, so
  `Basics` is `Basics` everywhere.
- Two modules answering to one name in one package is an error, reported **when the build is
  resolved** — before any module of that package is compiled — naming both modules and their
  packages. Two wrapped dependencies can never produce one, whatever they contain.
- At most one version of a package is in a build, and the resolved set is recorded in a
  generated `zelkova.lock`.

**Problem:** the compiler compiles exactly one package and has no representation of a second
one, so every rule above is unimplemented rather than merely unenforced. `private-modules` has
nothing to filter. A facade's package-internal rule is unenforceable. No namespace is ever
applied: `import AcmeWidgets.Size` parses as an ordinary dotted module name, is looked up in
the one flat interface map, and is reported as a module that cannot be found.

Nothing here needs the tokenizer or the grammar. A package name never appears in source text —
an `import` names a module and only ever a module — so the whole of this ticket is in the
driver and the environment.

**Approach:** after `LANG-13`, in roughly this order.

1. Resolve the dependency set — reading `zelkova.lock`, generating it when absent — and give
   `compile_package` a way to obtain each dependency's `Interface`s. Where those come from on
   disk is a toolchain question the spec leaves open; compiling each dependency from source is
   the simplest first answer.
2. For each direct dependency, load its public modules into the environment: keyed
   `<Namespace>.<module name>` by default, or by their own names when that dependency's entry
   says `"wrapped": false`. Private modules and `module javascript` facades are not loaded at
   all, so a name that reaches one of them fails as a module that does not exist rather than
   as one that is refused.
3. Build the whole name map before canonicalizing anything, and report a name claimed twice as
   a resolution error naming both sources — a local module against an unwrapped dependency's,
   or two unwrapped dependencies'. It is raised in the same place a missing manifest is, ahead
   of the per-module phases, and `zelkova-core` participates like any other unwrapped
   dependency.

**Acceptance:** `tests/pipeline.rs` tests over `tests/fixtures/` packages — a dependent
importing a public module of a dependency under its namespace and failing on a private one; the
same dependency unwrapped, imported without the prefix, with the namespaced spelling now
failing; and a collision between an unwrapped dependency and a local module reported before any
module is compiled, naming both. `cargo run` must still print `parsed 8 modules` and list all
eight.

**The two `expect=unimplemented` blocks in *The namespace* need a decision when this lands.**
They import `AcmeWidgets.Size`, and a `docs/spec/` block is compiled against the modules of its
own `package=` group and nothing else — so they will keep failing after this ticket, and their
tag will have stopped meaning what it says. Either `tests/spec.rs` gains a way for a group to
stand in for a dependency package, with a namespace and a `wrapped` flag, or those two blocks
become untagged `text` and the chapter says why. That choice belongs to whoever does this work.
