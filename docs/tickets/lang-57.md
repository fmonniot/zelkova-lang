# LANG-57 · The default imports are dropped entry by entry, not by package

**Sizing:** small-to-medium. The decision itself is a few lines in two phases; most of the
work is deleting the apparatus the old rule needed — a fixture, a property test, and two doc
comments that argue for an ordering that stops mattering.

**Location:** `src/compiler/dependencies.rs` — `add_default_import_edges` and its
*Why the loops are nested target-first* section, `ModuleWalker::new`;
`src/compiler/default_imports.rs` — `implicit_imports`, `is_default`, and the module doc's
*Where the cycle does not happen*; `src/compiler/canonical/environment.rs` —
`new_environment`; `src/compiler/canonical/mod.rs` — `canonicalize`; `src/compiler/mod.rs` —
`compile_package`, `check_module`. Tests: `tests/fixtures/package_default_import_priority/`,
`tests/pipeline.rs`'s two uses of it, and
`dependencies`' `renaming_a_module_does_not_change_which_default_imports_it_gets`.

**Chapter:** [*The default imports*](../spec/modules.md#the-default-imports), whose
**Known gap:** paragraph this closes. [`SPEC-33`](spec-33.md) is the review that settled the
rule, and `docs/decisions/` carries what it was weighed against.

**Problem:** the chapter now says a module receives all eight default imports unless it belongs
to `zelkova-core`, which receives none of them. The compiler decides one entry at a time
instead: `add_default_import_edges` refuses an implicit edge whose target already depends on
the importer, tested against the import graph *as built so far*, and `implicit_imports` drops
an entry whose module has not been checked yet. The two agree with each other, and neither
matches the rule.

Inside `std/core` the difference is visible today:

```
Tuple      -> []                                    Basics -> []
Js.Utils   -> ["Tuple"]                             Maybe  -> []
Js.Basics  -> ["Tuple"]                             Result -> []
Js.Bitwise -> ["Basics", "Maybe", "Result", "Tuple"]
Bitwise    -> ["Maybe", "Result", "Tuple"]
```

Every one of those sets should be empty. Nothing names an entry it receives that way —
`Bitwise` writes `import Basics exposing (Int)` itself and names no `Maybe`, `Result` or
`Tuple`; the three facades name only scalar types — so this is not a module that compiles and
should not. It is the compiler implementing a rule the language no longer has, and carrying
the machinery that rule needs.

Outside core the pass is already a no-op: `ModuleWalker::new` builds one graph per package and
drops imports of modules outside it, so `add_default_import_edges` finds no node for `Basics`
and adds nothing. No ordinary package changes behaviour here.

**Approach:**

1. Add one question to `default_imports` beside `is_default` — whether a package declares any
   of the eight, given its module names. Both phases ask it rather than deriving it twice.
2. `add_default_import_edges` adds no edge at all when the answer is yes. `names` already
   holds every module of the package, so it can ask without a new parameter.
3. `implicit_imports` returns empty for every module of such a package, which needs the answer
   threaded down: `compile_package` knows the full module list, `check_module` and
   `canonicalize` already carry a `package`, and `new_environment` calls `implicit_imports`.
   Note that `compile_package` hardcodes `PackageName::new("zelkova", "core")` for every
   package it compiles (the `TODO` above it), so the test cannot be *"is the package named
   `zelkova-core`"* — it has to be the module-set question of step 1 until that name is real.
4. Delete `tests/fixtures/package_default_import_priority/`, its two uses in `tests/pipeline.rs`
   and `renaming_a_module_does_not_change_which_default_imports_it_gets`. All three exist to
   pin which entry survives a collision, and under the new rule no entry is ever in a position
   to collide. The *Why the loops are nested target-first* argument goes with them; the loops
   themselves can stay as they are.
5. Delete the **Known gap:** paragraph in *The default imports*, and rewrite
   `default_imports`' *Where the cycle does not happen* to state the package rule — its rule 2
   and the propagation paragraph under it are the old rule written out.

**Interaction with [`LANG-53`](lang-53.md):** that ticket seeds the five scalar names for a
module that *dropped the `Basics` entry*; the chapter now gives them to every module of
`zelkova-core`, which is the same set of modules under this ticket's rule and a larger one
before it. Either ticket may land first. If `LANG-53` lands first, its trigger is rewritten
here; if this lands first, `LANG-53` is written against the package answer from step 1 and its
closing note about a coarser rule no longer applies.

**Acceptance:** the probe above prints an empty set for all seven `std/core` modules, and
`cargo run` still prints `parsed 8 modules`, lists all eight as checked, and exits 0. A test in
`src/compiler/dependencies.rs` pins that a package containing a default module gets no implicit
edges while one without is unchanged, and a test in `src/compiler/default_imports.rs` pins that
`implicit_imports` returns empty for a module of such a package and all eight for a module that
is not — each seen to fail with the change reverted. The **Known gap:** paragraph in
[*The default imports*](../spec/modules.md#the-default-imports) is gone, and no doc comment
still describes the per-entry rule. `cargo test --workspace` and `cargo test --test spec` are
green.
