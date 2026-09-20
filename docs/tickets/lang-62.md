# LANG-62 · The compiler carries no copy of `zelkova-core`, so a package has to write it in `dependencies`

**Sizing:** small-to-medium. The resolution half is small — one package added to the build
before the root's own entries are walked. What could make it bigger is deciding *where* the
compiler's copy of core sits, which is a toolchain question this ticket has to answer before it
can do anything.

**Location:** `src/compiler/resolve.rs` — `resolve`, which seeds the build from the root
manifest's `dependencies` and nothing else; `CORE_PACKAGE`; and
`ResolvedPackage::seen_unwrapped`, which is the half of the rule that already holds.
`src/main.rs`, which points the compiler at `std/core` — the copy in question, in this repo.

**Decided ([*`zelkova-core` is a dependency of every
package*](../spec/packages.md#zelkova-core-is-a-dependency-of-every-package)):** `zelkova-core`
is a dependency of every package and is **not written in `dependencies`**. It is seen
unwrapped, in every package, so `Basics` is `Basics` and `List` is `List` — and every one of
core's public module names is therefore taken everywhere, so a module of your own called `List`
is [a second module answering to one
name](../spec/packages.md#two-modules-under-one-name-is-an-error).

**Problem:** half of that is implemented. `ResolvedPackage::seen_unwrapped` returns `true` for
a package named `zelkova-core` whatever the entry naming it says, so once core *is* in a build
its modules are named by their own names — which is what keeps `Basics.Int`, the name
`scalars.rs` recognises a scalar by (`DEC-15` decision 1), pointing at one declaration. The
other half is missing entirely: nothing supplies core. A package that needs it writes it in
`dependencies` like any other package, with a `path` entry at that, so core's names are taken
only in a package that asked for them, and a package that forgot cannot name `Int`.

The reason it is missing is that a compiler shipping its own core has to know where that copy
sits, and nothing in the tree says where. `std/core` is found today only because `src/main.rs`
passes it as the package to compile.

**Approach:**

1. Decide where the compiler's copy of core lives and how it is found — beside the binary, in
   the cache `LANG-61` introduces, or named by an environment variable for a development tree.
   **This ticket does not pick**; it is the first thing whoever takes it has to settle, and the
   answer belongs in [the toolchain appendix](../spec/toolchain.md) once it is made.
2. Seed the build with that package before walking the root manifest's `dependencies`, so it is
   in `published` before any other package is compiled. `resolve` already orders a package
   after everything it depends on, and core depends on nothing.
3. Keep the escape hatch that makes `std/core` compilable: core is compiled *as* the root
   package there, and a package that is itself `zelkova-core` must not be given a second copy
   of itself.
4. An explicit `zelkova-core` entry in a manifest then either overrides the supplied copy or is
   refused — decide which, and say so in the chapter.

**Where this came from:** filed in review of #226, which closed `LANG-14`. `LANG-14` listed
this under **Decided**, implemented the unwrapping half and deliberately left the supply half;
closing it without a successor would have left the rule as spec text with nothing tracking it.
The **Not implemented:** paragraph in that chapter section names this ticket and is deleted
when it lands.

**Acceptance:** a `tests/pipeline.rs` test over a fixture package whose manifest has an **empty**
`dependencies` and whose module names `Int` — which fails today as a type that resolves to
nothing — compiling green. `tests/fixtures/package_core_basics_collision` and
`a_dependencys_basics_collides_with_cores` must stay green: a package of its own declaring
`Basics` is still a collision, and now without anyone having had to write core down.
`tests/fixtures/dep_core_fork` and `core_is_unwrapped_whatever_its_entry_says` pin the
unwrapping half and must stay green too. `cargo run` must still print `parsed 8 modules`, list
all eight as checked, and exit 0 — `std/core` is compiled as the root package there, and step 3
is what keeps that working.
