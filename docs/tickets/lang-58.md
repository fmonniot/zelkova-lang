# LANG-58 · A module underneath `Basics` cannot name a scalar type

**Sizing:** medium. The rule is four lines in one function; establishing that the five names
resolve without pulling `Basics`' interface into the graph is the work, and the acceptance
check runs through another ticket's fix.

**Location:** `src/compiler/default_imports.rs` — `implicit_imports`, which computes what a
module receives; `src/compiler/dependencies.rs` — `add_default_import_edges`, which is where a
dropped entry is decided; `src/compiler/canonical/environment.rs` — `new_environment`, where
what a module receives becomes a scope. In the tree: `std/core/src/Js/Basics.zel` and
`std/core/src/Js/Utils.zel`, the two modules the rule exists for.

**Decided ([`DEC-15` decision
3](../decisions/dec-15.md#3--a-module-that-loses-the-basics-default-import-receives-the-scalar-type-names-in-its-place),
by the language owner):** a module that drops the `Basics` entry receives `Int`, `Float`,
`Char`, `String` and `Bool` in its place, bound to the same declarations `Basics` exposes.
[*The default imports*](../spec/modules.md#the-default-imports) states the rule and [*Scalar
types*](../spec/types.md#scalar-types) the concept behind it.

**Depends on:** [BUG-26](bug-26.md), which is where the compiler comes to hold the five
qualified names at all. Landing this first would bind the five names to nothing the rest of the
compiler could tell apart from a fabrication.

**Blocks:** [BUG-16](bug-16.md). That ticket's fix turns `Js/Basics.zel` and `Js/Utils.zel` red
with no available spelling until this lands.

**Renumbered from LANG-53** on 2026-09-15: that ID had already been used and closed
(`unsafe` marks a facade signature). See `docs/tickets/README.md`'s tombstone row.

**Problem:** `unsafe idiv : Int -> Int -> Int` at `std/core/src/Js/Basics.zel:22` names a type
the module has no way to reach. Writing `import Basics exposing (Int)` is [a
cycle](../spec/modules.md#imports-may-not-form-a-cycle) — `Basics` imports both facades — and
the implicit import is withheld for that same cycle, which is the drop *The default imports*
describes. Both modules compile because the unresolved name is fabricated rather than reported
([`BUG-16`](bug-16.md)), and the fabrication passes for `Basics`' `Int` only because a type is
identified today by its unqualified name.

`std/core/src/Js/Bitwise.zel` is the control: also a facade, also naming `Int` in every
signature, also carrying no `import` line, but `Basics` does not import it, so it keeps its
`Basics` entry and resolves `Int` honestly.

**Fix:** where `implicit_imports` drops the `Basics` entry, record that the module receives the
five scalar names instead, and have `new_environment` seed them as bindings to the qualified
names the compiler holds.

The point to get right is that no dependency is created. The compiler knows each scalar's
qualified name and arity without reading the module that declares it, so seeding must not
consult `Basics`' `Interface` and must not add an edge in `dependencies`. If the check order
changes at all, the fix is wrong.

Type names only: no constructors and no values. A module receiving `Bool` this way can annotate
one and cannot write a `True`, which is [`DEC-15` decision
4](../decisions/dec-15.md#4--the-scalar-names-arrive-without-their-values) and is deliberate.

[`SPEC-33`](spec-33.md) has since replaced the drop rule this hangs off: the chapter now gives
the scalar names to every module of `zelkova-core` rather than to one that dropped the `Basics`
entry. [`LANG-57`](lang-57.md) is the ticket for that change, and the two may land in either
order — see its *Interaction* note.

**Acceptance:** `Js/Basics.zel` and `Js/Utils.zel` resolve `Int`, `Float` and `Bool` through
the seeded names, with no fabricated type standing in — verifiable by applying
[`BUG-16`](bug-16.md)'s fix on top and finding both modules still check. The module order
`dependencies` produces is unchanged, pinned by a test. A module that keeps its `Basics` entry
is unaffected, and one that receives the seeded names still cannot write `True` — tests in
`tests/compiler/canonical.rs`, each checked by reverting the seeding and watching it go red.
The **Known gap:** under the `package=below` blocks in [*The default
imports*](../spec/modules.md#the-default-imports) is deleted with its paragraph. `cargo run`
still prints `parsed 8 modules`, lists all eight as checked, and exits 0.
