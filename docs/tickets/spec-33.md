# SPEC-33 · Which default imports a module gets is a fixed point over the whole package

**Sizing:** small as a [`docs/decisions/`](../decisions/README.md) write-up that ratifies the
current rule. Medium if the conclusion is to replace it, which would change
`add_default_import_edges`, `implicit_imports` and three paragraphs of a chapter together.

**Location:** [`docs/spec/modules.md`](../spec/modules.md)'s *The default imports* — the three
paragraphs from "The eight modules receive none of the list themselves" through "`Basics` is
first, so it is the entry a module keeps when it can keep only one".
`src/compiler/dependencies.rs` — `add_default_import_edges`;
`src/compiler/default_imports.rs` — `implicit_imports`.

**Depends on:** the default-import mechanism, which arrives with `LANG-8` (PR #206). The rule
below does not exist on `main` until that merges.

**Problem:** this is a design review, not a defect. The rule is implemented correctly and
documented accurately; the question is whether it is a rule a programmer can hold in their head.

A module does not receive a fixed set of default imports. It is judged one entry at a time,
dropping the entry for any module that already depends on it, where "already depends on" is
evaluated against the import graph *as built so far* — which includes implicit edges allocated
for earlier entries of the same list. So the answer depends on the whole package's dependency
graph and on `DEFAULT_IMPORTS`' own ordering, and a drop propagates: the chapter needs a worked
example with two hops to state it precisely.

The observable consequence is that two modules in one package get different sets for reasons
neither module's text shows. Probing `std/core`:

```
Tuple      -> []                                    Basics -> []
Js.Utils   -> ["Tuple"]                             Maybe  -> []
Js.Basics  -> ["Tuple"]                             Result -> []
Js.Bitwise -> ["Basics", "Maybe", "Result", "Tuple"]
Bitwise    -> ["Maybe", "Result", "Tuple"]
```

Three facades, three different sets — `Js.Bitwise` gets `Basics` and the other two do not,
purely because `Basics` imports those two and not it.

Two things argue for leaving it alone. The rule is *necessary* at the boundary — something has
to give when a default import would close a cycle, and dropping only the offending entry is
strictly more useful than dropping all eight. And it is nearly unobservable in practice: only a
module sitting within one hop of one of the eight can feel it, which in `std/core` is four
modules and in an ordinary package is none.

The argument against is that "which defaults you get" becomes a question with no local answer.
A reader of `Js/Utils.zel` cannot work out what is in scope without reading `Basics.zel`, and
the chapter's precise statement of the rule runs to three paragraphs including a propagation
example and a tie-break rule — length that is itself evidence the rule is shaped by the
implementation rather than by what a user needs to know.

Found while reviewing `LANG-8` (PR #206), after re-deriving the rule in full to verify an
earlier finding. Not raised against that PR: the mechanism does what the ticket asked and the
prose describes it correctly, so this is a question about the decision rather than the diff.

**Approach:** weigh the current per-entry rule against the coarser alternative — **a module that
any default import depends on receives none of the list**, the same all-or-nothing treatment the
eight listed modules already get. That rule is one sentence, has a local answer, and costs
`Js.Basics` and `Js.Utils` the `Tuple` entry they currently receive and do not use. Establish
whether anything in `std/core` or in the fixtures actually relies on a facade keeping a
surviving entry; if nothing does, the expressiveness the per-entry rule buys is expressiveness
nobody needs.

A third possibility worth stating and probably rejecting: drop the graph-sensitivity entirely by
having the eight modules and their dependencies declared, rather than computed — a list the
compiler carries of what is exempt. Cheap to explain, and it goes stale the first time
`Basics`' imports change.

This ticket may correctly end in "no change, and here is why", in which case the
[`docs/decisions/`](../decisions/README.md) entry *is* the deliverable — the reasoning above is
currently nowhere, and the next person to find the `Js.Bitwise` asymmetry surprising will
re-derive it from scratch. If the rule is kept, it is still worth asking whether *The default
imports* can state it in fewer words by leading with the boundary case it exists for instead of
with the general algorithm.

**Acceptance:** a [`docs/decisions/`](../decisions/README.md) entry records the choice and what
it was weighed against, and *The default imports* links it. If the rule changes, the probe above
produces the new sets, `add_default_import_edges`' doc comment and its tests move with it, and
`cargo run` still prints `parsed 8 modules` with all eight checked. `cargo test --workspace` and
`cargo test --test spec` are green either way.
