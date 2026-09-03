# LANG-32 · A module may declare one type twice, and the second silently replaces the first

**Sizing:** small.

**Location:** `src/compiler/canonical/mod.rs`, `do_types` — the `(tpe_name, union)` pairs go to
`collect_accumulate`, which builds a `HashMap<Name, UnionType>` keyed by the type's name.

**Decided by:** [`docs/spec/name-resolution.md`](../spec/name-resolution.md)'s *A name is
declared at most once*.

**Problem:** two `type` declarations of one name introduce two types with one name, and no rule
chooses between them. Today the second wins outright:

```
type Size
  = Small

type Size
  = Big
```

The map keeps the last entry, so the module ends up with one type `Size` built by `Big`, and the
first declaration is gone entirely — `Small` does not resolve anywhere afterwards, because
`insert_union_type` is driven from that same map and never sees it. So the failure surfaces, if
it surfaces at all, as an unresolved constructor pointing at code that is not the mistake.

This is the `type` half of a rule the other two top-level declaration forms already have written
down: an operator's duplicate `infix` is [`LANG-27`](lang-27.md), and repeated value bindings are
not duplicates at all but the clauses of one declaration ([`LANG-20`](lang-20.md)).

**Fix:** `do_types` walks `types` in source order; collect the names seen and raise a new
`canonical::Error` variant on the second declaration of one, carrying that declaration's span as
the primary label and the first declaration's span as a secondary one — `parser::UnionType` has
a span, so both are available. Push it onto the error vector rather than short-circuiting, so a
module duplicating several types reports all of them.

**Acceptance:** the `expect=ok` block under *A name is declared at most once* in
[`docs/spec/name-resolution.md`](../spec/name-resolution.md) goes **red** — retag it
`expect=canonical-error:` with the new variant and delete the **Known gap:** paragraph above it.
A `tests/compiler/canonical.rs` case asserting the variant and both spans, seen to fail before
the fix.
