# LANG-25 · A declaration may not name fewer parameters than its annotation has arrows

**Sizing:** small.

**Location:** `src/compiler/canonical/mod.rs`, `do_values` — the
`if !patterns.is_empty() && (linear.len() - 1 != patterns.len())` check that raises
`Error::BindingPatternsInvalidLen`.

**Decided by:** [`docs/spec/declarations.md`](../spec/declarations.md)'s *Parameters*.

**Problem:** a binding may name **at most** as many parameters as its annotation has arrows;
the arrows left over are the type of what its body produces. Today the count must be either
zero or exactly the arrow count, and anything between is rejected:

```
apply : Flag -> Flag -> Flag
apply a =            -- rejected; should be accepted
  swap a
```

The `!patterns.is_empty() &&` guard is what lets the point-free spelling through, so the two
legal counts are the two ends of the range and nothing in the middle.

An arrow is part of a type rather than a promise about how a declaration is written, and
`Flag -> Flag -> Flag` and `Flag -> (Flag -> Flag)` are the same type — so a rule counting
arrows exactly gives two spellings of one type two different sets of legal declarations.

**Approach:** replace the equality with `patterns.len() <= linear.len() - 1`, and zip the
patterns against the leading `linear` entries rather than against all of them. Note that
`linear` is built by `Type::to_linear_types`, which flattens the arrow chain, so the
right-nested spelling above already arrives here identical to the flat one; nothing about
that needs changing.

More parameters than arrows stays an error, and keeps `BindingPatternsInvalidLen`.

**Acceptance:** the `expect=canonical-error:BindingPatternsInvalidLen` block under
*Parameters* in [`docs/spec/declarations.md`](../spec/declarations.md) goes **red** — retag it
`expect=ok` and delete the **Known gap:** paragraph beneath it. The
`expect=canonical-error:BindingPatternsInvalidLen` block in
[`docs/spec/types.md`](../spec/types.md)'s *The annotation and the declaration's parameters*,
which supplies two parameters against one arrow, must stay green. A `tests/compiler/canonical.rs`
case for each of the three counts (all, some, none), seen to fail before the fix.
