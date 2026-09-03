# LANG-27 · An operator may carry more than one `infix` declaration, and the last silently wins

**Sizing:** small.

**Location:** `src/compiler/canonical/mod.rs`, `do_infixes` — the iterator's
`(op_name, infix)` pairs are handed to `collect_accumulate`, which builds a
`HashMap<Name, Infix>`; `env.insert_local_infix` writes into the environment the same way.

**Decided by:** [`docs/spec/declarations.md`](../spec/declarations.md)'s *An operator has one
`infix` declaration*.

**Problem:** a module declares each operator once. Today two declarations for one operator are
both accepted, and the second replaces the first in the map:

```
infix left 6 (+) = add

infix right 7 (+) = mul
```

`+` ends up meaning `mul`, right-associative at 7, with nothing said about the declaration
that was discarded. Neither declaration is wrong on its own and no rule chooses between them,
so the pair has to be reported rather than resolved.

**Approach:** `do_infixes` already walks `infixes` in source order; collect the operator names
seen and raise a new `canonical::Error` variant on the second declaration of one, with the
first as a secondary label. It goes in the error vector rather than short-circuiting, so a
module with several duplicated operators reports all of them — `collect_accumulate` is
already the accumulating shape this needs.

Worth checking at the same time whether `Interface::infixes` can carry a duplicate across a
module boundary; it is a `HashMap` too, but it is built from a map that this fix makes
duplicate-free, so it should need nothing.

**Acceptance:** the `expect=ok` block under *An operator has one `infix` declaration* in
[`docs/spec/declarations.md`](../spec/declarations.md) goes **red** — retag it
`expect=canonical-error:` with the new variant and delete the **Known gap:** paragraph
beneath it. A `tests/compiler/canonical.rs` case asserting the variant, seen to fail before
the fix.
