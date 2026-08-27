# TIDY-6 · Stale doc comment on `canonical_type_to_typer_type`

**Sizing:** trivial — one doc comment.

**Location:** `src/compiler/typer/mod.rs` — `canonical_type_to_typer_type`'s doc comment
(immediately above its `fn` at line ~175).

**Problem:** the comment reads:

> Convert a canonical type to the typer's simplified Type representation.
> Returns None for types that cannot yet be represented (tuples, named types
> with parameters, etc.).

Both examples it names are handled today. `canonical::Type::Tuple(Tuple::Two(..))` and
`Tuple::Three(..)` each have a match arm producing `Some(Type::Tuple(..))` (added by `AST-2`,
which is also why the arms now match on `Tuple` rather than an older shape — see
[AST-2](ast-2.md)). Named types with parameters aren't obviously unhandled either: nothing in
the function's match arms special-cases arity-0 named types only, and the fallthrough behaviour
for a parameterized named type should be confirmed against the current match before rewriting
the sentence, rather than assumed from the old comment.

This is the exact class of defect `CLAUDE.md`'s standing invariant on doc comments exists for,
and the same failure `TIDY-4` fixed in the test headers: a comment describing an earlier state
of the code is what the next reader trusts, and here it would send them looking for a `None`
case that a tuple can no longer produce.

**Fix:** read the function's current match arms and rewrite the second sentence to state
accurately which `canonical::Type` shapes still produce `None` today (if any do — it may be
that only truly unrepresentable constructs, if any remain, need naming). Don't just delete the
"tuples" example and leave the rest; re-verify the whole claim against the current match.

**Acceptance:** the doc comment names only `canonical::Type` variants that the function's match
arms actually return `None` for as of the fix, verified by reading the match, not by guessing.
No code changes — this is comment-only, and `cargo test` / `cargo run` are unaffected.
