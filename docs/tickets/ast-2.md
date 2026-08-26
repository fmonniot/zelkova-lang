# AST-2 · Unify the tuple representation across the parser and canonical ASTs

**Sizing:** medium — the representations disagree in four places and the fix is a design
choice, not a rename.

**Depends on:** `AST-1` (see [INDEX.md](INDEX.md), closed) — it simplified `parser::Type::Tuple`'s
payload from `(Box<Type>, Box<Vec<Type>>)` to `(Box<Type>, Vec<Type>)`; the table below reflects
that already-landed shape.

**Location:** `src/compiler/parser/mod.rs` — `Type::Tuple` and `Pattern::Tuple`;
`src/compiler/canonical/mod.rs` — `Type::Tuple`, `Pattern::Tuple`, `Expression::Tuple`, and the
`from_parser*` conversions around them; `src/compiler/parser/grammar.lalrpop` — the tuple
productions.

**Problem:** four types model the same language construct four different ways.

| | shape | what it can express |
|---|---|---|
| `parser::Type::Tuple` | `(Box<Type>, Vec<Type>)` | head + rest — any size ≥ 1 |
| `parser::Pattern::Tuple` | `(Box<Pattern>, Box<Pattern>, Vec<Pattern>)` | two mandatory + rest — any size ≥ 2 |
| `canonical::Type::Tuple` | `(Box<Type>, Box<Type>, Option<Box<Type>>)` | exactly 2 or 3 |
| `canonical::Pattern::Tuple` | `(Box<Pattern>, Box<Pattern>, Option<Box<Pattern>>)` | exactly 2 or 3 |
| `parser::Expression::Tuple` | `Vec<Expression>` | any size |
| `canonical::Expression::Tuple` | `(Box, Box, Option<Box>)` | exactly 2 or 3 |

The language rule is Elm's: a tuple is size 2 or 3. Exactly one of these six encodes that, and
it is enforced three separate times at the canonicalization boundary — `from_parser_type`
matches on the rest-vector's length and returns `Error::InvalidTupleSize(1)` or
`InvalidTupleSize(n + 1)`, `from_parser` for expressions matches on a slice and returns
`InvalidTupleSize(vec.len())`, and the pattern conversion does its own thing again. Each of
those is an independent chance to get the arithmetic wrong, and `n + 1` versus `vec.len()` in
two neighbouring functions is exactly the shape of bug that gets found in a type checker three
months later.

There is already a TODO on `parser::Pattern::Tuple` saying "Use (p, p, maybe) or (vec) but not
(p, p, vec)".

**Approach:**

1. Pick one representation and use it in both ASTs. Two candidates, and this ticket does not
   decide:
   - `Option<Box<T>>` for the third element, matching what canonical already does. Smallest
     diff; keeps the 2-or-3 rule in the type, at the cost of `Option` noise at every use.
   - A dedicated `Tuple2(T, T)` / `Tuple3(T, T, T)` enum, generic over the element type and
     shared by types, patterns and expressions. More code up front, but every consumer then
     matches on the arity it is actually handling instead of unwrapping an `Option` and hoping.
     With three element types to serve, the shared generic is likely to pay for itself.
2. **Enforce the constraint at parse time**, so `InvalidTupleSize` becomes unreachable from the
   grammar and survives only as the error for constructs the grammar cannot reject. If the
   LALRPOP productions can be written to accept only 2 and 3 elements, this is where the real
   win is: one enforcement point instead of three.
3. Update all six sites plus the grammar in one commit — see `CLAUDE.md`'s standing invariant
   about grammar changes never being one-file changes.
4. Keep `Error::InvalidTupleSize` even if the grammar makes it unreachable from `.zel` source;
   `canonicalize` is a public API and can be handed a hand-built AST. Say so in its doc comment
   rather than deleting it.

**Acceptance:** one tuple representation appears in both `parser` and `canonical`, and the
arity check exists in exactly one place. `(1, 2)` and `(1, 2, 3)` still canonicalize; a
four-element tuple is rejected — with a parse error if step 2 succeeded, otherwise still with
`InvalidTupleSize(4)` — and there is a test pinning each of those three, in
`tests/compiler/canonical.rs` alongside the existing structural assertions. `cargo test` and
`cargo run` are unchanged otherwise.
