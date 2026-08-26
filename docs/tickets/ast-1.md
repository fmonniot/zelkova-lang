# AST-1 · Remove `Box<Vec<_>>` from the parser AST

**Sizing:** small — mechanical, but it touches the grammar, so it is wider than it is deep.

**Location:** `src/compiler/parser/mod.rs` — `Type::Unqualified`, `Type::Tuple`, and the two
standing TODOs that already ask for this ("Simplify all `Box<Vec<_>>` into `Vec<_>`" above the
`Type` enum, "Inline all `Box<Vec` into `Vec`" above `Expression`);
`src/compiler/parser/grammar.lalrpop` — every production building those variants;
`src/compiler/canonical/mod.rs` — `Type::from_parser_type`, which destructures them.

**Problem:** `Vec` is already a heap-allocated, pointer-sized handle. `Box<Vec<T>>` adds a
second allocation and a second indirection to reach the elements, and buys nothing — there is
no recursive-size problem to solve, because `Vec<Type>` is a fixed three words regardless of
what `Type` is. Every construction site pays an extra `Box::new`, every read site pays an extra
deref, and every pattern match is noisier than it needs to be.

The author already flagged it twice in comments. It survives because it is spread across the
grammar rather than because it is hard.

**Approach:**

1. `Type::Unqualified(Name, Box<Vec<Type>>)` → `Type::Unqualified(Name, Vec<Type>)`.
2. `Type::Tuple(Box<Type>, Box<Vec<Type>>)` → `Type::Tuple(Box<Type>, Vec<Type>)`. The `Box`
   on the head is load-bearing (recursion) and stays. Note that this variant is being
   reshaped entirely by `AST-2` — if both tickets are live, do `AST-1` first and let `AST-2`
   land on the simpler form, or fold this variant into `AST-2` and leave it alone here.
3. Check `Expression` for the same pattern before assuming the TODO above it is stale — the
   comment says `Box<Vec` but the variants below it may already have been cleaned up.
4. Update `grammar.lalrpop` and `canonical::Type::from_parser_type` in the same commit; a
   grammar change that lands without its AST and canonicalization counterparts is the failure
   mode named in `CLAUDE.md`'s standing invariants.
5. `Type::unqualified` (the constructor helper used heavily by `tests/compiler/parser/types.rs`)
   should absorb the change so the test files need no edits beyond what the type system forces.

While in `parser/mod.rs`: `UnionType.variants` is typed `Vec<Type>` but only `Type::Unqualified`
is ever valid there, and there is a TODO saying so. That is a different and more interesting
change (it wants a dedicated variant type) — **do not** do it here. Note it and move on.

**Acceptance:** `grep -n 'Box<Vec' src/compiler/parser/mod.rs` returns nothing, `cargo test`
passes, and `cargo run` still reports seven parsed modules with no new errors. No behaviour
changes, so no new tests are expected — the existing parser tests in `tests/compiler/parser/`
are the regression net, and they should have needed only mechanical edits.
