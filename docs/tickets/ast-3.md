# AST-3 · Unify the typer's tuple representation with `Tuple<T>`

**Sizing:** medium — the representation appears in `Type` and `Term`/`TypedTerm`, and is
matched in five files.

**Depends on:** [AST-2](ast-2.md) — it introduced `Tuple<T>` (`src/compiler/tuple.rs`, a
`Two`/`Three` enum) and moved the parser and canonical ASTs onto it, replacing four
disagreeing representations. Its acceptance criteria named only `parser` and `canonical`; the
typer was explicitly left out of scope.

**Location:** `src/compiler/typer/mod.rs` — `Type::Tuple` (line ~522, `(Box<Type>, Box<Type>,
Option<Box<Type>>)`), `Term::Tuple` (line ~484, same shape), the `Debug` impl for `Type`
(matches `None`/`Some(c)` separately), `canonical_type_to_typer_type` and its expression
counterpart (both re-derive the arity off `Tuple::Two`/`Tuple::Three` to build the
`Option`-shaped typer value), the substitution function (~line 687) and the free-variable/occurs
walk (~lines 868–871); `src/compiler/typer/annotate.rs` — the `Term::Tuple` match building
`TypedTerm::Tuple`; `src/compiler/typer/unifier.rs` — tuple unification (~line 72, one arm for
`None` pairs and one for `Some` pairs) and the type-variable walk (~line 128);
`src/compiler/typer/constraint.rs` — constraint generation for `TypedTerm::Tuple` (~line 112).

**Problem:** `AST-2` gave the parser and canonical ASTs one tuple representation so the 2-or-3
rule is written in the type instead of re-checked at each boundary. The typer still has its own
copy of the *old* shape, `(Box<T>, Box<T>, Option<Box<T>>)`, for both `Type::Tuple` and
`Term`/`TypedTerm::Tuple`. `canonical::Type::Tuple` and `canonical::Expression::Tuple` already
hold a `Tuple<T>`, so the two `canonical_*_to_typer_*` conversions have to re-derive the arity
on the way in — matching `Tuple::Two`/`Tuple::Three` and building the corresponding `None`/
`Some` — which is exactly the "re-derive the same fact at a boundary" shape `AST-2` removed
everywhere else. Downstream, unification, substitution and constraint generation each match
`None` and `Some(_)` as two separate arms rather than matching on a `Two`/`Three` the type
system already guarantees is one of only two shapes.

**Approach:**

1. Replace `Type::Tuple(Box<Type>, Box<Type>, Option<Box<Type>>)` with
   `Type::Tuple(Tuple<Type>)` and `Term::Tuple`/`TypedTerm::Tuple` likewise, reusing
   `crate::compiler::tuple::Tuple` the same way `canonical::Type`/`canonical::Expression`
   already do.
2. Update every site listed above. The two `canonical_*_to_typer_*` conversions should collapse
   the same way `AST-2` collapsed the parser→canonical ones — `Tuple::try_map` in place of a
   `Two`/`Three` match building an `Option`.
3. `unifier.rs`'s two-arm unification (`(None, None)` and `(Some, Some)`) becomes one arm on
   `(Tuple::Two, Tuple::Two)` and one on `(Tuple::Three, Tuple::Three)` — a `Two` unifying
   against a `Three` is already an arity mismatch and should fall through to whatever arm
   unification uses for a `Type` mismatch today; confirm that's still reachable and covered.
4. `Tuple<T>` does not derive anything needed for the `Debug` impl at line ~537 beyond what it
   already derives (`Debug, PartialEq, Clone`) — check the typer's `Display`-via-`Debug`
   formatting (`({:?}, {:?})` / `({:?}, {:?}, {:?})`) still reads the same for both arities
   once it matches on `Tuple` instead of `Option`.

**Acceptance:** `Type::Tuple` and `Term`/`TypedTerm::Tuple` hold a `Tuple<Type>` /
`Tuple<Term>` / `Tuple<TypedTerm>` respectively, with no remaining `Option<Box<Type>>`-shaped
third element in `src/compiler/typer/`. `cargo test` passes unchanged — in particular the
existing tuple-typing cases in `tests/typer.rs` still pass, pinning that `(1, 2)` and
`(1, 2, 3)` still infer and unify correctly. `cargo run` still parses 8 modules and exits 0.
