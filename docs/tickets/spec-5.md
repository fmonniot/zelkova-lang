# SPEC-5 · Write the Types and type annotations chapter

**Sizing:** medium.

**Location:** `src/compiler/parser/grammar.lalrpop` — `AtomicType`, `ArgType`, `Type`
productions; `src/compiler/parser/mod.rs` — `Type`/`TypeKind` (`Unqualified`, `Arrow`,
`Variable`, `Tuple`); `src/compiler/canonical/mod.rs` — `Type`/`Type::from_parser_type`;
`src/compiler/tuple.rs` — `Tuple<T>` (`Two`/`Three`).

**Grounding note:** the above and the specifics below came from one quick pass done only to
scope this ticket, not from `write-spec-chapter`'s Step 2 probing. Treat every specific claim
as a lead to re-verify, not as settled — and don't let this ticket's Approach cap what the
chapter ends up covering. Steps 1–2 (read the grammar/AST, then probe the compiler) and Step 4
(design questions) are what actually decide that.

**Problem:** type expressions, the function arrow, type variables, and tuple types have never
been written down, including the fixed 2-or-3 tuple arity (`AST-2`/`AST-3`, both closed,
unified this representation across the parser and canonical ASTs) that makes a four-element
tuple type a syntax error rather than a type error. Type aliases look unimplemented from a
quick pass — no `TypeKind::Alias` turned up anywhere in the tree — but confirm that in Step 2
rather than treating it as settled.

**Approach:** follow `write-spec-chapter` in full. Likely territory, to confirm by probing
rather than assume from this ticket: type expressions and how they nest, the function arrow's
associativity, type variables, tuple types and the fixed arity, and whatever Step 2 finds about
type aliases — mark them `**Not implemented:**` if it confirms they're absent.

**Acceptance:** `cargo test --test spec` green, `docs/spec/types.md` contributing its blocks
with every block tagged and each tag proven to fail, `docs/spec/README.md`'s row for this
chapter moved to `written`.
