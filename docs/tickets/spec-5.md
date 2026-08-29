# SPEC-5 · Write the Types and type annotations chapter

**Sizing:** medium.

**Location:** `src/compiler/parser/grammar.lalrpop` — `AtomicType`, `ArgType`, `Type`
productions; `src/compiler/parser/mod.rs` — `Type`/`TypeKind` (`Unqualified`, `Arrow`,
`Variable`, `Tuple`); `src/compiler/canonical/mod.rs` — `Type`/`Type::from_parser_type`;
`src/compiler/tuple.rs` — `Tuple<T>` (`Two`/`Three`).

**Problem:** type expressions, the function arrow, type variables, and tuple types have never
been written down, including the fixed 2-or-3 tuple arity (`AST-2`/`AST-3` unified this
representation across the parser and canonical ASTs) that makes a four-element tuple type a
syntax error rather than a type error. Type aliases are unimplemented — there is no
`TypeKind::Alias` anywhere in the tree.

**Approach:** follow `write-spec-chapter`. Cover type expressions and how they nest, the
function arrow's associativity, type variables, tuple types and the fixed arity, and mark type
aliases `**Not implemented:**` per `docs/spec/README.md`'s vocabulary rather than silently
omitting them.

**Acceptance:** `cargo test --test spec` green, `docs/spec/types.md` contributing its blocks
with every block tagged and each tag proven to fail, `docs/spec/README.md`'s row for this
chapter moved to `written`.
