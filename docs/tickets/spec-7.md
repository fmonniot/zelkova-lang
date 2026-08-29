# SPEC-7 · Write the Patterns chapter

**Sizing:** medium.

**Location:** `src/compiler/parser/grammar.lalrpop` — `Pattern`, `CasePattern`, `DeclPattern`;
`src/compiler/parser/mod.rs` — `PatternKind` (`Variable`, `Literal`, `Tuple`, `Constructor`,
`Anything`) and its doc comment's "Missing Patterns" list; `src/compiler/canonical/mod.rs` —
`PatternKind`, `Pattern::from_parser`.

**Problem:** every pattern form, and where each may appear, has never been written down —
including a real syntactic asymmetry grounding turned up: a constructor pattern is bare in a
`case` branch (`CasePattern`) but must be parenthesized in a function head (`DeclPattern`).
As-patterns, list/cons patterns, record patterns and the unit pattern are all unimplemented,
called out directly in `PatternKind`'s own doc comment.

**Approach:** follow `write-spec-chapter`. Cover wildcard, variable, literal, tuple and
constructor patterns, document the case-vs-function-head parenthesization asymmetry, and mark
the missing forms `**Not implemented:**`.

**Acceptance:** `cargo test --test spec` green, `docs/spec/patterns.md` contributing its blocks
with every block tagged and each tag proven to fail, `docs/spec/README.md`'s row for this
chapter moved to `written`.
