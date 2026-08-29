# SPEC-7 · Write the Patterns chapter

**Sizing:** medium.

**Location:** `src/compiler/parser/grammar.lalrpop` — `Pattern`, `CasePattern`, `DeclPattern`;
`src/compiler/parser/mod.rs` — `PatternKind` (`Variable`, `Literal`, `Tuple`, `Constructor`,
`Anything`) and its doc comment's "Missing Patterns" list; `src/compiler/canonical/mod.rs` —
`PatternKind`, `Pattern::from_parser`.

**Grounding note:** the above and the specifics below came from one quick pass done only to
scope this ticket, not from `write-spec-chapter`'s Step 2 probing. Treat every specific claim
as a lead to re-verify, not as settled — and don't let this ticket's Approach cap what the
chapter ends up covering. Steps 1–2 (read the grammar/AST, then probe the compiler) and Step 4
(design questions) are what actually decide that.

**Problem:** every pattern form, and where each may appear, has never been written down. One
asymmetry worth confirming by probing rather than assuming from this ticket: the grammar
appears to allow a bare constructor pattern in a `case` branch (`CasePattern`) while requiring
one to be parenthesized in a function head (`DeclPattern`). As-patterns, list/cons patterns,
record patterns and the unit pattern look unimplemented — `PatternKind`'s own doc comment lists
them as missing — but confirm that in Step 2 too.

**Approach:** follow `write-spec-chapter` in full. Likely territory, to confirm by probing
rather than assume from this ticket: wildcard, variable, literal, tuple and constructor
patterns, the possible case-vs-function-head asymmetry above, and the missing forms as
`**Not implemented:**` once Step 2 confirms them.

**Acceptance:** `cargo test --test spec` green, `docs/spec/patterns.md` contributing its blocks
with every block tagged and each tag proven to fail, `docs/spec/README.md`'s row for this
chapter moved to `written`.
