# SPEC-8 · Write the Name resolution and scoping chapter

**Sizing:** medium.

**Location:** `src/compiler/canonical/environment.rs` — `Environment` trait,
`RootEnvironment`, `ScopedEnvironment`, `ValueType` (`Local`/`TopLevel`/`Foreign`/`Foreigns`),
`process_import`; `src/compiler/canonical/mod.rs` — `Error::VariableNotFound`,
`Error::AmbiguousVariables`, `Error::VariantNotFound`.

**Grounding note:** the above came from one quick pass done only to scope this ticket, not from
`write-spec-chapter`'s Step 2 probing. Treat it as a lead to re-verify, not as settled — and
don't let this ticket's Approach cap what the chapter ends up covering. Steps 1–2 (read the
grammar/AST, then probe the compiler) and Step 4 (design questions) are what actually decide
that.

**Problem:** qualified names, what shadows what, and what makes a reference ambiguous rather
than merely unresolved have never been written down. Two already-open bugs look like they sit
in this chapter's territory — `BUG-15` (an imported operator is unresolvable unless the
function behind it is also separately in scope) and `BUG-16` (an unresolved type name is
invented rather than reported, instead of erroring) — but confirm that placement while
drafting rather than assuming it; the chapter's own probing may turn up a different or better
home for either.

**Approach:** follow `write-spec-chapter` in full. Likely territory, to confirm by probing
rather than assume from this ticket: qualified-name resolution, lexical shadowing
(`new_scope`/`expose_pattern`), and the ambiguous-vs-unresolved distinction
(`ValueType::Foreigns` vs. a plain miss), with `**Known gap:**` blocks for `BUG-15`/`BUG-16` if
Step 2 confirms they belong here.

**Acceptance:** `cargo test --test spec` green, `docs/spec/name-resolution.md` contributing its
blocks with every block tagged and each tag proven to fail, `docs/spec/README.md`'s row for
this chapter moved to `written`.
