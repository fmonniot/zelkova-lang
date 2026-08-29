# SPEC-8 · Write the Name resolution and scoping chapter

**Sizing:** medium.

**Location:** `src/compiler/canonical/environment.rs` — `Environment` trait,
`RootEnvironment`, `ScopedEnvironment`, `ValueType` (`Local`/`TopLevel`/`Foreign`/`Foreigns`),
`process_import`; `src/compiler/canonical/mod.rs` — `Error::VariableNotFound`,
`Error::AmbiguousVariables`, `Error::VariantNotFound`.

**Problem:** qualified names, what shadows what, and what makes a reference ambiguous rather
than merely unresolved have never been written down. Grounding this ticket surfaced two open
bugs squarely in this chapter's territory: `BUG-15` (an imported operator is unresolvable
unless the function behind it is also separately in scope) and `BUG-16` (an unresolved type
name is invented rather than reported, instead of erroring). Both belong in the chapter as
`**Known gap:**` blocks rather than being fixed here.

**Approach:** follow `write-spec-chapter`. Cover qualified-name resolution, lexical shadowing
(`new_scope`/`expose_pattern`), and the ambiguous-vs-unresolved distinction (`ValueType::Foreigns`
vs. a plain miss), with tagged `**Known gap:**` blocks linking `BUG-15` and `BUG-16`.

**Acceptance:** `cargo test --test spec` green, `docs/spec/name-resolution.md` contributing its
blocks with every block tagged and each tag proven to fail, `docs/spec/README.md`'s row for
this chapter moved to `written`.
