# SPEC-4 · Write the Declarations chapter

**Sizing:** medium.

**Location:** `src/compiler/parser/grammar.lalrpop` — `Decl`, `FunBinding`, `Union`, `Infix`
productions; `src/compiler/parser/mod.rs` — `Declaration`, `Function`, `FunBinding`, `Match`,
`Module::from_declarations`; `src/compiler/canonical/mod.rs` — `Value::Value` /
`Value::TypedValue`, `do_values`, `do_types`, `do_infixes`, `Error::MultipleBindingsUnsupported`.

**Grounding note:** the above and the specifics below came from one quick pass done only to
scope this ticket, not from `write-spec-chapter`'s Step 2 probing. Treat every specific claim
as a lead to re-verify, not as settled — and don't let this ticket's Approach cap what the
chapter ends up covering. Steps 1–2 (read the grammar/AST, then probe the compiler) and Step 4
(design questions) are what actually decide that.

**Scope narrowed (`SPEC-5`, 2026-08-29):** the two type-shaped declarations this ticket
originally claimed — the annotation `name : Type` and the `type` declaration — are specified in
[`docs/spec/types.md`](../spec/types.md) instead, together with the type expressions both are
made of. That chapter owns their syntax, where an annotation may be written, how many a
declaration may carry, and what a variant may be. This chapter refers to it rather than
restating any of it.

**Problem:** value and function declarations, `infix` declarations, and multi-line function
declarations with pattern matching have never been written down. The last is a deliberate
divergence from Elm — there is no inherited rule even
to restate. One thing worth checking early rather than assuming either way: the parser appears
to group a multi-clause binding into one `Function`/`Match` (`Module::from_declarations`),
while `do_values` in canonicalization appears to reject more than one binding for a name
outright (`Error::MultipleBindingsUnsupported`). `CLAUDE.md` lists multi-clause function
declarations as implemented; if probing confirms the rejection, that's a real discrepancy for
the chapter (or a `BUG-`) to address — not something to take as given from this description.

**Approach:** follow `write-spec-chapter` in full. Likely territory, to confirm by probing
rather than assume from this ticket: value/function declarations, `infix` declarations
(precedence, associativity, fixity), and multi-line function declarations with pattern
matching, resolving the `MultipleBindingsUnsupported` question above. Link to
[`types.md`](../spec/types.md) for the annotation and `type` forms rather than duplicating
them.

**Acceptance:** `cargo test --test spec` green, `docs/spec/declarations.md` contributing its
blocks with every block tagged and each tag proven to fail, `docs/spec/README.md`'s row for
this chapter moved to `written`.
