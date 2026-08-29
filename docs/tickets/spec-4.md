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

**Problem:** value and function declarations, type annotations, `type` declarations, `infix`
declarations, and multi-line function declarations with pattern matching have never been
written down. The last is a deliberate divergence from Elm — there is no inherited rule even
to restate. One thing worth checking early rather than assuming either way: the parser appears
to group a multi-clause binding into one `Function`/`Match` (`Module::from_declarations`),
while `do_values` in canonicalization appears to reject more than one binding for a name
outright (`Error::MultipleBindingsUnsupported`). `CLAUDE.md` lists multi-clause function
declarations as implemented; if probing confirms the rejection, that's a real discrepancy for
the chapter (or a `BUG-`) to address — not something to take as given from this description.

**Approach:** follow `write-spec-chapter` in full. Likely territory, to confirm by probing
rather than assume from this ticket: value/function declarations and their optional type
annotations, `type` (union) declarations, `infix` declarations (precedence, associativity,
fixity), and multi-line function declarations with pattern matching, resolving the
`MultipleBindingsUnsupported` question above.

**Acceptance:** `cargo test --test spec` green, `docs/spec/declarations.md` contributing its
blocks with every block tagged and each tag proven to fail, `docs/spec/README.md`'s row for
this chapter moved to `written`.
