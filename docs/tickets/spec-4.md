# SPEC-4 · Write the Declarations chapter

**Sizing:** medium. The grammar and canonical AST already cover most declaration forms; the
open question is about multi-clause function bindings, which the parser groups but
canonicalization currently rejects.

**Location:** `src/compiler/parser/grammar.lalrpop` — `Decl`, `FunBinding`, `Union`, `Infix`
productions; `src/compiler/parser/mod.rs` — `Declaration`, `Function`, `FunBinding`, `Match`,
`Module::from_declarations`; `src/compiler/canonical/mod.rs` — `Value::Value` /
`Value::TypedValue`, `do_values`, `do_types`, `do_infixes`, `Error::MultipleBindingsUnsupported`.

**Problem:** value and function declarations, type annotations, `type` declarations, `infix`
declarations, and multi-line function declarations with pattern matching have never been
written down. The last is a deliberate divergence from Elm — there is no inherited rule even
to restate — and grounding this ticket found a real gap worth confirming while drafting: the
parser groups a multi-clause binding into one `Function`/`Match` (`Module::from_declarations`),
but `do_values` in canonicalization rejects more than one binding for a name outright
(`Error::MultipleBindingsUnsupported`). `CLAUDE.md` lists multi-clause function declarations as
implemented; this suggests the feature parses but does not canonicalize, which the chapter
needs to either confirm or correct.

**Approach:** follow `write-spec-chapter` — probe rather than reason, settle design questions
with the language owner, file a ticket for whatever the multi-clause finding turns out to be
rather than fixing it here. Cover: value/function declarations and their optional type
annotations, `type` (union) declarations, `infix` declarations (precedence, associativity,
fixity), and multi-line function declarations with pattern matching, resolving the
`MultipleBindingsUnsupported` question above.

**Acceptance:** `cargo test --test spec` green, `docs/spec/declarations.md` contributing its
blocks with every block tagged and each tag proven to fail, `docs/spec/README.md`'s row for
this chapter moved to `written`.
