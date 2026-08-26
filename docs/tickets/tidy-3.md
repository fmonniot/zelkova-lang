# TIDY-3 · Fix the `associativy` typo

**Sizing:** trivial — six occurrences across four files, no behaviour change.

**Location:** `src/compiler/parser/mod.rs` — the `associativy` field on the parser's infix
declaration struct, which carries a `// TODO Fix typo` comment;
`src/compiler/parser/grammar.lalrpop` — the production that populates it;
`src/compiler/canonical/mod.rs` — `associativity: infix.associativy`, where the canonical AST
already spells it correctly and the typo is visible on exactly one side of the assignment;
`tests/compiler/parser/modules.rs` — three occurrences in the expected values.

**Problem:** `associativy` is not a word. The canonical AST spells the same concept
`associativity`, so the conversion reads `associativity: infix.associativy` and the mismatch is
the only thing telling you which side you are on. It has its own `// TODO Fix typo` in the
source, which has outlived several passes over that file.

Trivial, but worth an actual ticket rather than a drive-by: it touches the grammar, and a
grammar edit that does not compile is a worse outcome than the typo.

**Approach:** rename the field to `associativity` in the parser AST, the grammar production,
the canonical conversion, and the three test expectations. Remove the `// TODO Fix typo`
comment in the same commit — leaving it behind is how a fixed thing gets fixed twice. Grep for
`associativ` (not `associativy`) afterwards to catch any occurrence in a doc comment or a
string literal.

**Acceptance:** `grep -rn associativy src/ tests/` returns nothing, `cargo test` passes, and
`cargo run` still reports seven parsed modules — `Basics.zel` declares infix operators, so a
broken grammar production shows up there immediately.
