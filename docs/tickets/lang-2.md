# LANG-2 · `javascript` is reserved outright, unlike the other three soft keywords

**Sizing:** small. One production.

**Location:** `src/compiler/parser/grammar.lalrpop`, the `VarIdent` production — it lists
`"left"`, `"right"` and `"non"` as alternatives under the comment *soft keywords are
authorized identifier*, and omits `"javascript"`.

**Decided (SPEC-2, by the language owner):** all four soft keywords behave alike. Each is a
keyword in exactly one position — `left`/`right`/`non` as an `infix` declaration's
associativity, `javascript` in a module header — and an ordinary lowercase identifier
everywhere else.

**Problem:** the tokenizer treats all four the same, producing a distinct token for each, and
the grammar then re-admits three of them as identifiers and not the fourth. So `left = 1`
compiles and `javascript = 1` is a parse error whose "expected" list names the other three,
which reads as arbitrary because it is. `docs/spec/lexical-structure.md`'s *Reserved words*
section carries the `**Known gap:**` block.

**Approach:** add `"javascript" => Name::new("javascript")` to `VarIdent` beside its three
siblings.

Check for a grammar conflict before assuming it is that simple: `javascript` appears in the
`Module` production as `"module" <js: "javascript"?> <name: QualTypeIdent>`, where the token
following it is an *upper* identifier. `VarIdent` yields a lower identifier, so the two
positions should not collide — but LALRPOP is the authority on that, not this paragraph. If
it does conflict, say so in the PR rather than working around it; the alternative reading
(that `javascript` genuinely cannot be soft) would change the spec, not just the grammar, and
that is the owner's call.

**Acceptance:** `javascript = 1`, `f javascript = javascript`, and `exposing (javascript)`
all compile. The `**Known gap:**` block in `docs/spec/lexical-structure.md` goes red on its
`parse-error:UnexpectedToken` pin and is retagged `expect=ok` with its paragraph deleted.
`std/core/src/Js/*.zel` must still parse — `module javascript Js.Basics …` is the position
that must keep working.
