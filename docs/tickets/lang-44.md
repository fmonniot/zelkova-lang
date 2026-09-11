# LANG-44 · There is no list-literal production, so `[1, 2]` does not parse

**Sizing:** medium. A grammar change, so `CLAUDE.md`'s *A grammar change is never a one-file
change* applies — `grammar.lalrpop`, the `parser` AST and the `canonical` conversion land
together.

**Location:** `src/compiler/parser/grammar.lalrpop`, which has no production consuming
`Token::LBracket`; `src/compiler/parser/mod.rs`'s `ExpressionKind`; and
`canonical::Expression::from_parser_expression`.

**Depends on:** [`LANG-46`](lang-46.md), hard. A literal is read against `List`, `Nil` and
`Cons`, and until `std/core` declares them there is nothing for it to mean.

**Decided (`SPEC-22`, by the language owner; [`DEC-7`](../decisions/dec-7.md) decision 4):** a
list literal is a bracketed, comma-separated sequence of expressions with no upper limit and no
trailing comma, and it is sugar for the `Cons` chain its elements spell out.
[Lists](../spec/lists.md#list-literals) is the rule.

**Not implemented:** `[` and `]` are tokenized — `tokenizer.rs` emits `Token::LBracket` and
`Token::RBracket` — and no production consumes either, in any position. `f = [1, 2]` is
`UnexpectedToken` at the `[`, and so is `f = []`, a multi-line literal, and a literal in a
nested position. [Lexical structure](../spec/lexical-structure.md#punctuation) states that
accurately.

**Approach:** add the production to the atomic-expression level, since a literal is delimited on
both sides and needs no parenthesising in argument position — `f [1] [2]` is two arguments.

The element list is `Expr` separated by `,`, permitting zero elements and forbidding a trailing
one. Note the trailing-comma rule is the *opposite* of the `exposing` list's and matches the
variant list's; [Lists](../spec/lists.md#list-literals) carries the reasoning.

**Desugar in canonicalization, not later.** `[a, b]` becomes `Cons a (Cons b Nil)` and `[]`
becomes `Nil`, so nothing downstream of `canonical::Module` learns about lists: the typer infers
a list's type as an ordinary application of a declared type constructor, and exhaustiveness sees
two ordinary constructors. That is what
[Lists](../spec/lists.md#lists-in-patterns) means by list coverage being ordinary constructor
coverage. A `ExpressionKind::List` variant surviving into the canonical AST would put a second,
list-shaped case in every phase after it.

Resolving `Nil` and `Cons` from a module that does not expose them is the one genuinely new
thing here, since they are deliberately not in scope for the program being compiled
([`LANG-46`](lang-46.md)). Whether that reaches for the `List` interface directly or waits on
the default imports ([`LANG-8`](lang-8.md)) is the implementer's call; it is not a language
question and the chapter does not answer it.

**Acceptance:** `[]`, `[1]`, `[1, 2, 3]`, a multi-line leading-comma literal and a nested
`[[1], [2]]` all parse and canonicalize; `[1, 2,]` is a syntax error. Tests in the parser's own
test module assert the resulting `ExpressionKind` nesting, and a canonicalization test asserts
the `Cons`/`Nil` chain rather than only that it succeeded. The three `expect=unimplemented`
blocks in [Lists](../spec/lists.md#list-literals) go red — that tag's whole job — and are
retagged `expect=ok`; so does the `mixed` block in
[The type](../spec/lists.md#the-type), which becomes an `expect=type-error` — the harness runs
the type checker, so the error that ought to reject it is one the block can pin. The `[1, 2,]`
block is already `expect=parse-error` and stays
green; its **Not implemented:** paragraph is what changes, because after this the block is
rejected for the reason it illustrates.

**Found:** while writing [`docs/spec/lists.md`](../spec/lists.md) (`SPEC-22`).
