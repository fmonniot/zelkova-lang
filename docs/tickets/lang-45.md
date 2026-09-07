# LANG-45 · There is no list pattern, so neither `[]` nor `first :: rest` can be matched

**Sizing:** medium. A grammar change, so `CLAUDE.md`'s *A grammar change is never a one-file
change* applies — `grammar.lalrpop`, the `parser` AST and the `canonical` conversion land
together.

**Location:** `src/compiler/parser/grammar.lalrpop` — `Pattern`, `CasePattern` and
`DeclPattern`, none of which has a bracket or a `::` alternative;
`src/compiler/parser/mod.rs`'s `PatternKind`; and `canonical`'s pattern conversion.

**Depends on:** [`LANG-43`](lang-43.md), hard, for the same reason as
[`LANG-44`](lang-44.md) — a list pattern is a constructor pattern over constructors that have to
exist.

**Decided (`SPEC-22`, by the language owner; [`DEC-7`](../decisions/dec-7.md) decision 5):** the
bracket pattern and the cons pattern are both **pattern-grammar productions**. `::` in a pattern
is not the operator [`LANG-43`](lang-43.md) declares and is never looked up, which is
[`DEC-5`](../decisions/dec-5.md)'s answer for a negative literal's sign applied to the same
question. [Lists](../spec/lists.md#lists-in-patterns) is the rule.

**Not implemented:** brackets are unconsumed in pattern position exactly as in expression
position, so `[]`, `[a]` and `[a, b]` are all `UnexpectedToken` at the `[`. `::` fares slightly
differently and no better: it tokenizes as `Operator("::")`, and a `case` branch reading
`first :: rest ->` reports `UnexpectedToken` expecting `->`, because the pattern is complete at
`first` and the grammar wants the arrow. A cons pattern in a declaration head — `f (x :: xs)` —
reports `UnexpectedToken` expecting `)` or `,`.

**Approach:** add both productions. The bracket form is a comma-separated sequence of whole
patterns, zero or more, with no trailing comma — the same shape
[`LANG-44`](lang-44.md) adds one level over. The cons form is `Pattern :: Pattern`, grouping
rightward, and both of its sides are whole patterns, so `a :: b :: rest` and `Circle n :: rest`
both parse.

Two placement questions the grammar decides rather than the chapter. A bracket pattern is
delimited, so it belongs at the same level a tuple pattern does and needs no parentheses in a
declaration head. A cons pattern is *not* delimited, so it goes where an applied constructor
pattern goes: bare at the head of a `case` branch, parenthesised as a sub-pattern and in a
declaration head, which is the rule
[Patterns](../spec/patterns.md#constructor-patterns) already states for the same reason.

**Desugar in canonicalization**, as [`LANG-44`](lang-44.md) does for the expression half: `[]`
becomes the `Nil` pattern, `first :: rest` becomes `Cons first rest`, `[a, b]` becomes
`Cons a (Cons b Nil)`. Exhaustiveness then gets list coverage for free — `[]` and a cons pattern
are the type's two constructors — which is what
[Lists](../spec/lists.md#lists-in-patterns) claims and what a list-shaped `PatternKind`
surviving canonicalization would take away.

Note that `Cons first rest` is a constructor pattern *nested inside* another one for any
literal past length one, so [`LANG-16`](lang-16.md) — a constructor pattern may not appear
inside another pattern — is a soft prerequisite: without it `[a, b]` desugars to something the
canonical AST cannot hold, even though it parses. `[]`, `[a]` and a single `::` do not hit it.

**Acceptance:** `[]`, `[a]`, `[_, b]`, `first :: rest` and `a :: b :: rest` all parse in a
`case` branch, and the parenthesised forms parse in a declaration head. Tests in the parser's
own test module assert the resulting `PatternKind` nesting, and a canonicalization test asserts
the `Cons`/`Nil` pattern chain. The `expect=unimplemented` block in
[Lists](../spec/lists.md#lists-in-patterns) goes red and is retagged `expect=ok`, as do the
two in [Patterns](../spec/patterns.md#list-patterns).

**Found:** while writing [`docs/spec/lists.md`](../spec/lists.md) (`SPEC-22`).
