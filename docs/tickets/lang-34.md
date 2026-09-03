# LANG-34 · There is no lambda production, so `\x -> x` is read as an operator

**Sizing:** medium. A small grammar addition, but it lands in both ASTs and in the typer, and
`\` has to stop being an operator character first.

**Location:** `src/compiler/parser/tokenizer.rs` — `is_operator_char`, which lists `'\\'`
among the operator characters. `src/compiler/parser/grammar.lalrpop` — `Expr` and
`AtomicExpr`, neither of which has a lambda alternative.
`src/compiler/parser/mod.rs` and `src/compiler/canonical/mod.rs` — `ExpressionKind` in each,
the canonical one carrying `// Lambda` as a placeholder comment.

**Decided ([`docs/spec/expressions.md`](../spec/expressions.md), *Lambdas*;
[`docs/spec/name-resolution.md`](../spec/name-resolution.md), *Scopes*):** `\patterns ->
expression` is an anonymous function taking one or more parameters, and `\x y -> e` means
`\x -> \y -> e`. Its parameters are the patterns a function declaration's parameters accept,
so only irrefutable ones may appear — a lambda has one body, and a failed match has nowhere to
fall through to. The body extends as far to the right as it can, and a lambda cannot span
multiple lines. It is a binding position: its parameters are visible in its body and nowhere
else.

**Problem:** `\` is an ordinary operator character, so `\x -> x` lexes as
`Operator("\\")` followed by an identifier and an arrow, and the grammar rejects it where an
expression was wanted:

```zel
f =
  \x -> x
```

reports *UnexpectedToken `Operator("\\")`, expected `lo_ident`, `up_ident`, `integer`,
`float`, `char`, `true`, `false`, `(`, `-`, `case`, `if`, `left`, `right`, `non`*. As an
argument, `g (\x -> x)`, it fails the same way at the same token.

Because `\` is an operator character, nothing stops a module declaring `infix left 5 (\) = f`
today, which is what has to be given up: the character cannot both open a lambda and open an
operator name.

Neither AST can hold the construct — `parser::ExpressionKind` has no variant, and
`canonical::ExpressionKind` marks the hole with a comment.

Found while writing [`docs/spec/name-resolution.md`](../spec/name-resolution.md) (`SPEC-8`),
whose *Scopes* section names lambdas as a binding position the language has and the compiler
does not.

**Approach:** three things move, and the ticket picks none of them:

1. **`\` stops being an operator character.** Removing it from `is_operator_char` and adding a
   `Token::Backslash` is the direct route. Whether an operator name may still *contain* a
   backslash after the first character is a separate question and worth answering explicitly
   rather than falling out of the tokenizer's shape.
2. **Where the production sits.** A lambda's body extends as far right as it can, which makes
   it an `Expr` alternative rather than an `AtomicExpr` one, and means a lambda used as an
   argument is parenthesised. That is the same associativity question
   [LANG-22](lang-22.md) is about for an operator's right operand, and the two want reading
   together.
3. **Irrefutability.** Only irrefutable patterns may be parameters, which is the rule a
   function declaration's parameters already need and which nothing enforces —
   [`docs/spec/patterns.md`](../spec/patterns.md)'s *a pattern that can fail, and one that
   cannot* is where it is written down. Enforcing it for lambdas alone would put the check in
   two places.

[LANG-33](lang-33.md) is the sibling gap — the other expression form that introduces a
scope — and the two are independent.

**Acceptance:** both `expect=unimplemented` blocks in
[`docs/spec/expressions.md`](../spec/expressions.md)'s *Lambdas* section — `\x y -> add x y`
and `\(Point x y) -> add x y` — go red and are retagged, with their **Not implemented:**
paragraphs deleted. The **Not implemented:** paragraph in
[`docs/spec/name-resolution.md`](../spec/name-resolution.md)'s *Scopes* section goes with them,
and that chapter gains a block showing a lambda parameter shadowing a top-level name. A parser
test pins that `\x y -> e` builds the same tree as `\x -> \y -> e`, and a `tests/typer.rs`
test pins that a lambda passed as an argument checks. `cargo run` still prints
`parsed 8 modules` and lists all eight as checked.
