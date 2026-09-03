# BUG-22 · An operator's declared precedence and associativity are recorded and then ignored

**Severity:** high — every expression mixing two operators without parentheses is grouped
wrongly, so it computes the wrong value. `1 + 2 * 3` is compiled as `1 + (2 * 3)` only by luck
of the spelling; `2 * 3 + 1` is compiled as `2 * (3 + 1)`.

**Location:** `src/compiler/parser/grammar.lalrpop` — the `InfixExpr` production, whose one
non-trivial alternative is `<lhs: AppExpr> <op: SpannedOp> <rhs: InfixExpr>`.
`src/compiler/parser/mod.rs` — `Infix`'s `associativity` and `precedence` fields, and the doc
comment on the latter. `src/compiler/canonical/mod.rs` — `canonical::Infix`, which carries both
into a module's `Interface`.

**Problem:** `InfixExpr` recurses on the right and consults nothing. Every operator therefore
groups rightward and every operator has the same precedence, whatever its `infix` declaration
says:

| Written | Should be | Is |
|---|---|---|
| `a * b + c` (`*` at 7, `+` at 6) | `(a * b) + c` | `a * (b + c)` |
| `a - b - c` (`-` is `infix left`) | `(a - b) - c` | `a - (b - c)` |
| `a == b == c` (`==` is `infix non`) | rejected | `a == (b == c)` |

`std/core/src/Basics.zel` declares ten distinct precedences across nineteen operators, so this
is not a latent problem: any Zelkova program that mixes `*` with `+`, or `++` with `|>`, is
miscompiled.

Both fields survive all the way into an `Interface` — an importing module receives an
operator's precedence and associativity and has no more use for them than the declaring module
did. And `parser::Infix::precedence`'s doc comment states "The higher precedence will be parsed
first", which is what the code does not do; `CLAUDE.md`'s *A doc comment describes what the
code at that site does* makes that a defect on its own.

The grammar says as much where it happens: "TODO Might need a custom node here, to be able to
establish the correct application order in a next pass (once we have all infix rules)."

Found while writing [`docs/spec/expressions.md`](../spec/expressions.md) (`SPEC-6`), whose
*Precedence and associativity* section is where the rule is written down.

**Approach:** the operator table is not known to the grammar — an operator may be declared in
another module, whose `Interface` canonicalization is what reaches — so LALRPOP cannot resolve
this with precedence annotations. The `TODO` names the shape that works: parse a run of
operator applications into a flat node holding the operands and the operators between them,
and re-associate it in canonicalization, where the infix environment already exists
(`canonical/environment.rs` resolves an operator to its `Infix` today, which is what
`InfixReferenceInvalidValue` and `BUG-15` are about).

`CLAUDE.md`'s *A grammar change is never a one-file change* applies in full:
`grammar.lalrpop`, the `parser` AST, and `canonical/mod.rs`'s `from_parser*` conversions move
together. The rewrite to nested `Application` nodes currently happens in the grammar action;
it moves to canonicalization, and the invented nodes' spans have to move with it — the
grammar's comment there explains what each span is for and that reasoning still holds.

Re-association must reject rather than guess when two operators of equal precedence disagree,
including an `infix non` operator against itself. That is a new `canonical::Error` variant with
a message naming both operators and their declarations.

**Acceptance:** tests in `tests/compiler/canonical.rs` asserting the *shape* of the
canonicalized expression for `a * b + c`, `a - b - c` and `a ++ b ++ c` against `Basics`'
declarations, plus one asserting the error for `a == b == c`. `cargo run` still prints
`parsed 8 modules` and lists all eight as checked.

[`docs/spec/expressions.md`](../spec/expressions.md)'s *Equal precedence, disagreeing
associativity* block goes red when this lands — it is tagged `expect=ok` against today's
acceptance of `a == b == c`, and becomes a parse-or-canonicalization error — and is retagged
with its `**Known gap:**` paragraph deleted.

**The precedence half of this gap has no red test.** Grouping changes which value an
expression has, and nothing else: the `poly` block in that chapter's *Precedence and
associativity* section canonicalizes identically before and after this ticket, so its
`**Known gap:**` paragraph has to be deleted by hand. The spec harness stops at
canonicalization ([TEST-2](test-2.md)), and even with the type checker wired in, `a * b + c`
and `a * (b + c)` have the same type.
