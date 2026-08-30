# SPEC-6 · Write the Expressions chapter

**Sizing:** medium-to-large. Likely the chapter with the most unimplemented surface — `let …
in` and lambdas have no grammar production at all — but confirm the rest by probing rather
than assuming it from this ticket.

**Location:** `src/compiler/parser/grammar.lalrpop` — `AtomicExpr`, `AppExpr`, `InfixExpr`,
`Expr`; `src/compiler/parser/mod.rs` — `ExpressionKind`, `CaseBranch`;
`src/compiler/canonical/mod.rs` — `ExpressionKind` (`Apply`, `If`, `Case`, `Tuple`, `VarLocal` /
`VarTopLevel` / `VarForeign` / `VarConstructor`) and its commented-out `// Lambda`, `// Let`,
`// LetRec` variants; `Infix`/`canonical::Infix` for precedence and associativity.

**Grounding note:** the above and the specifics below came from one quick pass done only to
scope this ticket, not from `write-spec-chapter`'s Step 2 probing. Treat every specific claim
as a lead to re-verify, not as settled — and don't let this ticket's Approach cap what the
chapter ends up covering. Steps 1–2 (read the grammar/AST, then probe the compiler) and Step 4
(design questions) are what actually decide that.

**Problem:** application, `if`/`then`/`else`, `case … of`, `let … in`, lambdas, and the
operator table (precedence, associativity, the fact that no operator's meaning is built in)
have never been written down. `let … in` and lambdas are unimplemented per `CLAUDE.md`. One
open question worth probing early rather than assuming either way: `InfixExpr`'s own comment in
the grammar questions whether it needs a real precedence-climbing node, which suggests operator
precedence may not actually be resolved anywhere — confirm with a real example mixing two
operators of different precedence before writing the operator table as settled fact.

**Also owns the numeric-literal rule.**
[`docs/spec/type-classes.md`](../spec/type-classes.md) (`SPEC-12`) handed it here rather than
answering half of it in passing — it settles that an undetermined numeric literal defaults to
`Int` and that nothing else in the language defaults, but not what the literal's type *means*. The type checker gives an integer
literal an internal type that unifies with `Int` and `Float` and nothing else — so `x : Float`
with a body of `1` checks and `x : Char` with the same body does not. Whether that is a rule of
the language or an implementation detail is undecided, and this chapter is where literals are
specified. Note [ERR-13](err-13.md) is about how that type is *spelled* in a diagnostic, not
about what it means, and the two are independent. Writing this chapter takes the literal-typing
prose out of that chapter's *Numeric literals* section: what stays there is the pair that is a
class rule rather than a literal one — the constraint a literal carries, and the one default
that discharges it.

**Approach:** follow `write-spec-chapter` in full. Likely territory, to confirm by probing
rather than assume from this ticket: the implemented forms (application, `if/then/else`,
`case … of`, tuples, the infix-to-application rewrite); `let … in` and lambdas as
`**Not implemented:**`; and whatever Step 2 finds about precedence/associativity — file a
ticket rather than fix it if that turns out to be a real gap.

**Acceptance:** `cargo test --test spec` green, `docs/spec/expressions.md` contributing its
blocks with every block tagged and each tag proven to fail, `docs/spec/README.md`'s row for
this chapter moved to `written`.
