# SPEC-6 · Write the Expressions chapter

**Sizing:** medium-to-large. This is the chapter with the most unimplemented surface — `let …
in` and lambdas have no grammar production at all, and precedence/associativity resolution
looks unfinished (see Problem).

**Location:** `src/compiler/parser/grammar.lalrpop` — `AtomicExpr`, `AppExpr`, `InfixExpr`,
`Expr`; `src/compiler/parser/mod.rs` — `ExpressionKind`, `CaseBranch`;
`src/compiler/canonical/mod.rs` — `ExpressionKind` (`Apply`, `If`, `Case`, `Tuple`, `VarLocal` /
`VarTopLevel` / `VarForeign` / `VarConstructor`) and its commented-out `// Lambda`, `// Let`,
`// LetRec` variants; `Infix`/`canonical::Infix` for precedence and associativity.

**Problem:** application, `if`/`then`/`else`, `case … of`, `let … in`, lambdas, and the
operator table (precedence, associativity, the fact that no operator's meaning is built in)
have never been written down. `let … in` and lambdas are unimplemented per `CLAUDE.md`.
Grounding this ticket also found `InfixExpr`'s own comment questioning whether it needs a real
precedence-climbing node — worth confirming during drafting whether operator precedence is
actually resolved anywhere, or whether an example mixing two operators of different precedence
currently parses as naive left/right nesting instead.

**Approach:** follow `write-spec-chapter`. Cover the implemented forms in full (application,
`if/then/else`, `case … of`, tuples, the infix-to-application rewrite); mark `let … in` and
lambdas `**Not implemented:**`; confirm and document precedence/associativity behaviour, filing
a ticket rather than fixing it if the grounding finding above turns out to be a real gap.

**Acceptance:** `cargo test --test spec` green, `docs/spec/expressions.md` contributing its
blocks with every block tagged and each tag proven to fail, `docs/spec/README.md`'s row for
this chapter moved to `written`.
