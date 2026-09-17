# LANG-1 · Remove the `true`/`false` keywords; booleans are ordinary constructors

**Sizing:** medium. Small in the parser, and it reaches through both ASTs into the typer.

**Location:** `src/compiler/parser/tokenizer.rs` (`keyword`, `Token::True`/`Token::False`),
`src/compiler/parser/grammar.lalrpop` (the `extern` token list and `Lit`), the
`Literal::Bool` variant in `src/compiler/parser/mod.rs` and its canonical counterparts
(`canonical::ExpressionKind::Bool`, `canonical::PatternKind::Bool`), and in
`src/compiler/typer/mod.rs` the `TermKind::Bool` and `TypedTermKind::Bool` variants with
the two arms that build them (`canonical_expr_to_term`'s `ExpressionKind::Bool`,
`translate_pattern`'s `PatternKind::Bool`) plus the `TypedTermKind::Bool` arm in
`src/compiler/typer/constraint.rs`. `typer::bool_type` is not one of them: an `if`
condition still needs a `Bool` no source spelled.

**Decided (SPEC-2, by the language owner):** Zelkova has no boolean literal syntax. `Bool` is
an ordinary union type, `True` and `False` are its constructors, and they are resolved,
imported, exposed and shadowed like every other constructor in the language.

**Problem:** `true` and `false` are reserved words producing `Literal::Bool`, while
`std/core/src/Basics.zel` separately declares `type Bool = True | False` and exposes both
constructors. Booleans therefore exist twice over, with two spellings that mean the same
values and no rule anywhere saying so. It also costs two names: `true` cannot be a variable,
which [`docs/spec/lexical-structure.md`](../spec/lexical-structure.md#reserved-words) says it
should be, and where that chapter's `**Known gap:**` block sits.

A second-order effect is already answered: the typer has no literal `Bool` left, and
`Reason::IfCondition` requires an `if` condition to be `Basics.Bool` — the union `Basics`
declares, whatever else a module calls `Bool` ([`DEC-15`](../decisions/dec-15.md) decisions 1
and 5, closed as LANG-60). So once `True`/`False` are ordinary constructors, the type they
build has to be that union for an `if` to accept it, and `typer::bool_type` is the one place
that name is written.

**Approach:** delete the two keywords and the two `Lit` productions, then follow the type
errors. `Literal::Bool` and its canonical/typer counterparts go with them; `True` and `False`
arrive instead as `ExpressionKind::TypeConstructor` and `PatternKind::Constructor`, both of
which already exist and already work — `type Bool = True | False` and `case b of True -> …`
compile today.

**Acceptance:** `true = 1` and `f true = 1` both compile, `true` and `false` behaving as
ordinary lowercase identifiers with no special meaning. A `case` over a locally-declared
`type Bool = True | False` still type-checks, and an `if` whose condition is `Basics.True`
still type-checks. That qualification is the whole of what the paragraph above changes for
this ticket: an `if` requires `Basics.Bool`, so a module's own `type Bool = True | False`
does not satisfy one and is not the type to write the acceptance case against. The `**Known gap:**` block in `docs/spec/lexical-structure.md`'s *Reserved
words* section goes red on the parse-error pin and is retagged `expect=ok`, its paragraph
deleted.

`cargo run` must still print `parsed 8 modules` and exit 0. Grep `std/core/src/` for `true`
and `false` first — `Basics.zel` uses `True`/`False` and should be unaffected, but the
`.ignored` modules are not checked and may hide the lowercase spelling.
