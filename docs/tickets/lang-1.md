# LANG-1 · Remove the `true`/`false` keywords; booleans are ordinary constructors

**Sizing:** medium. Small in the parser, and it reaches through both ASTs into the typer.

**Location:** `src/compiler/parser/tokenizer.rs` (`keyword`, `Token::True`/`Token::False`),
`src/compiler/parser/grammar.lalrpop` (the `extern` token list and `Lit`), the
`Literal::Bool` variant in `src/compiler/parser/mod.rs` and its canonical counterparts
(`canonical::ExpressionKind::Bool`, `canonical::PatternKind::Bool`), and
`src/compiler/typer/mod.rs` around lines 634, 692 and 774.

**Decided (SPEC-2, by the language owner):** Zelkova has no boolean literal syntax. `Bool` is
an ordinary union type, `True` and `False` are its constructors, and they are resolved,
imported, exposed and shadowed like every other constructor in the language.

**Problem:** `true` and `false` are reserved words producing `Literal::Bool`, while
`std/core/src/Basics.zel` separately declares `type Bool = True | False` and exposes both
constructors. Booleans therefore exist twice over, with two spellings that mean the same
values and no rule anywhere saying so. It also costs two names: `true` cannot be a variable,
which [`docs/spec/lexical-structure.md`](../spec/lexical-structure.md#reserved-words) says it
should be, and where that chapter's `**Known gap:**` block sits.

Note a second-order effect that has to be answered rather than discovered: the typer maps a
canonical type *named* `Bool` to `TypeLiteral::Bool` by string comparison
(`typer/mod.rs:634`), ignoring which module declared it, and `Reason::IfCondition` requires an
`if` condition to be that type. Once `True`/`False` are ordinary constructors, the type they
build is whichever `Bool` was in scope. Decide whether `if` is defined against
`Basics.Bool` specifically — which makes `Basics` privileged, and needs the implicit-import
question the modules chapter will raise — or against any two-constructor type named `Bool`,
which is what the string comparison accidentally implements today.

**Approach:** delete the two keywords and the two `Lit` productions, then follow the type
errors. `Literal::Bool` and its canonical/typer counterparts go with them; `True` and `False`
arrive instead as `ExpressionKind::TypeConstructor` and `PatternKind::Constructor`, both of
which already exist and already work — `type Bool = True | False` and `case b of True -> …`
compile today.

`TypeLiteral::Bool` in the typer is a separate question from `Literal::Bool` and probably
stays: it is how the *type* is represented, not the literal, and `if` still needs it.

**Acceptance:** `true = 1` and `f true = 1` both compile, `true` and `false` behaving as
ordinary lowercase identifiers with no special meaning. A `case` over a locally-declared
`type Bool = True | False` still type-checks, and an `if` whose condition is `True` still
type-checks. The `**Known gap:**` block in `docs/spec/lexical-structure.md`'s *Reserved
words* section goes red on the parse-error pin and is retagged `expect=ok`, its paragraph
deleted.

`cargo run` must still print `parsed 8 modules` and exit 0. Grep `std/core/src/` for `true`
and `false` first — `Basics.zel` uses `True`/`False` and should be unaffected, but the
`.ignored` modules are not checked and may hide the lowercase spelling.
