# LANG-72 · `()` is not recognised as a type, an expression or a pattern

**Sizing:** small-to-medium. The change runs through every phase a construct touches: three grammar
productions, three AST variants on each side, and one typer type. None of it is hard. It could grow
if `()` as a type turns out to need to be a named type in `Basics` and not a built-in shape (see
Approach, step 3).

**Part of:** [Active work: effects](README.md#active-work-effects). `main : Task ()` and every
test facade's `Task (Result Failure ())` need it. No other ticket on that path does.

**Location:** `src/compiler/parser/grammar.lalrpop` — the parenthesised productions that build
`TypeKind::Tuple`, `ExpressionKind::Tuple` and `PatternKind::Tuple`; `src/compiler/parser/mod.rs` —
`TypeKind`, `ExpressionKind`, `PatternKind`, whose doc comment already lists `Unit` among the
missing patterns; `src/compiler/canonical/mod.rs` — `Type` and `ExpressionKind`, each carrying a
`// Unit` placeholder comment where the variant goes, and the `from_parser*` conversions;
`src/compiler/typer/` — the type the unifier sees; `src/compiler/ir/` — whatever a backend reads.

**Problem:** [Types](../spec/types.md#the-unit-type) specifies `()` as the type with exactly one
value, which is also written `()`, and [Patterns](../spec/patterns.md#the-unit-pattern)
specifies `()` as a pattern that matches that value and binds nothing. Both chapters' blocks are
`expect=unimplemented`, because the grammar reads a type expression after `(` and finds `)`, and
the same happens in an expression and a pattern.

That was harmless until effects. [`main` must have type `Task ()`](../spec/packages.md#programs),
and a [test facade](../spec/interop.md#testing-a-companion) declares each check as
`Task (Result Failure ())`. So neither a program nor an effectful test can be written without it.

**Approach:**

1. Add the three productions, and one variant to each AST, per `CLAUDE.md`'s *A grammar change is
   never a one-file change*: grammar, parser AST and `from_parser*` land in one commit.
2. Keep `()` apart from the tuple rule. [`Tuple<T>`](../../src/compiler/tuple.rs) holds two or three
   elements by its shape, and `()` is not a tuple of zero. Adding a zero-arity case to `Tuple`
   would undo `AST-2`.
3. **The ticket does not pick** how the typer names the type. It can be a dedicated `Type::Unit`,
   or a type known by qualified name the way [the scalars](../spec/types.md#scalar-types) are.
   The first is smaller. The second means declaring `()` somewhere in `std/core`, which
   [Types](../spec/types.md#the-unit-type) does not say. Say which was chosen and why. If it
   is the second, the chapter needs a sentence first, and that sentence lands in a separate
   commit ahead of the code, per [the conventions](../spec/conventions.md#a-spec-change-and-a-semantics-change-do-not-share-a-diff).
4. `()` in a facade signature is admitted by [the table](../spec/interop.md#which-types-may-cross-the-boundary):
   make `check_facade_admitted_type` accept it.
5. Code generation for the new IR node is [`GEN-20`](gen-20.md). Until it lands, `javascript::emit`
   has to refuse a module holding `()` with an error rather than a panic, the way it refuses
   other things it cannot emit yet.

**Tests:** `tests/compiler/canonical.rs` for each of the three positions; `tests/typer.rs` for
`nothingUseful : ()` and for `always : () -> Flag` from the patterns chapter, plus a mismatch
(`() ` where `Int` is expected) that is a type error.

**Acceptance:** the `expect=unimplemented` blocks under [The unit type](../spec/types.md#the-unit-type)
and [The unit pattern](../spec/patterns.md#the-unit-pattern) are retagged `expect=ok`, and their
**Not implemented:** paragraphs are removed. `cargo test --test spec` is green. A module
annotating `x : Int` and defining `x = ()` is a type error whose caret sits under the `()`.
`cargo run -- compile std/core` is unaffected.
