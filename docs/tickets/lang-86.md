# LANG-86 · There is no `type alias` production, so a type cannot be given a second name

**Sizing:** medium. A grammar change, so `CLAUDE.md`'s *A grammar change is never a one-file
change* applies. What could make it bigger is parameters: expansion is then a substitution.

**Location:** `crates/zelkova-syntax/src/parser/grammar.lalrpop` — `Union`, the only production
that opens on `"type"`, and `PlainVarIdent`, where a soft keyword is re-admitted as a name;
`crates/zelkova-syntax/src/parser/tokenizer.rs` — the keyword table, which has no `alias`;
`crates/zelkova-syntax/src/parser/mod.rs` — the module AST, which holds `UnionType`s and nothing
else under a type name; `crates/zelkova-compiler/src/canonical/mod.rs` — `Module::types`,
`ExportType`, `Type` and the `from_parser*` conversions; `crates/zelkova-compiler/src/lib.rs` —
`Interface`, whose `unions` and `opaque_unions` are all an importer can name a type through.

**Decided ([Types](../spec/types.md#type-aliases)):** a type alias gives an existing type a
second name and **introduces no new type**: the alias and what it names are interchangeable
everywhere, in both directions, across module boundaries, and a type error mentioning one may
mention the other. An alias over a record introduces no constructor function
([`DEC-8` decision 2](../decisions/dec-8.md#2--a-record-type-is-a-structural-order-insensitive-set-of-fields)).
It may take parameters and is always applied to all of them, cannot name itself, and is exposed
by its bare name; [DEC-25](../decisions/dec-25.md) holds what those were chosen over.

**Not implemented:** the grammar reads `type` and then wants an uppercase name.

```
error: unexpected token: `LowerIdentifier("alias")`
  ┌─ alias-repro:src/Example.zel:6:6
  │
6 │ type alias Pair = (Size, Size)
  │      ^^^^^ unexpected token
  │
  = we were expecting one of the following tokens: ["up_ident"]
```

The chapter's own **Not implemented:** paragraph says so and cites no ticket. `Array.ignored`
under `std/core/src/` declares two aliases, and the block under
[A record type is a set of fields](../spec/records.md#a-record-type-is-a-set-of-fields) waits on
this as well as on the record tickets.

**Approach:**

1. **`alias` is a soft keyword, in the one position after `type`.**
   [Lexical structure](../spec/lexical-structure.md#reserved-words) lists it as the seventh, and
   its `expect=ok` block of soft keywords used as names already holds `alias = 7`, which has to
   stay green. A type's name is uppercase-initial, so one token of context decides it; if
   `alias` becomes a token, `PlainVarIdent` re-admits it the way it does `left`.

2. **A declaration `type alias Name = Type`** reaches the parser AST as a form of its own, not
   as a `UnionType` with one variant — it declares no constructor.

3. **An alias is resolved away in canonicalization.** Every use is replaced by the type it
   names as annotations are converted, so `canonical::Type` gains no alias form and the typer
   never sees one. "Interchangeable everywhere, in both directions" then needs no rule in the
   unifier: there is one type. The cost is that a type error spells the expansion, which the
   chapter permits. Keeping the alias's name for diagnostics is not required here.

4. **An alias is a name in the type namespace.** It collides with a union of the same name the
   way two unions do ([`LANG-32`](lang-32.md)), is listed in `exposing` by its bare name, and
   reaches an importer through the `Interface`, which has to carry what it expands to — the
   expansion is canonical already, so every name in it is qualified and means the same thing in
   the importing module.

5. **An alias takes parameters and is always applied to all of them**
   ([`DEC-25` decision 1](../decisions/dec-25.md#1--an-alias-may-take-parameters-and-is-always-applied-to-all-of-them)).
   `type alias Pair a = (a, a)` binds `a` throughout the right-hand side, and a use substitutes
   its arguments as it expands. An alias written with too few or too many is
   `canonical::Error::TypeArityMismatch`, the error a union's application already gets. A
   parameter the right-hand side never uses, and a variable there that is no parameter, follow
   whatever a `type` declaration does for the same shape ([`LANG-31`](lang-31.md)).

6. **An alias that names itself is an error**, directly or through another alias, reported at
   the alias with a new `canonical::Error` variant before anything is expanded — step 3 does
   not terminate on one. The check is over a module's aliases only: an imported alias arrives
   expanded.

7. **`Pair(..)` is an error**, in an `exposing` list and in an import: an alias has no
   constructors. On import, `EnvError::ConstructorsNotExposed` is what an opaque type's `(..)`
   already gets, and its message has to read correctly for a type with none to expose. In
   `do_exports` it is a new `canonical::Error` variant, with the caret on the entry.

8. **Nothing is written for an instance head.**
   [`DEC-25` decision 2](../decisions/dec-25.md#2--an-instance-head-written-through-an-alias-is-the-type-the-alias-names)
   has a head written through an alias judged as the type it names, which step 3 delivers once
   [`LANG-39`](lang-39.md) resolves heads: the head it reads is already expanded. If `LANG-39`
   has landed first, add its test here; if not, there is nothing to do and nothing to defer.

**Acceptance:** the first two blocks under [Type aliases](../spec/types.md#type-aliases) go red
and are retagged `expect=ok`; the third, the alias that names itself, becomes
`expect=canonical-error:` with the new variant; and the section's **Not implemented:** paragraph
goes, as does the sentence about `alias` in
[Lexical structure](../spec/lexical-structure.md#reserved-words)'s. A parser test asserts the
AST of an alias, and that `alias = 1` and `type alias Alias = Size` both parse. In
`crates/zelkova-compiler/tests/typer.rs`: a value annotated with an alias is accepted where its
expansion is wanted, and the reverse. In `crates/zelkova/tests/pipeline.rs`: the same across two
modules, the alias imported. In `crates/zelkova-compiler/tests/canonical.rs`: an alias and a
union of one name collide; an alias not exposed is not importable; `Pair Size` expands with its
argument substituted, and `Pair` and `Pair Size Size` are each `TypeArityMismatch`; an alias
naming itself, and two naming each other, are each the new error with its caret on the alias;
`Pair(..)` is an error in `exposing` and in an import. Each test seen red with what it pins
neutralised.

`cargo test --workspace` is green and `cargo run -- compile std/core` still lists all ten
modules as checked.

**Found:** while ordering the record tickets for *Active work: records* in
[the index](README.md). It is not a record question, but a record type is written out in full
wherever it appears, so records are where its absence is felt.
