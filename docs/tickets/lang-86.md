# LANG-86 · There is no `type alias` production, so a type cannot be given a second name

**Sizing:** medium. A grammar change, so `CLAUDE.md`'s *A grammar change is never a one-file
change* applies. What could make it bigger is the four questions below the chapter does not
answer, two of which the implementation cannot avoid.

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
   [Lexical structure](../spec/lexical-structure.md#reserved-words) lists the reserved words and
   says no other word is reserved, so `alias = 1` stays a legal binding. A type's name is
   uppercase-initial, so one token of context decides it. The chapter's table of soft keywords
   has six rows and gains a seventh, and its `expect=ok` block of soft keywords used as names
   gains `alias`; if it becomes a token, `PlainVarIdent` re-admits it the way it does `left`.

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

**Not decided, and the language owner's.** The section is three paragraphs and one example. Ask
before starting; the first two cannot be left for later.

- **Parameters.** The chapter's example takes none. `Array.ignored` writes
  `type alias Tree a = …`. If an alias may take parameters, whether it must then always be
  applied to all of them is a second question.
- **An alias that names itself**, directly or through another.
  [Records](../spec/records.md#the-type) says a record type cannot contain itself and that
  recursion goes through a `type` declaration, which reads as an error here too. The chapter
  does not say it of an alias, and step 3 does not terminate on one.
- **`Pair(..)`** in an `exposing` list or an import. An alias has no constructors to expose.
- **An alias as the head of an instance.**
  [A head](../spec/type-classes.md#what-an-instance-is-declared-for) is a declared type applied
  to distinct variables; whether `instance Eq Pair` names the tuple instance, or is an error, is
  not stated.

**Acceptance:** the block under [Type aliases](../spec/types.md#type-aliases) goes red and is
retagged `expect=ok`, and its **Not implemented:** paragraph goes. A parser test asserts the
AST of an alias, and that `alias = 1` and `type alias Alias = Size` both parse. In
`crates/zelkova-compiler/tests/typer.rs`: a value annotated with an alias is accepted where its
expansion is wanted, and the reverse. In `crates/zelkova/tests/pipeline.rs`: the same across two
modules, the alias imported. In `crates/zelkova-compiler/tests/canonical.rs`: an alias and a
union of one name collide; an alias not exposed is not importable; and each answer to the four
questions above is pinned. Each test seen red with what it pins neutralised.

`cargo test --workspace` is green and `cargo run -- compile std/core` still lists all ten
modules as checked.

**Found:** while ordering the record tickets for *Active work: records* in
[the index](README.md). It is not a record question, but a record type is written out in full
wherever it appears, so records are where its absence is felt.
