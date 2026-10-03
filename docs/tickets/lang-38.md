# LANG-38 · `class` and `instance` declarations parse, with a `where` block of members

**Sizing:** large. It is the one ticket of the type-class order that touches the tokenizer, the
declaration chunker, `layout.rs`, the grammar and the parser AST at once, and the layout half is
the part nobody has built yet. It is deliberately *only* syntax: canonicalization rejects what
it parses, and [LANG-39](lang-39.md) is where a class first means something.

**Location:** `crates/zelkova-syntax/src/parser/tokenizer.rs` — the keyword table
(`"foreign" => Some(Token::Foreign)` and its siblings) and the `Token` enum;
`crates/zelkova-syntax/src/parser/chunk.rs` — `can_start_declaration`;
`crates/zelkova-syntax/src/parser/layout.rs` — `Context`, `Context::description`,
`Layout::handle_next_token`'s explicit-pop match, `Layout::explain`;
`crates/zelkova-syntax/src/parser/grammar.lalrpop` — `PlainVarIdent`, `VarIdent`, `AtomicType`,
`Union`, `FunType`, `FunBinding`, `Decl`; `crates/zelkova-syntax/src/parser/mod.rs` —
`Declaration`, `Module`, `Module::from_declarations`;
`crates/zelkova-compiler/src/canonical/mod.rs` — `Error`, `canonicalize_recovering`;
`docs/spec/lexical-structure.md` — *Reserved words*;
`editors/vscode/syntaxes/zelkova.tmLanguage.json` and
`crates/zelkova-syntax/tests/editor_grammar.rs`, which holds the two together.

**Depends on:** [LANG-71](lang-71.md), for the shape of a context: a class head and an instance
head each carry one, as `parser::Context`. [LANG-37](README.md) and [LANG-9](README.md) are
closed, so `=>` is a token and `instance Comparable (List a)` already has a type argument it can
parse.

**Decided (`SPEC-12` and `SPEC-14`, by the language owner; [DEC-2](../decisions/dec-2.md)
decision 2 and [DEC-24](../decisions/dec-24.md) decisions 2 to 4):** members live in a `where`
block, one per line. Superclasses exist from the start. An instance may carry a context. Each
body has a second shape, `derived`.

```zel
class Eq a => Comparable a where
  compare : a -> a -> Order

  derived compare
    matched = EQ
    differed i j =
      compare i j

    combine x y =
      y

instance Comparable Colour where
  compare a b =
    EQ

instance Eq a => Eq (Box a) where
  derived
```

**Problem:** none of it parses, and the reason is not only that there are no productions.

**`class` and `instance` cannot be soft keywords.** The soft ones — `left`, `right`, `non`,
`foreign`, `unsafe` — work because each sits where one token of context says which reading is
meant, so the grammar can re-admit them as identifiers (`PlainVarIdent`, `VarIdent`). `class`
would sit at the *start* of a declaration, where `FunBinding` also starts, and a parameter's
`Pattern` can begin with an uppercase name. On lookahead `up_ident` the parser can neither
reduce `"class"` to a name nor shift it as a keyword. Both become hard keywords, and `class : Int` / `instance = 1`
stop compiling — both compile on `main`, which the chapter's `expect=ok` block under *The words
this reserves* pins.

**And `instance` is worse than merely unreserved: an instance declaration misparses.** This
compiles today, declaring one value, called `instance`:

```zel
type Thing
  = Comparable
  | Colour
  | EQ

instance Comparable Colour where
  compare a b =
    EQ
```

`instance` is read as a function name and `Comparable`, `Colour`, `where`, `compare`, `a` and `b`
as its parameters, because `Pattern` admits a bare `QualTypeIdent` as a nullary constructor
pattern. So the failure mode for someone writing an instance before this ticket lands is not a
syntax error they can act on — it is a different program that happens to compile whenever the
names resolve. That is the sharpest argument for reserving both words.

**`where` is soft in value positions and hard as a type variable.** Probed in both directions on
the tree of 2026-08-29, before `PlainVarIdent` was split out of `VarIdent`; re-run both before
relying on them:

- With `"where" => Name::new("where")` among the identifiers `AtomicType` accepts, LALRPOP
  rejects the grammar — *Local ambiguity detected* on `ArgType = QualTypeIdent (*) AtomicType+`.
  `Comparable a where` cannot be resolved at `where`: it is either another type argument or the
  start of the body.
- Splitting the production — a `TypeVarIdent` that omits `where`, used by `AtomicType` and by
  `Union`'s type parameters, while the value-side identifiers keep it — **built clean**, and
  `where : Int`, `f where = where` and `exposing (where)` all still compiled. Only
  `type Box where = Box where` stopped.

**`derived` is soft too, and needs the token after it.** `derived` alone on a line of an
instance body is the request; `derived eq` in a class body opens a derivation; `derived : …` and
`derived = …` declare a member or a value of that name
([*The words this reserves*](../spec/type-classes.md#the-words-this-reserves)). `unsafe` is the
worked example of a soft keyword decided by what follows it: read the comments on `VarIdent`,
`FunType` and `FunBinding` before adding a fourth such word, and note the rule `PlainVarIdent`'s
comment states — a word in the tokenizer's soft-keyword table with no arm anywhere in
`VarIdent`'s closure is taken out of the language in every position.

**The layout pass has to give each member its own block, and today it gives none.** This is the
substance of the ticket. An indented body under a head produces a *flat* token stream:

```
OpenBlock class Comparable a where compare Colon a Arrow Order lt Colon a Arrow a Arrow Bool CloseBlock
```

One block for the lot. That is unparseable, and not for a fixable grammar reason: `Order lt` is
a **valid type application** — `f : Order lt` parses on `main` — so the parser cannot stop at
`Order`, swallows the next member's name, and dies on its colon. Two top-level annotations on
separate lines parse only because layout wraps each in `OpenBlock … CloseBlock`. **The block is
the member separator**, and a `where` body needs the same treatment — twice over for a
derivation, whose bindings are a block of blocks inside one member.

**Approach:**

1. **Tokenizer.** `"class"`, `"instance"`, `"where"` and `"derived"` join the keyword table.

2. **The chunker.** `parser::parse_recovering` cuts a module into one chunk per top-level
   declaration *before* layout, at each column-1 token `can_start_declaration` accepts. It has
   to accept `Token::Class` and `Token::Instance`, or a class written under another declaration
   stays in that declaration's chunk. `where` and `derived` are also names a function can have,
   so they join the list for the reason its doc comment gives for the other soft keywords. A
   class or instance and its whole indented body is then one chunk, which is right — and means
   a syntax error in one member costs the whole declaration, the same price a `case` pays.

3. **Layout.** A new `Context` for a class or instance body, pushed when `Token::Where` is seen
   *while the chunk's declaration opened on `class` or `instance`*, emitting an
   `OpenBlock`/`CloseBlock` pair per member the way `Context::CaseBlock` does per branch. The
   nearest model in the file is the `(Token::Of, Context::CaseExpression)` arm of the
   explicit-pop match: a keyword that only means something while a particular context is open,
   and falls through to ordinary handling otherwise. That is also what keeps `where` soft —
   outside a class or instance head there is no context to consume, so `where = 1` is an
   ordinary declaration. A `derived <member>` line opens a nested body the same way, one block
   per binding.

   **This half is untested.** The grammar below assumes layout emits `"open block" <member>
   "close block"` per member; nothing has produced that stream yet. Expect the real work here.
   `Context::description` needs a sentence for each new variant so a `LayoutError` inside a
   body says which block it is about. `Context` is `Copy` and its size is load-bearing —
   `CaseBlock`'s doc comment says what widening it costs — so keep a new variant within the
   size the existing ones have.

   `Layout::explain` turns a rejected indented line into `LayoutError::IndentedDeclaration`
   when the line starts with a token a declaration can start with. A member line is exactly
   that shape. It must not be explained away as a mis-indented top-level declaration: pin it
   with a test.

   Two things `CLAUDE.md`'s *A `Result`-yielding iterator must advance or stop* requires of any
   new error path here: consume input or stop.

4. **Grammar.** The head is parsed as **one `ConstrainedType`** and taken apart afterwards, for
   the LALR(1) reason `LANG-37` hit. Splitting it — `"class" <ctx:(<Type> "=>")?>
   <name:TypeIdent> <vars:VarIdent*>` — was tried and **rejected by LALRPOP**: with the context
   optional, the parser cannot tell at `up_ident` whether it is reading the context or the
   class name. This shape built clean on the older tree:

   ```
   ClassDecl = "class" <head:ConstrainedType> "where"
                 <members:("open block" <ClassMember> "close block")*>

   InstanceDecl = "instance" <head:ConstrainedType> "where"
                 <body:InstanceBody>
   ```

   A `ClassMember` is a `FunType` or a derivation: `"derived" <member:VarIdent>` followed by one
   block per `FunBinding`. An `InstanceBody` is one block holding the single word `derived`, or
   one block per `FunBinding` — never a mixture, which the grammar should make unwritable and
   not merely report. Whether a `where` with nothing under it parses is the grammar's to refuse
   or allow; [LANG-39](lang-39.md) reports a missing member either way.

   There are two sets of derivation bindings — `matched`, `differed` and `combine` for a member
   walked over two values, `atConstructor` and `combine` for one walked over a single value — and
   they differ only in the names the block holds, not in how it parses. This ticket parses a
   block of `FunBinding`s and does not choose between them.

5. **The parser AST.** `parser::Declaration` gains `Class` and `Instance`, and `parser::Module`
   gains a list of each, filled by `Module::from_declarations`. The head stays as the grammar
   read it: the `(Option<Context>, Type)` a `ConstrainedType` yields, with nothing checked — a
   head that is not shaped like one is [LANG-39](lang-39.md)'s to report. A member signature is
   a `FunType`, so one written `compare : Eq b => a -> b -> Order` arrives with
   `FunType::context` set, and one written `unsafe compare : …` with `marked_unsafe` set. Carry
   both; do not drop them the way `canonicalize` once dropped an annotation's context. Both are
   errors, and `LANG-39` reports them.

6. **Canonicalization rejects, one error per declaration.** `canonicalize_recovering` reports
   each `class` and each `instance` declaration with a new `canonical::Error` variant in the
   mould of `MultipleBindingsUnsupported` — a construct that parses and is not checked yet —
   with a `message()` in the reader's vocabulary and a label on the declaration's first line.
   Nothing is put in `canonical::Module`. This is what `CLAUDE.md`'s *A grammar change is never
   a one-file change* asks for: the construct is neither silently dropped nor half-converted,
   and no later phase can meet a class it does not know.

   A module that held a class is missing that class's members, so mark its scope incomplete
   (`RootEnvironment::set_incomplete`) the way a failed `type` declaration does. A use of a
   member is then not reported a second time as an unresolved name
   ([`DEC-23`](../decisions/dec-23.md)).

**Acceptance:**

- The three declarations at the top of this ticket parse, with tests in
  `crates/zelkova-syntax/tests/parser/` (register a new file in `parser_tests.rs` if one is
  added) asserting the member list, the derivation's bindings, the superclass context, the
  instance's context and the `derived` body.
- A layout test in `crates/zelkova-syntax/tests/parser/layout.rs` asserts the token stream for a
  two-member body contains an `OpenBlock`/`CloseBlock` pair per member, and for a derivation one
  per binding inside the member's own. That is the assertion the whole ticket turns on, and it
  goes red if step 3 regresses.
- `class : Int`, `instance = 1` and `type Box where = Box where` are parse errors. `where : Int`,
  `f where = where`, `exposing (where)`, `derived = 5` and `derived : Int` still compile. Each
  has a test recording the split deliberately.
- An instance body mixing `derived` with a binding is a parse error.
- A member line is not reported as `LayoutError::IndentedDeclaration`.
- A test in `crates/zelkova-compiler/tests/canonical.rs` asserts a module holding a class and an
  instance returns exactly one error for each, by variant, with the caret on the declaration —
  asserted on `diagnostic.labels[..].range`, since `NodeSpan`'s `PartialEq` is blind.
- `cargo test --workspace` is green. `cargo run -- compile std/core` still prints
  `parsed 10 modules`, lists all ten as checked and exits 0, and `cargo run -- test std/core`
  still reports `98 tests: 98 passed`.

**The documents that move with it**, each forced by a test going red except where noted:

- [`docs/spec/lexical-structure.md`](../spec/lexical-structure.md), *Reserved words*: `class`
  and `instance` join the reserved block, so the count in the sentence above it changes; `where`
  joins the soft-keyword table with its two positions; the `**Not implemented:**` paragraph
  about `derived` is deleted. `crates/zelkova-syntax/tests/editor_grammar.rs` then fails until
  `class` and `instance` move from the grammar's `not-yet-reserved-words` rule into
  `reserved-words` and `NOT_YET_RESERVED` goes — its doc comment describes the move. `where` is
  not in the reserved block, so it goes wherever the grammar keeps its other soft keywords.
  `editors/vscode/tests/keywords.zel` and `editors/vscode/README.md` both describe the three
  words as not reserved yet.
- [`docs/spec/type-classes.md`](../spec/type-classes.md): the two `expect=ok` blocks under *The
  words this reserves* go red and are retagged `expect=parse-error`, their `**Known gap:**`
  paragraph rewritten as the rule it was waiting on. The `expect=ok` block under *Declaring an
  instance* that shows the misparse goes red too; delete it and its `**Known gap:**` paragraph,
  since the block above it already shows an instance.
- **One block in that chapter needs retagging and will not go red.** The `class Functor f`
  block under *A class is always over a complete type* keeps failing — on `f a` now, and no
  longer on `class` — so its `expect=unimplemented` tag stays green while the reason changes
  underneath it. Retag it `expect=parse-error`, the tag that says the rejection is permanent,
  and reword the sentence after it that says the block fails "because `class` does not parse".
- `CLAUDE.md`, *Language notes*: the paragraph beginning "One of its rules constrains diffs
  outside that program today" describes the three words as ordinary identifiers and the
  misparse as live. Rewrite it for what is now true.
