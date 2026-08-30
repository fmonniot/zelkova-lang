# CLASS-2 · `class` and `instance` declarations parse, with a `where` block of members

**Sizing:** large, and the largest of the program. It is the one ticket that touches the
tokenizer, `layout.rs`, the grammar and both ASTs at once, and the layout half is the part
nobody has built yet.

**Location:** `src/compiler/parser/tokenizer.rs` — the keyword table (`"javascript" =>
Some(Token::Javascript)` and its siblings) and the `Token` enum;
`src/compiler/parser/layout.rs` — `Context`, `Contexts`, and `Layout::handle_next_token`'s
explicit-pop match; `src/compiler/parser/grammar.lalrpop` — `VarIdent`, `AtomicType`, `Union`,
`Decl`; `src/compiler/parser/mod.rs` — `Declaration`; `src/compiler/canonical/mod.rs` —
`Module`, `from_parser_module`.

**Depends on:** [CLASS-1](class-1.md), for the `=>` token — a superclass context is written in
the class head (`class Eq a => Comparable a where`) and reuses the same `ConstrainedType`
production. [LANG-9](lang-9.md) is not a hard dependency but sequences well before this one: an
instance head like `instance Comparable (List a)` needs a parenthesised type argument, which
does not parse today.

**Decided (`SPEC-12`, by the language owner):** members live in a `where` block, one per line.
Superclasses exist from the start.

```zel
class Eq a => Comparable a where
  compare : a -> a -> Order
  lt : a -> a -> Bool

instance Comparable Colour where
  compare a b =
    EQ
  lt a b =
    False
```

**Problem:** none of it parses, and the reason is not only that there are no productions.

**`class` and `instance` cannot be soft keywords.** The other four — `left`, `right`, `non`,
`javascript` — work because each sits after an unambiguous prefix (`infix`, `module`), so the
grammar can re-admit them as identifiers with no conflict. `class` would sit at the *start* of
a declaration, where `FunBinding: VarIdent DeclPattern* "=" Expr` also starts, and `DeclPattern`
can begin with an uppercase name. On lookahead `up_ident` the parser can neither reduce
`"class"` to `VarIdent` nor shift it as a keyword. Both become hard keywords, and
`class : Int` / `instance = 1` stop compiling — probed, both compile on `main`.

**And `instance` is worse than merely unreserved: an instance declaration misparses.** Probed
while writing the chapter — this compiles today, declaring one value, called `instance`:

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
as its parameters, because `DeclPattern` admits a bare `QualTypeIdent` as a nullary constructor
pattern. So the failure mode for someone writing an instance before this ticket lands is not a
syntax error they can act on — it is a different program that happens to compile whenever the
names resolve. A two-member instance fails, but only incidentally, on the second `=`. That is
the sharpest argument for reserving both words rather than finding a way to keep them soft.

**`where` is soft in value positions and hard as a type variable.** Narrower than it first
looks, and probed in both directions:

- With `"where" => Name::new("where")` in `VarIdent`, LALRPOP rejects the grammar — *Local
  ambiguity detected* on `ArgType = QualTypeIdent (*) AtomicType+`. `Comparable a where` cannot
  be resolved at `where`: it is either another type argument or the start of the body.
- Splitting the production — a `TypeVarIdent` that omits `where`, used by `AtomicType` and by
  `Union`'s type parameters, while `VarIdent` keeps it — **builds clean**, and `where : Int`,
  `f where = where` and `exposing (where)` all still compile. Only `type Box where = Box where`
  stops.

**The layout pass has to give each member its own block, and today it gives none.** This is the
substance of the ticket. Probed, an indented body under a head produces a *flat* token stream:

```
OpenBlock class Comparable a where compare Colon a Arrow Order lt Colon a Arrow a Arrow Bool CloseBlock
```

One block for the lot. That is unparseable, and not for a fixable grammar reason: `Order lt` is
a **valid type application** — `f : Order lt` parses on `main` — so the parser cannot stop at
`Order`, swallows the next member's name, and dies on its colon. The identical shape written at
top level fails the identical way:

```zel
f : Order  g : Order
-- UnexpectedToken { value: Colon, expected: ["lo_ident", "up_ident", "close block", "->", …] }
```

Two top-level annotations on *separate* lines parse only because layout wraps each in
`OpenBlock … CloseBlock`. **The block is the member separator**, and a `where` body needs the
same treatment.

**Approach:**

1. **Tokenizer.** `"class"`, `"instance"` and `"where"` join the keyword table.

2. **Layout.** A new `Context` for a class or instance body, pushed when `Token::Where` is seen,
   emitting an `OpenBlock`/`CloseBlock` pair per member the way `Context::CaseBlock` does per
   branch. The nearest model in the file is the `(Token::Of, Context::CaseExpression)` arm of
   the explicit-pop match: a keyword that only means something while a particular context is
   open, and falls through to ordinary handling otherwise. That is also what keeps `where` soft
   — outside a class or instance head there is no context to consume, so `where = 1` is an
   ordinary declaration.

   **This half is untested.** The grammar experiments below assumed layout emits `"open block"
   <member> "close block"` per member; nothing has produced that stream yet. Expect the real
   work here, and expect `Context`'s `describes()` to need a sentence for the new variant so a
   `LayoutError` inside a class body says which block it is about.

   Two things `CLAUDE.md`'s *A `Result`-yielding iterator must advance or stop* requires of any
   new error path here: consume input or stop. `BUG-4` and `BUG-5` are what that rule is made
   of.

3. **Grammar.** The head is parsed as **one `ConstrainedType`** and validated afterwards,
   for the same LALR(1) reason `CLASS-1` hit. Splitting it — `"class" <ctx:(<Type> "=>")?>
   <name:TypeIdent> <vars:VarIdent*>` — was tried and **rejected by LALRPOP**: with the context
   optional, the parser cannot tell at `up_ident` whether it is reading the context or the
   class name. This shape builds clean:

   ```
   ClassDecl = "class" <head:ConstrainedType> "where"
                 <members:("open block" <FunType> "close block")*>

   InstanceDecl = "instance" <head:ConstrainedType> "where"
                 <members:("open block" <FunBinding> "close block")*>
   ```

   So `class` takes signatures and `instance` takes bindings, and both validate `head` into
   (superclass context, class name, arguments) afterwards — reusing `CLASS-1`'s validation, and
   raising a real error for a head that is not shaped like one.

4. **Both ASTs, same commit.** `parser::Declaration` gains `Class` and `Instance` variants,
   `canonical::Module` gains somewhere to hold them, and `from_parser_module` converts. What
   canonicalization *does* with them — resolution, the orphan rule, the instance environment —
   is [CLASS-3](class-3.md); this ticket only has to get them across the boundary without
   dropping anything. `CLAUDE.md`'s invariant is explicit that silently dropping a construct
   during canonicalization is the failure mode the same-commit rule exists to prevent, and
   [BUG-18](bug-18.md) is what it looks like when it happens.

**Acceptance:** the two declarations at the top of this ticket parse, with tests in the parser's
own test module asserting the member list and the superclass context. A layout test asserts the
token stream for a two-member body contains an `OpenBlock`/`CloseBlock` pair per member — that
is the assertion the whole ticket turns on, and it goes red if step 2 regresses. `class : Int`
and `type Box where = Box where` are parse errors, and `where : Int`, `f where = where` and
`exposing (where)` still compile, each with a test recording the split deliberately. A
canonicalization test asserts a `class` and an `instance` survive into `canonical::Module`.
`cargo run` still prints `parsed 8 modules` and lists all eight as checked.

In [`docs/spec/type-classes.md`](../spec/type-classes.md), the `expect=ok` blocks under *The
words this reserves* (`class`/`instance` as value names, and `where` as a type variable) and the
one under *Declaring an instance* go red and are retagged with their `**Known gap:**` paragraphs
deleted.

**Two blocks in that chapter need retagging even though they do not go red**, and they will not
announce themselves. The class declarations under *Declaring a class* and *Superclasses* start
parsing when this lands and their `expect=unimplemented` tags correctly go red — but the
`class Functor f where` block under *A class is always over a complete type* keeps failing, on
`f a` rather than on `class`, so its tag stays green while the reason changes underneath it.
Retag it `expect=parse-error` — the rejection is permanent, and that is the tag that says so.
