# CLASS-1 · A type annotation may carry a constraint context, written `Class a =>`

**Sizing:** medium. The grammar shape is settled and probed (see **Approach**); the work is in
the parser AST, the canonical conversion, and one validation step that has to produce a real
diagnostic rather than a silent acceptance.

**Location:** `src/compiler/parser/tokenizer.rs` — the operator table in `consume_operator`
(`"->" => Token::Arrow` and its siblings) and the `Token` enum;
`src/compiler/parser/grammar.lalrpop` — the `extern` token list, `Type`, and `FunType`;
`src/compiler/parser/mod.rs` — `TypeKind`, `FunType`; `src/compiler/canonical/mod.rs` —
`Type::from_parser_type` and the `FunType` half of `from_parser_module`.

**Depends on:** nothing. This is the first ticket of the type-classes program and the only one
that can start today.

**Decided (`SPEC-12`, by the language owner):** a constraint is written before the type, with
`=>`. One constraint is bare; several are parenthesised and comma-separated.

```zel
min : Comparable a => a -> a -> a
lookup : (Comparable k, Eq v) => k -> v -> Bool
```

**Problem:** there is no surface for a constraint at all. `=>` lexes as an ordinary
`Token::Operator("=>")` and the type grammar has no use for it, so a constrained annotation is
a parse error today and there is nowhere in either AST to put a context.

**This is a breaking change**, and the ticket should say so in its PR: `=>` is a legal
user-defined infix operator on `main`. Probed — this compiles today and stops compiling with
this ticket:

```zel
infix left 5 (=>) = f

f : Int -> Int -> Int
f a b =
  a
```

Nothing in `std/core/src/` or `tests/` uses it.

**Approach:** four files, one commit — `CLAUDE.md`'s *A grammar change is never a one-file
change* applies in full.

1. **Tokenizer.** Add `"=>" => Token::FatArrow` beside `"->" => Token::Arrow`, and the variant
   to `Token`.

2. **Grammar.** The obvious production is ambiguous and the working one is not. Both were
   built against LALRPOP during `SPEC-12`'s design session:

   - `Type: … | <ctx:Type> "=>" <t:Type>` — **rejected by LALRPOP**, four *Local ambiguity
     detected* errors with the hint *This looks like a precedence error related to `Type`*.
   - A separate nonterminal one layer above `Type`, reachable only from the annotation
     position — **builds clean**:

     ```
     ConstrainedType: Type = {
         Type,
         <ctx:Type> "=>" <t:Type> => …,
     }

     FunType: FunType = <l:@L> <name:VarIdent> ":" <tpe:ConstrainedType> <r:@R> => …
     ```

   Note what the second shape means: the constraint context is parsed **as a type** and has to
   be validated into a context afterwards. That is not a shortcut, it is forced — LALR(1)
   cannot decide at the `(` of `(Comparable k, Eq v)` whether it is looking at a constraint
   list or at a two-tuple type, because those are the same tokens. Probed: all three of
   `Comparable a => a -> a`, `(Comparable k, Eq v) => k -> v -> Bool` and `(Comparable a) => a
   -> a` parse under this shape, and so does `Int -> Int => a -> a`, which is why step 4 exists.

3. **Parser AST.** The context has to be carried somewhere, and there is a real choice here
   that **this ticket does not make**:

   - **`TypeKind::Constrained(Box<Type>, Box<Type>)`** — the grammar action is total, the node
     is spanned like every other `Type`, and validation happens in canonicalization where
     errors already have somewhere to go. Costs a `TypeKind` variant that is only ever legal at
     the top of an annotation, which every match over `TypeKind` then has to handle.
   - **`FunType` gains a `context` field**, with the grammar action doing the validation. Keeps
     the illegal-nesting problem out of `TypeKind` entirely. Needs a fallible action — LALRPOP
     spells that `=>?` and the grammar already declares `type Error = Error`, so it is
     available; confirm it before committing to this branch, and check what the resulting
     diagnostic looks like, because a `ParseError::User` is not a `PhaseError` and will not
     carry a `SpanLabel` for free.

4. **Validate the left side into a context**, wherever step 3 put it. `Int -> Int => a -> a`
   parses and is meaningless; so does `(Int, Char) => a`. A well-formed context is one
   constraint, or a tuple of them, where each is an uppercase name applied to one or more
   arguments. Anything else is a new `canonical::Error` variant naming what was written and
   carrying its span — `parser::Type` has one (`ERR-3`), so the caret lands under the offending
   text and not under the whole annotation. Per `CLAUDE.md`'s *An error has to describe
   itself*, its `message()` is written in the reader's vocabulary: something is in the
   constraint position that is not a constraint.

5. **A `module javascript` facade may not carry a constraint** (`SPEC-12` decision 6). The
   facade's companion `.mjs` is promised a plain parameter list, and a constrained facade is
   the one thing that could not honour it. Canonicalization knows the module is a facade —
   `canonical::Module::binding_javascript` — so this is a check with its own error variant and
   its own message, not a note bolted onto step 4's.

The canonical `Type` does not gain a constraint case in this ticket. Nothing consumes a context
yet, so canonicalization validates it, reports on it, and discards it; `CLASS-3` is what gives
it somewhere to live. Say so in the code at the discard site, because a reader finding a
validated-then-dropped value will otherwise assume it is a bug.

**Acceptance:** `min : Comparable a => a -> a -> a` and `lookup : (Comparable k, Eq v) => k ->
v -> Bool` parse, with tests in the parser's own test module asserting the resulting AST shape.
`f : Int -> Int => a -> a` is a canonicalization error naming the malformed context, and a
constrained signature inside a `module javascript` facade is a different one — both with tests
in `tests/compiler/canonical.rs` asserting the variant, and both with a `diagnostic.labels[..]`
assertion pinning the caret rather than an `assert_eq!` on the whole value (`NodeSpan`'s
`PartialEq` is blind — `CLAUDE.md`, *An error has to describe itself*). `infix left 5 (=>) = f`
becomes a parse error, with a test recording that deliberately. `cargo run` still prints
`parsed 8 modules` and lists all eight as checked.

In [`docs/spec/type-classes.md`](../spec/type-classes.md), the `expect=ok` block under *The words
this reserves* showing `(=>)` declared as a user infix goes red and is retagged with its
`**Known gap:**` paragraph deleted, and the four `expect=unimplemented` blocks showing a
constrained annotation go red.

**One block needs retagging without going red.** The facade block under *A constrained function
may not be a JavaScript facade* fails today because `=>` does not parse; after this ticket it
fails because a facade may not be constrained. Same verdict, different reason, and
`expect=unimplemented` cannot tell them apart. Retag it
`expect=canonical-error:<the new variant>` — that is the tag that pins what the chapter's prose
actually claims.
