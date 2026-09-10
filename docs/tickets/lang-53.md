# LANG-53 · A facade signature cannot be marked `unsafe`, and an unmarked one is held to nothing

**Sizing:** medium. One optional token in a grammar production, a `bool` carried through two
ASTs, and 45 signatures rewritten in `std/core`. It is medium rather than small because
`CLAUDE.md`'s *A grammar change is never a one-file change* applies, and because the soft-keyword
half has a real choice in it (step 1 below).

**Location:** `src/compiler/parser/tokenizer.rs` — the keyword table at `"javascript" =>
Some(Token::Javascript)`; `src/compiler/parser/grammar.lalrpop` — the token map, and the
`FunType` production (`<name:VarIdent> ":" <tpe:Type>`), which is what a facade declaration
parses as; `src/compiler/parser/mod.rs` — `FunType`, and `Module::from_declarations` where
`binding_javascript` is already derived from the module modifier;
`src/compiler/canonical/mod.rs` — the `if source.binding_javascript` branch of `canonicalize`.
Then all three of `std/core/src/Js/*.zel`.

**Depends on:** nothing hard. [`LANG-9`](lang-9.md) gates *half* the acceptance below and not the
other half — see step 4, and [`LANG-54`](lang-54.md) gates the retagging, every facade block in
the spec naming the `foreign` modifier.

**Problem:** [Foreign interoperability](../spec/interop.md) settles two rules the compiler has
neither of.

A `module javascript` facade **declares an effect by default**: its result type must be
`Task (Result Failure a)` ([An effectful facade](../spec/interop.md#an-effectful-facade)).
Writing **`unsafe`** before a signature instead declares a plain function, whose author asserts
that the companion is pure and that it returns
([An `unsafe` facade](../spec/interop.md#an-unsafe-facade)). The word is the language's only
unchecked claim, and putting it on the declaration is what makes the trusted surface greppable —
[`DEC-12`](../decisions/dec-12.md) is the argument.

Neither exists. `unsafe` is an ordinary lower identifier, so `unsafe idiv : Int -> Int -> Int`
reaches `FunType` as two names where it expects one and fails to parse. And nothing looks at an
unmarked facade's result type at all: the facade branch of `canonicalize` checks that there are
no infix declarations, no type declarations, and that every value carries an annotation and no
bindings, then resolves the annotation with `Type::from_parser_type`.

Every facade in the tree is therefore an unmarked one asserting purity by saying nothing, which
is the shape the chapter now rejects.

**Approach:**

1. **Decide how `unsafe` is tokenized, and this ticket does not pick.**
   [Lexical structure](../spec/lexical-structure.md#reserved-words) lists it as a **soft**
   keyword — a keyword before a name in a facade signature, an ordinary identifier everywhere
   else, so `unsafe : Int` stays a facade constant of that name and `unsafe f : Int` is the
   modifier. Two ways to get there:
   - **Reserve it outright**, the way `javascript` is really handled today (`tokenizer.rs`
     returns `Token::Javascript` unconditionally, and `lexical-structure.md` carries a
     **Known gap:** saying so). Half a day, and it takes a plausible identifier out of the
     language — and adds a second word to the inconsistency [`LANG-2`](lang-2.md) exists to
     undo, rather than clearing it. The `expect=ok` block in that chapter listing the soft
     keywords as ordinary names goes red if this route is taken.
   - **Make it genuinely soft**, which needs the tokenizer to keep emitting
     `LowerIdentifier("unsafe")` and the grammar to distinguish the two readings on the token
     *after* it. `derived` has the same shape and is also unimplemented, so there is no prior art
     in the tree to copy; whichever of the two lands first should be written so the other reuses
     it.
   Say which was taken and why in the PR.
2. **Carry the flag.** `FunType` gains it in the grammar production and in
   `parser::FunType`; `canonical`'s facade branch reads it. Per *A grammar change is never a
   one-file change*, `grammar.lalrpop`, `parser/mod.rs` and the `from_parser*` conversions in
   `canonical/mod.rs` move in one commit. A marked signature outside a `module javascript` header
   is an error — `unsafe` means nothing on an ordinary declaration, and silently ignoring it
   would be the worst of the three options.
3. **Rewrite `std/core`'s facades.** All 45 existing signatures are asserted pure functions and
   every one gains the word: 30 in `Js/Basics.zel`, 8 in `Js/Utils.zel`, 7 in `Js/Bitwise.zel`.
   `cargo run` must still print `parsed 8 modules` and list all eight afterwards.
4. **The unmarked-facade check is [`LANG-43`](lang-43.md)'s, not this ticket's**, and it cannot be
   tested before [`LANG-9`](lang-9.md) lands: `Task (Result Failure a)` needs a parenthesised type
   argument, which does not parse. This ticket can land complete without either — marking a
   facade `unsafe` is independent of checking what an unmarked one declares — and doing so is
   what makes `std/core` legal under the new rule before the rule is enforced.

**Tests:** `tests/compiler/canonical.rs` for the flag surviving to `canonical::Module`, and a
parser test for each of the three readings — `unsafe f : Int -> Int` as a marked signature,
`unsafe : Int` as a constant named `unsafe` (whichever way step 1 goes, the test records it), and
`unsafe f : Int` outside a facade as an error. Neutralise each before trusting it
(`CLAUDE.md`'s *A green test proves nothing until you have seen it fail*).

**Interactions:**

- **[`LANG-43`](lang-43.md)** owns the check that an unmarked facade declares
  `Task (Result Failure a)`, and its admitted-types walk has to skip that shape rather than
  descend into it. It also stops needing to reject a bare type variable in `Js/Utils`' eight
  signatures *as facades* — they stay inadmissible, but for the reason `BUG-20` gives.
- **[`LANG-9`](lang-9.md)**, as step 4.
- **[`GEN-1`](gen-1.md)** emits a wrapper for an unmarked facade and none for an `unsafe` one;
  **[`GEN-2`](gen-2.md)** emits the predicate for both, since `unsafe` removes the `Task` and not
  the boundary check.

**Found:** while settling what a broken companion does (`SPEC-15`, continued), which inverted the
facade default and needed a word for the exception. Specified but not implemented, because
[a spec change and a semantics change do not share a
diff](../spec/conventions.md#a-spec-change-and-a-semantics-change-do-not-share-a-diff).

**Acceptance:** `unsafe fdiv : Float -> Float -> Float` parses inside a `module javascript`
header and is rejected outside one, and the flag is readable on the canonical declaration. All 45
`std/core` facade signatures carry the word and `cargo run` prints `parsed 8 modules` and lists
all eight. Four `expect=unimplemented` blocks go red and are retagged `expect=ok` in the same diff, once
[`LANG-54`](lang-54.md) has landed — each names the `foreign` modifier and fails on that first.
They are the `Core.Prim` and `Core.Basics` blocks in [`interop.md`](../spec/interop.md) and
`unsafe square` / `unsafe next` in
[`evaluation-semantics.md`](../spec/evaluation-semantics.md) — and their **Not implemented:**
paragraphs lose the clause naming this ticket. The three blocks that stay red are `LANG-9`'s.
`cargo test --test spec` green.
