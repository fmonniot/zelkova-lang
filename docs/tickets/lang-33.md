# LANG-33 · There is no `let … in` production, so a local binding cannot be written

**Sizing:** medium. The layout pass already carries the block; the grammar, both ASTs and the
typer do not, and the typer's `Term` language has to grow a `let` before generalisation can
mean anything.

**Location:** `src/compiler/parser/grammar.lalrpop` — the `extern` token block, which maps no
terminal to `Token::Let` or `Token::In`, and `Expr`, which has no alternative for either.
`src/compiler/parser/tokenizer.rs` — `Token::Let` and `Token::In`, both produced already.
`src/compiler/parser/layout.rs` — `Context::Let`, pushed on `Token::Let` and popped on
`Token::In`, already written. `src/compiler/parser/mod.rs` and
`src/compiler/canonical/mod.rs` — `ExpressionKind` in each, the canonical one carrying
`// Let`, `// LetRec` and `// LetDestruct` as placeholder comments.

**Decided ([`docs/spec/expressions.md`](../spec/expressions.md), *`let … in`*;
[`docs/spec/name-resolution.md`](../spec/name-resolution.md), *Scopes*):** `let bindings in
expression` introduces names visible in the bindings themselves and in the expression after
`in`, and the value of the whole is the value of that expression. A binding takes any form a
module's declarations take — a type annotation, a value binding, a function binding with
parameters, and an irrefutable destructuring pattern. The bindings of one `let` are mutually
recursive, so their order does not matter. A `let` binds nothing outside itself, and it is one
of the two binding positions the language has that is neither a declaration's parameters nor a
`case` branch's pattern.

**Problem:** `let` and `in` tokenize and lay out, and then stop. The tokenizer produces
`Token::Let` and `Token::In`; `layout.rs` pushes a `Context::Let` on the first and pops it on
the second, rendering it as ``a `let` block`` in its errors. The grammar's `extern` block never
names either token, so no production can consume one and the parse fails at the `let` itself:

```zel
f n =
  let
    x = n
  in
  x
```

reports *UnexpectedToken `Let`, expected `lo_ident`, `up_ident`, `integer`, `float`, `char`,
`true`, `false`, `(`, `-`, `case`, `if`, `left`, `right`, `non`*. The single-line spelling
`let x = n in x` fails identically and at the same token.

Neither AST can hold the construct either: `parser::ExpressionKind` has no variant, and
`canonical::ExpressionKind` marks the hole with three comments rather than variants.

Found while writing [`docs/spec/name-resolution.md`](../spec/name-resolution.md) (`SPEC-8`),
whose *Scopes* section names `let … in` as a binding position the language has and the
compiler does not.

**Approach:** four things move, and the ticket picks none of them:

1. **The layout questions come first.** [`docs/spec/layout.md`](../spec/layout.md)'s
   *`let … in`* section leaves two open — whether the bindings form a block with the same
   column discipline as `case … of` branches, and whether `in` must align with its `let`. The
   existing `Context::Let` answers neither, and a grammar written against the wrong answer is
   the expensive mistake here.
2. **How a binding is represented.** A `let` binding is a declaration in everything but
   position, so reusing the declaration productions is the obvious route and drags in
   annotations, clauses and destructuring patterns with it. The alternative — a narrower
   binding form that only grows later — is smaller now and diverges from the chapter, which
   says a binding takes any form a declaration takes.
3. **Mutual recursion.** Every binding is in scope in every other's body, so canonicalization
   cannot resolve a `let` in written order. It needs the same two-pass shape the module's top
   level already uses: collect the bound names, then walk the bodies.
4. **The typer.** `constraint.rs` has no `let`, so nothing generalises a local binding. Doing
   it wrong is the classic way to make a polymorphic local monomorphic without anyone noticing.

[LANG-34](lang-34.md) is the sibling gap — the other expression form that introduces a
scope — and the two are independent.

**Acceptance:** the `let … in` example in
[`docs/spec/expressions.md`](../spec/expressions.md) and the one in
[`docs/spec/layout.md`](../spec/layout.md)'s *`let … in`* section, both tagged
`expect=unimplemented`, go red and are retagged `expect=ok` with their **Not implemented:**
paragraphs deleted — layout.md's section says in as many words that the red block is the
signal to come back and finish it, and the two open layout questions are answered there in the
same change. The **Not implemented:** paragraph in
[`docs/spec/name-resolution.md`](../spec/name-resolution.md)'s *Scopes* section goes with them,
and that chapter gains a block showing a `let` binding shadowing a top-level name. A
`tests/typer.rs` test pins that a `let`-bound identity function used at two types checks, which
is the generalisation question in point 4. `cargo run` still prints `parsed 8 modules` and
lists all eight as checked.
