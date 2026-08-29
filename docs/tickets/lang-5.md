# LANG-5 · An `import` is accepted anywhere among the declarations

**Sizing:** small. One grammar production and the `Module::from_declarations` that consumes
it; could grow to small-to-medium if the error wanted is better than LALRPOP's default.

**Location:** `src/compiler/parser/grammar.lalrpop` — the `Decl` production, which lists
`Import` alongside `FunBinding`, `FunType`, `Infix` and `Union`, and the `Module` production
that takes `<declarations:Decl*>`.

**Decided (`SPEC-3`, by the language owner):** every `import` in a file sits between the
module header and the first other declaration. An `import` written after a value, type or
`infix` declaration is a syntax error.

The reason is that what a module depends on is a property of the module, not of the point
in the file where somebody happened to need it. Keeping the list in one place at the top is
what lets a reader answer "what does this module use" without reading the file, and lets a
tool answer it without parsing past the header.

**Problem:** the grammar treats `import` as an ordinary top-level declaration, so it may
appear at any point and in any number among the others. `Module::from_declarations` then
sorts the declarations into `imports`, `infixes`, `types` and `functions` by kind, discarding
the order they were written in — so nothing downstream can tell, or complain:

```zel
module Main exposing (x)

x = 1

import Widget      -- accepted today
```

**Approach:** split the `Module` production so imports are their own repetition ahead of the
rest — `"module" … <imports:ImportDecl*> <declarations:Decl*>` — with `Import` removed from
`Decl`. `Module::from_declarations` then takes the imports separately and its
`Declaration::Import(i) => imports.push(i)` arm goes away, along with `Declaration::Import`
itself if nothing else uses it.

Check what error a misplaced `import` then produces before calling it done. LALRPOP will
report an `UnexpectedToken` naming `import` against an "expected" list of everything a
declaration can start with, which is true but not helpful; if a better message is cheap,
write it, and if it is not, say so in the PR rather than pinning a poor one. Note this is a
grammar change and `CLAUDE.md`'s *A grammar change is never a one-file change* applies: the
parser AST and `canonical/mod.rs`'s conversions move in the same commit.

**Acceptance:** `module Main exposing (x)` followed by `x = 1` and then `import Widget` is a
parse error; the same file with the `import` above `x = 1` compiles. The `**Known gap:**`
block in [`docs/spec/modules.md`](../spec/modules.md) — the second block of the
`package=position` group, tagged `expect=ok` — goes red, and is retagged
`expect=parse-error:…` with its paragraph rewritten in the same change. `cargo run` still
prints `parsed 8 modules` and lists all eight as checked; every `std/core/src/*.zel` already
writes its imports at the top, so this must not move.
