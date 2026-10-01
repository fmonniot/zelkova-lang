# TOOL-4 · A module's first syntax error is the only one reported

**Sizing:** medium. The cut is one small iterator and the grammar gains two entry points with
no change to any AST. The size is in keeping the first error of every file what it is today,
which the existing parser tests and the spec's `expect=parse-error` blocks hold it to. No
decision is left to make.

**Part of:** the *Active work: editor support* section of [the index](README.md).
[`TOOL-8`](tool-8.md) depends on it: this ticket stops at the parser, and what the rest of the
pipeline does with a module that is only partly there is that ticket's.

**Location:** `src/compiler/parser/mod.rs` — `parse` and `parse_unexplained`;
`src/compiler/parser/grammar.lalrpop` — `Module` and `Decl`, the only public entry point and
the production it repeats; `src/compiler/parser/layout.rs` — `Layout::next_token`, which
places the end of its input at `Position::new(0, 1, 1)`, `Layout::explain`, and
`can_start_declaration`; `src/compiler/mod.rs` — `parse_root`. New:
`src/compiler/parser/chunk.rs`.

**Problem:** the first tokenizer, layout or grammar error in a module ends its parse, so a
file with three typos needs three compile-fix rounds. Nothing of the module survives either:
`parse` returns `Result<Module, Error>`, and the declarations that were well formed are
discarded with the one that was not.

A second defect sits on the path this ticket changes. A declaration left unfinished at the end
of a file is reported at the first byte of the file:

```
error: unexpected token: `CloseBlock`
  ┌─ acme-eof:src/Main.zel:1:1
  │
1 │ module Main exposing (..)
  │ ^ unexpected token
```

for a `Main.zel` whose last line is `f x =`. The layout pass closes the open blocks at the
position it invents for the end of its input, which is byte 0.

**Approach:** every choice below is made. The recovery is at the level of a declaration, by
cutting the token stream before the layout pass sees it. LALRPOP's `!` error recovery is not
used: it recovers grammar errors only, and it needs an error variant in the parser AST that
canonicalization would then have to carry.

1. **The cut is made on raw tokens, in `src/compiler/parser/chunk.rs`.** An iterator adaptor
   takes the tokenizer's output and yields chunks, each the tokens of one top-level
   declaration. A token starts a new chunk when it sits in column 1, is not the first token of
   the file, and `can_start_declaration` accepts it; that function moves out of `layout.rs` to
   be shared. The first chunk is the module header. This is
   [*Top-level declarations*](../spec/layout.md#top-level-declarations) read literally, and
   because it happens before layout, nothing a declaration contains can move the boundary of
   the next one.

2. **A column-1 token no declaration starts with does not cut.** `1`, `|`, `=` or `)` in
   column 1 stays in its chunk, where the layout pass closes the declaration at it exactly as
   it does today, and the mistake costs one error. A lowercase name in column 1 where a
   continuation was meant does cut, and is reported twice: once as a declaration whose body
   never arrived and once as a declaration with no `=`. The two cannot be told apart lexically.

3. **A tokenizer error belongs to the chunk it falls in and never starts one.** The tokenizer
   already carries on after an error, so the declarations after it are still cut and parsed. A
   tab at the start of a continuation line therefore fails the declaration it continues and
   nothing else.

4. **The adaptor enforces "advance or stop" on the tokenizer.** It is the first production
   consumer to read the tokenizer past an error, which is the operation `BUG-4` and `BUG-5`
   were about. It ends the stream at the second of two consecutive equal errors, so a tokenizer
   path that repeats one error truncates the parse where it would otherwise never end
   (`CLAUDE.md` — *A `Result`-yielding iterator must advance or stop*).

5. **Each chunk gets its own `Layout` and its own grammar entry point.** `grammar.lalrpop`
   gains `pub Header`, the `module … exposing (…)` line between its block tokens, and `Decl`
   becomes `pub`. The `Module` production is deleted, and `Module::from_declarations` is called
   from `parser/mod.rs` with the header and the declarations that parsed. No AST type changes,
   so `canonical/mod.rs` is untouched. `Layout` keeps its stop-at-first-`Err` contract
   unchanged: a chunk reports the first error its tokenizer, layout or grammar raises and
   nothing after it, which is today's rule for a module applied to a declaration.

6. **A `Layout` is told where its input ends**, and closes its open blocks there instead of at
   `Position::new(0, 1, 1)`. For every chunk but the last that is the start of the next chunk's
   first token, which is where the closing block token sits today. For the last it is the end
   of the source, which moves the caret in *Problem*'s example from 1:1 to the end of the file.

7. **`Layout::explain`'s `complete_before` asks about the chunk.** It answers whether the
   chunk's tokens before the given position parse on their own with the chunk's entry point.
   `parse_unexplained` goes; nothing parses a prefix of the source text any more.

8. **`parse_recovering` is the new entry point, and `parse` keeps its signature.**
   `parse_recovering` returns a `Parsed`: `module: Option<Module>`, holding every declaration
   that parsed, and `failures: Vec<Failure>` in source order, each a chunk's span and its
   `Error`. `module` is `None` exactly when the header is among the failures; the other chunks
   are still parsed, for their errors. What a failed declaration would have been called is not
   recovered. `parse` becomes `parse_recovering` followed by: no failures is `Ok(module)`,
   otherwise `Err` of the first failure's error. Every existing caller of `parse`, `tests/spec.rs`
   included, therefore keeps meaning "the first error", and `expect=parse-error:Reason` keeps
   naming it.

9. **`parse_root` calls `parse_recovering` and pushes every failure's error**, each wrapped
   with its file as today. A file with any failure still counts once in `failures` and still
   contributes no module, so the status line, the `private-modules` check and everything after
   parsing see what they see today. The `Module` of a file with failures is dropped here;
   [`TOOL-8`](tool-8.md) is what puts it to use.

One reported error changes besides the caret of step 6. A declaration that can start in
column 1 arriving while a `case` scrutinee is still open:

```zel
f x =
  case x

g = 1
```

is today a layout error on `g` (*this line is not indented far enough*), raised by the token
that belongs to the next declaration. After step 1 the first chunk ends before `g`, its blocks
close, and the grammar reports the missing `of` at that position; `g` parses. No block in
`docs/spec/` pins the old error. A test under `tests/` that does is updated, and says why.

**Acceptance:**

- A test in `tests/compiler/parser/` runs `parse_recovering` on a module with a syntax error
  in each of two top-level declarations, a well-formed declaration between them and one after.
  It gets two failures in source order, each `Error`'s position inside its own declaration, and
  a `Module` holding the two well-formed declarations. It is mutation-checked by making step
  1's cut never fire and confirming the test fails.
- A second test parses the `case x` example above and gets one failure and a `Module` holding
  `g`. Same mutation.
- A third has an odd indentation in one declaration and a grammar error in a later one, and
  gets both, the first an `Error::Tokenizer`.
- A fourth parses a module whose last line is `f x =` and asserts the failure's position is the
  end of the source. A fifth leaves the same declaration unfinished in the middle of a file and
  asserts the position is the first byte of the next declaration. Both are mutation-checked by
  restoring `Position::new(0, 1, 1)` in `Layout::next_token`.
- A unit test in `chunk.rs` hands the adaptor a source that yields one token and then the same
  `Err` without end, polls it a bounded number of times, and asserts it stopped. It is
  mutation-checked by removing step 4's check.
- A test in `tests/pipeline.rs` compiles a package whose one module has two syntax errors and
  gets two errors back, each with a label in that file.
- `tests/compiler/parser/` is otherwise green unchanged, apart from a test pinning one of the
  two changed reports named above. `cargo test --test spec` is green with no chapter edited.
- `cargo test --workspace` is green, and `cargo run -- compile std/core` still prints
  `parsed 10 modules`, lists all ten as checked, and exits 0.
