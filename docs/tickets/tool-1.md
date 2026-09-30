# TOOL-1 · No editor highlights a `.zel` file

**Sizing:** small-to-medium. A grammar file, an extension manifest, and a fixture the grammar
is checked against. It grows if the soft keywords are to be highlighted only in their keyword
position (see *Approach*), because a TextMate grammar sees one line at a time and has no parse
to lean on.

**Part of:** the *Active work: editor support* section of [the index](README.md). Depends on
nothing, and nothing depends on it: it is the one piece of that effort that ships value alone.

**Location:** new, under `editors/vscode/` — no editor support exists anywhere in the tree
today. What it has to agree with: `docs/spec/lexical-structure.md` — *Reserved words*,
*Comments*, *Literals*, *Operators*, *Punctuation* — and `src/compiler/parser/tokenizer.rs` —
`keyword`, `is_operator_char`.

**Problem:** a `.zel` file opens as plain text in every editor. `std/core/src/` alone is ten
modules, and reading them without highlighting is the itch that started this effort. None of
it needs the compiler: coloring is a lexical job, and editors do it from a declarative grammar
rather than a language server.

**Approach:**

1. Write a TextMate grammar, `editors/vscode/syntaxes/zelkova.tmLanguage.json`, scoped
   `source.zelkova`, covering what the spec's *Lexical structure* chapter defines: the thirteen
   reserved words, `--` line comments and `{- -}` block comments (which nest — a `begin`/`end`
   rule that includes itself), `Int`, `Float`, `Char` and single-line `String` literals with
   their escapes, operators, and upper- versus lower-case identifiers (types and constructors
   versus values). Where the spec and `tokenizer.rs` disagree, **the spec wins**:
   `true`/`false` are not reserved words ([`LANG-1`](lang-1.md) tracks the tokenizer still
   reserving them), so they are not highlighted as keywords.
2. Write the smallest VS Code extension around it, `editors/vscode/package.json` plus a
   `language-configuration.json` for comment toggling, bracket pairs and auto-closing quotes.
   It is installed from the folder or a locally built `.vsix`. Publishing to a marketplace is
   out of scope.
3. Add a scope-assertion fixture, e.g. `editors/vscode/tests/*.zel` in the format
   [`vscode-tmgrammar-test`](https://github.com/PanAeon/vscode-tmgrammar-test) reads, and a CI
   step that runs it. The `javascript` job in `.github/workflows/rust.yml` already has `node`.

Two choices this ticket leaves open:

- **Soft keywords.** `left`/`right`/`non`, `foreign`, `unsafe` and `derived` are keywords only
  in one position. A rule matching `infix (left|right|non)`, `module foreign`, and `unsafe`
  before a signature is cheap and mostly right. Highlighting them everywhere is simpler and
  sometimes wrong. Not highlighting them at all is always "right", but less useful.
- **Words that are not reserved yet.** `class`, `instance` and `where` become reserved when
  [`LANG-38`](lang-38.md) lands (`docs/spec/type-classes.md`). Whether to highlight them ahead
  of the compiler is a choice about what the grammar is for.

**Drift:** a keyword list copied into JSON is a second copy of the spec's. A Rust test that
reads the grammar and checks it names exactly the words the spec's *Reserved words* block
lists would keep them together. Whether it is worth the harness is part of this ticket's
review.

**Acceptance:**

- `npx vscode-tmgrammar-test 'editors/vscode/tests/**/*.zel'` (or the runner chosen instead)
  passes, and runs in CI. It is mutation-checked by deleting the nesting `include` from the
  block-comment rule and confirming that a nested-comment assertion fails.
- Opening `std/core/src/Basics.zel` and `std/core/src/Js/Basics.zel` in VS Code with the
  extension loaded shows keywords, comments, literals, types and operators each in their own
  scope. `Developer: Inspect Editor Tokens and Scopes` confirms it on one token of each.
