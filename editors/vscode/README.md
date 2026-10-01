# Zelkova for VS Code

Syntax highlighting for `.zel` files: a TextMate grammar and the smallest extension around it.
Highlighting is lexical — the grammar reads one line at a time and never runs the compiler.

## Install

From the folder: copy or symlink `editors/vscode` to `~/.vscode/extensions/zelkova-0.0.1`
and reload VS Code.

From a `.vsix`: `npx @vscode/vsce package` in this folder, then
`code --install-extension zelkova-0.0.1.vsix`.

## Check the grammar

```sh
cd editors/vscode
npm ci
npm test
```

`tests/*.zel` are scope assertions in the format `vscode-tmgrammar-test` reads; CI runs them in
the `javascript` job. `cargo test --test editor_grammar` checks the grammar's keyword list
against the spec's *Reserved words* block in `docs/spec/lexical-structure.md`.

## What the grammar approximates

- **Soft keywords** are positional. `left`/`right`/`non` after `infix`, `foreign` in
  `module foreign`, `unsafe` before a signature and `derived` alone on an indented line (or
  followed by one member name) are keywords; anywhere else they are ordinary identifiers. A
  line-at-a-time grammar cannot see a class or instance body, so `derived` is matched by shape.
- **`class`, `instance` and `where`** are highlighted as keywords although the compiler does
  not reserve them until `LANG-38`. `where` is reserved only as a type variable, so the
  grammar over-highlights it where a value is named.
- **`true` and `false`** are ordinary identifiers, as the spec has it (`LANG-1`).
