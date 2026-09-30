# TOOL-6 · There is no language server

**Sizing:** large, and meant to be cut. Diagnostics alone are medium once [`TOOL-2`](tool-2.md)
and [`TOOL-3`](tool-3.md) have landed. Hover and go-to-definition each need a position-to-node
lookup the compiler does not have. Whoever picks this up should split it into one ticket per
capability before starting, not land it as one PR.

**Part of:** the *Active work: editor support* section of [the index](README.md).
**Depends on:** [`TOOL-2`](tool-2.md) (checking an unsaved buffer) and [`TOOL-3`](tool-3.md)
(diagnostics as data, nothing printed or written). [`TOOL-4`](tool-4.md) is not a prerequisite,
but without it every capability except diagnostics goes dark for a file that does not parse,
which is most files mid-edit. [`TOOL-5`](tool-5.md) is not a prerequisite either.

**Location:** new, as a workspace member (`zelkova-lsp`) depending on the compiler library.
What it reads: `src/compiler/mod.rs` — `CompilationError::as_diagnostic`, `SourceFiles`,
`CheckedModule`; `src/compiler/ir/` — the typed tree, where every node carries a type;
`src/compiler/position.rs` — `NodeSpan`, `BytePos`.

**Problem:** nothing speaks the Language Server Protocol, so editor feedback on a `.zel` file
is limited to what [`TOOL-1`](tool-1.md)'s grammar can do lexically. Errors are seen only by
running `zelkova compile`, types only by reading annotations, and definitions only by
searching.

**Approach**, in the order the capabilities should land:

1. **Diagnostics.** On open, change and save, check the package owning the file, with the
   editor's buffers as [`TOOL-2`](tool-2.md)'s overlay. Publish each `CompilationError`'s
   `Diagnostic`, converting its labels' byte ranges into LSP positions. Primary and secondary
   labels become the diagnostic's range and its `relatedInformation`, and `notes()` go in the
   message.
2. **Hover** shows the type of the name under the cursor, from the `ir::Module`, whose every
   node carries a type.
3. **Go-to-definition** for a value, a type or a constructor, local or imported. A
   `QualName` names its module, and an `Interface` carries the declaration's span and
   `SourceFileId`.
4. **Semantic tokens**, which let highlighting tell a constructor from a type from a value
   where [`TOOL-1`](tool-1.md)'s grammar can only go by case.

The editor side is [`TOOL-1`](tool-1.md)'s VS Code extension gaining a client that launches
the server binary.

Choices this ticket does not make:

- **Library.** `tower-lsp` (async, `tokio`) or `lsp-server` plus `lsp-types` (synchronous,
  what rust-analyzer uses). The compiler is synchronous, so `lsp-server` fits without an async
  runtime.
- **Position encoding.** LSP defaults to UTF-16 code units. Protocol 3.17 lets the server
  negotiate UTF-8, which matches `BytePos` directly, but not every client offers it. The
  tokenizer is Unicode-aware, so a UTF-16 fallback is needed and a test with a non-ASCII
  identifier before the cursor is what proves it.
- **How much to re-check.** Re-checking the whole package on every change is correct and, at
  `std/core`'s size, fast enough to start with. Incremental checking (e.g. `salsa`) is a
  separate, much larger decision about the compiler's architecture and belongs in its own
  ticket if the whole-package check proves too slow.
- **Position-to-node lookup** for hover and go-to-definition. It is either a walk of the
  `ir::Module` for the innermost span containing an offset, or an index built once per check.
  `NodeSpan`'s `PartialEq` always returns `true` (its doc comment), so the lookup compares
  spans' fields, never whole spans.

**Acceptance** (for the diagnostics cut, which is the first PR; each later capability states
its own when it is split off):

- `cargo run -p zelkova-lsp` starts a server over stdio that answers `initialize`.
- An integration test drives the server with `initialize`, `didOpen` of a module containing a
  type error, and asserts one `publishDiagnostics` whose range is the error's primary label in
  line and character terms. A second `didChange` that fixes the error clears it. The test is
  mutation-checked by publishing an empty diagnostic list unconditionally.
- The same test with a non-ASCII identifier earlier on the error's line places the range
  correctly under whichever position encoding was negotiated.
- Opening `std/core` in VS Code with the extension shows no diagnostics, and introducing a
  type error in `std/core/src/Maybe.zel` shows it underlined before the file is saved.
