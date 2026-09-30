# TOOL-2 · A source file can only be read from disk, so nothing can check an unsaved buffer

**Sizing:** medium. The read is one line. The size is in threading a source provider from
`compile` down to `SourceFile::load`, and in deciding what else goes through it. It grows if
the manifest and companion reads go through it too (see *Approach*).

**Part of:** the *Active work: editor support* section of [the index](README.md).
[`TOOL-6`](tool-6.md) depends on it.

**Location:** `src/compiler/source/files.rs` — `SourceFile::load_private`'s
`std::fs::read_to_string(abs_path)`; `src/compiler/source/mod.rs` —
`load_package_sources_into`, whose `WalkDir` loop decides which files exist;
`src/compiler/mod.rs` — `compile` and `compile_in_build`, the callers;
`src/compiler/manifest.rs` — `load`, which reads `zelkova.toml` the same way.

**Problem:** a module's text comes from exactly one place, the file on disk, read inside
`SourceFile::load_private`. A language server checks what the editor holds, which differs from
the disk on every keystroke until the user saves. Today it could only check the saved state,
which makes diagnostics lag behind the text they describe. The same wall stops a test or a
future REPL from compiling a package that exists only in memory. `tests/support/mod.rs`'s
`parse_source` and `canonicalize_standalone` work around it by skipping `compile` altogether,
and a test that needs a whole build has to put a package on disk first.

**Approach:**

1. Introduce a source provider, a trait or a concrete overlay, that answers "what is the text
   of this path" and "which `.zel` files are under this root". The filesystem answers both by
   default. An overlay answers from a map of open buffers first and falls back to the
   filesystem.
2. Thread it through `compile` into `load_package_sources_into` and `SourceFile::load`. The
   walk has to consult it too: a new, unsaved module exists in the editor and not on disk, and
   a deleted one the reverse.
3. The public entry points (`compile_package`, `compile_package_with_tests` and their `_into`
   variants) keep their signatures and pass the filesystem provider, so `src/main.rs` and
   every existing test are untouched.

Not decided here:

- **Trait or overlay.** A `trait SourceProvider` is the general answer and costs a generic or
  a `dyn` on every loading function. A concrete `Overlay { buffers: HashMap<PathBuf, String> }`
  that `load_private` consults before the disk is smaller, and covers the language server and
  tests. Which one depends on whether anything but an overlay is ever wanted.
- **How far it reaches.** `manifest::load` also reads from disk, and an edited `zelkova.toml`
  is a buffer too. A facade's companion `.mjs` is read in `output.rs` at write time, which a
  check that writes nothing ([`TOOL-3`](tool-3.md)) never reaches. The minimum is `.zel`
  sources; whether manifests go through the provider in the same change is this ticket's call.

**Acceptance:**

- A test, in `tests/pipeline.rs` or beside the provider as a unit test, compiles a fixture
  package with one module overridden in the overlay by text that differs from its file on disk.
  The diagnostics (or their absence) are the overlay's, not the disk's. It is mutation-checked
  by making the overlay lookup always miss and confirming the test fails.
- A second test adds a module that exists only in the overlay, imported by one on disk, and it
  resolves.
- `cargo test --workspace` is green, and `cargo run -- compile std/core` still prints
  `parsed 10 modules`, lists all ten as checked, and exits 0.
