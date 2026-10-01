# TOOL-2 · A source file can only be read from disk, so nothing can check an unsaved buffer

**Sizing:** small to medium. The read is one line and the overlay is one small type. The size
is in threading it from the checking entry point down to `SourceFile::load`, and in the walk,
which has to learn about a module that is in a buffer and not on disk. No decision is left to
make.

**Part of:** the *Active work: editor support* section of [the index](README.md).
[`TOOL-6`](tool-6.md) depends on it.

**Depends on:** [`TOOL-3`](README.md), which adds the entry point that checks a package and
writes nothing. The overlay is a parameter of that entry point and of nothing that writes a
build.

**Location:** `src/compiler/source/files.rs` — `SourceFile::load` and `load_private`, whose
`std::fs::read_to_string(abs_path)` is the read; `src/compiler/source/mod.rs` —
`load_package_sources_into`, whose `WalkDir` loop and `tests/` existence check decide which
files exist; `src/compiler/mod.rs` — the checking half of `compile` as `TOOL-3` leaves it, and
`compile_in_build`, the two callers of `load_package_sources_into`; `tests/pipeline.rs` — the
two calls of `load_package_sources_into` in it. New: `src/compiler/source/overlay.rs`.

**Problem:** a module's text comes from exactly one place, the file on disk, read inside
`SourceFile::load_private`. A language server checks what the editor holds, which differs from
the disk on every keystroke until the user saves. Today it could only check the saved state,
which makes diagnostics lag behind the text they describe.

**Approach:** every choice below is made.

1. **`Overlay`, a concrete type in `src/compiler/source/overlay.rs`**, re-exported from
   `source` beside `SourceFiles`. It holds a `HashMap<PathBuf, String>` of open buffers, private
   to the type, and offers `new` (also its `Default`, an empty overlay), `insert(path, text)`
   and `remove(path)`. It is not a trait: an overlay is the only source of text other than the
   disk that anything wants, and the functions that would take a `&dyn` take an `&Overlay`
   in the same positions, so turning it into one later changes a type and no call graph.

2. **One function normalises a path, and both sides of every comparison go through it.** It
   canonicalizes the deepest ancestor of the path that exists on disk and appends the rest
   unchanged, so a path that is not on disk yet still normalises. `insert` and `remove` apply
   it to their key, and a lookup applies it to the path being looked up. This is needed because
   the two sides are spelled differently: `resolve::resolve` canonicalizes every package root,
   so the walk yields paths under the canonical root, while a caller's path is whatever its
   editor reported (`/tmp/…` against `/private/tmp/…` on macOS). The walk follows symbolic
   links, so a file reached through one is matched by where it points. Normalisation is for
   the lookup only: a module is still named from the path the walk yielded.

3. **`SourceFile::load` and `load_package_sources_into` each gain an `&Overlay` parameter.**
   `load_private` takes the buffer's text when the overlay holds the path and reads the disk
   otherwise. `load_package_sources` keeps its signature and passes an empty overlay.

4. **The walk adds what only the overlay holds.** `load_package_sources_into` walks the disk
   exactly as it does today, then loads every overlay key that ends in `.zel`, sits under the
   normalised form of `root_dir` and was not matched by a walked file, in path order. Such a
   file is named by its path under the normalised `root_dir`. The `tests/` check that returns
   early when the directory is missing skips the walk and no longer returns, so a buffer under
   a `tests/` that is not on disk is still loaded. A `src/` that is not on disk is the error
   it is today whatever the overlay holds.

5. **The overlay only replaces and adds.** It has no way to say a file on disk is gone. An
   editor that deletes a module deletes the file, and the next walk does not find it.

6. **The checking entry point `TOOL-3` adds gains an `&Overlay` parameter**, passed down
   through the checking half of `compile` and `compile_in_build` to the two
   `load_package_sources_into` calls. The CLI half passes an empty one. `compile_package`,
   `compile_package_with_tests` and their `_into` variants keep their signatures, so
   `src/main.rs` and `tests/cli.rs` are untouched and no build is ever written from the text
   of a buffer.

7. **`Overlay`'s doc comment states rules 2, 4 and 5 and the two limits below**, so that they
   outlive this file.

Two reads stay on disk, deliberately:

- **The manifest.** `manifest::load` and `resolve.rs` are untouched, and an unsaved
  `zelkova.toml` is not checked until it is saved. A manifest being typed is malformed TOML
  most of the time, and a manifest error returns before any source is loaded, so reading it
  from a buffer would clear every `.zel` diagnostic of the package on each keystroke in the
  manifest. An overlay key that does not end in `.zel` is ignored, so a caller may hand over
  every open buffer without filtering.
- **A facade's companion `.mjs`.** Its text is read only by a build that writes, and no such
  build takes an overlay. `to_modules_to_emit` asks the disk whether one exists, and keeps
  asking the disk.

A package therefore has to exist on disk, with its `zelkova.toml` and its `src/`, for the
overlay to apply to it. Compiling a package that exists only in memory — a REPL, a test with
no fixture — is out of scope, and is a new ticket if something comes to need it.

**Acceptance:**

- A test in `tests/pipeline.rs` runs `TOOL-3`'s entry point on a fixture package that checks
  clean on disk, with one module overridden in the overlay by text holding a type error. The
  error comes back, and the source its label's file id resolves to in the returned
  `SourceFiles` is the overlay's text. It is mutation-checked by making the overlay lookup
  always miss and confirming the test fails.
- A second test adds a module that exists only in the overlay, imported by a module on disk
  that does not check without it, and the package checks. It is mutation-checked by removing
  step 4's addition to the walk.
- A third inserts the overridden module under a spelling of its path that differs from the
  canonical one, a `..` segment through a sibling directory, and gets the first test's result.
  It is mutation-checked by making the normalising function return its argument unchanged.
- `tests/cli.rs` is green unchanged.
- `cargo test --workspace` is green, and `cargo run -- compile std/core` still prints
  `parsed 10 modules`, lists all ten as checked, and exits 0.
