# TOOL-3 · Checking a package always prints to stderr and writes JavaScript

**Sizing:** small-to-medium. `compile` already separates checking from writing and rendering
with `errors.is_empty()` gates. The work is lifting the checking half into an entry point of
its own without leaving two copies of the pipeline.

**Part of:** the *Active work: editor support* section of [the index](README.md).
[`TOOL-2`](tool-2.md), [`TOOL-6`](tool-6.md) and [`TOOL-7`](tool-7.md) depend on it.

**Location:** `src/compiler/mod.rs` — `compile`: its `print_status` closure and
`StandardStream::stderr` writer, the `output::write` calls under `debug!("phase: write the
build")` and `"phase: write the test build"`, and the final loop that
`term::emit_to_write_style`s every accumulated error; `print_parse_status` and `check_root`,
which print through `print_status`.

**Problem:** the only public way to check a package is `compile_package` and its three
siblings. Each one does three things a caller other than the CLI does not want:

- it prints `success`/`failure` status lines to stderr as it goes;
- it renders every diagnostic to stderr with `codespan_reporting` before returning;
- on success it writes `build/out/js/` (and `build/test/js/` for a test build) beside the
  manifest.

A language server needs the diagnostics as data: `CompilationError`s it can turn into its own
protocol's shape, with nothing printed, since its stdout and stderr are the protocol channel or
its log, and nothing written, since checking on every keystroke must not rewrite the build
tree. `CompilationError::Many` does carry the errors back today, but only after they have been
rendered to stderr, and only alongside a disk write on success.

**Approach:** split `compile` into a checking half that returns what it found (the accumulated
`CompilationError`s, the `SourceFiles` database their `SourceFileId`s index into, and the
checked modules) and a CLI half that renders, prints status and writes. `compile_package` and
friends keep their behaviour and become the second half calling the first. The status lines
either move out of the checking half or go through a reporter the caller supplies. Which one
is this ticket's call; the CLI's visible output must not change either way.

`CompilationError::as_diagnostic` stays the single place a `Diagnostic` is built
(`CLAUDE.md` — *Standing invariants*). A caller of the checking half converts from that
`Diagnostic`, not from the error types directly.

**Acceptance:**

- A new public entry point checks a package and returns its errors and file database without
  writing to stderr or to disk. A test in `tests/pipeline.rs` runs it on a fixture with one
  type error and asserts the error comes back with a label in the right file, and that no
  `build/` directory was created.
- `tests/cli.rs` is green unchanged. That is the check that the CLI's own output did not move.
- `cargo test --workspace` is green, and `cargo run -- compile std/core` still prints
  `parsed 10 modules`, lists all ten as checked, and exits 0.
