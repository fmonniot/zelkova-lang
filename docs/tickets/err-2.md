# ERR-2 · Unify the error-handling strategy across compiler phases

**Sizing:** medium — the mechanical part is small, but it forces a decision about what a
compiler diagnostic is in this project, and that decision is the actual work.

**Location:** `src/compiler/mod.rs` — `CompilationError`, its `PlaceHolder` variant,
`as_diagnostic`, and the `From<typer::Error>` / `From<exhaustiveness::Error>` impls;
`src/compiler/typer/mod.rs` — `Error`; `src/compiler/exhaustiveness.rs` — `Error`;
`src/compiler/canonical/mod.rs` — `Error`; `src/compiler/parser/error.rs` — `Error`;
`src/compiler/dependencies.rs` — `Error`; `src/compiler/source/files.rs` — `SourceFileError`.

**Problem:** every phase has its own error type, which is fine, and they compose badly, which
is not. Three concrete symptoms:

- `CompilationError::PlaceHolder` exists and is documented in the source as "Not an error, but
  something I use until I get to implement the actual error. Ultimately, this error should be
  removed from the code base." `From<typer::Error>` discards the typer error entirely and
  returns it, so a real type error reaches the user as `Diagnostic::bug()` with the message
  "A non implemented error message have been emitted".
- `From<exhaustiveness::Error>` is `todo!()`. It cannot fire today only because
  `exhaustiveness::Error` is `pub enum Error {}` — an uninhabited type — so
  `exhaustiveness::check` can never return `Err`. The moment that checker is written, the
  first error it reports panics the compiler.
- Only `parser::Error` renders properly. `as_diagnostic` handles it via `err.diagnostic(file_id)`;
  the canonical and dependency arms fall back to `format!("{:?}", e)` in a note under the
  message "… error messages are not implemented yet", and they build `Diagnostic::warning()`
  for what are errors.

The underlying reason is that only `parser::Error` carries source positions. `canonical::Error`
variants carry `Name`s and `QualName`s; `dependencies::Error` carries module names. Neither can
be pointed at a span, so neither can produce a real `codespan_reporting` diagnostic no matter
how the `From` impls are written. That is the thing to fix, and it is why this is not a
half-hour of plumbing.

Note that one part of this as originally written in `TODO.md` is **already done**:
`typer::Error` is no longer an empty enum. It has `TypeMismatch`, `UnificationFailed`,
`CircularType` and `UnboundVariable`, and `typer::type_check` is wired into `check_module` for
real. What remains is that those variants carry no location and are thrown away on conversion.

**Approach:**

1. Decide what a phase error owes the reporter. The minimum that makes `as_diagnostic` real is
   a byte span plus the `SourceFileId` it belongs to. `parser::Error` already models this; look
   at how `Spanned` and `Position` (`src/compiler/position.rs`) flow through the parser and
   stop at the canonical AST, and decide whether canonical/typer errors carry spans directly or
   carry a node identity that can be resolved back to one.
2. Thread that through `canonical::Error` and `typer::Error`. This is the bulk of the diff and
   it can land incrementally — one phase at a time, each with its own diagnostic arm.
3. Give `exhaustiveness::Error` at least one variant, or delete the type and have `check`
   return `Result<(), Infallible>` until the checker exists. Either way `todo!()` goes.
4. Delete `CompilationError::PlaceHolder` and its `as_diagnostic` arm. It should not be
   possible to reintroduce it accidentally; its absence is the test.
5. Settle the `Vec<Error>` inconsistency while in here: `canonicalize` returns
   `Result<_, Vec<Error>>`, `type_check` returns `Result<_, Error>`, `check_in_order` returns
   `Result<_, Vec<E>>`. Accumulating diagnostics is the goal (`compile_package` is built for
   it), so prefer the accumulating shape everywhere and use `Error::Many` where a single value
   is required.

`BUG-1` wants the typed errors kept alive to the end of `compile_package` instead of being
flattened into diagnostics early; that is the same restructuring seen from the other side.
Doing `BUG-1` first, or the two together, avoids writing the plumbing twice.

**Acceptance:** `grep -rn PlaceHolder src/` returns nothing, and neither `From` impl in
`src/compiler/mod.rs` contains `todo!()`. A type error in a `.zel` source reaches the user as a
`Diagnostic::error()` with a message naming the mismatched types, not as a `Debug` dump in a
note — pin it with a test in `tests/pipeline.rs` asserting on the rendered diagnostic, not just
on `is_err()`.
