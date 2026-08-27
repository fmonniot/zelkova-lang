# ERR-5 · A diagnostic can point into another module

**Sizing:** medium. Small in code, but it revisits a standing invariant, so the write-up matters
as much as the diff.

**Depends on:** `ERR-3`.

**Location:** `Interface` in `src/compiler/mod.rs`, `src/compiler/canonical/environment.rs`,
the `SpanLabel` type and `PhaseError::labels` in `src/compiler/mod.rs`.

**Problem:** a `codespan_reporting::Label` carries **its own** `SourceFileId` — one per label,
not one per diagnostic. Every label the compiler can produce today points into the single file
the erroring module was read from, and `ERR-3` deliberately keeps it that way: a phase only ever
sees one module, so `compile_package` pairs its file id with each span.

That is exactly enough for errors that are *about* one module and no more, and exactly not
enough for the best half of what rustc does:

```
error: cannot find a value named `withDefault`
  ┌─ src/Main.zel:12:9
   │
12 │   x = withDefault 0 maybe
   │       ^^^^^^^^^^^ not in scope here
   │
  ┌─ src/Maybe.zel:31:1
   │
31 │ withDefault : a -> Maybe a -> a
   │ ^^^^^^^^^^^ defined here, but `Main` does not import it
```

The second label lives in a different file. Nothing in the compiler can express it, because
`Interface` — the only thing one module ever learns about another — carries
`canonical::Type`/`UnionType`/`Infix` with **no positions and no file id**.

`ERR-3` records the constraint that leads here: `canonical::Type` and `TypeConstructor`
deliberately get no span, because they are cloned out of an `Interface` and a bare span would
name the *importing* module's file for something written in the *exporting* module's. The fix
is not to add the span; it is to add the pair.

**Approach:** a `SourceSpan { file: SourceFileId, span: Span<BytePos> }` for anything stored in
an `Interface`, and a `module`-or-file field on `SpanLabel` so a phase error can offer a label
that is not in the module being checked.

This relaxes `CLAUDE.md`'s standing invariant "The `SourceFileId` is never a phase's business —
`compile_package` attaches it, being the only place that knows which file a module was read
from." That invariant is right for the module under check and cannot cover cross-module
references. Rewrite it rather than deleting it: the driver still attaches the id for the module
being checked, and the *interface* is what carries ids for everything else. Update `CLAUDE.md`
in the same commit.

**Acceptance:** a diagnostic rendering two labels in two different files, asserted in
`tests/pipeline.rs` on `diagnostic.labels` — including that the two labels' `file_id`s differ.
