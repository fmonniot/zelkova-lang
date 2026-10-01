# ERR-16 · `ModuleNameCollision` and `ReservedModuleName` have a file to point at and don't

**Sizing:** small.

**Location:** `crates/zelkova-compiler/src/resolve.rs` — `Error::ModuleNameCollision`, `Error::ReservedModuleName`,
`ModuleOrigin`, `OriginKind::Local`, `LocalModule`, and the `impl PhaseError for Error` block
(only a `message()` arm, no `labels()` arm at all); `crates/zelkova-compiler/src/lib.rs` — the loop in
`compile_in_build` (around the `for (id, file) in sources...` walk) that parses each file,
already holds the file's `SourceFileId` as `id` at the exact point it builds each
`resolve::LocalModule`, and discards it.

**Problem:** `PhaseError::labels()` defaults to an empty `Vec`, which renders as no caret
(`crates/zelkova-compiler/src/lib.rs`, `PhaseError` trait doc). `resolve::Error`'s `impl PhaseError` never
overrides it — the whole `impl` block is one `message()` match — so every variant in the enum
renders with no caret, `ModuleNameCollision` and `ReservedModuleName` included.

The enum's own doc comment (added in `LANG-14` alongside `ModuleNameCollision` itself) says
"none of these has a span" and justifies that by saying each variant is about a `zelkova.toml`
manifest, which is not a file the `Files` database holds. That justification fits the
manifest-related variants (`Manifest`, `UnsupportedSource`, `SourceUnreachable`,
`NameMismatch`, `ConflictingSources`) — a `zelkova.toml` genuinely isn't in that database. It
does not fit `ModuleNameCollision` or `ReservedModuleName`: both are about a `.zel` source file,
and that file **is** already in the `Files` database by the time either error is raised —
`compile_in_build` parses every file and gets a `SourceFileId` back for each one (`id` in the
loop cited above) before it ever calls into `resolve::visible_modules`. The id is simply not
passed along: `LocalModule` (`resolve.rs`) carries only `name: Name` and `file: String` — a
display path, built from `file.package_path()` — and that's what ends up in `OriginKind::Local
{ file }`, which `ModuleOrigin::describe()` uses only to build a message string, never a label.

So the gap isn't that these two variants lack any notion of "where" — they know exactly which
file, `LocalModule.file` says so in every message already — it's that the "where" never travels
as a `SourceFileId` a `SpanLabel` could carry, only as a string embedded in prose. Raised as a
[note] on PR #248 (SPEC-34), which added `ReservedModuleName` deliberately mirroring
`ModuleNameCollision` "including when it gives none" (its own ticket's approach note) — so this
is a pre-existing gap from `LANG-14`, not a regression either ticket introduced. The reviewer
suggested folding a mention into `ERR-2`, but `ERR-2` ("Unify the error-handling strategy across
compiler phases") is closed and tombstoned (`docs/tickets/README.md`, closed 2026-08-26) and its
body is deleted per this project's closed-ticket convention, so there is nothing left to append
to. This ticket is what replaces that suggestion, scoped to just these two variants.

**Fix:** thread a `SourceFileId` through, not a byte span — `parser::Module` has no span on its
`name` field (only `Name`, no position), so pinpointing the exact `module Foo` line is not
reachable without a grammar/AST change, which is a separate, larger concern and explicitly out
of scope here. What's reachable today:

1. Add `file_id: SourceFileId` to `LocalModule` (`resolve.rs`), filled from `id` at the
   `local_modules.push(resolve::LocalModule { .. })` call site in `crates/zelkova-compiler/src/lib.rs` — the
   value is already in scope there, this is a field addition and one extra assignment.
2. Give `OriginKind::Local` the same field alongside `file: String` (keep `file` — `describe()`
   still needs a display path for the message).
3. In `impl PhaseError for Error`, add a `labels()` arm for `ModuleNameCollision` (label both
   `first` and `second`'s origins, primary on `second` since that's the one being rejected) and
   for `ReservedModuleName` (label `module`'s origin). Each label is a **zero-width span at
   byte 0** of the origin's `file_id` — not a token, just "this file" — built with
   `SpanLabel { span: Span { start: BytePos(0), end: BytePos(0) }, file: Some(file_id), .. }`.
   That's honest about what's known: which file, not which line.
4. `Unwrapped`/`Namespaced` origins have no `SourceFileId` (they're a dependency's public
   interface, not a file this build parsed) — `labels()` skips those origins rather than
   fabricating one; a collision between a local module and an unwrapped/namespaced one still
   renders with only the local half underlined, which is strictly more than today's nothing.

**Acceptance:** a `crates/zelkova-compiler/tests/` (or wherever `resolve::Error` already has
coverage — check for an existing `crates/zelkova-compiler/tests/resolve.rs` or similar first) case building a package with two
same-named local modules, asserting the returned `ModuleNameCollision`'s `labels()` is
non-empty and each label's `file` is `Some(..)` matching the expected `SourceFileId`. A second
case for a non-core package declaring a module named `Basics` (or another of the eight),
asserting `ReservedModuleName`'s `labels()` is non-empty the same way. Assert on
`labels()[..].file` and `.span`, not on the enum variant alone — `NodeSpan`'s blind `PartialEq`
is exactly the trap `CLAUDE.md`'s *Testing notes* warns about, and the same lesson applies here
even though these labels are hand-built rather than `NodeSpan`-derived. Mutation-check by
reverting the `labels()` arm to the default (delete it) and confirming the assertion on
non-empty labels goes red.
