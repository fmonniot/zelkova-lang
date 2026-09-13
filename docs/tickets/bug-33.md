# BUG-33 · `SourceFileError::notes()` dumps `io::Error`'s `Debug` form instead of its `Display` form

**Severity:** low (the diagnostic still names the right file and the right kind of failure;
only the note's own detail line is unreadable).

**Location:** `src/compiler/source/files.rs` — `SourceFileError`'s `impl PhaseError` `notes()`,
the `SourceFileErrorType::Io` arm, and (once [PR #200](https://github.com/fmonniot/zelkova-lang/pull/200)
lands) the `SourceFileErrorType::Walk` arm's `io_error()` branch.

**Problem:** `notes()`'s `SourceFileErrorType::Io` arm reads:

```rust
SourceFileErrorType::Io(err) => vec![format!("detailled error: {:?}", err)],
```

`{:?}` is `io::Error`'s `Debug` form, so a permission-denied read renders as `detailled error:
Os { code: 13, kind: PermissionDenied, message: "Permission denied" }` in the note the user
actually sees. `CLAUDE.md`'s standing invariant on `PhaseError` says exactly this:
`format!("{:?}", e)` in a note names a Rust type, not a source construct — it is not a
hypothetical, this arm is a live instance of it on `main` today.

PR #200 (open, not yet merged as of filing) adds `SourceFileErrorType::Walk` to report every
error `WalkDir` produces (closing [BUG-21](../tickets/README.md)) and, in its `notes()` arm,
repeats the same pattern for the wrapped `io::Error`:

```rust
if let Some(io_err) = err.io_error() {
    notes.push(format!("detailled error: {:?}", io_err));
}
```

This was flagged as a should-fix review comment on that PR (inline comment on
`src/compiler/source/files.rs`, id 3999967290) but left for a follow-up ticket rather than
blocking the PR, since it mirrors a pre-existing defect rather than inventing a new one. The
review comment also notes both sites spell "detailed" as "detailled" — a typo worth fixing in
the same edit since it's the same string literal, but not the point of this ticket; don't go
looking for other typos in the file beyond these two lines.

**Fix:** in both arms, format the wrapped `io::Error` with `{}` (`Display`) instead of `{:?}`
(`Debug`) — e.g. `format!("detailed error: {}", err)`, which renders as something like `"No
such file or directory (os error 2)"` instead of the `Os { code, kind, message }` struct dump.
If PR #200 has already merged by the time this is picked up, apply the same change to whatever
form the `Walk` arm's `notes()` took at merge — check it wasn't already fixed there before
editing.

**Acceptance:** `grep -n '{:?}' src/compiler/source/files.rs` no longer matches either
`notes()` arm (the `#[derive(Debug)]` on `SourceFileErrorType` itself is unaffected and should
stay). `cargo build && cargo test --workspace` stay green. No existing test currently asserts
on this note's exact text, so this is a formatting fix confirmed by inspection rather than by
a new test; if one already exists after PR #200 merges, update its expected string rather than
adding a new one.

**Related:** found via a should-fix review comment on [PR #200](https://github.com/fmonniot/zelkova-lang/pull/200)
(closing `BUG-21`); left as a follow-up rather than fixed in that PR because it mirrors a
pre-existing defect in the sibling `Io` arm rather than being new to that diff.
