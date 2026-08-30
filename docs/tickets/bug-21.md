# BUG-21 · Every error from the source-directory walk is discarded, so a missing package root compiles as success

**Severity:** medium. A build that never read a single file reports success and exits 0.

**Location:** `src/compiler/source/mod.rs` — `load_package_sources`, specifically the
`.filter_map(|r| r.ok())` applied to the `WalkDir` iterator.

**Problem:** `WalkDir` yields `Result<DirEntry, walkdir::Error>`, and every `Err` is thrown
away. A root that does not exist yields exactly one `Err` and then ends, so
`load_package_sources` returns an empty `SourceFiles`, and `compile_package` goes on to print

```
success parsed 0 modules
success checked modules: []
```

and return `Ok(())`. `src/main.rs` then exits 0. Reproduced by calling
`compile_package` on a path that does not exist.

The same swallowing covers two other cases. A directory the process cannot read contributes
no modules and says nothing. A symbolic-link loop is detected by `walkdir` and reported as an
error — the walk uses `follow_links(true)` — so the modules under it are silently missing
rather than the loop being named.

A typo in the source root is the ordinary way to hit this: `std/core/srcc` is a green build.

**Approach:** stop discarding. The loop underneath already accumulates a `Vec` of
`SourceFileError` and returns it as `CompilationError::LoadingFiles`, which is exactly where
a walk error belongs — loading is the phase whose errors name a path rather than a span, and
a walk error has a path. Add a `SourceFileErrorType` variant carrying the `walkdir::Error`,
push one per failed entry, and give it a `message()` written for the reader: a root that does
not exist should say so rather than surfacing a raw I/O error.

Whether an existing root holding no `.zel` files at all is an error is a separate question
and this ticket does not answer it — the fix here is only that an error the walk *reported*
must not vanish.

**Acceptance:** a `tests/pipeline.rs` test asserting `compile_package` on a path that does not
exist returns `Err`, and that the error names the path. `cargo run` must still print
`parsed 8 modules` and list all eight as checked.
