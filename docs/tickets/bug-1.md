# BUG-1 · `compile_package` reports success after emitting error diagnostics

**Severity:** medium (a package that does not compile is reported as compiling; the binary
exits 0)

**Location:** `src/compiler/mod.rs` — `compile_package`, its local `diagnostics` vector and
the unconditional `Ok(())` it ends on; `src/main.rs` — the `match` that prints
`success: {:?}`.

**Problem:** `compile_package` deliberately accumulates errors instead of stopping at the
first one, which is the right design — but it never converts the accumulation back into a
failure. Parse failures are partitioned off and pushed into `diagnostics`; check failures land
there too via `unwrap_or_else`; the loop at the end renders every one of them to stderr; and
then the function returns `Ok(())` regardless of how many there were.

`cargo run` shows it today:

```
success parsed 7 modules
success checked modules: []
warning: [Bitwise] Canonical error messages are not implemented yet
 = EnvironmentErrors([InterfaceNotFound(Name("Elm.Kernel.Bitwise"))])

success: ()
```

A module failed to canonicalize, the error was printed, and the process still reported success
and exited 0. Nothing downstream — a future codegen phase, a build script, CI — can tell the
two outcomes apart. The two `success` lines above the warning compound it: `print_success` is
called before the phase's failures are known, so "parsed 7 modules" prints even when some of
those seven did not parse.

Note also that `as_diagnostic` builds `Diagnostic::warning()` for canonical and dependency
errors. They are errors; the severity is wrong independently of the return value.

**Fix:** make the return value a function of `diagnostics`. The awkward part is that by the
end of `compile_package` the errors have been flattened into
`Diagnostic<SourceFileId>` values and the typed `CompilationError`s are gone, so there is a
choice to make: either keep the typed errors in a `Vec<CompilationError>` alongside (or
instead of) the diagnostics and return `Err(CompilationError::Many(..))`-style at the end, or
add a variant that carries already-rendered diagnostics. The first is better — it keeps
`as_diagnostic` as the single rendering point — and it is the same shape `ERR-2` needs anyway,
so the two are worth doing in that order. Then have `src/main.rs` exit non-zero on `Err`
rather than printing `failure: {:?}` and returning 0.

Switch `as_diagnostic`'s canonical and dependency arms to `Diagnostic::error()` while there.

**Acceptance:** a package containing a module that fails canonicalization makes
`compile_package` return `Err`, and the binary exits non-zero; a package where every module
checks still returns `Ok`. Note that `std/core/src` is *not* currently such a package — that
is `BUG-3` — so the passing half of this needs a fixture package or an existing
`tests/pipeline.rs` helper rather than the standard library.
