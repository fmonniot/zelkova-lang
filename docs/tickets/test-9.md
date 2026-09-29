# TEST-9 · A test companion's import of a companion under test that the build does not rewrite fails at run time, with a build path in the message

**Sizing:** small-to-medium. One check where the build already computes every rewrite target.
What could make it bigger: deciding what counts as "a literal that resolves under `src/`" without
parsing the `.mjs` file, which `javascript::rewrite_imports` deliberately does not.

**Location:** `src/compiler/javascript.rs` — `test_companion_import`, `rewrite_imports`.
`src/compiler/output.rs` — `Contents::Rewritten`. [DEC-14](../decisions/dec-14.md) — *What nothing
checks*.

**Problem:** when the tests are compiled, a root `tests/` facade's companion is written with each
string literal spelling the shortest source path to a `src/` companion replaced by the path between
the two built companions. A literal that reaches the same file by another spelling
(`./../../src/Js/Basics.mjs`) is not matched, is copied as written, and fails when the file is
loaded: one `ERROR … Cannot find module '…/build/test/js/src/Js/Basics.mjs'` per test in the module.
The message names a build path the author never wrote and points at no line of the `.mjs`.
DEC-14 records this as "kept by whoever places the next file", so nothing enforces it.

**Approach:** the compiler knows every `from` specifier `test_companion_import` produces for the
facade's package. Options, not picked by this ticket:
1. Scan the companion for quoted literals that resolve, from the companion's own directory, to a
   file under the package's `src/`, and report a `CompilationError` for each that is not one of the
   `from`s, naming the file and the spelling the author should use.
2. Do the same only for literals in `import`/`export … from` position, which needs a parse of the
   file and so more machinery.
Option 1 can flag a comment or an unrelated string that happens to resolve; say whether that is
acceptable.

**Acceptance:** a test companion importing its target as `./../../src/Js/Basics.mjs` fails
`compile_package` with a diagnostic naming that file and the expected spelling, in a test in
`tests/pipeline.rs`, and the correctly spelled import still compiles. DEC-14's *What nothing
checks* stops listing the non-shortest path.

**Found:** reviewing PR #277 ([TEST-7](README.md)), which added the rewrite. Left unfixed there
because it is a new compile-time check, outside that ticket.
