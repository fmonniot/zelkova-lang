# ERR-8 · Let a phase report a warning

**Sizing:** medium.

**Depends on:** [ERR-10](err-10.md) — this ticket's own "open question" below concluded that
building the severity channel before there is a single caller risks guessing wrong; ERR-10 is
that first caller (unused imports), filed to be resolved before this one starts.

**Location:** `PhaseError` and `CompilationError` in `src/compiler/mod.rs`,
`phase_diagnostic`, and the accumulate-then-decide logic at the end of `compile_package`.

**Problem:** every diagnostic the compiler emits is a `Diagnostic::error()`, and the return
value is decided by whether *any* error was accumulated: an empty vector is `Ok(())`, a
non-empty one is `Err(CompilationError::Many(..))` and `main` exits non-zero. There is no way
to tell the user something is wrong without failing the build.

Everything a compiler normally warns about is therefore unrepresentable: an unused import, a
binding that is never read, a `case` branch that can never match, an exposed name nothing
references.

**Approach:** severity has to flow through `PhaseError` (or its containers) and reach
`Diagnostic::warning()`, and — the part that is easy to get wrong — the final decision in
`compile_package` has to count *errors*, not diagnostics. The standing invariant "a pass that
emitted an error must not report success" must keep holding in its original sense while a pass
that emitted only warnings reports success. Write that distinction down where the decision is
made; it is exactly the kind of thing `BUG-1` was.

**Open question — resolve before starting.** Nothing in the compiler currently wants to emit a
warning. No phase has a diagnostic it is suppressing for want of a severity, and the checks that
would produce one (unused imports, unreachable branches) are themselves unwritten — the
exhaustiveness checker is still a stub. This ticket may be premature: building the severity
channel before there is a single caller risks guessing wrong about what the callers need, and
the invariant rewrite above is not free.

Prefer to pick this up **together with the first real warning** — most likely from the
exhaustiveness checker, or from an unused-import check in canonicalization, where the
requirements are concrete. If it is still empty-handed when someone reaches for it, closing it
unbuilt is a legitimate outcome.

Resolved by filing [ERR-10](err-10.md): unused-import detection in canonicalization, chosen
over an exhaustiveness-based warning because the exhaustiveness checker is itself unwritten.
Do not start this ticket until ERR-10 has landed a concrete diagnostic to carry.

**Acceptance:** a phase emits a warning; it renders with warning severity; `compile_package`
returns `Ok(())` and `main` exits 0 in its presence; a test pins all three.
