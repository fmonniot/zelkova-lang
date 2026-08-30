# LANG-16 · Nothing checks that a `case` covers its type

**Sizing:** medium. The algorithm is the work; the wiring already exists.

**Location:** `src/compiler/exhaustiveness.rs` — `check`, which inspects nothing and returns
`Ok(())` for every module. `Error::NonExhaustiveMatch` is defined and renders, and nothing
constructs it. `check_module` (`src/compiler/mod.rs`) already calls the phase and already
threads its `Vec<Error>` into the accumulated diagnostics.

**Decided ([`docs/spec/patterns.md`](../spec/patterns.md), *A pattern that can fail, and one
that cannot*):** the language does not require any one pattern to be irrefutable. It requires
that the patterns in a position **cover** the type between them — a `case` covers it with its
branches, and a function declaration covers it with its clauses. That is what makes a
refutable pattern safe in a function head, and it is the whole reason the language allows one
there.

**Problem:** the phase is a stub, so a `case` missing a variant is accepted:

```zel
ignore : Flag -> Flag
ignore flag =
  case flag of
    On ->
      Off        -- no branch for Off; `ignore Off` has no value to produce
```

The rule the chapter states is therefore unenforced everywhere it applies.

Found while writing [`docs/spec/patterns.md`](../spec/patterns.md) (`SPEC-7`).

**Approach:** implement coverage over `canonical::Module`'s `case` branches. Scope it to what
the language has: wildcard, variable, literal, tuple and constructor patterns, over a union
type whose variants are known from the module's own types or from an imported `Interface`. A
literal pattern never covers a type on its own, so a `case` over a numeric or character type
needs a wildcard or a variable branch; a tuple is covered when the product of its elements'
coverage is.

Sequence after [LANG-13](lang-13.md) and [LANG-14](lang-14.md): nested constructor patterns
are the case the algorithm is hardest for and cannot be written today, and an unchecked arity
would make a pattern's shape disagree with the constructor it names.

Two things to keep straight. Declaration clauses are the same question one level up, and
`MultipleBindingsUnsupported` means a declaration has exactly one clause today — so the
`case` form is all that can be checked now, and the clause form arrives with multi-clause
support. And `CLAUDE.md`'s *A pass that emitted an error must not report success* applies:
`exhaustiveness::check` returns `Result<_, Vec<Error>>` so that one uncovered `case` does not
hide the next.

**Note — this gap has no red test behind it.** `tests/spec.rs` stops at canonicalization
([TEST-2](test-2.md)) and exhaustiveness runs after the typer, so the chapter's block stays
`expect=ok` and green when this lands. Its `**Known gap:**` paragraph has to be deleted by
hand as part of this ticket; nothing will fail to remind you.

**Acceptance:** the example above is rejected with `Error::NonExhaustiveMatch`, pointing at
the `case`, with tests in `tests/typer.rs` (which reaches `check_module`, and so the phase)
covering a missing variant, a `case` made exhaustive by a wildcard, and a `case` over a
literal with no catch-all. `cargo run` still prints `parsed 8 modules` and lists all eight as
checked — `std/core/src/` is the real test of whether the algorithm is right. The
`**Known gap:**` paragraph in [`docs/spec/patterns.md`](../spec/patterns.md)'s *A pattern that
can fail, and one that cannot* section is deleted.
