# TIDY-16 · `ir::specialise` still tolerates a constrained variable that its assignment has nothing for, a case `LANG-12` closes

**Sizing:** small. The edit is a few lines in one function; the work is establishing, for both
paths that call it, that the case cannot arise. If that cannot be shown for the member path, the
ticket stops being a cleanup and becomes a question for the type checker, and it says so rather
than deleting the guard.

**Location:** `crates/zelkova-compiler/src/ir/specialise.rs` — `bind`, whose `filter_map` over
`constrained(context)` drops any variable `assignment.get(..)` has nothing for, so the key is
shorter by it; and its two callers, `resolve_member` and `resolve_declaration`, which build that
`assignment` with `match_type` and discard the `bool` it returns.

**Depends on:** `LANG-12`, closed, which made an annotation's type variables rigid
(`typer/unifier.rs`, `TypeVariable::rigid`). Its ticket file is deleted, so there is nothing to
link; `git log --diff-filter=D -- docs/tickets/lang-12.md` finds it.

**Found while:** reviewing and fixing the `LANG-12` PR. Before it, an annotation's variable could
be solved to a concrete type by the body, so a published context could name a type and no
variable (`min : Comparable a => ..` over a body that forced `a := Int`), and the module doc
comment of `specialise.rs` said so: "Where the declaration's body forced a constrained variable
to a concrete type … the context holds that type, there is no variable to bind, and the key is
shorter by it." That paragraph was deleted in the change that closed `LANG-12`, and the doc comment of `bind`
and a comment in `resolve_declaration` were reworded to stop describing the case as live. The
behaviour itself was left alone, deliberately: that change had not proved the case unreachable
for a *member* of an instance, and widening a type-checker change into the IR was not the
ticket's job.

**Problem:** with rigid annotation variables, a given constraint is always on a rigid variable
and a body can no longer narrow it, so the reason `bind` skips a variable has gone. The skip
remains, and it now does something else without anyone having decided it should: any
constrained variable the assignment fails to bind silently shortens the key, where the pass's
other failures are reported (`Error::NotGround`, `Error::NoInstance`).

Two things in the code make "cannot arise" a claim to prove and not to assume:

1. `constrained` collects the variables of a predicate's type whatever its shape, so a
   predicate over a non-variable (or a type holding both variables and concrete parts) is
   representable in `ir::Predicate`. Whether `ir::build` can still produce one — from an
   unannotated declaration, from an instance member's own inferred context, or from a derived
   instance — is the question.
2. `match_type` returns `false` on a shape mismatch between the declaration's type and the
   type of the use, and both callers ignore it. A mismatch would leave a variable unbound and
   so, through `bind`'s skip, a shorter key with no error. Rigidity does not obviously rule that
   out.

**Approach:** the ticket picks none of the options in step 2.

1. **Establish whether the skip can fire, for both callers.** Add a temporary assertion (or
   `eprintln!`) at the point `bind` drops a variable, and run it over everything the build
   compiles: `cargo test --workspace`, `cargo run -- compile std/core`, `cargo run -- test
   std/core`, `std/test`, and `tests/fixtures/`. Then argue it from the source and not from the
   run alone: what the contexts of `ir::Declaration` and of an instance member can hold after
   `typer/classes.rs`'s discharge, for an annotated declaration, an unannotated one, and a
   member of an instance with a context (`same = sameWrap`, the case the module doc comment
   names, is the one that pins a member's variables away from its head's). Write what was found
   in the PR body.
2. **If it cannot fire**, choose between:
   - *Remove the skip and report a failure*: a constrained variable `assignment` lacks becomes
     an `Error` (a phase error with a message and a span, per `PhaseError`; no `panic!`), so a
     bug upstream is a diagnostic and not a silently shorter key. This also gives `match_type`'s
     ignored `bool` somewhere to go.
   - *Keep the skip and say it is defensive*: the doc comment states it is a guard, names what
     would have to be wrong for it to fire, and nothing else changes.
3. **If it can fire**, stop. Say which path, with a program, and file it as a ticket against the
   typer or the IR builder; do not remove the guard, and do not widen this ticket to fix it.
4. Remove any test that exists only to pin the old behaviour, if one is found; the change that closed
   `LANG-12` did not look for one.

**Acceptance:**

- The PR body states, for each of `resolve_member` and `resolve_declaration`, whether the skip
  can fire, and the evidence (the instrumented run and the reading of `ir::build`'s contexts).
- Whichever option step 2 or 3 leads to is the diff, and no comment in `specialise.rs` says a
  body forces a constrained variable to a type.
- If the skip becomes an error: a test in `crates/zelkova-compiler/tests/ir.rs` asserts the
  variant and its span, and is seen red with the guard removed; if no program reaches it, a
  unit test over `bind` with a hand-built assignment is acceptable and the PR body says why.
- `cargo test --workspace`, `cargo clippy --workspace --all-features --all-targets -- -D
  warnings` and `cargo fmt --all` pass; `cargo run -- compile std/core` prints `parsed 10
  modules`, lists all ten as checked and exits 0; `cargo run -- test std/core` reports
  `151 tests: 151 passed, 0 failed, 0 errored`.
