# SPEC-9 · Write the Evaluation semantics chapter

**Sizing:** medium, but the work is design rather than code exploration — there is no evaluator
to ground against (see Problem), so most of the ticket's value is in settling design questions
with the language owner rather than surfacing compiler bugs.

**Location:** none in `src/compiler` — confirmed no `codegen`/`backend`/`eval`/`interpret`
module exists anywhere in the compiler; `std/core/src/Basics.zel` — `and`, `or`, `eq`/`neq`
(bound to `==`/`/=` via `infix`), whose doc comments assert short-circuit and structural-equality
semantics that nothing in the compiler enforces today.

**Problem:** order of evaluation, what `==` means structurally, which operators short-circuit,
and what a function value is have never been written down — and unlike every other chapter,
there is nothing to probe: the pipeline stops at type checking, `&&`/`||` are ordinary `infix`
declarations with no special-casing in the grammar, and `==`/`/=` are ordinary functions
(`eq`/`neq`). The only existing claims are prose in `std/core/src/Basics.zel`'s doc comments,
unverified by anything.

**Approach:** follow `write-spec-chapter`, weighted toward Step 4 (settle design questions with
the owner) over Step 2 (probe the compiler) since there is little compiler behaviour to probe.
Write the intended semantics down as design, tagging examples `**Not implemented:**` throughout
since none of it is checkable against a real evaluator yet.

**Acceptance:** `cargo test --test spec` green, `docs/spec/evaluation-semantics.md`
contributing its blocks with every block tagged and each tag proven to fail,
`docs/spec/README.md`'s row for this chapter moved to `written`.
