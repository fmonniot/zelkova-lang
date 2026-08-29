# SPEC-9 · Write the Evaluation semantics chapter

**Sizing:** medium, but the work is design rather than code exploration — there is no evaluator
to ground against (see Problem), so most of the ticket's value is in settling design questions
with the language owner rather than surfacing compiler bugs.

**Location:** none in `src/compiler` — confirmed no `codegen`/`backend`/`eval`/`interpret`
module exists anywhere in the compiler; `std/core/src/Basics.zel` — `and`, `or`, `eq`/`neq`
(bound to `==`/`/=` via `infix`), whose doc comments assert short-circuit and structural-equality
semantics that nothing in the compiler enforces today.

**Grounding note:** the module-absence above is a structural fact, easy to re-check with a
grep. The rest of this ticket — what the chapter should say the language's semantics *are* —
is not: that's Step 4 territory, settled with the language owner, not something this ticket
presumes.

**Problem:** order of evaluation, what `==` means structurally, which operators short-circuit,
and what a function value is have never been written down — and unlike every other chapter,
there is nothing to probe: the pipeline stops at type checking, `&&`/`||` are ordinary `infix`
declarations with no special-casing in the grammar, and `==`/`/=` are ordinary functions
(`eq`/`neq`). The only existing claims are prose in `std/core/src/Basics.zel`'s doc comments,
unverified by anything.

**Approach:** follow `write-spec-chapter`, weighted toward Step 4 (settle design questions with
the owner) over Step 2 (probe the compiler), since there is little compiler behaviour to probe.
The Problem section names four questions as a starting point, not an exhaustive list — bring
whatever else comes up while drafting into the same design conversation rather than scoping the
chapter to just these four. Write the intended semantics down as design, tagging examples
`**Not implemented:**` throughout since none of it is checkable against a real evaluator yet.

**Acceptance:** `cargo test --test spec` green, `docs/spec/evaluation-semantics.md`
contributing its blocks with every block tagged and each tag proven to fail,
`docs/spec/README.md`'s row for this chapter moved to `written`.
