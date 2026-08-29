# SPEC-11 · Write the Constrained type variables chapter

**Sizing:** small. Per `docs/spec/README.md`, this chapter's job is to record an open question,
not settle one — there is no probing to do, since grounding already confirmed the compiler has
zero special-casing to find.

**Location:** `std/core/src/Basics.zel` — `add : number -> number -> number`, `lt : comparable
-> comparable -> Bool`, `append : appendable -> appendable -> appendable` (mirrored in
`std/core/src/Js/Basics.zel` / `Js/Utils.zel`); confirmed absent from
`src/compiler/canonical/mod.rs` and all of `src/compiler/typer/` — `number`, `comparable` and
`appendable` are ordinary `TypeKind::Variable`/`Type::Variable` with no special handling
anywhere.

**Problem:** whether `number`, `comparable` and `appendable` become real type classes,
compiler-known constraints, or nothing at all is undecided, per `CLAUDE.md`'s *Language notes*.
The chapter's job is to record that the question is open, not to resolve it.

**Approach:** follow `write-spec-chapter`'s Step 4, framed as recording rather than deciding:
show where the three names appear in `std/core/src/`, state plainly that the compiler treats
them as ordinary type variables today, and lay out the candidate resolutions (type classes /
compiler-known constraints / nothing) without picking one.

**Acceptance:** `cargo test --test spec` green, `docs/spec/constrained-type-variables.md`
contributing its blocks (if any — this chapter may be mostly prose), `docs/spec/README.md`'s
row for this chapter moved to `written`.
