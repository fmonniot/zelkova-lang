# SPEC-11 · Write the Constrained type variables chapter

**Sizing:** small. Per `docs/spec/README.md`, this chapter's job is to record an open question,
not settle one.

**Location:** `std/core/src/Basics.zel` — `add : number -> number -> number`, `lt : comparable
-> comparable -> Bool`, `append : appendable -> appendable -> appendable` (mirrored in
`std/core/src/Js/Basics.zel` / `Js/Utils.zel`); a quick pass found no special-casing of
`number`/`comparable`/`appendable` anywhere in `src/compiler/canonical/mod.rs` or
`src/compiler/typer/` — they read as ordinary `TypeKind::Variable`/`Type::Variable`.

**Grounding note:** re-confirm the absence above during Step 2 rather than taking it as given
— a quick pass can miss special-casing tucked away somewhere. What doesn't need re-confirming
is the framing: this chapter records an open question rather than settling one, per
`docs/spec/README.md` and `CLAUDE.md`'s *Language notes*, and that framing is not up for
revision by this ticket or by drafting — it's the language owner's call already made.

**Problem:** whether `number`, `comparable` and `appendable` become real type classes,
compiler-known constraints, or nothing at all is undecided, per `CLAUDE.md`'s *Language notes*.
The chapter's job is to record that the question is open, not to resolve it.

**Approach:** follow `write-spec-chapter`'s Step 4, framed as recording rather than deciding:
show where the three names appear in `std/core/src/`, confirm and state plainly how the
compiler treats them today, and lay out the candidate resolutions (type classes /
compiler-known constraints / nothing) without picking one.

**Acceptance:** `cargo test --test spec` green, `docs/spec/constrained-type-variables.md`
contributing its blocks (if any — this chapter may be mostly prose), `docs/spec/README.md`'s
row for this chapter moved to `written`.
