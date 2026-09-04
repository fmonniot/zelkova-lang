# TEST-2 · The spec harness stops at canonicalization, so no chapter can pin a type error

**Sizing:** small-to-medium. The harness change is small; the reason-pinning vocabulary needs
a decision.

**Location:** `tests/spec.rs` — `evaluate` and `evaluate_group`, both of which run a block
through `parse` then `canonical::canonicalize` and stop; the `Expect` enum, which has no
type-level variant; and `docs/spec/conventions.md`'s `expect=` vocabulary table, which
documents the same set for chapter authors.

**Problem:** `docs/spec/` exists so that a chapter claim the compiler disagrees with is a red
test rather than a stale sentence. That guarantee currently covers the parser and
canonicalization, and stops there. Anything the **type checker** decides is invisible to the
harness: a block whose annotation contradicts its body is `expect=ok`, and passes.

That is not a hypothetical shortfall. `docs/spec/types.md`'s *An annotation is a promise*
section states the rule that a declaration's annotation may not be more general than its body
can support ([LANG-12](lang-12.md)) — and its example compiles identically before and after
that ticket lands, so the paragraph has to be deleted by hand rather than being forced red.
`docs/spec/types.md`'s *Applying a type to arguments* section has the same problem for the
annotation half of [BUG-17](bug-17.md): `f : Maybe Int` with `f = Just 'c'` type checks clean
today, and nothing in the chapter can say so in a way that survives the fix.

Every remaining planned chapter has type-level claims to make. *Expressions*, *Patterns* and
*Evaluation semantics* all do, and each will hit this the moment it is written.

**Approach:** add `expect=type-error` and `expect=type-error:Variant` to the harness,
mirroring the existing `canonical-error:` pair. The block parses, canonicalizes, and is then
run through `typer::type_check`; the tag passes when at least one error comes back, and the
`:Variant` form additionally matches against the real `typer::ErrorKind` variant names.

Four things to settle, none of them mechanical:

- **What `expect=ok` should mean.** Today it means "parses and canonicalizes". Extending it
  to "and type checks" is the honest reading and would be the more valuable change — but it
  will turn existing chapters' blocks red wherever they lean on an unchecked annotation, and
  those are the same blocks the `**Known gap:**` paragraphs are about. Decide whether this
  ticket tightens `expect=ok` or only adds the new tag; if it tightens it, expect to retag
  blocks across `modules.md` and `types.md` and to say in each chapter why.
- **Which name a `:Variant` pins.** `typer::Error` wraps an `ErrorKind`; write the match over
  the real enum explicitly, the way `variant_names` and `parse_error_reasons` already are, so
  a new variant fails `tests/spec.rs` to compile rather than silently never matching.
- **`package=` groups.** `evaluate_group` runs `ModuleWalker::check_in_order` with
  `canonicalize_tagged`; type checking a group means running the typer per module in that same
  order, and deciding what an interface built from a module that failed to type check contains.
- **`check_module` versus the phases directly.** `check_module` runs canonicalization, the
  typer and exhaustiveness together and returns a `CompilationError`, which would collapse the
  `canonical-error:` and `type-error:` distinction the tags need. Calling the phases
  separately keeps them apart, at the cost of the harness knowing the pipeline's shape.

Keep `docs/spec/conventions.md`'s vocabulary table and `tests/spec.rs`'s module doc comment in step
with whatever lands — they are written to be the same list read by two audiences, and the
skill that writes chapters (`.claude/skills/write-spec-chapter`) sends authors to the first
and reviewers to the second.

**Acceptance:** a fixture under `tests/fixtures/spec/` whose block is tagged
`expect=type-error:UnificationFailed` and whose source canonicalizes cleanly but fails the
typer, with a harness self-test beside the existing ones proving both directions — the right
variant passes, a wrong variant fails. The `**Known gap:**` paragraphs in
`docs/spec/types.md` named above are converted to tagged blocks, and the notes in
[BUG-17](bug-17.md) and [LANG-12](lang-12.md) saying they must be deleted by hand are removed.
