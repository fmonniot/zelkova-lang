# LANG-14 · A constructor pattern's arity is never checked

**Sizing:** small.

**Location:** `src/compiler/canonical/mod.rs` — `Pattern::from_parser`, the
`parser::PatternKind::Constructor` arm, which resolves the constructor and then canonicalizes
`args` without comparing its length to `ctor.type_parameters`.
`src/compiler/typer/mod.rs` — `translate_pattern`, whose
`args.iter().zip(param_types.iter())` stops at the shorter of the two.

**Decided ([`docs/spec/patterns.md`](../spec/patterns.md), *The arguments must be the ones it
was declared with*):** a constructor pattern supplies exactly as many argument patterns as the
`type` declaration gave that constructor. A constructor's arity is part of what it is.

**Problem:** nothing counts. Given `type Shape = Dot | Rect Count Count`, all of these are
accepted through canonicalization *and* type checking:

```zel
case shape of
  Rect w -> w        -- one argument for a two-argument constructor
  Dot -> One

case shape of
  Dot a b -> a       -- two arguments for a nullary constructor
  _ -> One
```

The typer builds a `TermPatternKind::Constructor` whose bindings come from zipping the
argument patterns against the declared parameter types, so a short pattern silently binds a
prefix and a long one silently drops its tail. `Dot a b` binds nothing and the body's `a` then
fails to resolve for an unrelated reason, which is the closest thing to a diagnostic today.

Found while writing [`docs/spec/patterns.md`](../spec/patterns.md) (`SPEC-7`).

**Approach:** check in `Pattern::from_parser`, not in the typer. Canonicalization already has
the resolved `TypeConstructor` and therefore `type_parameters.len()` in hand, and it is the
phase whose errors the spec harness can hold to account — a check that lives only in the typer
leaves the chapter's block green, because `tests/spec.rs` stops at canonicalization
([TEST-2](test-2.md)).

Add a `canonical::Error` variant carrying the constructor's `QualName`, the expected and given
counts, and the pattern's `NodeSpan` — which already covers the constructor and its arguments.
`CLAUDE.md`'s *An error has to describe itself* applies: the message is written in the
vocabulary of the source ("`Rect` takes 2 arguments, but this pattern gives 1"), and the new
variant must be added to `tests/spec.rs`'s `variant_names`, which is an explicit match on
purpose so a new variant fails to compile rather than silently never matching.

**Acceptance:** both examples above are rejected with the new error, with a test in
`tests/compiler/canonical.rs` for each of too-few and too-many. The `expect=ok` block in
[`docs/spec/patterns.md`](../spec/patterns.md)'s *The arguments must be the ones it was
declared with* section goes red, and is retagged `expect=canonical-error:<new variant>` with
its `**Known gap:**` paragraph deleted. `cargo run` still prints `parsed 8 modules` and lists
all eight as checked.
