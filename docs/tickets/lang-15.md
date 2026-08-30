# LANG-15 · A pattern may bind the same name more than once

**Sizing:** small.

**Location:** `src/compiler/canonical/environment.rs` — `ScopedEnvironment::expose_pattern`,
which walks a pattern inserting each `PatternKind::Variable` into `self.variables` with no
check for what is already there. `src/compiler/canonical/mod.rs` — its two callers:
`do_values` (a declaration's parameter patterns) and `Expression::from_parser`'s
`parser::ExpressionKind::Case` arm (a branch's pattern).

**Decided ([`docs/spec/patterns.md`](../spec/patterns.md), *A pattern binds each name
once*):** every variable in one pattern position is distinct. Repeating a name is an error
rather than a demand that the two values be equal. The rule spans a whole clause, not one
pattern, so a declaration's parameters are one binding position between them.

**Problem:** `HashMap::insert` overwrites, so the last occurrence wins silently:

```zel
same : Pair -> Flag
same (Pair a a) =
  a              -- resolves to the second field

f : Flag -> Flag -> Flag
f a a =
  a              -- resolves to the second parameter
```

Neither is reported by any phase. The body reads as if it names the first binding and does not.

Found while writing [`docs/spec/patterns.md`](../spec/patterns.md) (`SPEC-7`).

**Approach:** `expose_pattern` is the wrong level on its own — the rule is per *clause*, and
`do_values` calls it once per parameter pattern in a loop, so a check confined to one call
would miss `f a a`. Have the walk collect `(Name, NodeSpan)` pairs and report a duplicate
against the set accumulated for the whole binding position.

Add a `canonical::Error` variant carrying the name, the span of the repeat and the span of the
first occurrence — a secondary label under the first is what makes the message readable, and
`PhaseError::labels` already supports more than one (`CLAUDE.md`, *An error has to describe
itself*). Add it to `tests/spec.rs`'s `variant_names`.

`NodeSpan`'s `PartialEq` always returns `true`, so a test that cares which occurrence is
underlined asserts on `diagnostic.labels[..].range` rather than on the whole error value.

**Acceptance:** both examples above are rejected, with a test in `tests/compiler/canonical.rs`
for the within-one-pattern case and one for the across-parameters case. The `expect=ok` block
in [`docs/spec/patterns.md`](../spec/patterns.md)'s *A pattern binds each name once* section
goes red, and is retagged `expect=canonical-error:<new variant>` with its `**Known gap:**`
paragraph deleted. `cargo run` still prints `parsed 8 modules` and lists all eight as checked.
