# BUG-18 · A variant that is not a constructor application is silently dropped

**Severity:** medium (source stops existing without a diagnostic — the declaration is accepted
and the constructor is simply not there).

**Location:** `src/compiler/canonical/mod.rs` — `do_types`, the `filter_map` over
`tpe.variants`:

```rust
.filter_map(|t| match &t.kind {
    parser::TypeKind::Unqualified(name, vars) => Some((name, vars)),
    _ => None,
})
```

**Problem:** the grammar parses a `type` declaration's variants as `Pipe<Type>` — each variant
is a full type expression, not a restricted constructor form (`grammar.lalrpop` says so in a
`TODO` above `Union`). `do_types` then keeps only the variants that happen to be
`TypeKind::Unqualified` and **discards the rest with no error**. The comment above it —
"other types can be safely ignored in this context" — is describing the intent, not the
consequence.

Four inputs, all of which canonicalize successfully today with the variant missing:

| Source | Variant kind | Result |
|---|---|---|
| `type Bad = (Int, Int)` | `Tuple` | `Bad` with zero variants |
| `type Bad a = a` | `Variable` | `Bad` with zero variants |
| `type Bad = Wrap Int -> Int` | `Arrow` | `Bad` with zero variants |
| `type T = c` | `Variable` (lowercase) | `T` with zero variants |

The last is the one most likely to be written by accident — a mistyped constructor name — and
it produces a type whose only constructor has vanished, so every later mention of `c` fails to
resolve and nothing points at the line that caused it.

Note that `type Bad = Wrap Int -> Int` loses `Wrap` too, not just the arrow: the whole variant
is one `Arrow` node, and `Wrap Int` is its left operand.

Found while writing [`docs/spec/types.md`](../spec/types.md) (`SPEC-5`), which specifies a
variant as a constructor name followed by zero or more type arguments — nothing else.

**Fix:** turn the `filter_map` into a `map` that reports. A non-`Unqualified` variant needs a
new `canonical::Error` variant carrying `t.span` (the variant's own span, so the caret is
under the variant and not under the whole declaration) and a message in the vocabulary of the
source: a variant is a constructor name and its arguments.

A lowercase name arrives as `TypeKind::Variable` rather than `Unqualified`, so it falls into
the same arm — but it deserves its own message ("a constructor name begins with an uppercase
letter") rather than the generic one, and the two are easy to tell apart at this site.

The better long-term fix is the `TODO` the grammar already carries: give `Union` its own
`Variant` production instead of reusing `Type`, so a tuple or an arrow in variant position is
a *parse* error and this arm becomes unreachable. That is a larger change (`grammar.lalrpop`,
the `parser` AST and `from_parser*` move together — see `CLAUDE.md`) and it would change the
tags in the chapter from `canonical-error:` to `parse-error:`. Either is acceptable; decide
which before starting, and retag the chapter to match.

**Acceptance:** each of the four sources above fails canonicalization with an error naming the
variant, with the caret under the variant. Tests in `tests/compiler/canonical.rs`. The four
`expect=ok` blocks in [`docs/spec/types.md`](../spec/types.md)'s *What a variant may be*
section go red on their tags and are retagged with their `**Known gap:**` paragraph deleted.
