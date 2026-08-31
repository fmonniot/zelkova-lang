# LANG-20 · A declaration may have only one clause

**Sizing:** medium. The parser already carries the construct end to end; the work is a
desugaring in canonicalization plus the scoping and arity rules that go with it.

**Location:** `src/compiler/canonical/mod.rs` — `do_values`, whose `match function.bindings.len()`
has a `1` arm and a `_` arm returning `Error::MultipleBindingsUnsupported`. The `_` arm already
carries the note describing the fix: *if multiple bindings, we need to create synthetics
variables and put all bindings into a case expression*.
`src/compiler/parser/mod.rs` — `Function::bindings: Vec<Match>` and
`Module::from_declarations`, which collects every `Declaration::Function` sharing a name into
one `Function`; `Match`, which holds one clause's patterns and body.

**Decided ([`docs/spec/patterns.md`](../spec/patterns.md), *A pattern that can fail, and one
that cannot*):** a declaration may be written as several clauses, one per line, each with its
own patterns and its own body. The clauses are tried in the order written, and between them
they must cover the type. This is a deliberate divergence from Elm, which has no equivalent —
there is no inherited rule even to restate.

```zel
invert : Flag -> Flag
invert On = Off
invert Off = On
```

**Problem:** the parser groups the two lines into one `Function` with two `Match`es, and
canonicalization then rejects any `Function` with more than one, so the construct reaches the
last phase that could implement it and is turned away there. Today a declaration has exactly
one clause and any pattern that can fail has to go in a `case`.

The error is honest rather than wrong — it names the construct and says it is unsupported —
so this is a gap to fill, not a defect to repair.

Found while writing [`docs/spec/patterns.md`](../spec/patterns.md) (`SPEC-7`).

**Approach:** desugar in `do_values` rather than growing a second representation. A `Function`
with *n* clauses of *k* patterns each becomes one binding of *k* fresh parameters and a body
that is a `case` over them — a tuple of them when *k* > 1, which is why the desugaring is
capped at the arities `Tuple` has (`CLAUDE.md`, *Tuples are size 2 or 3 only*). A `k` above
three needs either a scrutinee shape that is not a tuple or a nested `case` per parameter;
decide which before writing the arm, and say so in the doc comment.

Three rules the desugaring has to preserve, each of which has a home already:

- **Every clause has the same number of patterns.** This one already works and must keep
  working: `do_values`'s `bindings_size` check compares every clause's pattern count against
  the first and reports `BindingPatternsInvalidLen`, and it runs *before* the rejection, so it
  is live today even though nothing downstream is. Because the counts are equal, the existing
  comparison against the annotation's linear length still only has to be made once.
- **A clause's bindings are visible in that clause's body and no further** — the same scoping
  a `case` branch has, which is what the desugaring gives it for free.
- **Each clause is one binding position**, so `LANG-18`'s duplicate-name rule applies across a
  clause's parameters. Sequence this after `LANG-18` or the two arms are written twice.

Fresh parameter names must not be able to collide with a name the source wrote. `Name` is
built from source text, so pick a spelling the tokenizer cannot produce.

**Acceptance:** the `expect=canonical-error:MultipleBindingsUnsupported` block in
[`docs/spec/patterns.md`](../spec/patterns.md)'s *A pattern that can fail, and one that cannot*
goes red and is retagged `expect=ok`, with its `**Not implemented:**` paragraph deleted.
`Error::MultipleBindingsUnsupported` is removed along with its `message`, `notes` and `labels`
arms and its entry in `tests/spec.rs`'s `variant_names`. Tests in `tests/compiler/canonical.rs`
cover clause order, per-clause scoping, and a clause whose pattern count disagrees with the
annotation; a test in `tests/typer.rs` covers two clauses whose bodies have different types.
`cargo run` still prints `parsed 8 modules` and lists all eight as checked.

**Note for `LANG-19`:** exhaustiveness over clauses becomes checkable only once this lands —
`LANG-19` says so, and can be done for `case` alone before it.
