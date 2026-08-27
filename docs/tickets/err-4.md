# ERR-4 · Type errors point at the sub-expression, not at the whole declaration

**Sizing:** large — comparable to `ERR-3`. It reaches every stage of the typer's own pipeline
and changes the shape of `Constraint`, which is the type the whole inference algorithm is
written against.

**Depends on:** `ERR-3` (a canonical AST that carries spans at all).

**Location:** `src/compiler/typer/mod.rs` (the `Term` / `TypedTerm` language and `type_check`),
`src/compiler/typer/annotate.rs`, `src/compiler/typer/constraint.rs`,
`src/compiler/typer/unifier.rs`.

**Problem:** `ERR-3` gives `canonical::Value` a span, which is enough for a type error to name
the declaration that failed. It cannot do better, and the reason is not the canonical AST.

The typer does not type-check the canonical AST. It translates it into a **separate, simplified
term language of its own** — `Term`, `TypedTerm`, `Type`, `Constraint` — declared under the
comment "First try of an implementation. Not linked to the rest of the code base for
simplicity's sake." The translation (`canonical_expr_to_term`, `translate_pattern`,
`value_to_term_and_annotation`) drops everything that is not needed for inference, positions
included; names even degrade from `Name`/`QualName` to plain `String`.

So by the time `unify` fails, there is nothing left to point at. `Error::UnificationFailed` is
constructed in `unifier.rs` from two `Type`s that have no idea which expressions produced them,
and `Error::CircularType` carries nothing at all. A user writing

```zel
answer : Int
answer = if true then 1 else false
```

should be told that `false` is the problem and that `Int` is expected *because of the
annotation on the line above*. Both facts exist during inference; neither survives into the
error.

**Second half of the same problem: errors the typer never reports.** `type_check`'s third pass
`continue`s past two failure classes — a value whose term cannot be built at all (an unsupported
construct) and `Error::UnboundVariable`. This is documented on `type_check` today and is
deliberate, but it belongs in this ticket rather than outliving it: an error nobody reports is
one that no amount of span plumbing will help, and the `UnboundVariable` case in particular is
skipped precisely because the typer cannot say anything useful about *where* it happened.

**Approach:** give `Constraint` an origin — the spans that produced it, plus a *reason* naming
why the two types were required to match ("the annotation says", "this argument", "these
branches must agree") — and have `unify` propagate the origin of the constraint it failed on.
This is the standard shape: Elm calls it `Reason`, rustc calls it `ObligationCause`. Expect
`annotate.rs` to be where spans enter the term language and `constraint.rs` to be where the
reason is chosen, since that is where the structure of the expression is still visible.

Note `Constraint` currently lives in a `HashSet`, so whatever the origin is has to keep
`Hash`/`Eq` working — or the collection has to change. Decide that early; it shapes everything
else.

**Acceptance:** the example above renders with a primary label under `false` and a secondary
label under the `: Int` annotation. `tests/typer.rs` grows assertions on rendered diagnostics —
today all nineteen of its tests are bare `is_ok()`/`is_err()`, so a typer that inferred the
wrong-but-consistent type would pass the entire file. Neither failure class listed above is
still silently skipped, or `type_check`'s doc comment is updated to explain what is and why.
