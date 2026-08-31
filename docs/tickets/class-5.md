# CLASS-5 · Retire `Type::Number`: an integer literal is an `Int`

**Sizing:** small-to-medium. Small in the unifier — a variant and two special-case arms go
away and nothing replaces them — but it changes what type checks, so every expectation written
against the old behaviour moves with it.

**Location:** `src/compiler/typer/mod.rs` — the `Number` variant of `Type`, both `Display`
arms for it (the `Debug`-flavoured one and the user-facing one), `Signature::of_type` in the
test module; `src/compiler/typer/constraint.rs` — the integer-literal arm of `collect`, which
constrains a literal to `Type::Number`; `src/compiler/typer/unifier.rs` — `is_numeric` and the
`(Type::Number, other)` arm of `unify_one_constraint`; `tests/typer.rs`, whose expectations are
written against the `number` spelling.

**Decided ([`docs/spec/expressions.md`](../spec/expressions.md), *A literal's type is its
spelling*, by the language owner):** a numeric literal written without a point is an `Int`; one
written with a point is a `Float`. Nothing else determines either and a literal is never any
other type. A literal carries **no constraint**, so nothing in the language defaults and the
compiler knows no class by name.

That is narrower than this ticket originally assumed, and the difference is the whole of the
work. There is no `Number` obligation to collect, no defaulting pass, and no need for an
instance environment to make an integer literal check — an integer literal simply has the type
`Int`.

**Depends on:** nothing in the `CLASS-` program, now. The original entry said `CLASS-4`, on the
grounds that a literal's `Number` obligation needed a solver to discharge it. Under the settled
rule there is no obligation, so this can land at any point. Confirm that before sequencing it
early: `std/core/src/Basics.zel` annotates its arithmetic `a -> a -> a`, which unifies with
`Int` without any class machinery, but check the modules that use it.

**Problem:** `Type::Number` is the type an integer literal gets. It unifies with `Int`, `Float`
and itself, and with nothing else — a class constraint wearing a type's clothes, with no
instance environment behind it, no source syntax, and no way to fail. A literal that ends up
unconstrained simply stays `Number` forever.

Under the settled rule it is also just wrong. `x : Float` with a body of `1` is accepted today
and is an error in the language: `1` is an `Int`, and `1.0` is what the declaration means.

It is rendered `number` in a diagnostic, which [Types](../spec/types.md#type-variables) reads
as an ordinary type variable — see [ERR-13](err-13.md), which this ticket supersedes by
deleting the variant and the spelling with it.

**Approach:**

1. The integer-literal arm of `collect` gives the literal `Type::Int` rather than
   `Type::Number`. Confirm the float-literal arm already gives `Type::Float`.
2. `Type::Number`, `is_numeric` and the special arm in `unify_one_constraint` go away.
   `grep -rn "Type::Number" src/` returns nothing.
3. `tests/typer.rs`'s expectations move off the `number` spelling. Several will change from
   passing to failing — an annotation of `Float` against an integer-literal body among them —
   and each is a case where the new behaviour is the specified one; check each rather than
   retagging in bulk.
4. `cargo run` is the risk here, not the unit tests: `std/core/src/` is written against a
   compiler that accepted an integer literal at `Float`. Any literal there that meant a
   `Float` needs a point. Expect this to be the bulk of the diff.

**Acceptance:** tests in `tests/typer.rs`. `x = 1` infers `Int`. `x = 1.5` infers `Float`.
`x : Float` with a body of `1` is now an **error** — the reversal this ticket is for — and
`1.0` checks. `x : Char` with a body of `1` is an error whose message contains no spelling the
grammar would accept as a type variable, which is the assertion ERR-13 asked for, surviving
into this ticket. `cargo run` still prints `parsed 8 modules` and lists all eight as checked.

**This gap has no red test behind it.** The spec harness stops at canonicalization
([TEST-2](test-2.md)), so the `**Known gap:**` paragraph in
[`docs/spec/expressions.md`](../spec/expressions.md)'s *A literal's type is its spelling*
section is not held to account by any block and has to be deleted by hand when this lands.
