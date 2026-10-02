# LANG-41 · Retire `Type::Number`: an integer literal is an `Int`

**Sizing:** small-to-medium. Small in the unifier — a variant and two special-case arms go
away and nothing replaces them — but it changes what type checks, so every expectation written
against the old behaviour moves with it.

**Location:** `crates/zelkova-compiler/src/typer/mod.rs` — the `Number` variant of `Type`, both
`Display` arms for it (the `Debug`-flavoured one and the user-facing one), the arms that pass it
through in `Types::instantiate` and the two substitution walks beside it, `Signature::of_type`
in the test module; `crates/zelkova-compiler/src/typer/constraint.rs` — the integer-literal arms
of `collect`, which constrain a literal to `Type::Number`;
`crates/zelkova-compiler/src/typer/unifier.rs` — `is_numeric` and the `(Type::Number, other)`
arm of `unify_one_constraint`; `crates/zelkova-compiler/tests/typer.rs`, whose expectations are
written against the `number` spelling and three of whose doc comments say "until `LANG-41`";
`crates/zelkova-compiler/tests/ir.rs`, one of whose doc comments names the variant;
`std/core/src/Basics.zel` — `degrees` and `turns`.

**Decided ([`docs/spec/expressions.md`](../spec/expressions.md), *A literal's type is its
spelling*, by the language owner):** a numeric literal written without a point is an `Int`; one
written with a point is a `Float`. Nothing else determines either and a literal is never any
other type. A literal carries **no constraint**, so nothing in the language defaults and the
compiler knows no class by name ([`DEC-2`](../decisions/dec-2.md#what-has-changed-since), on
its own decision 8).

There is therefore no `Number` obligation to collect, no defaulting pass, and no need for an
instance environment to make an integer literal check — an integer literal simply has the type
`Int`.

**Depends on:** nothing. **[LANG-40](lang-40.md) depends on it**: with `Type::Number` gone, no
class obligation is ever raised at a type that is neither `Int` nor `Float`. It can land at any
point before that, in parallel with [LANG-38](lang-38.md) and [LANG-39](lang-39.md), which
touch nothing it touches.

**Problem:** `Type::Number` is the type an integer literal gets. It unifies with `Int`, `Float`
and itself, and with nothing else — a class constraint wearing a type's clothes, with no
instance environment behind it, no source syntax, and no way to fail. A literal that ends up
unconstrained simply stays `Number` forever.

Under the settled rule it is also just wrong. `x : Float` with a body of `1` is accepted today
and is an error in the language: `1` is an `Int`, and `1.0` is what the declaration means. The
emitter already agrees with the rule and not with the typer — it writes an integer literal as a
`BigInt` whatever type was solved for it — so the declaration that type checks today is one
that mixes a `bigint` with a number at run time.

It is rendered `number` in a diagnostic, which [Types](../spec/types.md#type-variables) reads
as an ordinary type variable — see [ERR-13](err-13.md), which this ticket supersedes by
deleting the variant and the spelling with it.

**Approach:**

1. The integer-literal arms of `collect` give the literal `Type::Literal(TypeLiteral::Int)` in
   place of `Type::Number`. Confirm the float-literal arm already gives `Float`.
2. `Type::Number`, `is_numeric` and the special arm in `unify_one_constraint` go away.
   `grep -rn "Type::Number" crates/` returns nothing.
3. `crates/zelkova-compiler/tests/typer.rs`'s expectations move off the `number` spelling.
   Several will change from passing to failing — an annotation of `Float` against an
   integer-literal body among them — and each is a case where the new behaviour is the specified
   one; check each rather than retagging in bulk.
4. `std/core/src/Basics.zel` is written against a compiler that accepted an integer literal at
   `Float`. Two literals there mean a `Float` and need a point: the `180` in `degrees` and the
   `2` in `turns`. The comment above `degrees` describes the first as a defect waiting on this
   ticket; trim it to what [BUG-44](bug-44.md) still explains. `negate n = -n` and
   `abs n = if lt n 0 then -n else n` go on checking: prefix `-` is `0 - n`, both are annotated
   over `a`, and an annotation's variable still unifies with `Int`. Look for others in
   `std/core/src/`, `std/core/tests/`, `std/test/` and `tests/fixtures/` by building, not by
   reading.

**What gets worse, and is meant to.** Prefix negation is desugared to `0 - e`
([LANG-4](lang-4.md)), so `-x` for a `Float` `x` becomes a type error where today it type
checks and aborts at run time (`BUG-44`). That is the rule being applied to a desugaring that is
already wrong, and `LANG-4` — which wants `-e` to mean `negate e` — is the fix; it becomes
doable once [LANG-42](lang-42.md) gives `negate` a `Float` instance. Do not special-case the
invented zero here.

**Acceptance:** tests in `crates/zelkova-compiler/tests/typer.rs`, each seen red against the old
behaviour. `x = 1` infers `Int`. `x = 1.5` infers `Float`. `x : Float` with a body of `1` is now
an **error** — the reversal this ticket is for — and `1.0` checks. `x : Char` with a body of `1`
is an error whose message contains no spelling the grammar would accept as a type variable,
which is the assertion `ERR-13` asked for, surviving into this ticket.

`cargo test --workspace` is green. `cargo run -- compile std/core` still prints
`parsed 10 modules`, lists all ten as checked and exits 0, and `cargo run -- test std/core`
still reports `98 tests: 98 passed`.

**Closing it.** [`docs/tickets/err-13.md`](err-13.md) is deleted and its row tombstoned beside
this one's: its subject no longer exists. One chapter cites this ticket, and
`cargo test --test spec` goes red on a citation of a deleted ticket file:

- [`docs/spec/expressions.md`](../spec/expressions.md), *A literal's type is its spelling*: the
  `**Known gap:**` paragraph goes. **No block there goes red to remind you** — add one annotated
  `Float` with a body of `1`, tagged `expect=type-error`, in place of the paragraph, so the rule
  has a test behind it from now on.
- [`docs/spec/type-classes.md`](../spec/type-classes.md), *Numeric literals*, needs no change;
  check it.

`crates/zelkova-compiler/tests/typer.rs` carries three doc comments beginning "An integer
literal is still `number` until `LANG-41`"; they describe the tree this ticket removes.
