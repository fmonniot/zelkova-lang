# GEN-10 · Emit a `case`

**Sizing:** small to medium. The hard part — deciding what is tested in what order — is
[`GEN-5`](gen-5.md)'s; this walks the tree it produced and writes JavaScript.

**Depends on:** [`GEN-5`](gen-5.md) (the decision tree), `GEN-9`, closed (the emitter it
sits inside, `src/compiler/javascript.rs`) and [`GEN-8`](gen-8.md) (the abort the fall-through leaf calls).

**Part of:** [`GEN-1`](gen-1.md).

**Location:** `src/compiler/javascript.rs`, whose `expression` answers
`Error::Unsupported` with `Construct::Case` for a `case` today, and with
`Construct::ParameterPattern` for a parameter written as a pattern, which the IR holds as a
single-branch match on that parameter (`ir::CaseForm::Parameter`) and which emits the same way.

**Decided:** the tests, their order and the names each leaf binds are already settled by the
tree [`GEN-5`](gen-5.md) built, from
[Conditional evaluation](../spec/evaluation-semantics.md#conditional-evaluation) — the scrutinee
is evaluated, branches are tried in the order written, and the body of the first that matches is
evaluated, never another. This ticket adds no rule; it must not reorder or merge tests, because
the order is the semantics.

The representation each test reads is the one [`GEN-1`](gen-1.md) collects: a union value's
constructor is its `$` field, a tuple is an array, and a `Bool` is a JavaScript boolean rather
than a tagged object — so a `case` on a `Bool` tests the value itself.

**Problem:** `src/compiler/javascript.rs` emits every expression form except this one, and `case` is how
every union type in the language is taken apart. `std/core`'s `Maybe`, `Result` and `Basics`
are all unreachable without it.

**Approach:** walk the tree. A test becomes a condition, an edge an arm, a leaf its branch body
with the leaf's bindings introduced as names first. Whether that comes out as nested conditional
expressions, an `if`/`else` chain in a block, or a `switch` on the `$` field is the ticket's to
pick — the constraint is that a `case` is an expression in Zelkova and has to emit as something
an expression position accepts, or the emitter has to be able to lift it.

**The scrutinee is evaluated once.** It may be an arbitrary expression and the tree tests it
more than once, so bind it to a name before the first test rather than re-emitting it per test —
re-emitting would evaluate it once per test, which
[the chapter forbids](../spec/evaluation-semantics.md#order-of-evaluation) and which is
observable through non-termination.

The fall-through leaf calls [`GEN-8`](gen-8.md)'s abort, naming the declaration, per
[`GEN-5`](gen-5.md).

**Not in this ticket:** exhaustiveness ([`LANG-19`](lang-19.md)) — this emits the fall-through
that exists precisely because coverage is not checked yet.

**Acceptance:** tests assert the emitted text for a `case` on each pattern form, and for a
`case` whose scrutinee is a call, asserting the scrutinee appears once. A test in the emitted
output run under `node` — the harness [`GEN-14`](gen-14.md) sets up, or a local `node --test` if
this lands first — asserts a `case` over a three-constructor union returns each branch's value
for the matching input, and that a value no branch matches aborts rather than returning
`undefined`. `cargo run` still prints `parsed 8 modules`, lists all eight and exits 0.
