# CLASS-4 · Discharge class constraints in the type checker

**Sizing:** large. Hindley–Milner with a constraint set is a different solver from the one in
the tree: `unify` currently answers every constraint immediately, and a class obligation is one
it may not be able to answer yet.

**Location:** `src/compiler/typer/mod.rs` — `Type`, `Constraint`, `Origin`, `Reason`,
`ErrorKind`, `canonical_type_to_typer_type`, `infer_annotated`,
`value_to_term_and_annotation`; `src/compiler/typer/constraint.rs` — `collect`;
`src/compiler/typer/unifier.rs` — `unify`, `unify_one_constraint`.

**Depends on:** [CLASS-3](class-3.md), for an instance environment to discharge against; and
[LANG-12](lang-12.md), for rigid annotation variables. The second is the one that is easy to
get wrong by sequencing, so it is worth spelling out.

**Why `LANG-12` comes first.** `canonical_type_to_typer_type` turns every variable written in
an annotation into a fresh *unification* variable, so an annotation more general than its body
is silently specialised. Add constraints on top of that and the specialisation becomes unsound
rather than merely permissive: checking

```zel
min : Comparable a => a -> a -> a
min x y =
  …
```

would let the body solve `a := Int`, at which point the obligation the checker proves is
`Comparable Int` — discharged, declaration accepted — while the signature still promises
`Comparable a` for every `a`. The compiler would have proved something strictly weaker than
what it published, and no later phase would notice.

Inside a declaration's own body, `a` has to be **rigid**: an opaque constant whose only
operations are the ones its context provides. That is precisely `LANG-12`'s skolemization, and
it is what makes "given `Comparable a`, `a` has a `compare`" a statement the checker can hold.
At a *call site* the opposite is true and needs nothing new — `a` is instantiated fresh and the
obligation is discharged against a concrete type.

**Problem:** the typer has no notion of an obligation. `Constraint` is a pair of types plus an
`Origin`, `unify` solves each one on sight, and the only thing resembling a class today is
`Type::Number` — a hard-coded case that unifies with `Int`, `Float` and itself and is never
recorded, deferred, or reported as unsatisfiable. It is the degenerate ancestor of what this
ticket builds, and [CLASS-5](class-5.md) is what retires it.

**Approach:**

1. **A class obligation is a second kind of constraint**, not a new `Type` case. `SPEC-12`
   decision 5 — no higher-kinded variables — is what makes this simple: a class is always over
   a complete type, so an obligation is a (class name, type) pair and never a partial
   application. Keep it in the same `Vec` as the equalities, for the reason `CLAUDE.md` gives:
   constraints live in a `Vec` and not a `HashSet` because deduplication drops provenance and an
   unordered collection makes *which* error is reported vary between runs.

2. **Obligations are deferred, not solved on sight.** `Comparable t7` cannot be answered while
   `t7` is unsolved. The usual shape: `unify` collects obligations as it goes, applying each
   substitution to them the way it already applies one to the remaining equalities, and a
   second pass discharges what is left against the instance environment. Order matters here for
   the same reason it already does in `unify` — it decides which of several unsatisfiable
   constraints is the one reported.

3. **Provenance carries over unchanged, and must.** Every obligation records an `Origin` — the
   span it came from and a `Reason` naming why. `Reason` gains at least one variant for *this
   call requires an instance*, and its `describes()` / `explains()` / `note()` arms are written
   for the reader. Note `Reason::describes` names no type deliberately, and that reasoning
   applies to obligations too: by the time one fails, substitution has moved types around and
   neither side is reliably the type of the text under the caret.

4. **The rigid half.** Inside a constrained declaration's body, the context's obligations are
   *given* rather than proved: `Comparable a` with `a` rigid is discharged by the annotation
   itself, and any obligation on `a` that the context does not provide is an error. This is the
   interesting case and the one to write tests around first.

5. **New `ErrorKind` variants.** At minimum: no instance for this class and this type; and an
   obligation on a rigid variable its context does not provide. Each with a `message()` in the
   user's vocabulary — the second one especially, because *`a` is not `Comparable` here* is
   meaningless without saying that `a` is the annotation's own variable and the fix is to add
   the constraint. `ERR-4`'s labelling gives the caret and the secondary label for free once the
   obligation carries an `Origin`.

**What this ticket does not reach.** `type_check` skips a declaration whose body it cannot
translate, and that is most of what a constraint would be about in `std/core`: probed, 45 of the
package's 133 values are in `module javascript` facades that `type_check` returns early on, and
43 more are bare facade re-exports (`add = Js.Basics.add`) that `value_to_term_and_annotation`
returns `None` for. `min`, `compare`, `add` and `append` are all in that second set. So the
solver built here will be exercised by tests and by user code long before it is exercised by the
standard library, and [CLASS-6](class-6.md) is where that changes. Do not read a green
`cargo run` as evidence this ticket works.

**Acceptance:** tests in `tests/typer.rs`, each with its neutralised-and-seen-red counterpart
per `CLAUDE.md`'s *A green test proves nothing until you have seen it fail*:

- A call whose obligation is discharged by a concrete instance checks.
- A call whose obligation has no instance is a type error naming the class and the type, with a
  `diagnostic.labels[..]` assertion putting the caret under the call and not under the
  declaration.
- A constrained declaration whose body uses only its context's operations checks; one that uses
  an operation its context does not provide is an error.
- `min : Comparable a => a -> a -> a` with a body that forces `a := Int` is an error, not a
  silent specialisation — the `LANG-12` interaction, and the reason this ticket is sequenced
  after it.

`cargo run` still prints `parsed 8 modules` and lists all eight as checked.
