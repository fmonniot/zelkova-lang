# LANG-12 · An annotation more general than its body is accepted and silently specialised

**Sizing:** medium. The change is small in `infer_annotated`; deciding what a type variable in
an annotation *is* is the part that takes thought, and since [LANG-40](README.md) it is also
what a given constraint is a constraint *on*.

**Location:** `crates/zelkova-compiler/src/typer/mod.rs` — `infer_annotated`, which turns the
annotation into one ordinary `Constraint` against the body's inferred type;
`value_to_term_and_annotation`, which runs the annotation through
`canonical_type_to_typer_type` with a fresh `var_map`, so each type variable written in the
source becomes a fresh **unification** variable; and the discharge of a class obligation
[LANG-40](README.md) added, whose *given* constraints are on those same variables.

**Depends on:** [LANG-42](lang-42.md). **This ticket closes [the type-class
order](README.md#active-work-type-classes)**, where it used to open it
([`DEC-24` decision 10](../decisions/dec-24.md#10--lang-12-closes-the-order-instead-of-opening-it)).
Until `LANG-42`, `std/core`'s `Basics` holds thirteen declarations this ticket rejects —
`add : a -> a -> a` over a body of `Js.Basics.addInt`, and its siblings — and only the class
mechanism can make them honest. After `LANG-42` there is nothing in the library left for it to
reject, and it is what makes the mechanism sound.

**Decided (`SPEC-5`, by the language owner):** a type annotation is a promise to callers. The
declared type must be no more general than what the body can actually support; a declaration
whose body cannot honour the type it claims is a type error
([*An annotation is a promise*](../spec/types.md#an-annotation-is-a-promise)).

`f : a -> a` says "give me anything and I give you back that same thing". A body that always
returns a particular `T` cannot do that, and a caller reading the annotation and passing a
`Char` would be misled by a signature the compiler had quietly narrowed behind their back.

**Problem:** a variable written in an annotation is a unification variable, so it happily
unifies with whatever the body turns out to need. This checks clean today:

```zel
type T
  = C

f : a -> a
f x = C
```

`a` is solved to `T` and no error is reported. The annotation in the source and the type the
compiler ends up with are two different things, and only the second one is real.

With classes it is worse than permissive. `LANG-40` discharges an obligation still on one of
the annotation's variables by the annotation's own context, and discharges one at a concrete
type by an instance. So

```zel
min : Comparable a => a -> a -> a
min x y =
  if lt x 0 then x else y
```

lets the body solve `a := Int`, proves `Comparable Int`, and publishes `Comparable a` — and
[GEN-24](gen-24.md) then specialises `min` at `Colour` out of a body that only ever worked for
`Int`. `LANG-40` carries a test that pins this as accepted, with a comment naming this ticket.

**Approach:** the annotation's variables have to be **rigid** — universally quantified by the
declaration, and therefore unifiable only with themselves. The usual shape is to skolemize:
replace each variable of the annotation with a fresh opaque constant before constraining, and
report a type error when `unify` tries to solve one against anything else. That needs a new
`Type` case (or a marker on the existing variable case) that `unifier::unify` refuses to
substitute, plus an `ErrorKind` whose message is written for the reader: the annotation
promises any type, and the body only produces this one.

`Reason` already has an `Annotation` variant with the right wording for the secondary label —
*expected because of this type annotation* — so the existing labelling carries over; what is
new is the primary message.

Rigid variables are per declaration and exist only while its own body is checked. At a *use* of
the declaration nothing changes: `Types::by_name` instantiates every variable fresh, as it does
today.

Three things follow in the class machinery, and each is a simplification:

- **A given is on a rigid variable.** `LANG-40`'s "an obligation still on a variable is
  discharged when a given is on the same variable" becomes exact: a given can no longer follow
  its variable to a concrete type, because the variable can no longer go anywhere.
- **An instance's bindings** are checked with the head's variables rigid, for the same reason:
  `instance Eq a => Eq (Box a)` whose `eq` only works when `a` is `Int` is the same lie.
- **A derivation's bindings** mention no variable of the class, and a derived instance's
  generated definitions are honest by construction; neither should need a change. Check.

Two things must keep checking, and are the real test of the change:

- `f : a -> a` with `f x = x` — the honest polymorphic identity. Its body genuinely works for
  any `a`, so the rigid variable never needs solving.
- All of `std/core`, `std/test` and `tests/fixtures/`. `LANG-42` removed the declarations this
  ticket was known to reject; it did not go looking for others. One that turns up is an
  annotation that promises more than its body delivers: narrow the annotation, or give it the
  constraint it was missing. If neither is possible without changing what the declaration
  means to its callers, stop and say so rather than weakening the check.

**Acceptance:** tests in `crates/zelkova-compiler/tests/typer.rs`, each seen red:

- `f : a -> a` with `f x = C` is a type error naming the annotation, with a
  `diagnostic.labels[..].range` assertion; `f : a -> a` with `f x = x` still checks.
- `min : Comparable a => a -> a -> a` with a body that forces `a := Int` is an **error**. This
  is the test `LANG-40` left pinned the other way: turn it round, and delete its comment.
- An instance binding that forces one of the head's variables to a concrete type is an error.
- A constrained declaration whose body uses only what its context provides still checks, at a
  rigid variable.

`cargo test --workspace` and `node --test 'tests/js/**/*.mjs'` pass.
`cargo run -- compile std/core` prints `parsed 10 modules`, lists all ten as checked and exits
0, and `cargo run -- test std/core` passes every test with the count `CLAUDE.md` states.

**The spec blocks.**

- The block in [`docs/spec/types.md`](../spec/types.md)'s *An annotation is a promise* section —
  `f : a -> a` with a body of `Small` — goes red by itself: it is `expect=ok`, and that means
  the block type checks. Retag it `expect=type-error:` with the kind the new error carries, and
  delete the `**Known gap:**` paragraph beside it.
- The `double` block in [`docs/spec/expressions.md`](../spec/expressions.md) **will not go
  red**, and [SPEC-36](spec-36.md) is the ticket about why. With this ticket landed the failure
  its paragraph describes is finally available: give the block a class with a `mul` member and
  an `Int` instance, and `mul x 2` under `Number a => a -> a` is the error the paragraph
  claims. Retag it `expect=type-error:` with the same kind, delete the paragraph's
  `**Not implemented:**` half, and close `SPEC-36` with this ticket.
- [`docs/spec/type-classes.md`](../spec/type-classes.md), *Numeric literals*, makes the same
  claim in prose and needs no change.

The order is finished when this lands. `CLAUDE.md`'s *Language notes* stop pointing at the
type-class tickets, and the *Active work: type classes* section of [the index](README.md) goes.
