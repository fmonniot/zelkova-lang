# LANG-12 · An annotation more general than its body is accepted and silently specialised

**Sizing:** medium. The change is small in `infer_annotated`; deciding what a type variable in
an annotation *is* is the part that takes thought.

**Location:** `src/compiler/typer/mod.rs` — `infer_annotated`, which turns the annotation into
one ordinary `Constraint` against the body's inferred type; and
`value_to_term_and_annotation`, which runs the annotation through
`canonical_type_to_typer_type` with a fresh `var_map`, so each type variable written in the
source becomes a fresh **unification** variable.

**Decided (`SPEC-5`, by the language owner):** a type annotation is a promise to callers. The
declared type must be no more general than what the body can actually support; a declaration
whose body cannot honour the type it claims is a type error.

`f : a -> a` says "give me anything and I give you back that same thing". A body that always
returns a particular `T` cannot do that, and a caller reading the annotation and passing a
`Char` would be misled by a signature the compiler had quietly narrowed behind their back.

**Problem:** a variable written in an annotation is a unification variable, so it happily
unifies with whatever the body turns out to need. Probed — this checks clean today:

```zel
type T
  = C

f : a -> a
f x = C
```

`a` is solved to `T` and no error is reported. The annotation in the source and the type the
compiler ends up with are two different things, and only the second one is real.

Found while writing [`docs/spec/types.md`](../spec/types.md) (`SPEC-5`).

**Approach:** the annotation's variables have to be **rigid** — universally quantified by the
declaration, and therefore unifiable only with themselves. The usual shape is to skolemize:
replace each variable of the annotation with a fresh opaque constant before constraining,
and report a type error when `unify` tries to solve one against anything else. That needs a
new `Type` case (or a marker on the existing variable case) that `unifier::unify` refuses to
substitute, plus an `ErrorKind` whose message is written for the reader: the annotation
promises any type, and the body only produces this one.

`Reason` (`typer/mod.rs`) already has an `Annotation` variant with the right wording for the
secondary label — *expected because of this type annotation* — so the existing labelling
carries over; what is new is the primary message.

Two things must keep checking, and are the real test of the change:

- `f : a -> a` with `f x = x` — the honest polymorphic identity. Its body genuinely works for
  any `a`, so the rigid variable never needs solving.
- `std/core/src/Basics.zel`'s `eq : a -> a -> Bool` and
  `append : appendable -> appendable -> appendable`, whose bodies are `Js.Utils` facade values
  annotated with the same variables. If a rigid variable makes those stop checking, the
  interaction is with how a `module javascript` facade's type flows in, not with this rule —
  investigate before relaxing anything.

Constrained type variables (`number`, `comparable`, `appendable`) are an unresolved design
question and this ticket must not settle it by accident — see `docs/spec/README.md`'s planned
*Constrained type variables* chapter ([SPEC-11](spec-11.md)). Treating them as ordinary rigid
variables is the conservative reading and is what the tree does today.

**Acceptance:** `f : a -> a` with `f x = C` is a type error naming the annotation, and
`f : a -> a` with `f x = x` still checks — tests in `tests/typer.rs`. `cargo run` still prints
`parsed 8 modules` and lists all eight as checked.

**This gap has no red test behind it.** The spec harness stops at canonicalization and never
runs the typer, so the block in [`docs/spec/types.md`](../spec/types.md)'s *An annotation is a
promise* section canonicalizes cleanly both before and after this fix and its `expect=ok` tag
stays green. Delete the `**Known gap:**` paragraph by hand as part of this ticket.
[TEST-2](test-2.md) is what would make it a red test instead.
