# LANG-70 · A constraint in an annotation is resolved, and its context reaches the canonical module

**Sizing:** medium. The checks are small; the size is in carrying the validated context to the
two places [LANG-40](lang-40.md) reads it from, one of which is another module's `Interface`.

**Location:** `crates/zelkova-compiler/src/canonical/mod.rs` — `validate_context`,
`InvalidConstraintKind`, the loop over `source.functions` in `canonicalize_recovering` that
validates a context and then drops it, `Value::TypedValue`, `Broken`, `Module::to_interface`;
`crates/zelkova-compiler/src/lib.rs` — `Interface::values` and `Interface::infix_functions`;
`crates/zelkova-compiler/src/canonical/environment.rs` — `RootEnvironment`, which
[LANG-39](README.md) has given a class table.

**Depends on:** [LANG-39](README.md), for a class table to resolve against and for the
canonical constraint type it introduces for class heads and instance contexts.
[LANG-71](README.md), closed, is why the context arrives as a list.

**Found while:** working [LANG-37](README.md), which made `Comparable a => a -> a` parse and
validated its *shape* only. It left three things undone on purpose, because each needs a class
to exist, and `LANG-39`'s own Problem is the declarations and not their use in an annotation.

**Decided:** the chapter says a constraint
["names a class and the variable that class applies to"](../spec/type-classes.md#constraining-an-annotation),
and that the variable must be one the type mentions. The second half was written into the
chapter by the session that brought these tickets up to date
([DEC-24](../decisions/dec-24.md#what-the-session-settled-without-asking)).

**Problem:** `validate_context` accepts "an uppercase name applied to one or more arguments" and
resolves nothing, so today this canonicalizes with no error at all:

```zel
min : Nonsense a => a -> a -> a
```

Nothing is reported about `Nonsense`, and after canonicalization nothing records that the
annotation had a context, because the loop drops it. Three gaps, all in the same place:

1. **The class name is not resolved.** `Nonsense` is not declared anywhere and passes.
2. **The constraint's argument is not restricted.** `validate_context` takes any number of
   arguments of any shape, so `Comparable Int a => …` and `Comparable (Maybe a) => …` are both
   accepted. A constraint has exactly one argument, it is a type variable, and that variable
   occurs in the annotation's type.
3. **The validated context is dropped.** Nothing downstream can read it, so
   [LANG-40](lang-40.md) has no context to treat as given inside the body, and an importer has
   no way to learn that the function it calls is constrained.

**Approach:**

1. **The checks.** Each is a new `canonical::Error` variant or a new `InvalidConstraintKind`
   case, written per `CLAUDE.md`'s *An error has to describe itself*, with its span on the
   offending constraint and not on the whole annotation. `(Int, Char) =>`'s
   one-error-per-constraint reporting is the model: every bad constraint of a context is
   reported, each at its own span. Resolve the class through the routine `LANG-39` wrote for a
   superclass and an instance context; an annotation's constraint is the third caller of it and
   should not be a second implementation.

   A constraint repeated in one context, and one already implied by another through a
   superclass (`(Eq a, Comparable a) =>`), are both legal: the chapter says the second "says
   nothing more", not that it is an error.

2. **The context is a field beside the annotation's type, not a case of `canonical::Type`.**
   `Value::TypedValue` gains the list of resolved constraints; so does `Broken`, for a
   declaration whose body failed and whose annotation did not. That keeps a context out of
   every match over `Type`, which is the argument `parser::FunType::context`'s doc comment
   makes for the parser AST, and it is enough: [a member signature carries no
   context](../spec/type-classes.md#declaring-a-class), so nothing needs a context *inside* a
   type. Say so in a doc comment at the field.

3. **The context crosses the module boundary with the type.** `Interface::values` and
   `Interface::infix_functions` map a name to a `(NodeSpan, canonical::Type)`. An importer's
   call to a constrained function is checked against that entry, so the entry has to say the
   function is constrained. Put the context **in the entry** — the pair becomes a small struct
   holding the span, the context and the type — and not in a parallel map keyed the same way.
   [TIDY-10](tidy-10.md) is what a parallel map costs: a miss reads silently as "no context",
   which here is a constrained function called with nothing checked.

4. Replace the discard comment above the loop in `canonicalize_recovering` with one that says
   where the context now lives, in the same commit. `Error::FacadeConstrained` is unchanged: a
   facade signature with a context is still rejected, and still at the context's span.

The typer still reads only the type. Nothing here changes what type checks.

**The spec blocks this will break.** The `expect=ok` blocks under *Constraining an annotation*
(`min`, `describe`, and the four-constraint `four`) name `Comparable` and `Eq`, and nothing in
them declares either: they pass today only because a class name is never looked up. Once it is,
they fail on the unresolved class, and [LANG-42](lang-42.md) — the first ticket to declare `Eq`
and `Comparable` where a block can see them — is later than this one. Make each block declare
the class it names (the chapter's own *Declaring a class* blocks are the form) and keep them
`expect=ok`. The same goes for the block in
[`docs/spec/evaluation-semantics.md`](../spec/evaluation-semantics.md) that annotates
`alike : Eq a => …`, if it does not already fail for another reason; read what it fails on.
Shorten the chapter's `**Not implemented:**` paragraph under *Constraining an annotation* to
what still holds — the constraint is resolved and kept, and still asks nothing of a caller — and
drop its `LANG-70` link. `cargo test --test spec` will not tell you a block needs this until it
is red, so run it before assuming the change is clean.

**Acceptance:** tests in `crates/zelkova-compiler/tests/canonical.rs`, each asserting the variant
and a `diagnostic.labels[..].range` caret (`NodeSpan`'s `PartialEq` is blind), each seen red
with its check neutralised:

- `min : Nonsense a => a -> a -> a` is an error naming `Nonsense`, with the caret under the
  constraint and not under the annotation.
- `Comparable Int => …`, `Comparable (Maybe a) => …` and `Comparable a b => …` are each an
  error. `Eq b => a -> a` is an error naming `b`.
- A constraint naming a declared class resolves, and the context is on the canonical value —
  an assertion on the value `LANG-40` will read, not on the absence of an error.
- A class imported from another module resolves through the import, and the importing module
  sees the exporting module's constrained function *with its context* in the `Interface` it was
  canonicalized against.
- A constrained function behind an exposed operator carries its context in
  `Interface::infix_functions`.

`cargo test --workspace` is green, with the blocks above still `expect=ok`.
`cargo run -- compile std/core` still prints `parsed 10 modules`, lists all ten as checked and
exits 0.
