# LANG-70 · A constraint in an annotation is resolved, and its context reaches the canonical module

**Sizing:** medium. The checks are small; the size is in where the validated context lives once
it stops being discarded, because [LANG-40](lang-40.md) reads it from there. Could grow if that
shape turns out to need the canonical `Type` to change.

**Location:** `src/compiler/canonical/mod.rs` — `validate_context`, `InvalidConstraintKind`, and
the loop over `source.functions` in `from_parser_module` that validates a context and then
drops it; `src/compiler/canonical/environment.rs` — `RootEnvironment`, once
[LANG-39](lang-39.md) has given it a class table.

**Depends on:** [LANG-39](lang-39.md), for a class table to resolve against. Nothing here can be
checked until a class can be declared, and `LANG-39` is where one becomes something the
environment knows.

**Found while:** working [LANG-37](README.md), which made `Comparable a => a -> a` parse and
validated its *shape* only. It left three things undone on purpose, because each needs a class to
exist. `LANG-37`'s ticket and the discard-site comment in `from_parser_module` both said
`LANG-39` would do them, but `LANG-39`'s own Problem lists four things — a class's members, an
instance crossing a module, a duplicate instance, an orphan — and none of them is a constraint
in an annotation. Nothing else on the type-class program owns it, and `LANG-40`'s rigid half
assumes it is done.

**Problem:** `validate_context` accepts "an uppercase name applied to one or more arguments" and
resolves nothing, so today this canonicalizes with no error at all:

```zel
min : Nonsense a => a -> a -> a
```

Nothing is reported about `Nonsense`, and after canonicalization nothing records that the
annotation had a context, because the loop drops it. Three gaps, all in the same place:

1. **The class name is not resolved.** `Nonsense` is not declared anywhere and passes. This is
   the same failure `BUG-16` was for an instance head: a name that resolves to nothing has to be
   reported, not accepted.
2. **The constraint's arguments are not restricted.**
   [`docs/spec/type-classes.md`](../spec/type-classes.md), *Constraining an annotation*, says a
   constraint "names a class and the variable that class applies to", and *A class has exactly
   one variable*. `validate_context` takes any number of arguments of any shape, so
   `Comparable Int a => …` and `Comparable (Maybe a) => …` are both accepted. Whether either is
   an error is what the chapter says it is; read it before writing the check, because it is the
   normative side of this and it is the chapter that decides whether a variable that does not
   occur in the type is also wrong.
3. **The validated context is dropped.** The canonical `Type` has no place for one, so
   [LANG-40](lang-40.md)'s rigid half — the context's obligations are *given* inside the body —
   has nothing to read.

**Approach:** the checks (1) and (2) are new `canonical::Error` variants or new
`InvalidConstraintKind` cases; each is written per `CLAUDE.md`'s *An error has to describe
itself*, with a span on the offending constraint or argument and not the whole annotation, and
`(Int, Char) =>`'s one-error-per-constraint reporting is the model.

For (3) **the ticket does not decide the shape.** The context is one of: a field on the
canonical function beside its annotation, or a case on the canonical `Type`. The first keeps a
context out of every match over `Type`, and is what the parser AST did for the same reason
(`parser::FunType::context`'s doc comment has the argument); the second lets a class member's
own signature carry a context if `LANG-38` gives it one. Read `LANG-40`'s Approach step 4 and
`LANG-39`'s class-member paragraph — a member's type outside its class is
`Comparable a => a -> a -> Order` — and pick the one both can read, and say why in a doc comment
at the site. Remove the discard comment in `from_parser_module`, which says this ticket's work
is `LANG-39`'s, in the same commit.

**The spec blocks this will break.** The two `expect=ok` blocks under *Constraining an
annotation* (`min`, `describe`) name `Comparable`, `Eq` and nothing declares them: they pass
today only because a class name is never looked up. Once it is, they fail with the unresolved
class, and `LANG-42` — the first ticket to declare `Eq` and `Comparable` in `std/core` — is
later than this one, so nothing else will repair them. Once `LANG-38` lets a block declare a
class, make each block declare the class it names (the chapter's own *Declaring a class* blocks
are the form) rather than retagging them; they should stay `expect=ok`. Delete the chapter's
**Not implemented:** paragraph after that heading, or shorten it to what still holds, and drop
its `LANG-39` reference if this ticket is what closes it. `cargo test --test spec` will not tell
you the blocks need this until it is red, so run it before assuming the change is clean.

**Acceptance:** tests in `tests/compiler/canonical.rs`, each asserting the variant and a
`diagnostic.labels[..]` caret (`NodeSpan`'s `PartialEq` is blind — `CLAUDE.md`, *An error has to
describe itself*), each seen red with its check neutralised:

- `min : Nonsense a => a -> a -> a` is an error naming `Nonsense`, with the caret under it and
  not under the annotation.
- A constraint whose argument is not a single type variable is an error, in whichever shapes the
  chapter rules out (`Comparable Int`, `Comparable (Maybe a)`, `Comparable a b`).
- A constraint naming a declared class resolves, and the context is reachable from the
  canonical module — an assertion on the value `LANG-40` will read, not on the absence of an
  error.
- A class imported from another module resolves through the import.

`cargo run -- compile std/core` still prints `parsed 8 modules` and lists all eight as checked.
`cargo test --test spec` is green with the two blocks above still `expect=ok`.
