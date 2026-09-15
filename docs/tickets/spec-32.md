# SPEC-32 · A module is made ambiguous by an import it never wrote

**Sizing:** small-to-medium. Small if the answer is that the current composition is right and
the chapter says so; medium if a written import is given priority over an implicit one, which
changes `process_import`'s contract.

**Location:** [`docs/spec/name-resolution.md`](../spec/name-resolution.md)'s *Ambiguous rather
than unresolved*, and [`docs/spec/modules.md`](../spec/modules.md)'s *The default imports*,
neither of which says what happens when the two rules meet.
`src/compiler/canonical/environment.rs` — `new_environment`, whose doc comment covers only the
case of a module writing a default entry out verbatim, and `insert_foreign_value`, which is
what turns a second contributor into a `ValueType::Foreigns`.

**Depends on:** the default-import mechanism, which arrives with `LANG-8` (PR #206). The
behaviour below does not exist on `main` until that merges.

**Problem:** *Ambiguous rather than unresolved* says a name brought into scope unqualified by
two different imports becomes an error where the bare name is written. The default imports make
one of those two imports invisible, so a module can be broken by a collision with a line it did
not write and cannot see.

Reproduced with a three-module package — a `Basics` exposing `add`, a `Helper` also exposing
`add`, and a `Main` that mentions neither `Basics` nor anything else:

```zel
module Main exposing (x)

import Helper exposing (add)


x : Int
x = add 1 2
```

`compile_package` rejects it:

```
error: [Main] `add` is exposed by several imported modules
  ┌─ Main.zel:7:5
7 │ x = add 1 2
  │     ^^^ this name is ambiguous
  = it is exposed by: Helper, Basics
```

`new_environment` suppresses a default entry only when the module writes *that same entry*
out; any other import exposing a colliding name goes through `insert_foreign_value` alongside
the implicit one and the use site becomes `AmbiguousVariables`. The diagnostic does label both
declaration sites, so it is not a caret-less error — but `Main.zel` contains no mention of
`Basics`, and the author of `Helper` is who chose the colliding name.

The chapter's stated fix does work: qualifying the use as `Helper.add` compiles. So this is an
ergonomics and diagnostics question rather than a dead end, which is why it was filed separately
from the default-import question `SPEC-31` settled ([`DEC-15`](../decisions/dec-15.md)) and at a
smaller size.

Found while reviewing `LANG-8` (PR #206). Left unfixed there because the PR implements the
mechanism correctly — the composition is a consequence of two rules that were each settled
elsewhere, and picking between the options below is not a change that belongs in the diff that
introduced the mechanism.

**Approach:** the ticket does not pick. Three shapes:

1. **The current composition is right; document it.** An implicit import is an import, and the
   rule is uniform. *The default imports* gains a paragraph saying a default entry participates
   in ambiguity exactly as a written import does, with the qualified spelling as the fix. The
   least work and the least magic; it accepts that a name collision can be reported against a
   module the file does not name.
2. **A written import beats an implicit one it collides with.** `Helper.add` shadows
   `Basics.add` for that module, no error. Ergonomic and matches what a reader of `Main.zel`
   would guess, but it makes shadowing silent in exactly the way
   [`LANG-29`](lang-29.md) is filed to stop for top-level declarations — the two should be
   decided consistently, and if they disagree the chapter has to say why.
3. **Give a module a way to suppress a default entry.** Most expressive, most new syntax, and
   nothing else in the language needs it yet.

Whichever wins, the diagnostic is worth a look on its own account: when one of the two
contributors is implicit, saying so in the note — that `Basics` was supplied rather than
imported — costs one sentence and removes most of the surprise.

**Acceptance:** *Ambiguous rather than unresolved* or *The default imports* states what happens
when a written import collides with a default entry, with a tagged block demonstrating it —
`expect=canonical-error:AmbiguousVariables` under shape 1, `expect=ok` under shape 2. If the
compiler changes, a `tests/pipeline.rs` test over a fixture package pins the chosen behaviour
and goes red under the other shape. `cargo test --test spec` and `cargo run` are green.
