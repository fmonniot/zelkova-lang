# CLASS-5 · Retire `Type::Number` in favour of a `Number` class, defaulting to `Int`

**Sizing:** medium. Small in the unifier, larger in what it forces everywhere `Type::Number` is
rendered or asserted on.

**Location:** `src/compiler/typer/mod.rs` — the `Number` variant of `Type`, both `Display`
arms for it (the `Debug`-flavoured one and the user-facing one), `Signature::of_type` in the
test module; `src/compiler/typer/constraint.rs` — the integer-literal arm of `collect`, which
constrains a literal to `Type::Number`; `src/compiler/typer/unifier.rs` — `is_numeric` and the
`(Type::Number, other)` arm of `unify_one_constraint`; `tests/typer.rs`, whose expectations are
written against the `number` spelling.

**Depends on:** [CLASS-4](class-4.md).

**Decided (`SPEC-12`, by the language owner):** an otherwise-undetermined `Number` constraint
resolves to `Int`. Every other constraint the solver cannot discharge is an error. There is no
`default` declaration form.

**Problem:** `Type::Number` is the type an integer literal gets, and it is a class constraint
wearing a type's clothes. It unifies with `Int`, `Float` and itself, and with nothing else —
which is exactly `Number a => a`, hard-coded, with no instance environment behind it and no way
for a user type to join. Once `CLASS-4` exists, keeping it means two mechanisms for one idea,
and the hard-coded one is the one that cannot be extended, cannot be reported on properly, and
has no source syntax.

It also has no way to *fail*. `unify` answers `Number` against a numeric type and moves on; a
literal that ends up unconstrained simply stays `Number` forever, which is the whole reason the
defaulting question was open.

**Interaction with [ERR-13](err-13.md), which this ticket makes moot.** ERR-13 is about how
`Type::Number` is *spelled* in a diagnostic: `Display` renders it `number`, which the language
reads as an ordinary type variable, so *cannot match `Char` with `number`* names a variable the
reader's source does not contain. This ticket deletes the variant and the spelling with it. That
is knowingly duplicated work and it is still worth doing ERR-13 first: it is one `Display` arm,
and this ticket is several tickets away behind `CLASS-1` … `CLASS-4`, so the wrong message would
otherwise stand for the whole of that time. Close ERR-13 on its own terms and let this one
supersede it.

**Interaction with [SPEC-6](spec-6.md).** What an integer literal's type *means* — whether
being usable as an `Int` or a `Float` is a rule of the language or an implementation detail —
belongs to the *Expressions* chapter, which `SPEC-11` handed it to. This ticket implements a
decision that chapter has to describe. If the two disagree when it is written, the chapter wins
and this becomes a `LANG-` ticket.

**Approach:**

1. An integer literal collects a `Number` obligation instead of a `Type::Number` equality, and
   its type becomes a fresh variable.
2. `Type::Number`, `is_numeric` and the special arm in `unify_one_constraint` go away.
3. Defaulting: after the discharge pass, an undischarged `Number` obligation on a variable that
   nothing else determines resolves to `Int`. Confine this to `Number` — decision 8 is
   deliberately narrow, and a general defaulting mechanism is a different ticket nobody has
   asked for.
4. `std/core` declares `Number` with instances for `Int` and `Float`, which is what makes step 1
   dischargeable at all. That is [CLASS-6](class-6.md)'s subject; this ticket needs enough of it
   to be testable, so either sequence CLASS-6 first or build the class in a test fixture and say
   which.
5. `Signature::of_type` renders the same type in test assertions, and its audience is different
   from a diagnostic's — ERR-13 flags this and the judgement is worth making explicitly rather
   than by following whichever arm was edited first.

**Acceptance:** tests in `tests/typer.rs`. `x = 1` with nothing else constraining it infers
`Int`. `x : Float` with a body of `1` still checks. `x : Char` with a body of `1` is an error
whose message contains no spelling the grammar would accept as a type variable — the assertion
ERR-13 asked for, surviving into this ticket. `Type::Number` no longer exists: `grep -rn
"Type::Number" src/` returns nothing. `cargo run` still prints `parsed 8 modules` and lists all
eight as checked.
