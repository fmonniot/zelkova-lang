# SPEC-34 · `package_declares_a_default` conflates "holds a default module" with "is `zelkova-core`", and a package's own `Basics` now declares an ordinary `Int` beside the seeded scalar

**Sizing:** small-to-medium (a decision for the language owner, per [`DEC-15`](../decisions/dec-15.md)
and [`DEC-17`](../decisions/dec-17.md), plus whatever code follows from it — could stay small if
the decision is "leave it", or touch `default_imports.rs`, `environment.rs` and every call site
of `declares_a_default` if it is not).

**Location:** `src/compiler/default_imports.rs` — `declares_a_default` (~line 230), which asks
"does this set of module names include one of the eight" and is the one flag both of the
following read; `src/compiler/canonical/environment.rs` — `new_environment`'s scalar-seeding
block (~line 359), gated on `package_declares_a_default`; `docs/decisions/dec-17.md` — decision
1, "The exception is `zelkova-core`, and it is all-or-nothing", and decision 3, "The five scalar
names go to every module of `zelkova-core`", both of which name the flag's *intended* scope as
`zelkova-core` while the flag itself is computed from module names alone.

**Problem:** `declares_a_default` is deliberately computed from a package's module names, not
its `PackageName` — [`DEC-17`](../decisions/dec-17.md)'s own text explains why:
`compile_package` only ever compiles one package at a time, so there is no second package's name
to compare against, and the rule "no other package may declare a module under one of
`zelkova-core`'s names" (an *unwrapped* collision) held for every case DEC-17 could observe when
it was settled.

[`BUG-37`](README.md) changes what that assumption covers. A package may now depend on another,
**wrapped**, and hold its own module of the same name as one of the eight without any collision
being reported at all — `resolve::visible_modules`' collision rule only fires for two modules
answering to the same *import spelling*, and a wrapped dependency's own `Basics` is spelled
`AcmeBasics.Basics`, not `Basics`. `tests/pipeline.rs`'s `dep_rival_basics`/`acme-basics` fixture
is exactly this: a package that is not `zelkova-core` and holds its own module literally named
`Basics`.

Before `BUG-37`, that package's own `Basics.Int` and the seeded scalar were the same `QualName`
— nothing to tell apart. Since `BUG-37` put the package in every `QualName`, they are two
distinct types inside one package: a bare `Int` anywhere in `acme-basics` resolves to the seeded
scalar (`new_environment` seeds it whenever `package_declares_a_default` is true, which it is
here — `acme-basics` "declares a default module" by holding a module named `Basics`), while
`acme-basics`' own `Basics.Int` is an ordinary union, checked by `a_wrapped_dependencys_basics_declares_no_scalar`
and `another_packages_basics_int_is_not_the_scalar` (both in `tests/pipeline.rs`). This is
consistent with `Scalar::declares`'s package check and is not a miscompile — the two types really
are two types, correctly — but it means "this package declares a default module" and "this
package is `zelkova-core`" are two different questions the compiler currently answers with the
one flag, and the harness had to be rerouted around the seam: `tests/spec.rs`'s
`package_of`/`package_declares_a_default` (~line 164), `package_default_imports`, and
`true_is_javascripts_true` all had to compile as `zelkova-core` specifically — not merely "a
package that declares a default module" — to stay meaningful once scalar identity became
package-qualified.

**Approach:** undecided; this is the question for [`DEC-15`](../decisions/dec-15.md)/[`DEC-17`](../decisions/dec-17.md)
to settle, not something to guess at:

1. **Leave it.** `declares_a_default` stays a question about module names, and a package that
   chooses to name one of its own modules `Basics` (or `Maybe`, `Result`, `Bitwise`) accepts
   getting the default-import suppression *and* the scalar seeding that come with that, exactly
   as `zelkova-core` does — the two questions are the same question by design, and a package
   colliding with core's naming is choosing that shape. This needs no code change; it would mean
   closing this ticket by updating `DEC-17`'s text to say so explicitly, since right now it reads
   as if the flag's scope is `zelkova-core` specifically.
2. **Split the two questions.** Default-import suppression stays keyed on module names (DEC-17
   decision 1's reasoning holds regardless); scalar seeding in `new_environment` is re-gated on
   whether the package **is** `zelkova-core` — which needs `new_environment` (or its caller) to
   be handed the checked package's `PackageName` and compare it against `PackageName::core()`,
   something `compile_package` already knows once per package and does not currently thread down
   this far for this purpose.

Whichever is chosen, `tests/spec.rs`'s `package_of` (or its replacement) and the
`dep_rival_basics`/`acme-basics` fixture pair are the concrete surface to re-check the decision
against.

**Acceptance:**

- `DEC-15` or `DEC-17` gains a decision (or an amendment to an existing one) stating explicitly
  whether "declares a default module" and "is `zelkova-core`" are one question or two.
- If option 2 is chosen: a test with a wrapped dependency holding its own `Basics` (the existing
  `dep_rival_basics` fixture is one) shows a bare `Int` inside that dependency's own modules
  resolving to its own `Basics.Int`, not the seeded scalar — i.e. `new_environment` seeds nothing
  for a package that is not `zelkova-core`, whatever module names it holds.
- If option 1 is chosen (leave it): no code changes, and the decision entry says so along with
  the reasoning, closing this ticket without touching `default_imports.rs` or `environment.rs`.
- `cargo test --workspace` is green either way, and `cargo run` still prints `parsed 8 modules`,
  lists all eight as checked, and exits 0.

**Related:** found in review of [PR #245](https://github.com/fmonniot/zelkova-lang/pull/245)
(`BUG-37`, "put the package in every `QualName`"), which is what first made the two `Int`s in
`acme-basics` distinguishable and surfaced the seam. [`DEC-15`](../decisions/dec-15.md) decision
1 and [`DEC-17`](../decisions/dec-17.md) decisions 1 and 3 are the existing rules this would
amend, not replace.
