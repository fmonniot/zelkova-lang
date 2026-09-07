# SPEC-27 · A derivation's `combine` must be a monoid and nothing checks it, at any point

**Sizing:** small as an investigation — the deliverable is a decision plus the paragraph in
[`docs/spec/type-classes.md`](../spec/type-classes.md) that records it. Medium-to-large if the
decision is to check something: two of the four candidates below need machinery the tree does not
have (a compile-time evaluator, or a test runner).

**Location:** [`docs/spec/type-classes.md`](../spec/type-classes.md) —
*[What a derivation is trusted to keep](../spec/type-classes.md#what-a-derivation-is-trusted-to-keep)*
and the *Holding a derivation to its law* entry under *Open questions*. If a check is adopted it
lands in the type-class ticket program: [`LANG-38`](lang-38.md) is where a class body is parsed
and its derivation's shape could be constrained, [`LANG-39`](lang-39.md) where a class and its
instances are resolved.

**Problem:** a class that carries a derivation supplies `matched`, `differed` and `combine`, and
the walk folds a constructor's arguments with them. That fold is only meaningful if **`combine` is
associative and `matched` is its identity on both sides** — the chapter says so, and says that
nothing establishes it.

The failure is silent and its shape is known: an answer whose meaning depends on how many things
were combined. `matched = 1.0`, `differed _ _ = 0.0`, `combine x y = divide (add x y) 2.0` — the
average of the arguments' scores — weights a three-argument constructor's first argument a half
and its last an eighth, and folds `matched` in as though it were a fourth argument. It compiles,
and the number of arguments a variant happens to have changes the answer.

Two things make this worse than an ordinary unchecked property. It is not the class author who
pays: a derivation is written once and every `derived` instance of that class inherits it, so the
wrong answer surfaces in code that did nothing but ask for the obvious definition. And a
derivation is a compile-time construct — the compiler is *writing* the code that folds — so this
is one of the few places where the compiler could in principle look at the thing it is trusting.

**What is true of the tree today**, and what each candidate would therefore cost:

- There is no evaluator. `src/compiler/` ends at the typer; `exhaustiveness.rs` is a stub and code
  generation has not started ([`GEN-1`](gen-1.md)). Nothing can run a Zelkova expression, at
  compile time or otherwise.
- There is no test root and nothing runs a package's tests ([`LANG-15`](lang-15.md)), so
  "discharge it as a test obligation" has no runner to discharge it in.
- The typer is Hindley–Milner over `Term`/`Constraint` and has no vocabulary for a property of a
  function's *values*, only of its type.

**The candidates, none of which this ticket picks:**

1. **Nothing, stated as a decision.** The law stays prose and the chapter says the compiler will
   not check it, the way every language with a `Monoid`-shaped abstraction does. Cheapest, and
   the honest baseline the other three have to beat.
2. **A shape restriction on `combine`.** Require it to be a `case` on one argument whose branches
   return the other argument or a constant. This accepts `Eq`'s and `Comparable`'s and rejects
   the averaging derivation — and it also rejects `combine = add` with `matched = 0`, which is a
   perfectly good monoid the chapter itself offers. A syntactic rule that forbids a correct
   program is worth writing down as considered and probably not worth adopting.
3. **Exhaustive checking when the answer type is finite.** A member's derivation has signature
   `a -> a -> R` with the class variable absent from `R`. Where `R` is a union of nullary
   constructors, its values can be enumerated: `Bool` has two and `Order` has three, so the three
   equations are 4, 4 and 8 closed evaluations respectively. That covers exactly the two classes
   `std/core` derives ([`LANG-42`](lang-42.md)) and reaches nothing over `Int`, `Float` or a type
   with arguments. It needs an evaluator for the fragment of the language a derivation's three
   bindings may use, which is the first piece of `GEN-1`'s job pulled forward — the question is
   whether a check that covers the finite cases and stays silent on the rest is worth that, or
   whether partial coverage is worse than none because it reads as a guarantee.
4. **A test obligation.** Generate the three equations as a test of the package declaring the
   class, and let a test run fail. Later than compile time, earlier than the program, and it
   works for any `R` whose values can be produced. Gated on [`LANG-15`](lang-15.md), and on a way
   to obtain values of an arbitrary `R` — which is the same "run the walk backwards from a
   description of the type" the chapter rules out for members taking no `a`, so it may only be
   reachable for finite `R` too, in which case candidate 3 subsumes it.

**Approach:**

1. Decide, with the language owner, between the four. The question to settle first is whether a
   check that covers only a finite `R` is acceptable, because candidates 3 and 4 both collapse to
   "the finite case" and candidate 1 is what remains if it is not.
2. Write the decision into
   *[What a derivation is trusted to keep](../spec/type-classes.md#what-a-derivation-is-trusted-to-keep)*
   as a rule or as a stated non-check, with the reason, and delete the *Holding a derivation to
   its law* entry from that chapter's *Open questions*.
3. If a check is adopted, file the `LANG-` ticket that implements it, naming the phase it runs in
   and the diagnostic it produces, and add a tagged block to the chapter for a derivation that
   fails it.

**What this is not.** Not a general property or law language for classes — a class states no laws
about its members today and this ticket does not propose that it should. The scope is the one law
the *derivation mechanism itself* depends on, which is why the compiler has standing to check it
at all: it is the compiler that writes the fold.

**Acceptance:** [`docs/spec/type-classes.md`](../spec/type-classes.md) states either the rule or
the deliberate non-check, and its *Open questions* no longer carries the *Holding a derivation to
its law* entry. `grep -n "SPEC-27" docs/` returns only this file's own row in
[`docs/tickets/README.md`](README.md) once it closes. `cargo test --test spec` green, with any new
block tagged `expect=unimplemented` and proven to fail.

**Found:** while reviewing the derivation sections of
[Type classes](../spec/type-classes.md#what-a-derivation-is-trusted-to-keep) on 2026-09-06. The
chapter's account of the law was cut to the two sentences a specification needs; the question of
whether anything can enforce it is what this ticket keeps.
