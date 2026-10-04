# BUG-50 · A cycle of parameterless bindings that runs through an instance member is accepted and fails when the module loads

**Severity:** low (a program no one in the tree writes: it needs a cycle through a class member
with no parameters, and the failure is a load-time error and never a wrong answer — but it is
accepted, where the same cycle among declarations is a diagnostic).

**Location:** `crates/zelkova-compiler/src/ir/specialise.rs` — `initialisation_items`, whose doc
comment says a cycle "is left as it is, in name order" and that one through an instance "is a
language question and not this function's"; `crates/zelkova-compiler/src/canonical/` and
`crates/zelkova-compiler/src/dependencies.rs`, where `Error::SelfDependency` is found over the
declarations of one module and instance bindings are not in the graph;
[*A binding may not depend on itself*](../spec/evaluation-semantics.md#a-binding-may-not-depend-on-itself).

**Problem:** `SelfDependency` reads the declarations a name mentions. An instance's bindings are
not nodes of that graph, because which instance a name reaches is a fact about types. So this
builds:

```zel
module Cycle exposing (..)

class Pick a where
  pick : a

instance Pick Int where
  pick =
    other

other : Int
other =
  pick
```

`other` mentions the member `pick`, which is `Pick Int`'s, which is `other`. With a test
`Test.equal other 0` in a package that depends on it, `cargo run -- test <package>` checks and
emits it, then reports

```
ERROR CycleTests.reads: module failed to load: Cannot access 'other' before initialization
```

`specialise::initialisation_items` does read what each parameterless item mentions, through
instance members and specialisations, so it sees the cycle; it places each item in the cycle once
and returns, which is the behaviour the doc comment describes. Noticed while working
[GEN-24](README.md) (PR #321) and deliberately left unfixed there: whether the language rejects
such a cycle, and what it counts as a mention, is the language owner's to say.

**Fix:** undecided. The rule's text covers "a parameterless binding that depends on itself", and
an instance member bound with no parameters is one, so the likely answer is that it is an error;
what is open is where it is found.

1. **After specialisation, in this pass.** `initialisation_items` already has the graph. Have it
   report the cycle as a `specialise::Error` naming the items in it, and make `specialise` return
   it. The cost is that the diagnostic arrives after type checking and speaks of items the user
   may not have written (a specialisation), and a cycle through a constrained function exists
   only per key.
2. **In the type checker or canonicalization.** The member a name reaches is known once types
   are, so it would be a new check over the typer's resolved references. The cost is a second
   graph beside `dependencies.rs`.
3. **Say the rule does not cover it** and make the runtime answer defined, which `const`
   initialisation cannot give without changing what a parameterless binding is.

This ticket does not pick. Whichever is chosen, [*A binding may not depend on
itself*](../spec/evaluation-semantics.md#a-binding-may-not-depend-on-itself) gets an instance
example and a `SPEC-` ticket files the change if the chapter has to move.

**Acceptance:** the program above fails `cargo run -- compile` and `cargo run -- test` with a
diagnostic that names the cycle and exits non-zero, and does not emit `Cycle.mjs`; a test in
`crates/zelkova-compiler/tests/ir.rs` (option 1) or `typer.rs` (option 2) pins it and is seen red
without the check; and `initialisation_items`'s doc comment no longer calls the cycle a
language question.
