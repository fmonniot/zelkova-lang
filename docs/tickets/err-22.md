# ERR-22 · A type error between a scalar and a same-named union spells both types alike

**Sizing:** small. One predicate in `AdtNames::collide` and the pinned message in one test.

**Location:** `crates/zelkova-compiler/src/typer/mod.rs` — `AdtNames::collide`, `Type::collect_adt_names`;
`crates/zelkova-compiler/tests/typer.rs` — `a_module_declaring_its_own_int_does_not_get_the_scalar`.

**Found:** while reviewing the PR for `LANG-41`, whose test pinned the new message. Left unfixed there because widening `collide` is a diagnostics change outside that ticket's Acceptance.

**Problem:** a message qualifies its unions only when two *unions* share a bare name. A scalar (`Type::Literal`, such as `Basics.Int`) is not a union, so `collide` never compares it against one. A module that declares its own `type Int = MkInt` and then writes `answer : Int` / `answer = 42` is rejected with ``cannot match `Int` with `Int` ``, naming two different types with one spelling and giving the reader nothing to act on. The test above pins that message.

**Fix:** make `collide` treat a scalar's name as a name to compare against the unions in the same message, so that the pair renders as `Test.Int` against `Basics.Int`. Which spelling a scalar takes when qualified is the one open choice; the ticket does not pick, and the answer should follow how a union from `Basics` is already written.

**Acceptance:** `a_module_declaring_its_own_int_does_not_get_the_scalar` asserts a message in which the two types are spelled differently, and goes red when the new comparison in `collide` is removed. `cargo test --workspace` stays green.
