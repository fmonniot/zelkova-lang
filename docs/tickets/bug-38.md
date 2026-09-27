# BUG-38 · A parameterless binding that reaches another only through a function it calls is not ordered after it

**Severity:** high (a miscompile: a well-typed module with no cycle among its parameterless
bindings emits JavaScript that throws a `ReferenceError` at load, and `compile_package` writes
that JavaScript to `build/js/` since [`GEN-13`](README.md)).

**Location:** `src/compiler/canonical/mod.rs` —

- `dependency_graph` (~line 1920), whose nodes are the `is_parameterless` declarations only,
  so a binding with parameters is never a node and nothing its body reads is ever an edge;
- `check_self_dependency` (~line 1972) and `initialisation_order` (~line 2048, `GEN-7`), the two
  readers of that one edge set;
- `Error::SelfDependency` (~line 1175), whose doc comment states the current rule, and its
  `message()` arm (~line 1338), which calls every member of a cycle a parameterless binding.

`src/compiler/javascript.rs`'s module doc comment, item 4 of *The shape of an emitted module*,
cites this ticket as a known defect.

**Problem:** a binding that mentions a function gets no edge to anything that function's body
reads. So a binding that *calls* a function reading another parameterless binding is not ordered
after that binding. Probed on `task/gen-9-emit-a-module` with `check_module` and
`javascript::emit`, `basics_interface()` in the map:

```zel
module Test exposing (a)

a : Int
a =
  f 1

f : Int -> Int
f x =
  z

z : Int
z =
  2
```

emits

```js
function f(x) {
  return z;
}

const a = f(1n);
const z = 2n;

export { a };
```

`const a = f(1n)` reads `z` inside its temporal dead zone, which throws at module load. With the
names the other way round, the name-sorted tie-break in `dependency_graph` happens to put `z`
first and hides it.

The same edge set also lets a cycle through a function past `check_self_dependency`. This module
is accepted, and emits `function f(x) { return a; }` followed by `const a = f(1n);`, which throws
the same way:

```zel
module Test exposing (a)

a : Int
a =
  f 1

f : Int -> Int
f x =
  a
```

[*A binding with no parameters is evaluated
once*](../spec/evaluation-semantics.md#a-binding-with-no-parameters-is-evaluated-once) says a
binding "is evaluated after everything it mentions", and [*Two
outcomes*](../spec/evaluation-semantics.md#two-outcomes) that a well-typed program does not
crash. `a` mentions `f`, not `z`, so whether the rule reaches through `f`'s body is not stated.
The chapter's own example, `shifted = other base`, passes `base` as an argument and so has the
direct edge.

Found by the review of PR #238 (`GEN-9`), whose emitter follows `initialisation_order` as `GEN-7`
gives it.

**Fix:** a binding **depends on** everything it can reach by following mentions — the top-level
declarations its body mentions, the ones *their* bodies mention, and so on, through functions as
well as parameterless bindings. Both the order and the cycle check read that one relation, as
they read one edge set today.

- **The graph has a node for every top-level declaration**, functions included, and an edge
  from `u` to `v` for every `VarTopLevel` reference `u`'s body makes to `v`.
  `collect_top_level_refs` already collects exactly those; only the `is_parameterless` filter
  on the nodes goes.
- **A strongly-connected component that holds a parameterless binding is an error**, and so is
  a parameterless binding with an edge to itself. A component made of functions only is not:
  `isEven`/`isOdd` and `f n = f n` stay legal. Reported as `Error::SelfDependency`, as now, so
  the chapter's `expect=canonical-error:SelfDependency` blocks keep their tag. The error names
  every member of the component and says which are functions; a message that calls `f` a
  parameterless binding is wrong, so the plural arm of `message()` is reworded to fit a mixed
  cycle, e.g. "`a` needs its own value before it has one: `a` mentions `f`, which mentions `a`".
- **`initialisation_order` is the graph's components in dependency-first order, keeping the
  parameterless bindings only.** After the check, every component holding a parameterless
  binding is that binding alone, so the order is well defined; ties between components with no
  path between them still break by name, as now. A plain `toposort` over the new graph no
  longer works, because a function-only cycle is legal and `toposort` refuses any cycle — sort
  the condensation instead.

**Why transitive mention and not something finer.** Mention is the rule because it is the
widest one that is *sound* for a module's initialisation, and anything narrower has to guess
which mentioned code runs:

- Initialising a binding runs its own body, and whatever functions that body calls. A function
  can only be called if it is named somewhere in code that runs, or handed over as a value by
  code that named it. An imported module cannot call back in except through such a value,
  since imports may not form a cycle. So the set of code that can run while `a` is initialised
  is contained in what `a` reaches by mention.
- It over-approximates. `a = f` beside `f x = a` mentions `f` without calling it, so it would
  be safe at load, and it becomes an error; so does a binding that calls a function whose
  branch reading the binding is never taken during initialisation. Telling those apart is
  undecidable in general, and any fixed approximation between the two — "only a function in
  call position", say — is a rule a user has to learn and can still be defeated by passing the
  function along. A binding whose value is a function can be written with its parameter
  instead (`a x = f x`), which takes it out of the relation's constraints entirely.
- It is Elm's rule: Elm rejects a cycle among top-level definitions when any definition in it
  takes no arguments, and accepts one made of functions only.
- **Initialising lazily was considered and rejected.** Emitting each parameterless binding as a
  value computed on first read would accept every program here and turn a true cycle into
  non-termination, which [*Two outcomes*](../spec/evaluation-semantics.md#two-outcomes)
  permits. It contradicts the chapter's "evaluated once, before the program runs", puts a check
  on every read of a top-level value on both targets, and moves a mistake the compiler can
  report to a program that hangs.

**Where the rule is written down:** the chapter states it and the argument above is promoted to
a decision entry, `DEC-19`, when this lands. In
[*evaluation-semantics.md*](../spec/evaluation-semantics.md):

- *A binding with no parameters is evaluated once* — "evaluated after everything it mentions"
  becomes "after everything it **depends on**", with *depends on* defined as the transitive
  mention relation above, and an `expect=ok` example where the dependency runs through a
  function, shaped like the first probe.
- *A binding may not depend on itself* — the cycle rule is stated over *depends on*, so a cycle
  through a function is an error when it contains a parameterless binding, with an
  `expect=canonical-error:SelfDependency` example shaped like the second probe. The
  `isEven`/`isOdd` example stays: a cycle of functions only.

`canonical::dependency_graph`'s doc comment, `Error::SelfDependency`'s, and `javascript.rs`'s
item 4 are rewritten to match; the last loses its citation of this ticket.

**Acceptance:**

- In `tests/javascript.rs`, the first probe emits `const z = 2n;` before `const a = f(1n);`,
  and a copy with `a` and `z` renamed the other way round is ordered correctly too, so the
  name-sorted tie-break cannot be what passes it.
- In `tests/compiler/canonical.rs`, the second probe is rejected with `Error::SelfDependency`
  naming `a` and `f`, and its message does not call `f` a parameterless binding. `a = f` beside
  `f x = a` is rejected the same way, pinning the over-approximation as the rule rather than an
  accident. `isEven`/`isOdd` is still accepted.
- `tests/ir.rs`'s `a_reference_to_a_function_is_not_an_edge_in_the_initialisation_order` still
  passes — `helper` is a node now but is not in the order — and its doc comment and mutation
  note are rewritten, since the filter they describe is gone.
- The chapter states the rule with both examples, `docs/decisions/dec-19.md` records the
  argument and is listed in `docs/decisions/README.md`, and `cargo test --test spec` is green.
- `cargo test --workspace` is green, and `cargo run` still prints `parsed 8 modules`, lists all
  eight as checked, and exits 0 — `std/core`'s own top-level values have to pass the stricter
  check.
