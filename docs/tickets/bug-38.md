# BUG-38 · A parameterless binding that reaches another only through a function it calls is not ordered after it

**Severity:** high (a miscompile: a well-typed module with no cycle among its parameterless
bindings emits JavaScript that throws a `ReferenceError` at load. Nothing calls
`javascript::emit` yet, so no user meets it before [`GEN-13`](gen-13.md) writes the output).

**Location:** `src/compiler/canonical/mod.rs` — `dependency_graph`, whose edges are the
references a parameterless binding's body makes to *another parameterless binding*, and which by
design never has a node or an edge for a binding with parameters; `initialisation_order`
(`GEN-7`) and `check_self_dependency`, the two readers of that one edge set.
`src/compiler/javascript.rs` — `emit`, which emits the parameterless bindings as `const`s in
`ir::Module::initialisation_order`.

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
binding "is evaluated after everything it mentions", and
[*Two outcomes*](../spec/evaluation-semantics.md#two-outcomes) that a well-typed program does not
crash. `a` mentions `f`, not `z`, so whether the rule reaches through `f`'s body is not stated.
The chapter's own example, `shifted = other base`, passes `base` as an argument and so has the
direct edge.

Found by the review of PR #238 (`GEN-9`), whose emitter follows `initialisation_order` as `GEN-7`
gives it. Left unfixed there because the cause is the edge set, which `check_self_dependency`
shares, and the fix needs a rule the spec does not yet state.

**Fix:** not decided. The options as they stand:

1. **Put functions in the graph.** A binding with parameters becomes a node, with an edge to
   everything its body references; an initialisation order is read off the parameterless nodes'
   reachability. This over-approximates: `a = f` beside `f x = a` mentions `f` without calling
   it, so it is safe at load, and it would become a cycle that `check_self_dependency` rejects. Whether that rejection is wanted is a
   spec question.
2. **Order only.** Use the reachability for `initialisation_order` and leave
   `check_self_dependency` on the direct-mention edges. The first probe is then fixed, and the
   second still throws. This would need its own answer for a cycle through a function.

Either way, the evaluation-semantics chapter has to say whether "everything it mentions" includes
what a mentioned function reads, and whether a cycle through a function is an error. That
belongs in the chapter, with the argument in a decision entry. `canonical::dependency_graph`'s
doc comment currently justifies leaving functions out with `isEven`/`isOdd`, and it would have to
change too.

**Acceptance:**

- in `tests/javascript.rs`, the first probe above emits `const z = 2n;` before
  `const a = f(1n);`, and a copy with `a` and `z` renamed the other way round is ordered
  correctly too, so the name-sorted tie-break cannot be what passes it;
- the second probe is either rejected with a canonicalization error or emits a module that does
  not throw at load, whichever the chapter settles, with the test naming the rule it pins;
- the evaluation-semantics chapter states the rule, and `cargo test --test spec` is green with
  an example of each case.
