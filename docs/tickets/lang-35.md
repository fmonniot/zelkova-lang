# LANG-35 · A parameterless binding may depend on itself, and nothing notices

**Sizing:** small. A dependency graph over one module's value declarations, plus a new
`canonical::Error` variant. `petgraph` and Tarjan are already a dependency, used one level up.

**Location:** `src/compiler/canonical/mod.rs` — canonicalization builds each declaration
independently and never relates one to another; `Error` has no variant for this.
`src/compiler/dependencies.rs` detects cycles between **modules** and is not the place: the
cycle here is between declarations inside one module, and it has to be found while that module
is being canonicalized so the diagnostic can point at both bindings.

**Decided ([`docs/spec/evaluation-semantics.md`](../spec/evaluation-semantics.md), *A binding
may not depend on itself*):** evaluation is strict, and a top-level binding with no parameters
is evaluated once, before the program runs, in dependency order. A binding whose value depends
on its own value therefore describes nothing — there is no order that puts it after itself —
and a cycle among parameterless bindings is an error however long it is.

The restriction is on parameterless bindings only. A binding **with** parameters is not
evaluated at initialisation at all: its value is the function, and its body runs when the
function is applied. So `f n = f n` is fine, and so is mutual recursion between two functions.
Only a cycle every member of which names no parameters is an error.

**Problem:** all three of these canonicalize with no errors today:

```zel
x = x

a = b
b = a

y = f y      -- f is an ordinary function; y is still self-dependent
```

Nothing computes an initialisation order, so nothing notices there is no order to compute. The
first program a code generator emits from any of them either loops at startup or reads a value
that does not exist yet, depending on how the generator happens to lay bindings out — and by
then the diagnostic has no source to point at.

Found while writing [`docs/spec/evaluation-semantics.md`](../spec/evaluation-semantics.md)
(`SPEC-9`).

**Approach:** after a module's declarations are canonicalized, build a graph over the value
declarations that name no parameters, with an edge from a binding to every parameterless
binding its body mentions. A body mentions a name through any expression, including inside a
`case` branch or an `if` arm — a reference is a reference wherever it sits, since the whole body
is evaluated. Report every non-trivial strongly-connected component, and every self-loop.

The edge set is what needs care. An edge goes to a parameterless binding only: a reference to a
function binding is a reference to a value that already exists, so it is not an edge, which is
exactly what lets `isEven`/`isOdd` through. A reference to an imported name is likewise not an
edge — cross-module initialisation order follows the module order
`dependencies.rs` already computes, and a cycle there is already
[reported](../spec/modules.md) as a module cycle.

Add a `canonical::Error` variant for it, per `CLAUDE.md`'s *An error has to describe itself*: a
`message()` naming the bindings in the cycle in the vocabulary of the source, a primary label on
the binding the cycle was entered at, and a secondary label on each other member. A one-binding
cycle reads better as its own sentence than as a cycle of length one, so the message should
special-case it.

**Acceptance:** `x = x`, `a = b` beside `b = a`, and `y = f y` are each rejected with the new
variant, with a caret under the binding, and tests in `tests/compiler/canonical.rs` covering
those three plus the three that must stay accepted — `f n = f n`, mutual recursion between two
function bindings, and a parameterless binding that merely *mentions* a function which is itself
recursive. The `**Known gap:**` paragraph in
[`docs/spec/evaluation-semantics.md`](../spec/evaluation-semantics.md)'s *A binding may not
depend on itself* is deleted and its block is retagged — **that block goes red when this lands**,
which is what will remind you. `cargo run` still prints `parsed 8 modules` and lists all eight as
checked.
