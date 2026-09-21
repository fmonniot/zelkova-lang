# GEN-5 · A `case` becomes a decision tree in the IR

**Sizing:** medium. One pass over a `case` node, plus its tests. Patterns are shallow today,
which caps how hard the algorithm has to be — see the note on nesting below.

**Depends on:** [`GEN-4`](gen-4.md).

**Part of:** [`GEN-1`](gen-1.md).

**Location:** the `Case` node of the IR [`GEN-4`](gen-4.md) defines, and the pass that lowers
it. `src/compiler/exhaustiveness.rs` is the neighbour this is **not**: that phase decides
whether a `case` covers its type and inspects nothing today ([`LANG-19`](lang-19.md)); this one
decides what order the branches are tested in.

**Decided ([`docs/spec/evaluation-semantics.md`](../spec/evaluation-semantics.md#conditional-evaluation)):**
a `case` evaluates its scrutinee, then tries its branches in the order written and evaluates the
body of the first one that matches — never another. So the tree preserves source order, and a
branch's body is reached only from its own leaf.

**Problem:** a `case` in the IR is a scrutinee and a list of `(pattern, body)` pairs, which is
the shape the type checker wanted: every branch is checked against the same scrutinee type and
the order is irrelevant to it. Emitting that directly means each backend re-derives, at emission
time, which test distinguishes which branch and which names each pattern binds — the same
algorithm written twice, in two languages, neither of them testable without running the output.

**Approach:** lower a `case` to an explicit tree: a node is a test on a scrutinee (its
constructor tag, a literal value, a tuple's shape), an edge is an outcome, and a leaf is either
a branch body with its bindings already named, or the fall-through below.

Write the recursion over sub-patterns even though nothing currently produces one. A constructor
pattern may not nest today ([`LANG-16`](lang-16.md)), so every pattern is one level deep and a
tree built on that assumption would pass its tests — and would be the thing that has to be
rewritten the week `LANG-16` lands. A recursive lowering costs little more here and costs
nothing then.

**The fall-through leaf.** A tree needs an answer for a scrutinee no branch matches.
[Coverage is checked, so no such `case` compiles](../spec/evaluation-semantics.md#two-outcomes)
— but it is not checked yet ([`LANG-19`](lang-19.md)), so such a `case` compiles today and the
emitted code would otherwise fall off the end and produce `undefined`, which is a value the
language does not have. Give the tree an explicit fall-through leaf that
[aborts](../spec/evaluation-semantics.md#when-a-program-aborts), naming the declaration.
Record in its doc comment that it is unreachable once `LANG-19` lands, and that it is not
deleted then — an abort is what an unreachable leaf should do.

**Not in this ticket:** emitting the tree as JavaScript, which is [`GEN-10`](gen-10.md); the
abort helper it names, which is [`GEN-8`](gen-8.md)'s; and exhaustiveness, which is
[`LANG-19`](lang-19.md).

**Acceptance:** tests over a `case` of each pattern form the language has — a wildcard, a
variable, an `Int`, a `Char`, a `Bool`, a tuple, and a constructor with and without arguments —
asserting the tree's tests, the order they are tried in, and the names each leaf binds. One test
asserts two branches on the same constructor keep source order. One asserts a `case` with no
branch for some value of its type has a fall-through leaf. `cargo run` still prints
`parsed 8 modules`, lists all eight and exits 0.
