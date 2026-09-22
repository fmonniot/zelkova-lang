# GEN-7 · Parameterless bindings get an initialisation order

**Sizing:** small, if [`LANG-35`](README.md) has landed — it is the same graph, and this is the
topological order over it rather than the cycle check. Medium if taken first, because then the
graph is built here and `LANG-35` has to be rewritten around it.

**Depends on:** `GEN-4`, closed — the IR is `src/compiler/ir/` — and [`LANG-35`](README.md), see below.

**Part of:** [`GEN-1`](gen-1.md).

**Location:** `ir::Module` in `src/compiler/ir/`, which holds a module's declarations and
no order over them. `src/compiler/canonical/mod.rs` — where [`LANG-35`](README.md) builds the
graph this reads. `src/compiler/dependencies.rs` orders **modules** and is not it: the order
here is between declarations inside one module.

**Decided ([`docs/spec/evaluation-semantics.md`](../spec/evaluation-semantics.md#a-binding-with-no-parameters-is-evaluated-once)):**
a top-level binding that names no parameters is evaluated once, before the program runs, and
every reference to it is that one value. Such bindings are evaluated in **dependency order**:
one is evaluated after everything it mentions. [Declarations are
unordered](../spec/declarations.md#declarations-are-unordered) as text, and this is the one
place the language puts an order on them — read off the references rather than off the page.

A binding that *does* name parameters is not evaluated at initialisation at all: its value is
the function, and its body runs when the function is applied.

**Problem:** nothing computes the order. A backend that emitted declarations in the order they
happen to sit in a `HashMap` — which is what `canonical::Module::values` is — would read a
binding before it was initialised, differently on different runs.

**Take [`LANG-35`](README.md) first.** It builds a graph over exactly these declarations, with
exactly these edges, to report a cycle among them; this ticket needs the same graph to ask a
different question of it. Two graphs built from one rule is the case `CLAUDE.md` warns about
under *A doc comment describes what the code at that site does* — the copy nobody maintains is
the one the next reader trusts. If `LANG-35` has landed, this ticket reuses its edge set and
adds a topological sort; if it somehow has not, this ticket does not invent a second one.

The edge rule is `LANG-35`'s and is restated here only so a reader knows what is being sorted:
an edge goes from a parameterless binding to every **parameterless** binding its body mentions,
anywhere in the body including inside a `case` branch or an `if` arm. A reference to a binding
with parameters is not an edge — that value already exists — and neither is a reference to an
imported name, because cross-module initialisation follows the module order
`dependencies.rs` already computes.

**Approach:** put the order on the IR module, as a list of the parameterless declarations in the
order they are to be initialised. A module with none has an empty list. A cycle is
`LANG-35`'s error and has already been reported by the time this runs, so this pass sorts a
graph it may assume is acyclic — state that assumption in the doc comment rather than leaving it
implicit, and say which phase discharges it.

**A facade constant is not in this order.** `unsafe pi : Float` names a foreign binding directly
and has no Zelkova body to place, so it is evaluated on whatever schedule the target gives it —
[*Facade constants*](../spec/interop.md#facade-constants) says so explicitly, and promises
nothing about when. A facade module has no parameterless declarations to sort for this purpose.

**Not in this ticket:** rejecting a cycle ([`LANG-35`](README.md)) and emitting the bindings in
the order ([`GEN-9`](gen-9.md)).

**Acceptance:** a test over the chapter's own example — `base`, `shifted` and `other` — asserts
`base` is ordered before `shifted` when the two are written in either order, which is the whole
of the rule. A second asserts a module whose declarations all take parameters has an empty
order. A third asserts a parameterless binding that mentions only a *function* binding has no
edge to it, so it may be initialised first. `cargo run` still prints `parsed 8 modules`, lists
all eight and exits 0.

Neutralise-check by replacing the sort with the declarations in their `HashMap` order: the first
test goes red on one of the two writings.
