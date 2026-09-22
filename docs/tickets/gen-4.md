# GEN-4 · The backend IR

**Sizing:** medium. One module's worth of type definitions, plus growing the existing
canonical-to-term translation into one that keeps what it currently throws away. No emission.

**Depends on:** `GEN-3`, closed — `typer::type_check` returns a `typer::Solved` per
declaration, carrying a solved type on every node of the ones it could type.

**Part of:** [`GEN-1`](gen-1.md).

**Location:** `src/compiler/typer/mod.rs` — `Term`, `TermKind`, `TermPattern`,
`TermPatternKind`, `TypedTerm`, `TypedTermKind` and `Solved`, which move to a module of their
own; `canonical_expr_to_term`, `translate_pattern` and `value_to_term_and_annotation`, which
are the translation that has to stop discarding things. A new `src/compiler/ir/` for the moved definitions.
`src/compiler/canonical/mod.rs` — `ExpressionKind` and `Value`, the input side.
`src/compiler/mod.rs` — `check_module`, which holds the solved types and hands back only the
`canonical::Module`; threading them out to a caller is this ticket's.

**Decided ([`DEC-18` decision 1](../decisions/dec-18.md#1--the-backend-reads-a-typed-ir-and-the-typer-is-what-produces-it) and [`DEC-18` decision 2](../decisions/dec-18.md#2--one-ir-serves-both-targets-and-javascript-is-written-first)):** one IR, produced by the typer, consumed by
both backends. It carries a type on every node, because WebAssembly is statically typed and
polymorphism reaches it through the monomorphisation
[`DEC-2` decision 7](../decisions/dec-2.md#7--dictionaries-are-erased-by-specialisation-not-passed)
already requires for erasing class dictionaries.

**Problem:** `Term` is most of a backend IR already — `Fun`, `Apply`, `If`, `Case`, `Tuple`, the
literals — and it is missing exactly the things a type checker does not need and a code
generator cannot work without.

The sharpest of those is name kind. `canonical::ExpressionKind` distinguishes `VarLocal`,
`VarTopLevel`, `VarForeign(QualName, Type)` and `VarConstructor(QualName, Type)`, and
`canonical_expr_to_term` flattens all four into `TermKind::Identifier(String)`. Those are four
different things to emit: a local is a parameter or a bound name, a top-level is a binding in
this module's scope, a foreign is a named import from another emitted module, and a constructor
builds a tagged object. Recovering the distinction from a string afterwards is not possible —
and the string is unqualified for some of them and qualified for others, so it is not even a
reliable key.

Three more are missing for the same reason:

- **Arity.** `value_to_term_and_annotation` wraps a body in one nested `Fun` per parameter, so a
  declaration of two parameters is a function returning a function, and its arity has to be
  counted back off the spine. [`DEC-18` decision 3](../decisions/dec-18.md#3--a-function-emits-as-a-plain-n-ary-function-and-currying-is-a-runtime-helper) emits a plain n-ary function, and
  needs the arity as a fact rather than a shape.
- **Saturation.** `Apply` is one argument at a time. A call site that supplies every argument a
  known callee takes emits a direct call; one that does not goes through the runtime's `$curry`.
  Nothing currently says which a given application is.
- **A constructor's place in its declaration.** The `$` field carries the constructor's name and
  the [WIT spelling](../spec/interop.md#a-union-crosses-as-a-tagged-value) is a `variant` case,
  so both backends need the constructor's name, its argument count, and the set it belongs to.
  `TermPatternKind::Constructor` carries the union's name and the pattern's bindings, which is
  what unification needed and not what emission needs.

**Approach:** move the term definitions into `src/compiler/ir/` and grow them into the list
above. The translation from canonical is where each fact is available — the arity is the
`patterns` vector's length, the name kind is the `ExpressionKind` variant, the constructor's
arity and siblings are on the `UnionType` the canonical constructor already names.

Represent a module, not just an expression. A declaration is a name, an arity, parameters,
a body and a type; a module is its declarations plus the unions it declares, which is what
[`GEN-7`](gen-7.md) puts an order on and [`GEN-12`](gen-12.md) reads a facade's signatures out
of. A facade module has signatures and no bodies
([Foreign interoperability](../spec/interop.md)), so the shape has to admit a declaration with
no body rather than requiring one.

**Write the WebAssembly constraints into the module's doc comment**, at the code site, because
[`GEN-15`](gen-15.md) is unscheduled and the next reader will otherwise take this for a
JavaScript IR. What the shape is serving: a monomorphisation pass needs a type on every node; a
representation class (i64, f64, reference) is read off that type; a union is a `variant` and a
tuple a `tuple`, so a constructor's index within its declaration is wanted, not only its name;
and a facade's plain parameter list is the same call shape both targets make.

**Not in this ticket:** the decision tree ([`GEN-5`](gen-5.md)), the tail-call mark
([`GEN-6`](gen-6.md)), the initialisation order ([`GEN-7`](gen-7.md)) and anything that emits
text. Those three each add a field or a pass over what this ticket defines, which is why they
are separable at all — keep the shape open enough for them and do not write them here.
Monomorphisation is [`GEN-15`](gen-15.md)'s and is not started.

**Acceptance:** `check_module` produces an `ir::Module` for each of `std/core/src`'s eight
modules, and `cargo run` still prints `parsed 8 modules`, lists all eight as checked and exits 0.
Tests assert, over a small module built with `indoc!`: a two-parameter declaration's arity is 2;
an application supplying both arguments is marked saturated and one supplying a single argument
is not; a local, a top-level, an imported and a constructor reference are four distinguishable
things in the IR; and a constructor node carries its argument count and its index in its
declaration. Each is neutralise-checked by collapsing the distinction it pins back to what the
translation does today.
