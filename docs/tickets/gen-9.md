# GEN-9 · Emit a module

**Sizing:** medium, and the largest of the emitters — it is every expression form but `case`,
plus a module's shape. Split further only if it will not fit: the natural seam is declarations
and module structure first, expressions second.

**Depends on:** `GEN-4`, closed — the IR is `src/compiler/ir/` — [`GEN-7`](gen-7.md) (the initialisation order) and
[`GEN-8`](gen-8.md) (the runtime it calls into).

**Part of:** [`GEN-1`](gen-1.md).

**Location:** a new backend module under `src/compiler/`, reading the IR. Nothing writes files
yet — this ticket produces a module's JavaScript as text, which is what makes it testable in
Rust.

**Decided ([`DEC-18` decision 3](../decisions/dec-18.md#3--a-function-emits-as-a-plain-n-ary-function-and-currying-is-a-runtime-helper) and [`DEC-18` decision 4](../decisions/dec-18.md#4--a-constructor-of-no-arguments-is-hoisted-to-one-module-level-constant), and the representations [`GEN-1`](gen-1.md) collects):**

- A declaration of *n* parameters is `function f(a, b, …)`. A **saturated** call at a known
  callee is `f(a, b)`; anything else goes through the runtime's `$curry`.
- The scalar types have native representations, recognised by qualified name through
  `src/compiler/scalars.rs`: an `Int` is a `BigInt`, a `Float` a number, a `Bool` a boolean, a
  `Char` a one-character string. So `True` and `False` emit `true` and `false`, even though
  `Bool` is an ordinary union at the same time.
- Every other union value is `{$: "Ctor", a: …, b: …}`, the encoding
  [the chapter publishes](../spec/interop.md#a-union-crosses-as-a-tagged-value); a tuple is an
  array.
- **A constructor of no arguments is hoisted** to one module-level constant that every mention
  refers to. [Sharing](../spec/evaluation-semantics.md#sharing) permits reusing an existing
  value and equality is structural, so nothing observes the difference.
- An `Int` literal emits `1n`, because [`Int` is 64 bits](../spec/evaluation-semantics.md#numbers)
  ([`DEC-16`](../decisions/dec-16.md)).

**Problem:** nothing emits anything. This is the ticket that makes a Zelkova module a JavaScript
one.

**Approach:** one ES module per Zelkova module. Its shape, in order: imports, hoisted nullary
constructors, function declarations, then the parameterless bindings as `const`s in
[`GEN-7`](gen-7.md)'s initialisation order, then the exports.

An `import` in the IR becomes a named ES import from the emitted module that declares the name.
The path is resolved by [`GEN-13`](gen-13.md)'s layout, so keep the path-building behind one
function rather than spreading it through the emitter.

What each expression form becomes: application per the saturation rule above; `if` as a
conditional expression; a tuple as an array literal; a constructor as the object above, or the
hoisted constant when it takes no arguments; a local as its mangled name; a top-level as the
module-scope binding.

**Two rules constrain this and are easy to violate by accident.** Subexpressions are evaluated
[left to right, in the order they are written](../spec/evaluation-semantics.md#order-of-evaluation)
— an application evaluates the function expression first, then each argument in source order —
and JavaScript's own evaluation order agrees, so the obvious emission is correct and a
rearrangement for tidiness is not. And [nothing
short-circuits](../spec/evaluation-semantics.md#nothing-short-circuits): `&&` and `||` are
ordinary function calls through an `infix` declaration, so they emit as calls and never as
JavaScript's `&&` and `||`, which would skip an operand the language says is evaluated.

**Name mangling.** A Zelkova name may be a JavaScript reserved word — `new`, `class`, `in`,
`default`, `typeof` — and a module's declarations all land in one file's scope alongside the
runtime's helpers and the hoisted constants. Settle one scheme, document it in a doc comment
with the list it guards against, and apply it in one function. Nothing clever is needed; a `$`
prefix on a name that collides is enough, as long as the scheme cannot make two distinct Zelkova
names into one JavaScript name.

**Not in this ticket:** `case` ([`GEN-10`](gen-10.md)), the tail-call loop
([`GEN-11`](gen-11.md)), facades ([`GEN-12`](gen-12.md)) and writing the text anywhere
([`GEN-13`](gen-13.md)).

**Acceptance:** tests assert the emitted text for small modules written with `indoc!`, one per
rule above: a two-parameter declaration emits a two-parameter function; a call with both
arguments is a direct call and a call with one goes through the runtime; an `Int` literal ends
in `n` and a `Float` literal does not; `True` emits `true`; a nullary constructor appears once
as a constant and its mentions refer to it; a constructor with arguments emits `{$: …}` with
fields in declaration order; a tuple emits an array; a parameterless binding that mentions
another is emitted after it; a declaration named `class` is mangled and a declaration named
`classy` is not.

Keep each fixture small — the assertions are on text, and a large fixture makes every later
ticket's diff churn. `cargo run` still prints `parsed 8 modules`, lists all eight and exits 0.
