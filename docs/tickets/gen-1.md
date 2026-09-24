# GEN-1 · Emit runnable JavaScript for a checked module

**This ticket is the program's index, not a unit of work.** The design session on 2026-09-20
settled what a backend consumes, how a function value is represented, where output goes and how
it is tested, and broke the work into `GEN-3` through [`GEN-14`](gen-14.md), each
sized for one sitting. What is written below is the part that belongs to no single one of them:
the decisions they inherit and the order they land in. It is tombstoned when the last of them
closes.

Nine files cite this one — [`evaluation-semantics.md`](../spec/evaluation-semantics.md) and
[`interop.md`](../spec/interop.md) from their **Not implemented:** paragraphs, and seven
decision entries (`DEC-2`, `DEC-6`, `DEC-9`, `DEC-10`, `DEC-11`, `DEC-12`, `DEC-14`) as the
ticket their decision is waiting on. That is why it keeps its name and its title rather than
being deleted and replaced: `cargo test --test spec` checks every one of those citations
resolves.

## What is decided

[`DEC-18`](../decisions/dec-18.md) is where these were argued and is the record that outlives
this file. Below is only what a ticket has to apply; the alternatives each was chosen over are
there and are not repeated here.

**1 — The backend consumes a typed IR, and the typer produces it**
([`DEC-18` decision 1](../decisions/dec-18.md#1--the-backend-reads-a-typed-ir-and-the-typer-is-what-produces-it)).
Today's `Term` grows into that IR rather than a third tree appearing beside it. It carries a
type on every node, the four name kinds the canonical AST distinguishes and
`canonical_expr_to_term` currently flattens, explicit arity, saturation, and a constructor's
place in its declaration.

**2 — One IR, both targets, JavaScript first**
([decision 2](../decisions/dec-18.md#2--one-ir-serves-both-targets-and-javascript-is-written-first)).
[`GEN-15`](gen-15.md) holds the WebAssembly questions, unscheduled; `src/compiler/ir/`'s module
doc comment states what the IR owes it.

**3 — A declaration emits as a plain n-ary JavaScript function**
([decision 3](../decisions/dec-18.md#3--a-function-emits-as-a-plain-n-ary-function-and-currying-is-a-runtime-helper)).
A saturated call at a known callee is a direct call; anything else goes through a `$curry`
helper in a hand-written runtime module. No `F2`/`A2` convention, and no bridge at the facade
boundary — the companion already takes the parameter list the compiler calls with, which is
[the plain-parameter-list promise](../spec/interop.md#the-javascript-companion).

**4 — A constructor of no arguments is hoisted** to one module-level constant that every mention
refers to
([decision 4](../decisions/dec-18.md#4--a-constructor-of-no-arguments-is-hoisted-to-one-module-level-constant)).
A constructor *with* arguments is not.

**5 — Output goes to `build/js/<package-name>/<module path>.mjs`**, beside the root package's
manifest, one directory per package, each module named by its name *within its own package*
([decision 5](../decisions/dec-18.md#5--output-is-written-per-package-beside-the-root-manifest)).
A build that emitted any error writes no output
([*The compiler's interface*](../spec/toolchain.md#the-compilers-interface)). `build/` is
gitignored and `cargo run` writes it on every invocation.

**6 — Emission is checked in two halves**
([decision 6](../decisions/dec-18.md#6--the-generated-code-is-checked-in-two-halves-and-cargo-test-does-not-run-node)).
Rust tests for the IR, the decision tree, the tail-call marking, the initialisation order and
the emitted text; `node --test` for whether the emitted program computes the right value.
**`cargo test` does not shell out to `node`.** It converges with [`TEST-3`](test-3.md)'s CI job.

**7 — The program covers the language the front end accepts today**
([decision 7](../decisions/dec-18.md#7--the-program-covers-the-language-the-front-end-accepts-today)) —
literals, variables, application, `if`, `case`, tuples and constructors. Each construct that
lands afterwards gets a sibling `GEN-` ticket for its emitter, rather than its `LANG-` ticket
growing a code-generation half.

**The representations**, which are not this session's to decide and are collected here because
every emitter ticket needs them in one place: an `Int` is a `BigInt` and a literal emits `1n`
([`DEC-16` decision 5](../decisions/dec-16.md#5--on-javascript-an-int-is-a-bigint)); a `Float`
is a number, a `Bool` a boolean, a `Char` a one-character string, all four recognised by
qualified name through `src/compiler/scalars.rs`; a union value is
`{$: "Ctor", a: …, b: …}` and a tuple an array
([the chapter](../spec/interop.md#a-union-crosses-as-a-tagged-value),
[`DEC-6` decision 3](../decisions/dec-6.md#3--unions-cross-and-their-encoding-is-published-interop-interface)).
So `True` and `False` emit `true` and `false`, even though `Bool` is an ordinary union at the
same time.

## What is inherited and not decided here

**A class dictionary is erased by specialisation and never passed**
([`DEC-2` decision 7](../decisions/dec-2.md#7--dictionaries-are-erased-by-specialisation-not-passed)),
which is why decision 1's types are not optional — specialisation eats them, and a backend that
started by passing dictionaries would have to be unpicked when [`LANG-40`](lang-40.md) lands.

**Rules that constrain the output rather than the design**, from
[`evaluation-semantics.md`](../spec/evaluation-semantics.md): subexpressions evaluate left to
right; both operands of `&&` and `||` are evaluated; `Int` wraps at 64 bits; `n // 0`,
`modBy 0 n` and `remainderBy 0 n` are `0`; equality is structural and comparing functions is not
allowed. A ticket that can break one of these says how it does not.

## The order

```
LANG-35   a parameterless binding may not depend on itself   ── prerequisite
BUG-27    an infix is qualified under its symbol, not its function  ── prerequisite
LANG-56   std/core's companions carry an Int at 64 bits      ── prerequisite
   │
GEN-3   the typer hands back the types it solved   ── closed
   │
GEN-4   the backend IR   ── closed
   ├── GEN-5   a `case` becomes a decision tree   ── closed
   ├── GEN-6   a self tail call is marked
   └── GEN-7   parameterless bindings get an initialisation order   ── closed
   │
GEN-8   the JavaScript runtime module   ── closed
   │
GEN-9   emit a module   ── closed
   ├── GEN-10  emit a `case`
   ├── GEN-11  emit the tail-call loop
   └── GEN-12  emit an `unsafe` facade call, and place its companion   ── closed
   │
GEN-13  write the build
   │
GEN-14  the end-to-end check under node
```

Unscheduled, filed to keep their context: [`GEN-15`](gen-15.md) the WebAssembly backend,
[`GEN-16`](gen-16.md) the wrapper an effectful facade's call site gets, [`GEN-17`](gen-17.md) a
`zelkova` binary. [`GEN-2`](gen-2.md), the boundary predicates, sequences after
[`GEN-12`](README.md) and [`LANG-43`](lang-43.md) as it always did.

**Acceptance:** every ticket above is closed, and this file is tombstoned with them. What the
program as a whole has to show is [`GEN-14`](gen-14.md)'s: a small module compiled, its exported
value imported from the emitted output under `node`, and the value asserted — covering one self
tail call deep enough that a non-tail emission exhausts the stack, and one call through a
facade into its companion.
