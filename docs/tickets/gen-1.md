# GEN-1 · Emit runnable JavaScript for a checked module

**This ticket is the program's index, not a unit of work.** The design session on 2026-09-20
settled what a backend consumes, how a function value is represented, where output goes and how
it is tested, and broke the work into [`GEN-3`](gen-3.md) through [`GEN-14`](gen-14.md), each
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

Each of these was open in the ticket this file used to be. A child ticket states the ones it
depends on in its own **Decided** section rather than re-arguing them.

**1 — The backend consumes a typed IR, and the typer is what produces it.** The canonical AST
is shaped for name resolution: `translate_expression` in `src/compiler/typer/mod.rs` already
flattens `VarLocal`, `VarTopLevel`, `VarForeign` and `VarConstructor` into one
`TermKind::Identifier(String)`, which is precisely the distinction a code generator needs back.
So the backend reads an IR — and that IR is today's `Term` grown up rather than a third tree
beside it, because the typer is the only phase that knows a node's type and a type bolted on
afterwards is the thing a WebAssembly backend cannot use.

Types are in the IR from the start for that reason. WebAssembly is statically typed, and
polymorphism reaches it through monomorphisation — which is the same machinery
[`DEC-2` decision 7](../decisions/dec-2.md#7--dictionaries-are-erased-by-specialisation-not-passed)
already requires for erasing class dictionaries. An IR with no types cannot carry either.

**2 — A function is emitted as a plain n-ary JavaScript function.** A declaration of two
parameters is `function f(a, b)`. A saturated call at a known callee emits `f(a, b)` directly; an
unsaturated one, or one whose callee is not known statically, goes through a `$curry` helper in
the runtime module. Elm's `F2`/`A2` convention is not adopted.

This is also what makes [*The JavaScript companion*](../spec/interop.md#the-javascript-companion)'s
plain-parameter-list promise fall out rather than needing a bridge at the boundary: the companion
already takes the parameter list the compiler was going to emit a call with.

**3 — The scalar types have native representations; everything else is the published encoding.**
[The admitted-type table](../spec/interop.md#which-types-may-cross-the-boundary) decides these,
and `src/compiler/scalars.rs` is how the backend recognises them by qualified name: an `Int` is a
`BigInt`, a `Float` a number, a `Bool` a boolean, a `Char` a one-character string. A union value
is the `{$: "Ctor", a, b, c}` object
[the chapter publishes](../spec/interop.md#a-union-crosses-as-a-tagged-value), and a tuple is an
array. So `True` and `False` are `true` and `false` and not tagged objects, even though `Bool` is
an ordinary union at the same time.

**A constructor of no arguments is hoisted to one module-level constant** and every mention
refers to it, rather than an object literal being written at each mention.
[Sharing](../spec/evaluation-semantics.md#sharing) permits reusing an existing value, and
equality is structural, so nothing in the language can observe the difference.

**4 — `Int` is 64 bits, carried as a `BigInt`.** [`DEC-16`](../decisions/dec-16.md) and
[Numbers](../spec/evaluation-semantics.md#numbers). An `Int` literal emits `1n`.
[`LANG-56`](lang-56.md) is the sibling ticket that brings `std/core`'s two companions to the
same width, and it is a prerequisite of this program rather than part of it: without it the
first emitted program computes wrong arithmetic out of `Js/Basics.mjs`.

**5 — Output goes to `build/js/<package-name>/<module path>.mjs`**, beside the root package's
manifest and never beside a source it read, which is what
[*The compiler's interface*](../spec/toolchain.md#the-compilers-interface) requires. One emitted
file per Zelkova module, named by the module's name *within its own package* — the namespace a
dependent writes is added at the boundary and does not appear under `src/`
([*The namespace*](../spec/packages.md#the-namespace)), so it does not appear in the output tree
either. `build/` is gitignored, and `cargo run` writes it on every invocation.

**6 — Emission is checked in two halves.** Everything that can be tested without running
JavaScript is a Rust test: the IR, the decision tree, the tail-call marking, the initialisation
order, and the text a module emits. The behavioural half — that the emitted program computes the
right value — is a JavaScript test run by `node --test`, in the shape
[*Testing a companion*](../spec/interop.md#testing-a-companion) already uses for `.mjs` files.
`cargo test` does not shell out to `node`.

That converges with [`TEST-3`](test-3.md), which wires the existing companion checks into CI:
one job runs both, and its glob widens rather than a second harness appearing. A `zelkova`
binary that compiles *and runs* is the real destination for this and for
[`LANG-63`](lang-63.md)'s test runner — it is [`GEN-17`](gen-17.md), and it is deliberately not
a prerequisite, because a binary whose job is to run a program cannot be written before
something can be run.

**7 — JavaScript first, WebAssembly second, on one IR.** This program lands the JavaScript
backend. [`GEN-15`](gen-15.md) is the WebAssembly one, filed unscheduled so the constraints it
places on the IR are on record before the IR is written rather than rediscovered afterwards.
[`GEN-4`](gen-4.md)'s doc comment is where those constraints are stated at the code site.

**8 — The program covers the language the front end accepts today** — literals, variables,
application, `if`, `case`, tuples and constructors. There is no `let`, no lambda, no list, no
record, no string literal and no unit, and a declaration may have only one clause. Each of those
gets a sibling `GEN-` ticket when its own `LANG-` ticket lands, rather than this program
carrying emitters for constructs nothing can write.

## What is inherited and not decided here

**A class dictionary is erased by specialisation and never passed**
([`DEC-2` decision 7](../decisions/dec-2.md#7--dictionaries-are-erased-by-specialisation-not-passed)).
It costs nothing while no class exists, and a backend that started by passing dictionaries would
have to be unpicked when [`LANG-40`](lang-40.md) lands. Decision 1 above is what keeps the door
open: specialisation needs the types the IR carries.

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
GEN-3   the typer hands back the types it solved
   │
GEN-4   the backend IR
   ├── GEN-5   a `case` becomes a decision tree
   ├── GEN-6   a self tail call is marked
   └── GEN-7   parameterless bindings get an initialisation order
   │
GEN-8   the JavaScript runtime module
   │
GEN-9   emit a module
   ├── GEN-10  emit a `case`
   ├── GEN-11  emit the tail-call loop
   └── GEN-12  emit an `unsafe` facade call, and place its companion
   │
GEN-13  write the build
   │
GEN-14  the end-to-end check under node
```

Unscheduled, filed to keep their context: [`GEN-15`](gen-15.md) the WebAssembly backend,
[`GEN-16`](gen-16.md) the wrapper an effectful facade's call site gets, [`GEN-17`](gen-17.md) a
`zelkova` binary. [`GEN-2`](gen-2.md), the boundary predicates, sequences after
[`GEN-12`](gen-12.md) and [`LANG-43`](lang-43.md) as it always did.

**Acceptance:** every ticket above is closed, and this file is tombstoned with them. What the
program as a whole has to show is [`GEN-14`](gen-14.md)'s: a small module compiled, its exported
value imported from the emitted output under `node`, and the value asserted — covering one self
tail call deep enough that a non-tail emission exhausts the stack, and one call through a
facade into its companion.
