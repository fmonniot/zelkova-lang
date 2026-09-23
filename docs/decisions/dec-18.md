# DEC-18 · How Zelkova is compiled: seven decisions

**Settled:** 2026-09-20, by the language owner, in the session that broke
[`GEN-1`](../tickets/gen-1.md) into a program of tickets.
**Status:** live.
**Where the rule lives:** [`GEN-1`](../tickets/gen-1.md), which is that program's index, and
the tickets under it. None of these is a rule about the *language* — no chapter changes — so
each names below the code site it lands at, because the ticket index carrying them is deleted
when the program closes.

[Evaluation semantics](../spec/evaluation-semantics.md) specifies what evaluating an expression
does and the compiler implements none of it: the pipeline ends at type checking, and no Zelkova
program has ever run. `GEN-1` had been carrying the whole of that as one ticket, along with the
three questions it could not be started without answering — what the backend consumes, how a
function value is represented, and where output goes.

[DEC-9](dec-9.md) had already named the hazard these decisions exist to avoid, while arguing a
different question: *the first backend's incidental behaviour would otherwise become the rule
the second backend broke.* That is the standard the seven below are held to. Zelkova has two
targets, and a decision taken by whichever backend was written first is not a decision.

## What this entry does not decide

Four rules the program inherits are already settled and are not re-argued here. They are listed
so a reader looking for them does not conclude they were overlooked:

- **`Int` is 64 bits, and on JavaScript it is a `BigInt`** — [DEC-16](dec-16.md), decisions 2
  and [5](dec-16.md#5--on-javascript-an-int-is-a-bigint).
- **A union crosses as a tagged value, and the encoding is published** —
  [DEC-6](dec-6.md#3--unions-cross-and-their-encoding-is-published-interop-interface) and
  [A union crosses as a tagged value](../spec/interop.md#a-union-crosses-as-a-tagged-value).
  The generator makes that encoding true rather than choosing it.
- **A class dictionary is erased by specialisation and never passed** —
  [DEC-2 decision 7](dec-2.md#7--dictionaries-are-erased-by-specialisation-not-passed).
  Decision 1 below is what keeps that reachable.
- **Sharing is preserved and nothing is promised about a function value** — [DEC-9](dec-9.md).
  Decisions 3 and 4 below both spend that licence.

## 1 — The backend reads a typed IR, and the typer is what produces it

Three candidates: read the canonical AST directly; lower it to a fresh untyped IR beside the
typer; or grow the typer's own `Term` into the IR, carrying the types it solved.

**Reading the canonical AST loses information the front end had.** It is shaped for name
resolution, and the typer's own translation already demonstrates the cost:
`canonical_expr_to_term` collapses `VarLocal`, `VarTopLevel`, `VarForeign` and
`VarConstructor` into one `TermKind::Identifier(String)`. Those are four different things to
emit — a parameter, a module-scope binding, a named import, an object constructor — and the
string left behind is qualified for some of them and not others, so the distinction cannot be
recovered afterwards.
A backend over the canonical AST would rebuild it, along with arity, saturation and a
constructor's place in its declaration, as ad-hoc walks at emission time, once per backend.

**An untyped IR is the version of this that only works for JavaScript.** JavaScript needs
almost no type information — the canonical AST already separates an `Int` literal from a
`Float` one, and equality and arithmetic are ordinary functions behind facades. WebAssembly is
statically typed and needs a representation class at every node, and polymorphism reaches it
only through monomorphisation, which consumes solved types. So an untyped IR is one that works
until the second backend, which is precisely the failure DEC-9 named.

Nor can the types be added later. The typer currently returns `Result<(), Vec<Error>>`: it
builds a term with a type on every node, solves a substitution over it, and reduces the whole
result to whether the pass errored. Reattaching types to a separately-built tree afterwards
means keying one tree against another, and `NodeSpan`'s `PartialEq` always answers `true`, so
the obvious key is not one.

**So the typer produces the IR** — it is the only phase that knows a node's type, and the
question `check_module`'s own comment has been asking ("Should I have an intermediate AST
before type checking?") gets the answer that costs one tree rather than three. What it buys
besides WebAssembly is the specialisation [DEC-2 decision
7](dec-2.md#7--dictionaries-are-erased-by-specialisation-not-passed) requires, which needs the
same solved types and would otherwise be a second reason to do this work.

What it costs is honest and was accepted: the first ticket of the program is inside the typer
(`GEN-3`), and two silent skips there — a construct the translation cannot represent, and an
unbound variable — had to stop being silent, because a backend cannot tell a declaration the
typer verified from one it walked past.

Lands at: `src/compiler/ir/`, whose doc comment is where the shape's obligations are written.

## 2 — One IR serves both targets, and JavaScript is written first

The alternative was to defer WebAssembly entirely and shape the IR for whatever JavaScript
needed, on the grounds that the second backend is far off and its requirements are guesses.

Rejected for decision 1's reason: the requirement that actually bites — types on every node —
is knowable now, and is the one that cannot be retrofitted. The rest of what WebAssembly needs
is open and is *left* open. [`GEN-15`](../tickets/gen-15.md) is filed unscheduled and undecided,
listing the questions a WebAssembly backend has to answer — linear memory or WasmGC, how a
partial application is represented with no closure primitive, whether monomorphisation is
whole-program — and stating what the IR already owes it. Filing it now is the cheap half of
this: the expensive half is discovering those obligations after the IR is written.

Lands at: [`GEN-15`](../tickets/gen-15.md) until a WebAssembly backend exists, and
`src/compiler/ir/`'s doc comment meanwhile.

## 3 — A function emits as a plain n-ary function, and currying is a runtime helper

Zelkova's application is curried: every function takes one argument, and a function of several
is one that returns a function ([Function
values](../spec/evaluation-semantics.md#function-values)). Three ways to carry that into
JavaScript were weighed.

**All-unary closures** — `x => y => …` — are the direct translation and make every call an
allocation and an indirection, including the overwhelmingly common one where the call site
supplies every argument.

**Elm's `F2`/`A2` wrappers** pay the same cost in a different currency and impose a convention
on hand-written JavaScript, which is the thing [The JavaScript
companion](../spec/interop.md#the-javascript-companion) already refuses: a companion's exports
take a plain parameter list, and *currying is the compiler's business*.

**A plain n-ary function won.** A declaration of two parameters is a two-parameter JavaScript
function; a call that supplies both, at a callee whose arity is known, is a direct call; an
unsaturated call, or one whose callee is a value rather than a known declaration, goes through
one `$curry` helper in a hand-written runtime module. The fast path costs nothing, and the
facade boundary needs no bridge at all — the companion already takes the parameter list the
compiler was going to call with, so the promise above is kept by construction rather than by a
wrapper.

This is affordable only because [DEC-9](dec-9.md) declined to promise anything about a function
value: `$curry` may allocate on every partial application, and a program whose performance
depends on it not doing so was already depending on a backend rather than on the language.

It needs two facts at the call site — the callee's arity and whether the call is saturated —
which is part of why the IR carries them (decision 1).

Lands at: `runtime/js/zelkova.mjs` for the helper, and `src/compiler/javascript.rs` for the
call-site rule.

## 4 — A constructor of no arguments is hoisted to one module-level constant

`Red` is `{$: "Red"}`. Emitted at each mention it allocates an object per mention; emitted once
as a module-level constant that every mention refers to, it allocates one per program.

The second is chosen, and it is licensed rather than merely cheap. Nothing in the language can
observe the difference: [equality is
structural](../spec/evaluation-semantics.md#what-structural-equality-computes) so two spellings
of `Red` compare equal either way, there is no identity operator, and
[Sharing](../spec/evaluation-semantics.md#sharing) states that a value exists independently of
any name bound to it — binding a second name gives a second way to reach one value, not a
second value. Reusing an existing value is exactly what that permits.

Recorded because it is the kind of choice a generator otherwise makes silently, and because the
argument for it is a licence granted by a chapter rather than a preference: a reader wondering
whether the backend is allowed to do this should find that it is, not have to re-derive it.

The same reasoning does not extend to a constructor *with* arguments, which is a different value
per application and is not hoisted.

Lands at: `src/compiler/javascript.rs`.

## 5 — Output is written per package, beside the root manifest

[The compiler's interface](../spec/toolchain.md#the-compilers-interface) already requires that
output go beside the package root rather than beside any source it read, and marks itself
**Provisional:**. Two things it does not settle were settled here.

**One directory per package**, `build/js/<package-name>/`, rather than one flat tree. A build
holds several packages and [at most one version of
each](../spec/packages.md#one-version-of-each); a flat tree would have to encode the package
into every file name to keep two packages' same-named modules apart, which is the collision
[`BUG-37`](../tickets/bug-37.md) is open on one level up.

**A module's emitted path uses its name within its own package** — `Js.Basics` is
`zelkova-core/Js/Basics.mjs` — and never the namespace a dependent writes. [The
namespace](../spec/packages.md#the-namespace) is added at the boundary by whoever crosses it and
does not appear under `src/`; a package that wrote its own namespace into its output would be
naming itself something only its dependents call it, and two dependents of one package would
disagree about the path. So an import across a package boundary resolves to a sibling directory,
which is the other half of why the tree is not flat.

Lands at: [`GEN-13`](../tickets/gen-13.md), and `compile_package`'s emission step afterwards.

## 6 — The generated code is checked in two halves, and `cargo test` does not run `node`

The obvious route is a Rust test that shells out to `node`, keeping one command for the whole
suite. It was rejected, and the reason is not a dislike of the dependency.

A Rust test that shells out has to decide what to do when `node` is absent. Failing makes a Rust
toolchain insufficient to run the Rust tests; skipping produces a green test that proves nothing,
which `CLAUDE.md` names as the most common review finding there is. Neither is worth one command.

So: everything testable without running JavaScript is a Rust test — the IR, the decision tree,
the tail-call marking, the initialisation order, and the *text* each module emits. The
behavioural half is JavaScript, run by `node --test`, in the shape [Testing a
companion](../spec/interop.md#testing-a-companion) already established for `.mjs` files. Text
assertions alone would be a backend checked entirely by eye, which is what
`LANG-56` documented the cost of: it landed with no Rust test behind it, because nothing in
`cargo test` loads a `.mjs`.

This converges rather than duplicating. [`TEST-3`](../tickets/test-3.md) is already open to put
the existing companion checks in CI; one job runs both and its glob widens.

A `zelkova` binary that compiles *and* runs is the destination for this and for
[`LANG-63`](../tickets/lang-63.md)'s test runner, and is deliberately **not** a prerequisite:
[`GEN-17`](../tickets/gen-17.md) is filed, and a binary whose job is to run a program cannot be
written before anything can be run.

Lands at: `.github/workflows/rust.yml` and `CLAUDE.md`'s *Commands* section.

## 7 — The program covers the language the front end accepts today

The compiler accepts literals, variables, application, `if`, `case`, tuples and constructors.
There is no `let`, no lambda, no list, no record, no string literal, no unit type, and a
declaration may have only one clause.

The program emits exactly that and no more. The alternative — emitters written ahead of the
constructs they serve — costs a backlog of code no test can reach, since nothing can write the
syntax that would exercise it.

The consequence is a standing rule for the tickets that follow: **each front-end ticket that
lands a construct gets a sibling `GEN-` ticket for its emitter**, rather than either the
`LANG-` ticket growing a code-generation half or one omnibus ticket accumulating them. Both
alternatives were considered and rejected for the same reason — a ticket has to stay small
enough for one sitting, and a `LANG-` ticket that also emits is two pieces of work that fail
review together.

Lands at: [`GEN-1`](../tickets/gen-1.md)'s program, and the ticket index's conventions after it.
