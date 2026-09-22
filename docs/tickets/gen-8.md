# GEN-8 · The JavaScript runtime module

**Sizing:** small. One hand-written `.mjs` with two helpers in it, and its own tests. No Rust
changes beyond making the file reachable.

**Part of:** [`GEN-1`](gen-1.md). Independent of `GEN-4` through
[`GEN-7`](gen-7.md) — it can be written at any point before [`GEN-9`](gen-9.md) needs it.

**Location:** a new `runtime/js/zelkova.mjs`, with `runtime/js/tests/` beside it for its checks,
named the way `std/core/tests/Js/UtilsChecks.mjs` is. Nothing in `src/` yet; [`GEN-13`](gen-13.md)
is what copies the file into a build.

**Decided ([`DEC-18` decision 3](../decisions/dec-18.md#3--a-function-emits-as-a-plain-n-ary-function-and-currying-is-a-runtime-helper)):** a declaration is emitted as a plain n-ary
JavaScript function and a saturated call at a known callee is a direct call. Everything else —
an application that supplies too few arguments, or one whose callee is a value rather than a
known declaration — goes through a helper. That helper, and the abort, are the only code the
backend does not generate, so they are written by hand once rather than inlined into every
emitted module.

**Problem:** there is no runtime. Two things the emitted code needs cannot be emitted per call
site without repeating themselves in every module:

- **Partial and unknown application.** `pair 1`, where `pair` takes two arguments, is a value:
  the function carrying the argument it has already been given
  ([Function values](../spec/evaluation-semantics.md#function-values)). A call site that does
  not know the callee's arity cannot decide between calling and accumulating, so it defers to
  one helper that does.
- **Aborting.** [A program aborts](../spec/evaluation-semantics.md#when-a-program-aborts) when
  the runtime can no longer keep the language's guarantees, and **an abort says what caused it**.
  Three callers want it: [`GEN-5`](gen-5.md)'s fall-through leaf, [`GEN-12`](gen-12.md)'s
  missing-companion case, and later [`GEN-2`](gen-2.md)'s failed predicate under an `unsafe`
  facade, which names the export whose companion broke.

**Approach:** `$curry(fn, arity)` — or whatever the ticket settles on spelling it — returns
something that accumulates arguments until it has `arity` of them and then calls `fn` with the
whole list.

Two cases are easy to get wrong and are what the tests are for. **Over-application**: a function
whose result is itself a function may be handed more arguments than its own arity, and the
surplus applies to the result — `f 1 2` where `f` takes one argument and returns a function is
two applications, and the emitted call site may not know that. **Several arguments at once**: a
call site that has three arguments and a callee of arity two should not have to hand them over
one at a time.

The abort helper takes a description and stops the program. What "stops" means on Node is the
ticket's to pick — a thrown error that nothing catches is the obvious candidate, since the
language has [nothing that catches](../spec/evaluation-semantics.md#two-outcomes) — and whatever
is picked must not be confusable with a value.

Keep it small. Equality, comparison and arithmetic are **not** here: they are ordinary functions
behind `Js/Basics` and `Js/Utils` facades, and putting a second copy in the runtime is how the
two drift apart.

**Not in this ticket:** copying the file into a build ([`GEN-13`](gen-13.md)), and any emitter
that calls these ([`GEN-9`](gen-9.md) onwards).

**Acceptance:** `node --test 'runtime/js/tests/**/*.mjs'` is green over: a curried arity-2
function applied one argument at a time and both at once, giving the same answer; an arity-3
function applied 1+2, 2+1 and 3 at once; a function of arity 1 returning a function, applied to
two arguments in one call; and the abort helper stopping with a description that contains the
text it was given. [`TEST-3`](test-3.md) is what puts that command in CI, and its glob has to
widen beyond `std/core/tests/` to reach this directory — say so in that ticket if it has not
landed, or widen it if it has.

`cargo run` is unaffected and still prints `parsed 8 modules`, lists all eight and exits 0.
