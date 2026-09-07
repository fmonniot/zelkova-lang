# DEC-6 · Which types may cross the JavaScript boundary

**Settled:** 2026-09-06, by the language owner (`SPEC-18`).
**Status:** live.
**Where the rule lives:**
[JS interop — Which types may cross the boundary](../spec/js-interop.md#which-types-may-cross-the-boundary).

[JS interop](../spec/js-interop.md) had opened, since it was written, by saying the compiler
"accepts only a subset of the Zelkova standard types" in a facade signature, and by conceding in
its own open questions that no subset had ever been named. The compiler enforced nothing, so the
subset was in practice everything. Four questions had to be answered together, because the
answer to each moves the others.

## 1 — A type is admitted exactly when a predicate can decide it

The chapter already stated the test it wanted — "only things verifiable by the runtime are let
through" — and the whole of this decision is taking that sentence operationally rather than as a
sentiment. **A type may be named in a facade signature iff the compiler can emit a predicate
that decides, from a value alone, whether that value is a member of the type.**

Everything else follows from it, and that is the point of choosing it: the alternative shape for
this decision was a list of admitted types, which would have to be re-litigated every time the
language gained a type form. A rule that decides the cases means a new type form arrives with
its answer already computed — records and lists were admitted here before either has a chapter,
because a predicate for each is obvious even though its encoding is not.

The rejected alternative at this level was **no rule at all**: delete the "subset" sentence from
the chapter and let a facade name whatever the compiler happens to accept. That is not an answer
to the question, it is a decision to stop asking it, and it would make the narrowness that
justifies the whole facade design into a claim the language does not make. The boundary being
narrow *is* the design; widening it to whatever the code allows would have been settling the
question by not settling it.

`Int` deserves a note, because it looks like the exception and is not. `Int` and `Float` are
JavaScript's one number type, and it is tempting to write `Int` up as pragmatically tolerated.
It needs no tolerance: `Number.isInteger` is a real predicate over a value alone, so `Int` is
admitted on exactly the terms `Bool` is.

## 2 — A bare type variable is rejected, so a facade is monomorphic

This was the hard case, and it decides more than it appears to, because it is the difference
between the rule being about **types** and being about **what the JavaScript is allowed to do
with a value it cannot inspect**.

A type variable has no predicate — `a` excludes no value, so there is nothing for a check to
decide — and it is therefore rejected. Two alternatives were live.

**Admit variables, but forbid the JavaScript from inspecting a value of one.** This is the rule
that would keep `std/core`'s existing facades legal: `add : a -> a -> a` is fine as long as
`add`'s JavaScript treats its arguments opaquely. It loses because it is unenforceable in
principle rather than merely today: the compiler does not read the `.mjs`, and never will, so a
rule about what that file may do with a value is a rule nothing can check. It would put a
sentence in the chapter that the language cannot keep, which is the same failure as the "subset"
sentence this entry exists to repair.

**Admit variables unrestricted.** The status quo, and rejected for the reason in decision 1.

What makes rejection cheap is that the escape is already written down elsewhere. [Type
classes](../spec/type-classes.md#a-constrained-function-may-not-be-a-javascript-facade) forbids a
constraint on a facade signature and gives the same remedy in the same words — a constrained
function lives in ordinary Zelkova and calls a monomorphic facade underneath it. The constraint
rule and this rule are one rule seen twice: a facade's signature names the types its JavaScript
really handles, whether the wider type someone wanted to write was `Comparable a => a` or `a`.
Landing this makes twelve `std/core` signatures illegal ([`LANG-43`](../tickets/lang-43.md)), and
each becomes several monomorphic facades with the polymorphism moved up a level — which is what
those signatures always meant.

Function types are rejected by the predicate rule too: `typeof x === 'function'` decides that a
value is *some* function, not that it is a function of the declared type. They are also rejected
independently, by the plain-parameter-list promise — a Zelkova function passed into JavaScript
would have to be called from a `.mjs`, and calling it means knowing the currying convention that
promise exists to hide. The chapter gives the predicate reason only; the second is kept here
because it survives any future weakening of the first, and would have to be written back into
the chapter if that ever happened.

## 3 — Unions cross, and their encoding is published interop interface

The tempting narrow answer here is **primitives and tuples only**, or its slightly wider cousin,
**unions whose constructors take no arguments** — an enumeration crosses as a string, and nothing
else does. Both were rejected on the first case anyone hits: a facade cannot return a `Maybe`.
Optionality is the most common thing a JavaScript boundary produces, and a boundary that cannot
express it pushes every caller into encoding it by hand, differently each time.

So a union crosses, applied to admitted types, and the consequence is accepted rather than
avoided: **the runtime representation of a union value is part of the interface.** A union value
is an object carrying its constructor's name in a `$` field and that constructor's arguments in
`a`, `b`, `c`; a `.mjs` may read one and build one. The cost is that a constructor's *name*
becomes public interop surface, so renaming one is a breaking change for any companion that
mentions it — bounded by the fact that only an exposed type can be named in a facade signature,
so those names were already public API.

The alternative encoding, revisited on 2026-09-07 and rejected again, is a uniform
`{$: "Rgb", args: [255, 0, 0]}` — one shape for every constructor, a predicate that loops over
one array parallel to the declared argument types, and a companion that destructures by
position. It loses on allocation: a union value is the pervasive runtime shape — every cons
cell, every `Maybe`, every `Result` — and the array is a second object per value. The uniformity
is also not quite uniform, since a nullary constructor either carries an empty array it never
reads or becomes the exception the shape was meant to remove. Lettered fields cost a rule for a
constructor of more than 26 arguments, which is a case no program has.

One argument against this was made from a promise the chapter does not make. The chapter is
sometimes read as promising that a hand-written `.mjs` never has to know how anything is
represented; what it actually says is narrower — "no curried-wrapper convention to observe on
the JavaScript side — currying is the compiler's business". That is a promise about *currying*,
not about *data*. Data representation being interface and currying staying the compiler's
business are consistent, and it is the combination this decision takes.

Recursive unions are admitted on the same terms, because the predicate is a recursive walk and a
Zelkova value is immutable, so there is no cycle for it to loop on. The cost is real and the
chapter states it rather than letting someone discover it: a crossing checks the whole value, at
O(size of value), not just its outermost constructor.

The chapter publishes the union encoding and no other. Records and lists are admitted but have
no chapter yet ([`SPEC-21`](../tickets/spec-21.md), [`SPEC-22`](../tickets/spec-22.md)), and
their encodings belong there and to code generation. The union's is published here because the
admission of unions is meaningless without it and no other chapter is going to carry it.

## 4 — The predicate is real code that runs, not a principle for selecting a subset

The predicate could have been a purely static device: a way of deciding which types are admitted,
emitting nothing. Rejected, because it makes "only things verifiable by the runtime are let
through" a slogan rather than a fact. Nothing would ever catch a `.mjs` that returns the wrong
shape, and the boundary would be exactly as unchecked as it is today, with a longer chapter.

**Checking in debug builds only** was the other alternative, on cost grounds. It loses twice: the
compiler has no release/debug distinction to hang it on, and inventing one to answer this
question would be a large decision taken as a side effect of a small one; and it leaves a
released program with precisely the undefined behaviour this rule exists to remove, at the one
moment it is least observable.

So every value entering Zelkova from a companion is checked. [`BUG-20`](../tickets/bug-20.md) is
the case that motivates both halves of this entry: `_Utils_cmp`, handed a value of a user union
type, reads three fields that are not there and returns a comparison of nothing against nothing.
That it can be *called* that way is decision 2's business, and
[`LANG-43`](../tickets/lang-43.md)'s to fix; what decision 4 adds is the other end — a companion
returning a value its declared type does not describe is caught where it crosses rather than
believed by everything downstream. The work is [`GEN-2`](../tickets/gen-2.md), a sibling of
[`GEN-1`](../tickets/gen-1.md) rather than a part of it: `GEN-1` is a whole phase, this is a
bounded piece of its output with a chapter section to check it against.
