# DEC-24 · What implementing type classes had to settle: twelve decisions

**Settled:** 2026-10-02, by the language owner, in the session that brought the
[type-class tickets](../tickets/README.md#active-work-type-classes) up to date with a tree that
had gained a manifest, a backend and a test runner since they were filed.
**Status:** live.
**Where the rule lives:** [Type classes](../spec/type-classes.md) for decisions 1 to 9, with
[Modules](../spec/modules.md#the-exposing-list) and
[Name resolution](../spec/name-resolution.md#namespaces) for decision 6; the
[ticket index](../tickets/README.md#active-work-type-classes) for decisions 10 to 12, which are
about the order of the work and say nothing about the language.

[DEC-2](dec-2.md) decided what a class is. It was written before anything implemented one, and
each of the tickets filed from it left a sentence saying "the chapter decides" at a point where
the chapter was silent. Reading the tickets against the tree a month later found those
sentences, found three places where the tree had moved underneath a ticket, and found two
places where the chapter's own text did not survive being implemented. These are the answers.

## 1 — A context holds any number of constraints

```zel
four : (Eq a, Eq b, Eq c, Eq d) => a -> b -> c -> d -> Bool
```

The cap at three was never a decision. A context is parsed as a type because an LALR(1) parser
cannot tell `(Eq a, Eq b) =>` from a two-tuple at the `(`, and a tuple type has two or three
elements, so the context inherited a limit that belongs to tuples.

Two alternatives were on the table. Stating the cap as a rule costs the compiler nothing and
costs a signature that needs four classes a class invented only to have them as superclasses.
Lifting the cap for tuple types would have made a four-tuple legal everywhere a type is, against
[the limit Types states](../spec/types.md#tuple-types) and the one-representation rule `AST-2`
exists for. A production of its own for four or more was probed and builds beside the tuple
productions without a conflict: from the fourth element on it is the only one still viable.

The consequence is on the parser AST. A context stops being a type that canonicalization takes
apart and becomes a list of constraints, which is why [`LANG-71`](../tickets/README.md) moved
to the front of the order: the class and instance heads parse a context too.

## 2 — An instance head is a declared type, a tuple or `()`, over distinct variables

```zel
instance Eq Colour where …
instance Eq (Maybe a) where …
instance Eq (a, b) where …

instance Eq (Maybe Int) where …   -- rejected
```

With every argument a distinct variable, an instance is found by its class and the head's own
name, and two instances collide exactly when those two agree. A head that could name
`Maybe Int` beside `Maybe a` needs a rule for which wins, and makes "is this a duplicate" a
unification problem with an answer that depends on what else is in scope.

The tuple clause is the half that was argued. A tuple type is declared in no module, and the
first proposal treated a tuple the way [Records](../spec/records.md#records-and-derivation)
treats a record: never the head of an instance, walked element by element by a class that
carries a derivation. The owner's question was what that does to a class that serialises to
JSON. Such a class has the signature a one-value derivation takes and cannot usefully carry
one, for the reasons [the chapter gives for `toString`](../spec/type-classes.md#what-a-derivation-cannot-render) —
so under that proposal no tuple could ever be serialised, by the class's author or by anyone,
short of wrapping it in a declared type.

So a tuple and `()` are legal heads, and [the orphan rule](../spec/type-classes.md#where-an-instance-may-be-declared)
places them: with no module declaring the type, the module declaring the class is the only one
left. `std/core` covers tuples for `Eq` and `Comparable` itself, and a tuple having one shape,
those instances are `derived`.

The same objection applies to a record and is not answered here; it is the chapter's
[open question](../spec/type-classes.md#open-questions).

## 3 — A written instance may carry a context

```zel
instance Eq a => Eq (Set a) where
  eq left right =
    eq (toList left) (toList right)
```

A derived instance infers its context. A written one for a type with parameters has to be able
to say the same thing, or the only instances a parameterised type could have would be derived
ones.

## 4 — A member signature carries no context, and mentions the class variable

A member signature is parsed by the production an annotation is, so
`compare : Eq b => a -> b -> Order` reaches the compiler with a context of its own. It is an
error at the class declaration. Allowing a member to constrain its other variables is more to
specify, check and specialise, and can be added later without breaking a program.

A member that never mentions the class variable is an error for a different reason: no use of
it could say which instance it meant.

## 5 — A constraint is never inferred

```zel
same x y =
  eq x y        -- an error: say `same : Eq a => a -> a -> Bool`

isZero n =
  eq n 0        -- fine: the type is determined, and `Eq Int` is discharged
```

A constraint is part of a type only where an annotation wrote it. A declaration with no
annotation, left needing a class of a type nothing determines, is an error asking for the
annotation.

Inferring it would be friendlier to a private helper. It would also be the first place the
compiler publishes an unannotated declaration's inferred type to that declaration's callers,
which nothing does today, and it can be added later: every program this rule accepts keeps its
meaning under the other.

## 6 — A class name is a type name, and its members travel with it

A class shares the types namespace: a module cannot declare a type and a class of one name.
`exposing (Comparable)` in a header exposes the class and every member, and a member is not
listed there on its own. An import that names the class brings the name and its members into
scope; an import may also name a member alone, as it may any exposed value.

Listing members one by one was the alternative. It lets a module keep a member private, and a
class with a private member is one no other module can write a whole instance of — a state the
`exposing` list would make reachable by leaving a name out. A namespace of its own for classes
would have let `type Eq` and `class Eq` coexist, at the price of an `exposing` entry that names
both or a new entry form to say which.

## 7 — `std/core`'s classes: one member where the class is derivable

```zel
class Eq a where
  eq : a -> a -> Bool

class Eq a => Comparable a where
  compare : a -> a -> Order

class Number a where
  add : a -> a -> a
  sub : a -> a -> a
  mul : a -> a -> a
  pow : a -> a -> a
  negate : a -> a
  abs : a -> a

class Appendable a where
  append : a -> a -> a
```

`neq` is an ordinary function over `Eq`, as `lt`, `le`, `gt`, `ge`, `min`, `max` and `clamp`
are over `Comparable`. [DEC-2 decision 9](dec-2.md#9--stdcore-grows-four-classes) named the
classes and left the members "roughly"; a second member in `Eq` would need a second derivation
to keep the class derivable and a second definition in every written instance, for a function
that is `not` applied to the first.

`Number` carries `negate` and `abs` because neither can be written over the other four without
a numeric constant, and [a literal is never one](../spec/type-classes.md#numeric-literals).
The alternative was to declare `zero` and `one` as members and build the two on top. It was
passed over as more surface than anything yet needs; a class that wants a constant can still
declare one.

The instances: `Eq` for `Int`, `Float`, `Char`, `String`, `Bool`, `Order`, `Maybe`, `Result`,
`Failure`, `Position`, the two tuple arities and `()`; `Comparable` for `Int`, `Float`, `Char`,
`String`, `Position` and the two tuple arities; `Number` for `Int` and `Float`; `Appendable`
for `String`, with `List` joining it when lists exist.

## 8 — `combine`'s first parameter is a value, and its second is the rest of the walk

The chapter said a derivation's bindings are substituted and not called, which is what lets a
class decide its own short-circuiting. Its own `Comparable` derivation then wrote

```zel
combine x y =
  case x of
    EQ ->
      y

    _ ->
      x
```

and under substitution `x` is the expression `compare a1 b1`, written twice and so evaluated
twice. On a recursive type that compounds: each level of a list repeats the comparison of its
tail, and two lists differing at the end cost time exponential in their length.

So the two parameters are not the same kind of thing. The walk computes the answer for one part
and binds it once; `x` names that value. `y` stands for the rest of the walk, which is performed
only where the body reaches it. Nothing about short-circuiting changes, the chapter's examples
stand as written, and no part is answered twice.

Literal substitution with a warning was the alternative, and would have made the natural way
to write `Comparable` the wrong one. Evaluating each parameter at most once however often it is
written is the most forgiving rule and needs a memoised thunk in generated code for any
parameter used twice.

## 9 — A constrained function whose specialisations never end is an error

The chapter called a constrained function calling itself at a different type "already
impossible". It is not:

```zel
f : Eq a => a -> Bool
f x =
  f (Just x)
```

The call needs `Eq (Maybe a)`, which `Eq a` provides, so this type checks — and
[specialising it](dec-2.md#7--dictionaries-are-erased-by-specialisation-not-passed) asks for `f`
at `Int`, at `Maybe Int`, at `Maybe (Maybe Int)`, without end.

It is an error, found while specialising, by a limit on how deep one chain of specialisations
may go, and reported against the declaration with the type that kept growing. Rejecting the
self-reference in the type checker is earlier and more local, and does not see the same loop
written across two functions, so the limit would be needed behind it regardless.

## 10 — `LANG-12` closes the order instead of opening it

[`LANG-12`](../tickets/lang-12.md) makes an annotation's variables rigid, and was a hard
prerequisite of the solver: without it a constrained declaration can prove `Comparable Int` and
publish `Comparable a`.

Between that being written and this session, [a facade signature stopped being able to name a
type variable](dec-6.md#2--a-bare-type-variable-is-rejected-so-a-facade-is-monomorphic). So
`Basics` now reads `add : a -> a -> a` over a body of `Js.Basics.addInt`, and thirteen of its
declarations are exactly what `LANG-12` rejects. Only the class mechanism can make them honest,
and the class mechanism was waiting on `LANG-12`.

The order is turned round: the solver lands on today's flexible variables, `std/core` is
rewritten onto classes, and `LANG-12` goes last, onto a library with nothing left for it to
reject. For the length of the work a constrained declaration can under-prove its signature
exactly as any annotated declaration can today; the series does not finish until that is
closed.

A long-lived branch holding the whole series in the written order was the alternative, and
would have had no such window, and would have drifted against `main` for as long as the series
took. Landing `LANG-12` first and
narrowing those thirteen to `Int` would have disabled most of `std/core`'s tests, which compare
a `Bool`, a `Char`, a tuple or a union through them.

## 11 — Derivation is part of the series

`LANG-38` parsed a derivation and `LANG-39` was to check one, and no ticket produced the members
a derived instance stands for. [`LANG-83`](../tickets/README.md) does, before `std/core` is
rewritten: its test suites compare their own unions with `==`, and without `derived` each of
those types needs a hand-written instance.

## 12 — Specialisation is a ticket

[DEC-2 decision 7](dec-2.md#7--dictionaries-are-erased-by-specialisation-not-passed) was
recorded as a constraint the first code-generation ticket would inherit, because code generation
had not started. It has now shipped, and emits no class. [`GEN-24`](../tickets/README.md) is the
ticket, following [the rule that a construct landing after the backend gets its own emitter
ticket](dec-18.md#7--the-program-covers-the-language-the-front-end-accepts-today). It has since
landed, as `crates/zelkova-compiler/src/ir/specialise.rs`.

## What the session settled without asking

Five smaller rules were written into the chapter and the tickets by the session itself, each
because the chapter all but said it already. They are listed so that a reader can tell them
from the twelve above.

- A constraint's variable must occur in the type it constrains. `Eq b => a -> a` is an error at
  the annotation: no call could ever discharge it.
- An `infix` declaration may name a member of a class its module declares, a member being
  [a value the module declares](../spec/type-classes.md#declaring-a-class).
- `Position` is declared in `Basics`, the module declaring the two classes whose derivations
  receive one.
- A `derived` instance needs the type's constructors in scope, so one written against an opaque
  import is an error; and a scalar type has no shape to walk, so its instances are written.
- A one-value derivation cannot derive `()`, which has no element for the fold to begin at.
