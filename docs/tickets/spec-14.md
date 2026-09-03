# SPEC-14 · Nothing specifies how a structural instance is derived, and equality needs it

**Sizing:** small. Prose in one chapter, and one design question to settle before writing it.
No compiler change — the mechanism it specifies lands with the `CLASS-` program.

**Location:** [`docs/spec/type-classes.md`](../spec/type-classes.md) — *Declaring an instance*,
which is where the answer goes, and *What the standard library declares*, which is what makes it
necessary.

**Problem:** `Eq` is an ordinary class. Equality is not built into the language, and
[`docs/spec/evaluation-semantics.md`](../spec/evaluation-semantics.md)'s *Equality* says what
each of `std/core`'s instances computes: two values of a union type are equal when they are the
same constructor and their arguments are pairwise equal, two tuples when their elements are,
and so on down.

That definition is the same for every union type in every program, and no chapter says how a
type gets it without writing it out. So as specified today, every `type` declaration a program
makes needs a hand-written `Eq` instance — a block of boilerplate per type, mechanically derived
from the type's own shape, that says nothing a reader could not have worked out and that goes
wrong silently when a variant is added and the instance is not updated.

That is a tax the language should not charge, and it lands on the most ordinary thing a program
does. `Comparable` has the identical problem one step further along: its ordering over a union
type is "by constructor in declaration order, then by argument", again the same for every type.

**The question to settle:** how a type asks for the structural definition. Three shapes, and
they are not equivalent:

- **A clause on the `type` declaration** — `type Colour = Red | Green deriving (Eq)`. Reads at
  the type, which is where a reader looks. Needs new syntax in a declaration form that has none,
  and it puts a class name inside a `type` declaration, which nothing else there does.
- **An instance with no members** — `instance Eq Colour where` and nothing under it, meaning
  "take the structural definition for every member". Needs no new syntax at all, and it goes
  through [*Where an instance may be declared*](../spec/type-classes.md#where-an-instance-may-be-declared)
  unchanged, so the orphan rule covers it for free. Against it: an empty `where` block reading as
  a request rather than as an omission is not obvious from the spelling.
- **Automatic, with no declaration** — every union type has an `Eq` instance unless it declares
  one. Cheapest to use and the hardest to reason about: it makes an instance exist that no file
  mentions, which the orphan rule is written on the assumption cannot happen, and it privileges
  `Eq` and `Comparable` by name in a chapter whose whole claim is that no class is privileged.

Whichever wins, the chapter also has to say which classes can be derived and what happens when a
type's *arguments* have no instance — a `type Box a = Box a` deriving `Eq` needs `Eq a`, so a
derived instance carries a constraint it did not write.

Found while writing [`docs/spec/evaluation-semantics.md`](../spec/evaluation-semantics.md)
(`SPEC-9`), which specifies what structural equality computes and cannot say how a type asks for
it.

**Approach:** follow `write-spec-chapter`, at the scale of a section rather than a chapter.
Settle the question above with the language owner first — it is the whole ticket, and the other
two shapes are not variations on the winner. Then:

1. *Declaring an instance* gains the mechanism, with a block tagged `expect=unimplemented` like
   every other class construct in that chapter.
2. *What the standard library declares* says which of the four classes a program may derive.
3. [`docs/spec/evaluation-semantics.md`](../spec/evaluation-semantics.md)'s *How a structural
   instance is derived* open question is deleted, and its *What structural equality computes*
   section links to the new section instead.

**What this is not.** Do not answer it by making `==` a primitive again. Equality being a class
member is what lets a type whose representation has more than one spelling of a value define
equality up to a normal form, and a derived instance is the convenience on top of that, not a
replacement for it.

**Acceptance:** [`docs/spec/type-classes.md`](../spec/type-classes.md) specifies how a type
obtains a structural instance, what constraint a derived instance over a parameterised type
carries, and which classes may be derived. No open question in `docs/spec/` still asks it.
`cargo test --test spec` green, with the new block tagged and proven to fail.

**No block holds this to account** beyond the new one's `expect=unimplemented` tag, which says
only that the syntax does not parse yet. Whether the derived definition is the right one is a
claim about a runtime nothing in the harness reaches ([`TEST-2`](test-2.md)).

**Sequencing:** this is a prerequisite for [`CLASS-6`](class-6.md) only in the sense that
`std/core` will want it; it is not a prerequisite for [`CLASS-2`](class-2.md), which builds the
`instance` declaration this may or may not reuse. Settling it before `CLASS-2` starts is
cheaper, since one of the three shapes above is a change to that declaration's grammar.
