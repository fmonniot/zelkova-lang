# DEC-27 · A context written on a `derived` instance, and the constructors one reads: two decisions

**Settled:** 2026-10-06, by the language owner (`SPEC-39`).
**Status:** live.
**Where the rule lives:** [Type classes](../spec/type-classes.md#what-a-derived-instance-requires).

`LANG-83` implemented derived instances and met two questions the chapter did not answer. It made
the choice no later answer could break for the first, and took the reading that was nearest to
hand for the second. `SPEC-39` carried both to the language owner.

## 1 — A written context is an upper bound

`instance Eq a => Eq (Box a) where derived` has parsed since `LANG-38`, and the chapter said only
that a derived instance's context is inferred. A context written on one is the instance's
context, whole, and it must provide every constraint the type's arguments need: one it writes,
or a superclass of one it writes. An instance that writes none has the inferred one.

Three readings were rejected.

**An error**, which is what the compiler did. It leaves one spelling and nothing to go stale. It
loses on two counts. The instance line `instance Eq (Box a) where` reads as unconditional when
the instance is not, and what the instance asks of its users changes silently when the type
gains or loses an argument, with nowhere for the author to pin it. And one case had no answer
short of writing the instance out by hand: a derived `Comparable (Phantom a)` for
`type Phantom a = Phantom Int` infers no constraint, and fails its superclass obligation beside
`instance Eq a => Eq (Phantom a)`. The type checker's note for that error already read "add
`Eq a` to the context of the instance", advice the rejection made impossible to follow.

**Equal to the inferred one.** A written context becomes checked documentation. No new program
compiles, the `Phantom` case stays unanswered, and a representation change still forces an edit
to the instance line of every derived instance, which is the cost of pinning without its use.

**Merged with the inferred one.** Nothing written is ever rejected, so
`instance Hash a => Eq (Box a) where derived` quietly requires `Hash a` and `Eq a`, and the line
the author wrote is not the instance they got. Under the upper bound the context an instance has
is the one written on it or the one inferred, never a third thing.

A written context is fixed where a recursive or mutually recursive group infers its contexts
together: another derived instance that holds the type needs what was written, and the written
context does not grow by what its own arguments ask. That follows from "whole", and is why the
check is containment, not a second inference.

## 2 — The constructors are read off what the declaring module exposes

The chapter said a derived instance for a type imported "without its constructors" is an error,
which reads as the import list: `import Colour exposing (Colour)`. The compiler read the
declaring module's interface, accepting the derivation whenever `Colour` exposes `Colour(..)`.
The interface reading is the rule.

[Modules](../spec/modules.md#imports) makes every exposed name reachable qualified under any
import, so `Colour.Red` can be written in a module that imports `Colour` however it does. The
interface reading therefore accepts `derived` exactly where the same instance could be written
out by hand, and the import-list reading would reject a derivation whose hand-written equivalent
compiles. It would also oblige a class's module, which the orphan rule makes one of the two homes
an instance has, to bring a type's constructors into its unqualified namespace for no use but
satisfying the rule.

The type itself is named in the head under the ordinary scoping rules, which neither reading
touches: under a bare `import Colour` the head is `Colour.Colour`.
