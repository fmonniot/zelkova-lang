# DEC-25 · Type aliases: two decisions

**Settled:** 2026-10-02, by the language owner, when [`LANG-86`](../tickets/lang-86.md) was
filed and could not be started on the three paragraphs the chapter had.
**Status:** live.
**Where the rule lives:** [Types](../spec/types.md#type-aliases), with
[Type classes](../spec/type-classes.md#what-an-instance-is-declared-for) for decision 2.

The section said one thing: an alias introduces no new type, and is interchangeable with what it
names everywhere and in both directions. Two questions had more than one answer consistent with
that. Three more had only one, and are listed at the end so that nobody takes them for choices.

## 1 — An alias may take parameters, and is always applied to all of them

`type alias Pair a = (a, a)` is legal, and `Pair` on its own names no type.

**No parameters at all** was the alternative: an alias names one complete type. It is the
simpler rule and it makes an alias nearly useless for the thing it is most wanted for, a
container's internal shape — `Array.ignored` writes `type alias Tree a = …`, and without
parameters that is a `type` declaration with a constructor to wrap and unwrap at every use.

**A partly applied alias** was never on the table. It is a function from types to types, which
is what [a class is always over a complete type](../spec/type-classes.md#a-class-is-always-over-a-complete-type)
declines for a type variable, and an alias is not where the language starts to have one.

## 2 — An instance head written through an alias is the type the alias names

`instance Eq (Pair a b)` over `type alias Pair a b = (a, b)` is the tuple instance, held to the
head rule and to [the orphan rule](../spec/type-classes.md#where-an-instance-may-be-declared) as
`(a, b)` is. An alias for `Maybe Int` is rejected as a head, as `Maybe Int` is.

**Always an error** was the alternative: a head is written as the type itself. It buys an
instance that names its real type at the place it is declared, and it costs the one exception to
"interchangeable everywhere" the language would have. A reader who has been told an alias is
its expansion would have to learn the place where it is not.

## What followed without a decision

- **`alias` is a soft keyword**, in the position after `type`.
  [Lexical structure](../spec/lexical-structure.md#reserved-words) reserves thirteen words and
  no others, and a type's name is uppercase-initial, so one token of context decides it.
- **An alias cannot name itself.** It stands for the type on the right of its `=`, and one that
  mentions the alias has no finite spelling.
- **`Pair(..)` is an error.** `(..)` exposes constructors and an alias declares none.
