# Deriving in other languages

Zelkova derives an instance by walking two values in step and asking the *class* what a match, a
mismatch and a fold mean — [A class says how it is
derived](type-classes.md#a-class-says-how-it-is-derived) is the rule, and that chapter is the
normative one. Nothing here is normative. This appendix records the design space that rule was
chosen out of, because the choice looks arbitrary without it and because the arguments against
the alternatives are not recoverable from the chapter, which only states what was kept.

It is written against the mechanisms as they stand in September 2026 and it is a comparison, not
a tutorial: each language gets the smallest code that shows where it puts the knowledge.

## The four places the knowledge can live

Deriving is always the same trick — some description of a type's shape, and some rule turning
that shape into an implementation. Languages differ in *where each half is written*, and there
are only four answers.

### One: built into the compiler, one class at a time

Haskell 98 and 2010 name seven classes and specify each derivation in English prose in the
Report. Swift synthesizes `Equatable`, `Hashable`, `Comparable`, `Codable` and `CaseIterable`.
Java records, C# records, Kotlin data classes and Go's `==` on comparable structs are the same
answer with less ceremony.

```haskell
data Colour = Red | Green | Blue deriving (Eq, Ord, Show)
```

The Haskell Report's rule for `Ord` is, word for word, the walk Zelkova specifies: comparisons
are lexicographic "with earlier constructors in the datatype declaration counting as smaller than
later ones", and "derived comparisons always traverse constructors from left to right."

**Cost:** a closed set. A new derivable class needs a compiler release, and a program can never
add one. The knowledge is outside the language and unavailable to anything written in it.

Elm belongs here by refusal rather than by inclusion: `==` and `compare` are built in, apply to
almost every type, and there is no class to join and no derivation to ask for. `Debug.toString`
reads the runtime representation, is banned under `--optimize`, and is deliberately not a class —
which is a coherent answer to the problem [`SPEC-25`](../tickets/spec-25.md) is about.

### Two: a generic representation the classes program against

The compiler exposes one description of a type's shape, and every derivation is ordinary library
code over that description. GHC's `Generic`/`Rep`, `Data.Data` and SYB, Shapeless's
`HList`/`Coproduct`, Scala 3's `Mirror`, PureScript's `Generic`, Clean's `generic`, and Zig's
`@typeInfo` are all this answer.

```haskell
type Rep Colour = M1 D _ (M1 C _ U1 :+: (M1 C _ U1 :+: M1 C _ U1))

instance GEq U1 where
  geq _ _ = True                                             -- ≙ matched

instance (GEq f, GEq g) => GEq (f :*: g) where
  geq (a :*: b) (c :*: d) = geq a c && geq b d               -- ≙ combine

instance (GEq f, GEq g) => GEq (f :+: g) where
  geq (L1 a) (L1 b) = geq a b
  geq (R1 a) (R1 b) = geq a b
  geq _     _       = False                                  -- ≙ differed
```

The compiler still holds one built-in thing, but it is *one* thing and it is per **type** rather
than per class. Adding a derivable class is library work.

**Cost:** you need the type machinery to write those instances. `f :*: g` applies a variable to
a type, so it needs variables ranging over type constructors; the recursion over `Rep` needs type
families or Scala 3's `inline`/`summonInline`/match types.

Zelkova has none of it, and the reason is structural rather than a matter of time: [a class is
always over a complete type](type-classes.md#a-class-is-always-over-a-complete-type), so no
variable in the language can stand for `Maybe` or for `:*:`. **This route is closed and will stay
closed as long as that rule holds.** It is the single strongest argument for the design that was
adopted.

### Three: macros over syntax

The derivation is a program that reads the type declaration as syntax and writes an instance as
syntax. Rust's `#[derive]` and third-party derive macros (`TokenStream -> TokenStream`), OCaml's
`ppx_deriving`, Template Haskell, and Lean 4's registered deriving handlers
(`Array Name -> CommandElabM Bool`).

```rust
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
enum Colour { Red, Green, Blue }
```

```ocaml
type colour = Red | Green | Blue [@@deriving eq, ord, show]
```

Most general and least principled. A Rust derive macro sees tokens, not types: it cannot resolve
a type alias, cannot ask what a field's type actually is, and therefore cannot compute correct
bounds — which is why `#[derive(Clone)]` on a struct holding `PhantomData<T>` demands `T: Clone`
it does not need. The derivation is written in a *different language* than the class it is for,
and compile time pays for it.

### Four: the class supplies its own derivation, in the source language

Scala 3 is the clearest example. A `derives` clause on a type calls a `derived` method on the
class's companion, and that method is ordinary Scala the library author wrote:

```scala
trait Eq[T]:
  def eqv(x: T, y: T): Boolean

object Eq:
  inline def derived[T](using m: Mirror.Of[T]): Eq[T] =
    inline m match
      case s: Mirror.SumOf[T]     => eqSum(s, summonInstances[T, m.MirroredElemTypes])
      case p: Mirror.ProductOf[T] => eqProduct(p, summonInstances[T, m.MirroredElemTypes])

enum Colour derives Eq:
  case Red, Green, Blue
```

Haskell reaches the same place from the class declaration rather than from a companion, with
`DefaultSignatures` and `DeriveAnyClass`:

```haskell
class MyEq a where
  eq :: a -> a -> Bool
  default eq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
  eq x y = geq (from x) (from y)

data Colour = Red | Green | Blue deriving (Generic, MyEq)
```

**But every existing instance of answer four is answer four layered on answer two.** Scala's
`derived` is written against `Mirror`; Haskell's `default` is written against `Generic`; and
Clean's `generic` declaration — the oldest of the three, and the one Zelkova's design most
resembles — is written against a five-constructor structural representation:

```clean
generic gEq a :: a a -> Bool

gEq{|UNIT|}   UNIT UNIT                       = True
gEq{|PAIR|}   ex ey (PAIR x1 y1) (PAIR x2 y2) = ex x1 x2 && ey y1 y2
gEq{|EITHER|} el er (LEFT x)  (LEFT y)        = el x y
gEq{|EITHER|} el er (RIGHT x) (RIGHT y)       = er x y
gEq{|EITHER|} el er x y                       = False
gEq{|CONS|}   eq (CONS x) (CONS y)            = eq x y
gEq{|FIELD|}  eq (FIELD x) (FIELD y)          = eq x y

derive gEq List, Tree, []
```

## Where Zelkova sits

Zelkova is answer four **without** answer two underneath it, which is the part that appears to be
new. The three bindings a class supplies are Clean's equations with the representation curried
away:

| Clean | Zelkova | What was dropped |
|---|---|---|
| `gEq{\|UNIT\|}` | `matched` | nothing |
| `gEq{\|PAIR\|}` | `combine` | the two sub-*values*; the class sees only their two answers |
| `gEq{\|EITHER\|}`, mismatched | `differed` | the two values; the class sees only their two `Position`s |
| `gEq{\|EITHER\|}`, matched | — | hardwired into the walk |
| `gEq{\|CONS\|}`, `gEq{\|FIELD\|}` | — | no records, and a constructor is a `Position` rather than a name |

That currying is the whole of it. Because the class never names `PAIR` or `EITHER`, it never
applies a variable to a type, and the mechanism fits in a language with no kinds. Because it
never receives the sub-values, the three bindings never mention the class variable, and each is
an ordinary function over `R` that stands for every type deriving the class at once.

The price is paid in generality and is the subject of the next two sections.

The nearest relative outside deriving proper is C++20's defaulted three-way comparison, which
fixes the walk in the language and lets the type supply only the answer's category — one degree
*less* general than Zelkova, since the combinators are fixed too:

```cpp
struct Colour { int r, g, b; auto operator<=>(const Colour&) const = default; };
```

It is worth noting for a second reason. C++ synthesizes `<`, `>`, `<=` and `>=` from that single
`<=>`, which is exactly why [Zelkova's `Comparable` has one
member](type-classes.md#what-the-standard-library-declares) and the four ordering operators are
ordinary constrained functions rather than members: a lexicographic `lt` cannot be folded out of
the `lt` of each argument pair, and a lexicographic `compare` can.

## What the two-value shape gives up

A member carries a derivation only at `a -> a -> R`. Sorted by how far out of reach each excluded
shape is:

| Shape | Examples | Reachable? |
|---|---|---|
| `a -> a -> R` | `eq`, `compare`, a difference count | specified today |
| `a -> R` | `hash`, a size, a checksum | one value instead of two — a smaller mechanism ([`SPEC-25`](../tickets/spec-25.md)) |
| `a -> String` | `toString`, `show`, `Debug` | needs names and downward context; see below |
| `a -> a -> a` | `add`, `append`, a merge | no third value to produce |
| `R -> a`, `a`, `List a` | `decode`, `bottom`, `allValues` | needs the walk to run backwards |

`a -> R` is the expensive omission, because it is the family every other language derives most:
Haskell's `Show`, Rust's `Debug`/`Hash`/`Serialize`, Swift's `Hashable` and `Codable`,
PureScript's `genericShow`. As it stands, Zelkova can derive `Eq` and `Comparable` and then
nothing anyone asks for next.

**`toString` is the honest limit, and it is not a matter of arity.** Three separate things a fold
cannot supply:

1. A constructor's **name**. A `Position` orders and converts to an `Int`; neither yields `"Red"`.
2. **Parenthesisation**, which is context handed *downwards*. Haskell's derived `Show` is
   `showsPrec :: Int -> a -> ShowS` precisely because it threads a precedence into the arguments;
   a walk that folds answers upwards has no downward channel.
3. Knowing which argument is **first**, to place a separator. A fold over `String` with `""` as
   its identity gives `RedGreen`, never `Red Green`.

Two of Zelkova's own gaps bite here and are noted in [the chapter's open
questions](type-classes.md#open-questions): with no records there is no field label for any
scheme to use, and with no lists `combine` must be binary and pairwise, which is the root of the
failure in [what a lexicographic fold gets wrong](#what-a-lexicographic-fold-gets-wrong).

## Exposing constructor identity

Every language that derives an ordering has to say what a constructor *is* to the derivation, and
they agree more than they differ.

- **Haskell** uses the constructor's index and the Report states the consequence outright.
- **Rust** uses the discriminant, and this is a live footgun: reordering the variants of an enum
  under `#[derive(PartialOrd)]` is a silent breaking change, the documentation and the
  implementation have disagreed about whether declaration order or discriminant order governs,
  and one proposed fix is to require explicit discriminants before the derive is allowed.
- **Zelkova** uses the declaration position and [says so in the
  chapter](type-classes.md#what-a-derived-instance-computes), with the reason: the order a reader
  can see is the order that decides.

Where Zelkova differs is the *type*. Haskell and Rust hand the derivation an integer; Zelkova
hands it a `Position`, which has `Eq`, `Comparable` and `positionIndex : Position -> Int` and
nothing else. The benefit is narrow and real — arithmetic on a constructor's position no longer
typechecks, so a derivation cannot accidentally mean something by it. The cost is a type the
compiler must know by name, and two instances in `std/core` that exist only to serve
`Comparable`'s own derivation. It is the smallest of the design's decisions and the one most
easily reversed.

What no scheme here gives the class is the two *values* at a mismatch. That rules out a
`diff : a -> a -> Patch` — `differed` can report "constructor 0 against constructor 2" and
nothing about the payloads. Handing them over would put the class variable back into the
derivation's bindings and cost the property that makes them ordinary functions, so it was not
done.

## What a lexicographic fold gets wrong

`combine` folds the arguments' answers left to right. That is right for `Eq` and `Comparable`,
and the interesting question is what it is wrong for.

**It is not wrong for a class whose fields have a different order of significance.** Every
language's answer to that is "then write the instance by hand," and that is what deriving means.

**It is wrong, silently, for an answer whose meaning depends on how many things were combined.**
An average, a ratio, "what proportion of the fields matched". The chapter now states the law this
violates — `combine` associative, `matched` its two-sided identity — and states that nothing
checks it, with a worked failure in [What a derivation is trusted to
keep](type-classes.md#what-a-derivation-is-trusted-to-keep). Haskell's derived `Ord` relies on
the same law without naming it, since `Ordering`'s monoid is what `compare a1 b1 <> compare a2 b2`
folds under; the law is not a Zelkova-specific hazard, only a Zelkova-specific piece of prose.

**It very nearly went wrong on strictness, for a reason particular to this language.** Haskell's
derived `Eq` stops at the first unequal pair because Haskell is lazy. Elm's `==` stops because it
is a primitive. Zelkova is [strict](evaluation-semantics.md#evaluation-is-strict) and [nothing
short-circuits](evaluation-semantics.md#nothing-short-circuits), so a `combine` *called* as a
function would compare every argument pair in the whole value before the outermost call ran. The
compiler could not fix this without reading `combine` and understanding what its answer means,
which is the one thing this design refuses to do.

The way out keeps the refusal: [the three bindings are
inlined](type-classes.md#the-three-bindings-are-inlined-not-called) into the walk rather than
called, so a `case` written inside `combine` is an ordinary `case` under the ordinary rule, and
the class short-circuits itself by how it is written. The compiler still knows nothing about
`Eq`. This is the one place where the survey changed the design rather than confirming it.

## Sources

The mechanisms above, in the documentation that defines them.

- [The Haskell 98 Report, chapter 10: Derived Instances](https://www.haskell.org/onlinereport/derived.html)
- [The Haskell 2010 Report, chapter 11: Specification of Derived Instances](https://www.haskell.org/onlinereport/haskell2010/haskellch11.html)
- [GHC User's Guide: Deriving any other class](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/derive_any_class.html)
- [GHC User's Guide: Generic programming](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/generics.html)
- [Clean 2.2 Language Report, chapter 9: Generic Programming](https://clean.cs.ru.nl/download/html_report/CleanRep.2.2_9.htm)
- [Scala 3 Reference: Type Class Derivation](https://docs.scala-lang.org/scala3/reference/contextual/derivation.html)
- [rust-lang/rust#75620: Misleading documentation for derived Ord/PartialOrd for enums](https://github.com/rust-lang/rust/issues/75620)
- [PureScript documentation: Type Class Deriving](https://github.com/purescript/documentation/blob/master/guides/Type-Class-Deriving.md)
- [Lean 4 Language Reference: Deriving Instances](https://lean-lang.org/doc/reference/latest/Type-Classes/Deriving-Instances/)
