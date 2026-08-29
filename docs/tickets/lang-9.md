# LANG-9 · A type argument must be a bare name, so `Maybe (Maybe Int)` does not parse

**Sizing:** small in the grammar, but it is a grammar change, so `CLAUDE.md`'s *A grammar
change is never a one-file change* applies.

**Location:** `src/compiler/parser/grammar.lalrpop` — `AtomicType` (a qualified type name or a
type variable, and nothing else) and `ArgType`, whose application production is
`<name: QualTypeIdent> <types: AtomicType+>`.

**Decided (`SPEC-5`, by the language owner):** a parenthesised type is a type like any other,
and may appear as an argument in a type application. Nesting an application inside another one
is written with parentheses, and there is no depth limit.

**Problem:** `AtomicType` has no parenthesised alternative, and the parenthesised productions
live one level up in `Type`. An argument therefore has to be a bare name or a variable, and
every nested type is a syntax error:

```zel
type Maybe a
  = Just a
  | Nothing

f : Maybe (Maybe Int)      -- UnexpectedToken at the `(`
```

```zel
type Box a
  = Box a

f : Box (Int -> Int)       -- UnexpectedToken at the `(`
```

The same limit applies inside a `type` declaration, where a variant's arguments are parsed by
the same production — so `type Tree a = Node (Tree a) (Tree a)`, the shape a recursive
container is written in, cannot be declared at all. The `.ignored` modules under
`std/core/src/` are full of this: `Set.ignored`'s `Set_elm_builtin (Dict.Dict t ())`,
`Task.ignored`'s `List (MyCmd msg)`, `Result (Browser.DomError) ()`.

`grammar.lalrpop` already flags the area as unfinished — "Here I need to think about this some
more (and need better test coverage)" sits directly above the application production.

Note the workaround the grammar leaves open is a trap rather than a workaround: `type W a = W
Maybe a` parses, and produces a constructor taking *two* arguments, `Maybe` (unapplied) and
`a`. Nothing reports that today, for the separate reason in [BUG-17](bug-17.md).

Found while writing [`docs/spec/types.md`](../spec/types.md) (`SPEC-5`).

**Approach:** add a parenthesised alternative to `AtomicType`. The obvious spelling —
`"(" <Type> ")"` — makes the grammar ambiguous against `Type`'s own three parenthesised
productions, which additionally swallow an optional trailing `-> T`; LALRPOP will say so.
Expect to restructure rather than to add one line: the usual shape is for the parenthesised
forms (group, two-tuple, three-tuple) to live in `AtomicType`, with `Type`'s arrow production
taking `ArgType -> Type` as it already does and the trailing-arrow special case disappearing
because `(Int, Char)` is then an ordinary atomic type that an arrow can follow.

`Type::parenthesized` (`parser/mod.rs`) exists only to append that optional arrow and would go
away with it. Its doc comment explains the current shape and must not outlive it —
`CLAUDE.md`: *a doc comment describes what the code at that site does*.

Sequence this against [BUG-17](bug-17.md) rather than merging them: this ticket makes nested
arguments *writable*, and BUG-17 is what makes any argument mean anything. Landing this one
first is fine — it adds no new wrong behaviour — but the two together are what make
`f : Maybe (Maybe Int)` actually check.

**Acceptance:** `f : Maybe (Maybe Int)`, `f : Box (Int -> Int)` and
`type Tree a = Node (Tree a) (Tree a)` all parse, with tests in the parser's own test module
asserting the resulting `TypeKind` nesting. `cargo run` still prints `parsed 8 modules` and
lists all eight as checked. The two `expect=unimplemented` blocks in
[`docs/spec/types.md`](../spec/types.md)'s *Applying a type to arguments* section go red — that
tag's whole job — and are retagged `expect=ok` with their `**Not implemented:**` paragraph
deleted.
