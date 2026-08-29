# LANG-10 · A trailing `|` and a variant-less `type T =` are both accepted

**Sizing:** small. One macro use in the grammar.

**Location:** `src/compiler/parser/grammar.lalrpop` — the `Union` production's
`<variants: Pipe<Type>>`, and the `Pipe<T>` macro itself:

```
Pipe<T>: Vec<T> = {
    <v:(<T> "|")*> <e:T?> => …
}
```

**Decided (`SPEC-5`, by the language owner):** a `type` declaration has **at least one**
variant, and `|` appears strictly between variants — never before the first, never after the
last.

The trailing-separator argument that won for an `exposing` list — see
[`docs/spec/modules.md`](../spec/modules.md#the-exposing-list), where a trailing comma is
allowed so that appending a name touches one line — does not carry over. A variant list is
conventionally written with the separator *leading* each line (`= Red`, then `| Green`), so
appending a variant already touches exactly one line and a trailing `|` buys nothing.

**Problem:** `Pipe<T>` accepts an empty list and a trailing separator, because its `<e:T?>`
tail is optional and its `(<T> "|")*` head is a repetition of zero or more. Both leak into
`Union` unchecked. Probed:

```zel
type B
  = X
  |
```

canonicalizes to `B` with one variant, the trailing `|` silently ignored. And:

```zel
type T
  =
```

canonicalizes to `T` with **zero** variants — a type nothing can construct, declared by
accident rather than on purpose, and accepted in silence. Its `UnionType` also carries a span
whose end is `BytePos(0)`, an inverted range that would misrender if anything ever pointed at
it.

A leading `|` is already rejected (`UnexpectedToken` on the `Pipe`), which is the specified
behaviour and needs no change.

Found while writing [`docs/spec/types.md`](../spec/types.md) (`SPEC-5`).

**Approach:** stop using `Pipe<T>` for `Union` and write the one-or-more, separator-only shape
directly — `<first: Type> <rest: ("|" <Type>)*>` — which is what the rule says and also gives
LALRPOP a shape it can report on. `Pipe<T>` has no other use in the grammar today; delete it
if this was the only one, rather than leaving a macro whose permissiveness nothing needs.

Check the error a trailing `|` then produces. LALRPOP will report `UnexpectedToken` against
the "expected" list for a variant; that is acceptable to pin in the chapter, and the chapter's
blocks must be retagged with whatever it actually says rather than with a guess.

Note this is a grammar change: `CLAUDE.md`'s *A grammar change is never a one-file change*
applies, though the parser AST's `UnionType.variants` stays a `Vec<Type>` and
`canonical/mod.rs`'s `do_types` needs no change for this ticket. (It needs one for
[BUG-18](bug-18.md), which is a different defect in the same declaration.)

**Acceptance:** `type T =` and `type B = X |` are both parse errors; `type C = X | Y` and a
single-variant `type D = X` still compile. Tests in the parser's own test module. `cargo run`
still prints `parsed 8 modules` and lists all eight as checked. The two `expect=ok` blocks in
[`docs/spec/types.md`](../spec/types.md)'s *A variant list has at least one variant* section go
red and are retagged `expect=parse-error:…` with their `**Known gap:**` paragraph deleted.
