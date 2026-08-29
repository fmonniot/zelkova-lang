# LANG-11 · A type annotation may sit anywhere in the file, and a repeated one silently wins

**Sizing:** small-to-medium. The duplicate check is small and has a `TODO` waiting for it; the
adjacency rule needs the grammar to stop treating an annotation as an unordered declaration.

**Location:** `src/compiler/parser/mod.rs` — `Module::from_declarations`, which buckets
`Declaration::Function` and `Declaration::FunctionType` into a `HashMap<Name,
Vec<Declaration>>` keyed by name, then folds each bucket with `tpe.replace(t.tpe)`. The
`// TODO Error if more than function type is defined` sits on the line above.

**Decided (`SPEC-5`, by the language owner):** a value or function declaration carries **at
most one** type annotation, and that annotation is written immediately above the declaration
it annotates, with nothing between them but blank lines and comments.

An annotation is the first thing a reader of a declaration reads. Letting it live elsewhere in
the file means the question "what type is this" is answered by searching rather than by
looking up one line, and letting it be repeated means the answer can be two different things.

**Problem:** neither half holds. `from_declarations` groups by name and discards position
entirely, so an annotation may be written anywhere among the top-level declarations —
after its own body, or with unrelated declarations in between:

```zel
module Example exposing (f, g)

f : Int

g = 2

f = 1          -- accepted today; the annotation is three declarations up
```

And when a name carries two annotations, `tpe.replace(..)` keeps the **last** one, with no
diagnostic. Probed on a file declaring types `A` and `B`:

```zel
f : A
f : B
f = MkA
```

canonicalizes with `tpe: Type("B")` — so the type checker is checking the body against an
annotation the reader may well have taken as superseded, and would report a mismatch against
`B` on a declaration whose first stated type is `A`.

Found while writing [`docs/spec/types.md`](../spec/types.md) (`SPEC-5`).

**Approach:** two changes, and the duplicate one is worth landing on its own if the other
turns out to be large.

*Duplicates* are cheap: `from_declarations` already sees the whole bucket, so replace
`tpe.replace(t.tpe)` with a check that reports when `tpe` is already `Some`. It has no error
channel today — it builds a `Module` infallibly — so decide where the error goes before
starting. The lightest option is to leave the grammar alone and check in `canonicalize`
instead, which already returns `Vec<canonical::Error>`; that needs `Function` to carry every
annotation it saw rather than one, or a `Vec<NodeSpan>` beside it, so the error can put a
caret under both.

*Adjacency* is the larger half. It needs the order of declarations, which
`from_declarations` throws away. The tractable version is to keep the parsed `Vec<Declaration>`
order and require, while bucketing, that a `FunctionType` for `name` is immediately followed by
a `Function` for the same `name` — "immediately" being in declaration order, since blank lines
and comments never become declarations. Note this interacts with the multi-line function
declarations Zelkova has and Elm does not: several `Function` declarations for one name are
legal and adjacent, and the annotation precedes the run rather than each member of it.

A `module javascript` facade is annotations with no bodies at all
([`docs/spec/js-interop.md`](../spec/js-interop.md)) and must keep working — whatever rule is
written has to exempt it, or be phrased as "an annotation is followed by the declaration it
annotates, if that declaration exists".

**Acceptance:** two annotations for one name is an error naming the name, with a caret under
each — a test in `tests/compiler/canonical.rs`. An annotation separated from its body by
another declaration is an error. A `module javascript` facade still compiles, and `cargo run`
still prints `parsed 8 modules` and lists all eight as checked.

In [`docs/spec/types.md`](../spec/types.md)'s *Where an annotation goes* section, both
`expect=ok` blocks go red and are retagged with their `**Known gap:**` paragraphs deleted.
