# LANG-11 · A type annotation may sit anywhere in the file, and a repeated one silently wins

**Sizing:** small-to-medium. The duplicate check is small and has a `TODO` waiting for it; the
adjacency rule needs the grammar to stop treating an annotation as an unordered declaration.

**Location:** `src/compiler/parser/mod.rs` — `Module::from_declarations`, which buckets
`Declaration::Function` and `Declaration::FunctionType` into a `HashMap<Name,
Vec<Declaration>>` keyed by name, then folds each bucket with `tpe.replace(t.tpe)`. The
`// TODO Error if more than function type is defined` sits on the line above.

**Decided (`SPEC-5`, by the language owner):** a value or function declaration carries **at
most one** type annotation, and that annotation is written on the line directly above the
declaration it annotates, with nothing between the two — not another declaration, not a
comment, and not a blank line. A comment about the declaration goes above the annotation.

That is stricter than an earlier draft of this ticket, which allowed blank lines and comments
between the two, and it changes what the check needs. Adjacency in *declaration order* is no
longer sufficient: a blank line, and a comment, both leave the declaration order unchanged, so
neither is visible to a check that only asks which declaration comes next.

Answering "is the declaration on the very next line" needs something the AST does not carry.
`NodeSpan` (`src/compiler/position.rs`) holds byte offsets only — the tokenizer's `Position`
has `line` and `column`, but they are dropped by the time a node is built — so the check needs
either the source text in hand (to count newlines between the annotation's end and the
declaration's start, which also answers the comment case: anything but one newline and
indentation in that gap is a violation) or a line number kept on the two nodes. Settle which
before starting; the first needs no AST change but has to run somewhere the source is
available, which `Module::from_declarations` is not.

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

*Adjacency* is the larger half, and it is two conditions rather than one. The first needs the
order of declarations, which `from_declarations` throws away: keep the parsed
`Vec<Declaration>` order and require, while bucketing, that a `FunctionType` for `name` is
immediately followed by a `Function` for the same `name`. The second is the line-adjacency
condition in the *Decided* clause above, which declaration order cannot see at all, and which
is what rules out a blank line or a comment in the gap.

Note this interacts with the multi-line function declarations Zelkova has and Elm does not:
several `Function` declarations for one name are legal and adjacent, and the annotation
precedes the run rather than each member of it. Whether the members of such a run must
themselves be on consecutive lines is *not* decided here — this ticket's rule is about the
annotation and the declaration it annotates. Don't settle it by accident in the
implementation.

A `module javascript` facade is annotations with no bodies at all
([`docs/spec/js-interop.md`](../spec/js-interop.md)) and must keep working — whatever rule is
written has to exempt it, or be phrased as "an annotation is followed by the declaration it
annotates, if that declaration exists".

**Acceptance:** two annotations for one name is an error naming the name, with a caret under
each — a test in `tests/compiler/canonical.rs`. Three separations are errors, each with its own
test: another declaration between the annotation and its body, a blank line between them, and a
comment between them. An annotation directly above its declaration still compiles, including a
multi-line one. A `module javascript` facade still compiles, and `cargo run` still prints
`parsed 8 modules` and lists all eight as checked.

In [`docs/spec/types.md`](../spec/types.md)'s *Where an annotation goes* section, all three
`expect=ok` blocks go red and are retagged with their `**Known gap:**` paragraph deleted.
