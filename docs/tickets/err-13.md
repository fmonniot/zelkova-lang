# ERR-13 · A type error spells the numeric-literal type `number`, which the language reads as an ordinary type variable

**Sizing:** small. One `Display` arm and the wording of the note around it; the thought is in
picking a spelling that cannot be mistaken for source syntax.

**Location:** `src/compiler/typer/mod.rs` — the `Type::Number => write!(f, "number")` arm of
`impl Display for Type`, reached from `ErrorKind::message`'s
`format!("cannot match `{}` with `{}`", left, right)`.

**Found:** while writing
[`docs/spec/constrained-type-variables.md`](../spec/constrained-type-variables.md) (`SPEC-11`).

**Problem:** `Type::Number` is the type the checker gives an integer literal — it unifies with
`Int` and `Float` and nothing else. It has no source syntax: there is no way to write it in an
annotation, and nothing a user writes ever produces it. `Display` nevertheless renders it
`number`, so:

```zel
x : Char
x =
  1
```

reports **cannot match `Char` with `number`**.

Under the rule
[`docs/spec/constrained-type-variables.md`](../spec/constrained-type-variables.md) states, a
reader parses `number` in that sentence as a type variable named `number` — a lowercase name
in a type position is a type variable, and `number` is an ordinary one with no special
meaning. So the message reads as "cannot match `Char` with some type variable", which is both
wrong and unactionable: the variable is nowhere in their source.

The spelling is inherited from a language where `number` *is* the constrained variable and the
message is therefore coherent. Here the two are unrelated, and `SPEC-11` rewrote
`std/core/src/` off the spelling entirely, so the compiler is now the only place it appears.

`Display`'s own doc comment already draws exactly this distinction for the neighbouring case —
"Inference variables have no source syntax at all, so they are written `t3`" — and then
renders `Number` as though it did have one.

**Approach:** render it as something no annotation could be. `Int or Float` reads well in the
message this is nearly always in (*cannot match `Char` with `Int or Float`*) and cannot be
mistaken for a name. `number*`, or `{Int, Float}`, are alternatives; what matters is that a
reader cannot take it for an identifier.

Then check the neighbours: `Signature::of_type` in the test module renders the same type
`number` and `tests/typer.rs`'s expectations are written against that spelling, so they move
together. Whether the *test* rendering should follow the diagnostic one or stay as it is
worth a moment's thought — `Signature` exists to make a whole inferred type readable in an
assertion, which is a different audience.

Note this ticket does not decide what the numeric-literal rule *is*; that belongs to the
planned *Expressions* chapter ([SPEC-6](spec-6.md)), which `SPEC-11` handed it to. This is
only about how the type is spelled once it appears in a message.

**Acceptance:** a test in `tests/typer.rs` pinning the rendered message for a literal against
a non-numeric annotation, asserting the new spelling. No message anywhere renders a type using
a spelling the grammar would accept as a type variable.

**This gap has no red test behind it.** The spec harness stops at canonicalization
([TEST-2](test-2.md)), so no block in
[`docs/spec/constrained-type-variables.md`](../spec/constrained-type-variables.md) exercises a
type error at all; the chapter describes this in prose under a `**Known gap:**` lead-in and
that paragraph has to be deleted by hand when this lands.
