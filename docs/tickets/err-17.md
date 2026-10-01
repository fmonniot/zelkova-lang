# ERR-17 · A mistyped bare identifier or constructor body gets no caret of its own

**Sizing:** small-to-medium (the choice below decides which; the second option touches every identifier's blame).

**Location:** `crates/zelkova-compiler/src/typer/constraint.rs` — `collect`'s `TypedTermKind::Identifier(_) => ()` arm;
`crates/zelkova-compiler/src/typer/annotate.rs` — the annotation-versus-body constraint built for an annotated declaration;
`crates/zelkova-compiler/tests/typer.rs` — `a_mistyped_bare_constructor_body_is_blamed_only_through_the_annotation`.

**Problem:** `collect` adds no constraint for an `Identifier`, so a declaration whose whole body is a
bare name has nothing at the body's span to blame. With `Basics` in scope,

```zel
answer : Int
answer = True
```

reports `cannot match Int with Bool` with **one** label, under `answer : Int`. Nothing is under
`True`. A literal body (`answer = 'a'`) gets both: its own `Reason::Literal` constraint puts the
primary caret on the body and the annotation behind it. The same happens for any variable or
nullary constructor body: `answer = Red`, `m = Nothing`. A name nested in something larger is blamed
through the enclosing node (`if True then 2 else True` labels the branch, `b x = True` the
function), so only the bare-body case is left without a caret.

This is older than `LANG-1`, which is where it was noticed: `true` was a literal and so used to carry
the caret; with the keyword gone, `True` is a constructor and every boolean takes this path. It was
left unfixed there because it is a diagnostics change that reaches every identifier, not a part of
removing a keyword. The test above pins today's single label.

**Approach:** not decided. Two viable shapes:

1. Have the annotation-versus-body constraint of an annotated declaration carry the body's span
   as its primary location when the body is a bare identifier. Narrow, and it leaves `collect`'s
   `Identifier` arm alone, but it is a special case that other positions (an `if` branch that is a
   bare name) do not share.
2. Make an identifier contribute a constraint of its own, tied to its span, in the way a literal
   does. Uniform, but it changes the blame for every use of every variable and constructor, so
   every caret test has to be re-read.

**Acceptance:** `a_mistyped_bare_constructor_body_is_blamed_only_through_the_annotation` is
rewritten to expect a primary label on `True` and the annotation as the secondary, in that order,
with the same assertion style (`ranges(&error.labels())`); it is seen red before the change and
green after. `answer = Red` against an `Int` annotation gets the same two labels. `cargo test
--workspace` and `cargo run -- compile std/core` stay green.
