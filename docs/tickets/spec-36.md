# SPEC-36 · The `double` block in `expressions.md` cannot go red for the reason its paragraph gives

**Sizing:** small. One block and its paragraph, unless the option that waits on other tickets is
taken.

**Location:** `docs/spec/expressions.md` — the `expect=unimplemented` block declaring
`double : Number a => a -> a` and the `**Not implemented:**` paragraph after it; `crates/zelkova-compiler/tests/spec.rs` — `stdlib_interfaces`, the
stand-in `Basics` a block compiles against.

**Depends on:** none to file; the block's real repair depends on [LANG-12](lang-12.md) and
[LANG-40](lang-40.md).

**Found while:** reviewing the PR that closed [LANG-37](README.md), which made `Number a =>` parse
and rewrote that paragraph. Not caused by that PR: before it the block failed in the parser and
would have reached this failure next.

**Problem:** the paragraph says a constraint is read and ignored, so nothing holds `a` to `Number`,
and that once [LANG-40](lang-40.md) does, the declaration is an error because `mul x 2` forces `a`
to be `Int`. The tag exists so the block goes red the day that is true. It fails today for a
different reason. `cargo test --test spec -- --nocapture` prints

```
docs/spec/expressions.md:60 (expect=unimplemented) failed in canonicalization, as expected: [VariableNotFound(QualName { … name: "mul" }, …)]
```

because the stand-in `Basics` in `stdlib_interfaces` (`support::basics_interface`) declares the
scalar types and no `mul`. So `LANG-40` landing does not flip the block: it stays an unbound
`mul`. Giving the block its own `mul : Int -> Int -> Int` does not help either, and was tried: the
block then *compiles cleanly*, because the constraint is ignored and annotation variables are not
rigid ([LANG-12](lang-12.md)), so the `unimplemented` test fails with "this feature looks
implemented now".

**Approach:** the ticket does not choose. Options:

1. **Wait, and repair the block when `LANG-12` and `LANG-40` have both landed**: declare `mul`
   in the block, and the failure the paragraph names appears by itself. The block stays wrong
   until then, and the paragraph should stop citing `LANG-40` alone as what flips it.
2. **Retag it `expect=fragment` now**, which says honestly that nothing checks it, and retag it
   `expect=type-error` with the declaration of `mul` once the two land. It costs the mechanism
   that would have told whoever lands `LANG-40` to revisit the paragraph.
3. **Give the stand-in `Basics` a `mul`**, so the block reaches the type checker. That is not
   enough by itself for the reason above, and it widens a helper every chapter compiles against.

**Acceptance:** the block's tag and the paragraph agree about why it does not hold today.
`cargo test --test spec -- --nocapture` names, for that block, the failure the paragraph gives (or
the block is `expect=fragment` and the paragraph says nothing is checked), and `cargo test --test
spec` is green.
