# LANG-88 · A derived member's size is exponential in a constructor's arity when `combine` names its second parameter more than once

**Sizing:** medium. The bound is a design change rather than a fix: with no `let` and no lambda
there is nowhere to bind the rest of the walk without evaluating it. What could make it bigger is
what the emitted JavaScript does with a helper per fold level.

**Part of:** *Active work: type classes* in [the index](README.md), after `LANG-83`. Found in the
review of `LANG-83`'s PR, which measured it and did not bound it.

**Location:** `crates/zelkova-compiler/src/canonical/derivation.rs` — `Generated::rewrite`, whose
`VarLocal` arm returns `with.clone()` for each mention of the local `Scope::replaced` stands for;
`Generated::place_combine`, which sets `replaced` to `combine`'s second parameter and the rest of
the walk; the module doc comment, *A derived instance is given its members*.
`docs/spec/type-classes.md` — [*The bindings are inlined, not
called*](../spec/type-classes.md#the-bindings-are-inlined-not-called).
[`DEC-24` decision 8](../decisions/dec-24.md#8--combines-first-parameter-is-a-value-and-its-second-is-the-rest-of-the-walk).

**Problem:** `combine`'s second parameter is replaced by the rest of the walk at every place the
body names it, so the rest is evaluated where the body reaches it and nowhere else. That is the
run-time rule `DEC-24` decision 8 asks for, and at run time each path evaluates the rest at most
once. The code that results holds one copy of the rest per mention, and the rest of a constructor's
`n` arguments contains the copies of what follows it, so a body that names the parameter `k` times
makes `k^n` copies. Measured with `combine x y = case x of EQ -> y; LT -> y; GT -> x` (`k = 2`) for
`Comparable`, deriving a single constructor with `n` `Int` fields and counting the mentions of
`compare` in the member's canonical body:

| n | mentions of `compare` | size of the member's `Debug` text |
|---|---|---|
| 2 | 4 | 10 KB |
| 6 | 64 | 170 KB |
| 8 | 256 | 690 KB |
| 10 | 1024 | 2.7 MB |

A class author's `combine` that names its second parameter twice, written in the most natural
spelling, hangs the compiler on a type of a few more fields. Nothing in the chapter or in `DEC-24`
bounds it, and a reader of either would not expect it.

**Approach:** the ticket does not pick. The options it knows of:

1. **A generated helper per fold level**, called at each mention: the rest becomes a call with the
   walk's parameters, and each mention is a call and not a copy. Size is linear. The laziness holds
   because a call is evaluated where the body reaches it. It is a design change: the instance gets
   more definitions than the class has members, and `GEN-24`'s specialisation has to emit them.
2. **`let`** ([`LANG-33`](lang-33.md)), once the language has it, binding the rest where it is
   named more than once. It evaluates the rest eagerly unless `let` is lazy, which is a language
   question this ticket does not settle.
3. **Bound the arity or the mentions**, and report an error naming the derivation. It rejects a
   program the chapter says is valid, so it needs a **Known gap:** paragraph in the chapter.

**Acceptance:** a test in `crates/zelkova-compiler/tests/ir.rs` or `canonical.rs` derives the
`Comparable` above for a constructor of ten `Int` fields and asserts the member's size is at most
linear in the arity, seen red against the current `rewrite`. The module doc comment of
`derivation.rs`, which states today's cost, is rewritten to say what is true after the change.
`cargo test --workspace` is green and `cargo run -- compile std/core` still lists all ten modules
as checked.
