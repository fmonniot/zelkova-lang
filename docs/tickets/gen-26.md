# GEN-26 · A `case` over a tuple of constructors emits code exponential in its number of branches

**Sizing:** medium. The change is in how a `Decision` represents a fallback and how the emitter
writes it. What could make it bigger is the choice below: a DAG in the IR touches every reader of
`Decision` (the emitter, `ir.rs`'s tests, and whatever [`GEN-15`](gen-15.md) builds), where a
local in the emitted JavaScript touches only `zelkova-js`.

**Location:** `crates/zelkova-compiler/src/ir/decision.rs` — `build`, `lower` and the
`default` field of `Decision::Test`; `crates/zelkova-js/src/lib.rs` — `Emitter::decision`, which
emits `matched` and `default` each in full, and `Emitter::case_expression`. The module doc
comment of `decision.rs`, *Recursive over a pattern's sub-patterns*, states the copying and cites
this ticket.

**Found while:** reviewing the PR for [`LANG-16`](README.md) (closed), which let a constructor
or a literal sit below the top of a pattern. Before it no program could reach the problem: a
sub-pattern below the top was a variable, `_` or `()`, so a branch held at most one `Decision::Test`.
`LANG-16`'s Acceptance scopes an emitter that cannot handle a nested test out as "a `GEN-` ticket
to file and not a fix to make here", so the cost is recorded here and not repaired there.

**Problem:** `build` lowers branch `i` against `on_fail = build(rest)`, the tree for every branch
after it, and `lower` clones `on_fail` into the `default` of **every** `Test` the branch holds. A
branch with `k` refutable parts (each constructor or literal anywhere in its pattern is one)
therefore holds `k` full copies of the tree for the branches after it, and those copies are
themselves built the same way. The tree's size is the **product** over the branches of each
branch's `k`, not the sum. `Emitter::decision` writes each `default` where it stands, so the
JavaScript has the same size, and a leaf's body is re-emitted once per copy of its leaf. A
`case` over a pair or a triple of constructors, which is how Elm writes `update`, is the common
shape that reaches it: `case (msg, model) of`.

Measured at this branch's tip (`cargo run -- compile` on a scratch package whose `main` is
`Task.succeed ()` and whose only other declaration holds the `case`; the bytes are those of the
emitted `App.mjs`, and `$abort(` is counted by `grep -o`):

| `case` | branches | `k` per branch | emitted | `return $abort(` copies |
|---|---|---|---|---|
| `(Flag, Flag, Flag)`, a `Flag` of three constructors, the first eight of the 27 combinations | 8 | 3 | 2,240,875 bytes, 45,937 lines | 6,561 (3^8) |
| `(Msg, State)` with eight `Msg` constructors and two `State`, every `(Mi, State)` | 16 | 2 | 40,762,904 bytes, 655,374 lines | 65,536 (2^16) |

Each compiles with no diagnostic and, as far as the review that found it ran them, computes the
right answer under node: this is size, not a miscompile. Going further is the review's
measurement and not one this ticket reproduced: 11 branches over `(Int, Int, Int)` literals plus
`_` was reported at 85 MB, and a 20-branch `case` over a pair (about 2^20 copies) at 794 MB,
which node cannot load (`Cannot create a string longer than 0x1fffffe8 characters`). Nothing in
the compiler warns before that point, and a program that compiles but cannot be loaded is the
worst form of it.

**Approach:** the ticket does not choose between these; it is a decision for whoever takes it.

1. **Share each fallback in the emitted JavaScript.** Emit the tree for the branches after
   branch `i` once, as a local such as `const $next_i = () => { … }` that every inner `default`
   of branch `i` calls, so branch `i`'s tests cost `k` calls and not `k` copies. The emitter
   cannot recover the sharing from today's `Decision`, since the copies are structurally equal
   but separate values, so this needs the IR to say which `default`s are one (next option) or
   the emitter to build the fallback itself. It is JavaScript-specific; a backend that wants a
   jump rather than a call, as WebAssembly does, would redo it.
2. **Make `build` produce a DAG.** A shared node (an `Rc`, or an index into a table the
   `Decision` carries) for each fallback, so the IR states the sharing and every backend reads
   it. `Decision`'s doc comment already says a body can appear in more than one leaf, "which a
   borrow makes free"; this makes the tree equally free. It changes `Decision`'s equality, which
   is defined structurally and by leaf address, and the test helpers that read a tree as a
   tree.
3. **Compile the match as a table** — group the branches that test one value, so a constructor
   is tested once. This is the classical answer and removes the repeated tests as well as the
   repeated trees, but it contradicts the decision `decision.rs`'s *A chain, not a table*
   records (branches are tried in the order written, one at a time), and is the largest of the
   three. Naming it here so it is not rediscovered; it needs a decision first.
4. **Refuse past a bound.** A diagnostic when a `case`'s tree would exceed some size. It fixes
   nothing, and the bound is arbitrary; it is only worth considering as a stop-gap and is a
   language-owner decision, since it makes a valid program a compile error.

**Acceptance:** `cargo test --workspace` and `cargo run -- test std/core` stay green, and the
emitted size is no longer multiplicative in the number of branches, checked by a test in
`crates/zelkova-js/tests/javascript.rs` that compiles a `case` over `(Flag, Flag, Flag)` with
eight branches (the first row above) and asserts that the emitted text holds far fewer than
6,561 `$abort(` calls: one per `case`, or one per fallback if the chosen shape gives each its
own. The assertion names the number it expects, and is seen red against the current code. A
second test in the same file doubles the branch count and asserts the output at most about
doubles. An `ir.rs` test pins whatever the IR now says about a shared fallback if option 2 is
taken. `Wrapper (Circle n)`'s emitted text in `a_nested_constructor_pattern_is_an_if_inside_an_if`
and every evaluation-order test stay as they are, since the order branches are tried in does not
change.
