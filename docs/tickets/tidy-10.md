# TIDY-10 · `Interface::arities` is a parallel map, and a miss silently reads as arity 0

**Sizing:** small-to-medium — a shape change that touches several call sites but adds no new
logic; mostly moving an existing `usize` into a tuple in place of a second map.

**Location:** `crates/zelkova-compiler/src/lib.rs` — `Interface`'s `values`, `infix_functions` and `arities`
fields; `crates/zelkova-compiler/src/canonical/mod.rs` — `Module::to_interface`, which builds all three;
`crates/zelkova-compiler/src/typer/mod.rs` — the two `.unwrap_or(0)` sites that read `arities` (building
`foreign_arities` from `interface.arities.get(name)`, and `foreign_arity`'s own
`self.foreign_arities.get(qname)`); every hand-built `Interface` in `crates/zelkova-compiler/tests/support/mod.rs` and
inline in test files.

**Problem:** `arities: HashMap<Name, usize>` is kept beside `values: HashMap<Name, (NodeSpan,
Type)>` and `infix_functions: HashMap<Name, (NodeSpan, Type)>` rather than carried in the same
tuple as each value's type. Both non-test readers of it — the two sites named above — fall back
to `unwrap_or(0)` on a miss, which is read as "arity 0, a parameterless binding".

Today this is safe by inspection rather than by construction. `canonical::Module::to_interface`
is the only non-test constructor of `Interface`, and it records an arity for every key of both
`values` and `infix_functions` (added by the fix for [`BUG-43`](README.md)); every real
`VarForeign` resolves through one of those two maps, keyed exactly as `to_interface`'s `arities`
is. The eleven hand-built test interfaces that omit `arities` entirely are the one case the
fallback exists for, and there it is correct: they all model "every export is a parameterless
binding".

The risk is latent, not present. `arities` is a *parallel* map: nothing stops a future change
from adding a value to `values`, to `infix_functions`, or to some third map, without also
updating `arities` to match. That omission would silently reintroduce
[`BUG-43`](README.md)'s miscompile — a genuinely multi-parameter export read as arity 0, so an
importer calls it one argument at a time instead of directly — with no diagnostic and no test
that would necessarily catch it, because a hand-built test interface already models the same
shape as the bug on purpose.

Found while reviewing the `BUG-43` fix (PR #259); left unfixed there because the PR's own
`to_interface` change keeps every real path correct today, and the review said so explicitly
rather than asking for it as part of that fix.

**Approach:** carry the arity alongside the type in the same tuple on both `values` and
`infix_functions` — `HashMap<Name, (NodeSpan, Type, usize)>` in place of `HashMap<Name,
(NodeSpan, Type)>` plus a separate `arities` map — so a value present in either map always has
an arity by construction and the omission becomes unrepresentable. This is the same move
`CLAUDE.md`'s `Tuple<T>` invariant already made for tuple arity: put the rule in the *shape* of
the type rather than in a check kept in step by hand across two collections. Once done, both
`unwrap_or(0)` fallbacks in `crates/zelkova-compiler/src/typer/mod.rs` go away, `Interface::arities` is
deleted, and every hand-built interface in `crates/zelkova-compiler/tests/support/mod.rs` and elsewhere is updated to
the new tuple shape (a parameterless binding still writes arity `0` there, just in the same
tuple as its type).

**Acceptance:**

- `Interface` has no `arities` field; `values` and `infix_functions` carry arity in their tuple.
- `grep -n "unwrap_or(0)" crates/zelkova-compiler/src/typer/mod.rs` is empty.
- `cargo test --workspace` is green.
- `cargo run -- compile std/core` still prints `parsed 8 modules`, lists all eight, and exits 0.
