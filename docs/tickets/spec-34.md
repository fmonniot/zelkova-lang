# SPEC-34 · `Basics` declares three types the compiler already supplies

**Sizing:** small-to-medium. The compiler change is a deletion; what it costs is a home for
three doc comments, and that is the part to decide first.

**Location:** `std/core/src/Basics.zel` — `type Int` at `:123`, `type Float` at `:146`, `type
Bool` at `:458`, and the `Int, Float` and `Bool(..)` entries of its `exposing` header;
`std/core/src/Bitwise.zel:16`, `std/core/src/Maybe.zel:22` and `std/core/src/Result.zel:25`,
whose `import Basics exposing (…)` lines name them.

**Blocks nothing.** Related to [`BUG-26`](bug-26.md), which is the type checker's half of the
same duplication.

**Problem:** `Int`, `Float` and `Bool` reach a module twice.

[`DEC-15`](../decisions/dec-15.md) made the five scalar type names
[built-in](../spec/types.md#built-in-type-names) — supplied by the compiler, in scope in every
module — because the facades `Basics` is built from have no import that could bring them in.
`Basics` was left declaring three of them, and exposing all three, so an ordinary module
resolves `Int` through [the default import](../spec/modules.md#the-default-imports) and would
resolve it without one.

Nothing breaks today, and the reason is worth stating: the compiler identifies a type by its
unqualified name, so `Basics`' `Int` and the built-in `Int` are the same type by spelling
rather than by anything either of them says. The day type identity carries the module a name
came from, they become two types and every annotation in `std/core` picks whichever the scope
happened to hold.

`type Int = Int` is also a fiction in its own right — a nullary constructor standing in for a
machine integer, with `NOTE: The compiler provides the real implementation.` written beside it.

**What has to be decided first:** where the documentation goes. Those three declarations carry
the chapter-length doc comments a reader looks `Int` up to find, and a declaration is what a
future documentation generator would hang them on. Deleting them without an answer moves the
problem rather than solving it. Three shapes:

1. **A doc comment with no declaration** — `Basics` keeps the prose as module-level
   documentation naming the built-in. Cheapest, and it leaves the generator with nothing to
   attach.
2. **The prose moves to [`docs/spec/types.md`](../spec/types.md#built-in-type-names)**, which
   is where the rule is. The chapter is not reference documentation and would grow a section
   that reads like it.
3. **The declarations stay and the compiler learns they are the built-ins** — a `type` whose
   name is built-in is accepted as documentation for it rather than as a second type. This is
   the only shape that keeps one record and one home, and it is the most compiler work: the
   canonical phase has to tell that case apart from [a module legitimately shadowing the
   name](../spec/types.md#built-in-type-names), which is the same distinction `BUG-26` needs.

**Acceptance:** whichever shape wins, `Basics`' three declarations and the built-in names are
one record rather than two, and the **Known gap:** paragraph naming this ticket in
[`docs/spec/types.md`](../spec/types.md#built-in-type-names) goes with it. `cargo run` still
prints `parsed 8 modules`, lists all eight as checked, and exits 0; `cargo test --workspace`
is green.
