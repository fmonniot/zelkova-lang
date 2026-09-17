# BUG-16 · An unresolved type name is invented rather than reported

**Severity:** medium (wrong behaviour under normal use — a misspelled type name is accepted
silently and surfaces later, if at all, as a type error about something else).

**Location:** `src/compiler/canonical/mod.rs` — `Type::from_parser_type`, the `None` arm of
its `env.find_type(name)` match.

**Problem:** a type name that resolves to nothing is not reported; a type is fabricated for
it instead.

`from_parser_type` looks the name up and, on a miss, builds a `Type::Type(name, args)` out of
thin air:

```rust
parser::TypeKind::Unqualified(name, vars) => match env.find_type(name) {
    Some(declared) if declared.arity() == args.len() => { .. }
    Some(declared) => Err(Error::TypeArityMismatch(..)),
    None => Ok(Type::Type(name.clone(), args)),
},
```

So `label : Widgt` canonicalizes without complaint, and the invented type then flows into the
typer as a distinct nominal type that unifies with nothing.

Found while writing [`docs/spec/modules.md`](../spec/modules.md) (`SPEC-3`), whose *What an
import's `exposing` list does* section carries the remaining `**Known gap:**` block for it.
The annotation site belongs to the planned *Types and type annotations* chapter and is
recorded here so a fix covers it.

**What is already done:** the import side. `process_import`'s
`parser::ExposedKind::Upper(_, Privacy::Private)` arm — a bare `Size` entry in an import's
`exposing` list — used to insert a type without consulting the interface it was importing
from, so `import Widget exposing (Size)` was checked when written `Size(..)` and unchecked
when written `Size`. It now looks the name up and raises `EnvError::UnionNotFound` with an
`ERR-7` suggestion, the same as its `Public` sibling. That half is independent of the
question below and landed on its own; `git log -S"unknown_opaque_exposed_type"` finds it.

**Fix:** in `from_parser_type`, return a canonicalization error instead of fabricating. That
needs a new `canonical::Error` variant naming the type and carrying `tpe.span` — `parser::Type`
has a span (`ERR-3`) so the caret lands under the name. Type *variables* arrive as
`TypeKind::Variable` and must keep resolving to nothing.

Two things go red on their own alongside it, both measured:

- **A `type` declaration cannot name a type the same module declares.** `do_types` runs
  against the environment *before* `canonicalize` inserts the module's own unions into it, so
  `type Never = JustOneMore Never` (`std/core/src/Basics.zel:963`) resolves `Never` to
  nothing, and so does every variant naming a sibling declaration — `Count`, `Flag`, `Chain`
  and `Nat` across five spec chapters. Registering each declared name and its arity before
  `do_types` runs, and letting the existing `insert_union_type` fill the constructors in
  afterwards, is enough.
- **The `EnvError`/`Error` split hides the variant from integration tests.** `canonical`'s
  `mod environment;` is private, so `EnvError` cannot be named from `tests/`; a test there
  has to assert on `PhaseError::message()` instead of on the variant. A unit test inside
  `environment.rs` is the only place the variant itself can be matched.

**No longer blocked:** [`LANG-58`](README.md), closed, seeded the [scalar type
names](../spec/types.md#scalar-types) into every module of a package exempt from [the default
imports](../spec/modules.md#the-default-imports) — `zelkova-core` today. `Js/Basics.zel` and
`Js/Utils.zel` name `Int`, `Float` and `Bool` with no `import` line, and they are exactly the
two modules that package covers, so both now have a real, non-fabricated spelling for the three
names this fix would otherwise take away from them. `SPEC-31` asked that question and
[`DEC-15`](../decisions/dec-15.md) answers it; the shape `LANG-58` implemented is decision 3,
and decision 1 settled `BUG-26` with it.

**Measured** (2026-09-17, on a scratch branch, discarded afterward): the `do_types` change
above, `from_parser_type`'s `None` arm returning an error, and the scalar names widened to every
scope — not just the package `LANG-58` seeds them in, purely to see what else moved — leave
`cargo run` unchanged: it still checks all eight modules. `cargo test --workspace` is not simply
green once the five `env.types.len()` assertions in `environment.rs` are bumped by five each,
though — eleven more tests fail. One,
`an_unresolved_type_name_is_attributed_to_the_module_under_check`, is this ticket's own anchor
for the fabrication behaviour being replaced, so its failure is the fix working, not a new cost.
The other ten are not about unresolved names at all: `function_multiple_parameters`,
`foreign_facade_module`, `if_then_else_expression`,
`qualified_and_unqualified_spellings_canonicalize_to_one_head`, `tuple_of_three_canonicalizes`,
`tuple_of_two_canonicalizes`, `tuple_pattern_canonicalizes` and
`tuple_pattern_of_three_canonicalizes` in `tests/compiler/canonical.rs`, plus
`an_unresolved_qualified_scalar_name_is_not_the_scalar` and
`an_unresolved_qualified_type_name_is_not_a_local_type_of_the_same_stem` in `tests/typer.rs`,
each write a bare `Int`/`Char`/`Bool` with no `Basics` interface in scope and lean on today's
fabrication — attributed to the module under check — purely incidentally, never asserting
anything about it. Widening the seed resolves those names to `Basics.Int` and friends instead,
so the whole-value assertions built against the old fabricated head stop matching. Implementing
this fix for real has to update all ten of those call sites (and decide whether the scalar seed
really should widen to every scope, which is a design question this ticket does not itself
settle), not only the five `environment.rs` lines this paragraph used to name.

**Acceptance:** `label : Nope` in a module that declares no `Nope` fails with the new error,
and `label : a` (a type variable) still compiles — tests in `tests/compiler/canonical.rs`.
`cargo run` still prints `parsed 8 modules` and lists all eight as checked. The remaining
`**Known gap:**` block in [`docs/spec/modules.md`](../spec/modules.md) — the one under the
`exposing (Size)` example in *What an import's `exposing` list does* — goes red and is
deleted with its paragraph.
