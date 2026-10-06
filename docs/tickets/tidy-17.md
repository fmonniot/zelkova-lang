# TIDY-17 · `Solved::Untranslatable` is unreachable from source, and the `Option` returns behind it are nearly so

**Sizing:** small-to-medium. Removing the variant touches `typer`, `ir::build`, one test that
builds it and the doc comments that cite it; keeping it as a defensive path is a doc-comment change
only. Either grows if
`ERR-8` has landed, since a warning would then need a reason to exist.

**Location:** `crates/zelkova-compiler/src/ir/mod.rs` — `Solved::Untranslatable` and the arm of
`declare` that matches it beside `Solved::UnboundName`; `crates/zelkova-compiler/src/typer/mod.rs`
— `type_check`'s `else` branch on `value_to_term_and_annotation`, the `None => Solved::Untranslatable`
arm for an instance's binding, `value_to_term_and_annotation`, `canonical_expr_to_term`,
`translate_pattern`, `translate_sub_pattern`, `wrap_with_patterns` and
`canonical_type_to_typer_type`.

**Found while:** working `LANG-81` (Float and String patterns, PR #324), whose review and whose
author's own notes both flagged it. Left alone there, because choosing between the two directions
below is not that ticket's change.

**Problem:** `Solved::Untranslatable` marks a declaration `value_to_term_and_annotation` answered
`None` for. Its doc comment names two causes, a `VarKernel` reference and a constructor of a union
the typer was given no declaration of, and says what it wants is the warning
[ERR-8](err-8.md) describes. LANG-81 made the last literal pattern kinds translatable, so what is
left of the `None` returns, in the code as it stands:

- `canonical_expr_to_term`'s `_ => return None` arm covers `ExpressionKind::VarKernel` only, and
  nothing constructs one: `grep -rn VarKernel crates` finds the variant, its own match arms and
  nothing that builds it.
- `translate_pattern`'s `ctor.tpe` lookup in `Translation.unions`, and its `position` lookup of
  the variant in that union, answer `None` when a union or a variant is missing from the
  declaration. For source checked against its own interfaces canonicalization has already
  resolved the constructor against that declaration, so neither fails; the one test that builds
  the variant, `a_declaration_the_typer_cannot_translate_comes_back_marked` in
  `crates/zelkova-compiler/tests/typer.rs`, does it by canonicalizing against a
  `maybe_interface()` and type checking against a map without one, a mismatch no build produces.
- The same arm's `Basics.Bool` case returns `None` for a constructor that is neither `True` nor
  `False`, and `Basics.Bool` declares no other.
- `canonical_type_to_typer_type` is total on every `canonical::Type` canonicalization hands it in
  practice; its `Option` is only the propagation of the failures above (a record field or an
  argument of an `Adt`).

So `Untranslatable` is a state the compiler cannot reach from source, carrying a span for a
warning that has nothing to say. `ERR-8` is blocked on `ERR-10` and its own "open question"
says it may close unbuilt; this ticket removes the one candidate caller that was never real.
The cost today is the reader's: one test is written around a state no program reaches, several
mutation notes in `tests/typer.rs` and `tests/pipeline.rs` describe it, `ir::build` shares an arm
between it and `UnboundName`, and `spec.rs`'s account of what an `expect=ok` block leaves unchecked counts an
`Untranslatable` entry that cannot occur.

**Approach:** the ticket does not pick between two.

(a) **Remove it.** Make `translate_pattern`, `canonical_expr_to_term` and
`canonical_type_to_typer_type` total, or make their failure an internal-error value that the
phase reports as an `Error` rather than a silent skip. `VarKernel` would need an arm of its own:
either an error, or removal of the variant from `canonical::ExpressionKind`, which is a canonical
AST change that moves `from_parser*` with it. A missing union or variant becomes an error
instead of a marker, which is the more honest answer to a state that means a bug in
canonicalization. Delete `Solved::Untranslatable`, the `type_check` and instance-binding arms
that build it, its half of the shared `ir::build` arm, and the test written around it. The
standing invariant that a pass which emitted an error must not report success is what an error
here buys; a silent skip is what the variant gives today.

(b) **Keep it as a defensive path, and say so.** Leave the `Option` returns and the variant, and
rewrite the doc comments so they stop promising a warning and describe a state no source reaches,
with the one test that builds it named as a pin on that state. Cheaper, and leaves a declaration
the typer silently skipped, which is the failure shape `BUG-1` was. It also leaves the
`ERR-8` pointer in the `Solved` doc comment, which then names a warning with no caller.

Either way, fold the question of `ERR-8`'s warning in: under (a) the ticket removes the pointer
to it from the variant's doc comment, and says in `err-8.md` that this candidate caller is gone;
under (b) it records that the variant is not a caller.

**Acceptance:** under (a), `grep -rn Untranslatable crates` finds nothing,
`cargo test --workspace` is green, `cargo run -- compile std/core` still prints
`parsed 10 modules` and lists all ten as checked, `cargo run -- test std/core` still reports
`151 tests: 151 passed, 0 failed, 0 errored`, and the new error for a union or variant missing
from `Translation` is asserted by variant in `crates/zelkova-compiler/tests/typer.rs`,
mutation-checked per `CLAUDE.md`. Under (b), the variant's doc comment no longer cites `ERR-8`
as what it wants, and says no source reaches it.
