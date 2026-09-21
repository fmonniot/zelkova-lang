# GEN-3 · The typer hands back the types it solved

**Sizing:** medium. The solving already happens and the typed term already exists; this is
returning them instead of dropping them, plus making two silent skips say something.

**Part of:** [`GEN-1`](gen-1.md), and the first ticket of that program. Nothing downstream of
the typer can be written until it answers with something.

**Location:** `src/compiler/typer/mod.rs` — `type_check`, which returns
`Result<(), Vec<Error>>`; its third pass, which calls `infer_annotated` and matches `Ok(_)`;
`infer_annotated` itself, which returns `Result<Type, ErrorKind>` and whose caller discards the
`Type`; `value_to_term_and_annotation`, which returns an `Option` the loop turns into a bare
`continue`; the `Err(ErrorKind::UnboundVariable { .. }) => continue` arm beside it; and
`translate_expression`, whose `Int` arm is `TermKind::Int(*i as u32)`.
`src/compiler/typer/annotate.rs` — `annotate`, which builds the typed term the constraints are
collected from. `src/compiler/mod.rs` — `check_module`, which calls `type_check` for its errors
alone and hands back the `canonical::Module`.

**Decided ([`GEN-1`](gen-1.md) decision 1):** the backend consumes a typed IR, and the typer
produces it, because the typer is the only phase that knows a node's type. A type added after
the fact is one a WebAssembly backend cannot use: it is static, and polymorphism reaches it
through the same monomorphisation
[`DEC-2` decision 7](../decisions/dec-2.md#7--dictionaries-are-erased-by-specialisation-not-passed)
already requires.

**Problem:** the typer proves things and reports nothing but failure. `annotate` builds a term
with a type on every node, `unify` solves a substitution over it, and the whole result is
reduced to whether the pass errored. Every later phase is left to re-derive from the canonical
AST what this one already knew.

Two skips are silent, and both become miscompiles the moment something emits code from this
phase's output:

- `value_to_term_and_annotation` returns `None` for a construct it cannot translate and the
  loop `continue`s. The declaration is not checked and nothing says so.
- `Err(ErrorKind::UnboundVariable { .. })` also `continue`s, on the grounds that an unbound
  variable is a hole in the typer's environment rather than a mistake in the source. That is
  true today and it is still a declaration that was not checked. [`BUG-36`](bug-36.md) is one
  such hole.

A backend handed that output cannot tell a declaration the typer verified from one it walked
past.

There is also a plain truncation: `translate_expression`'s `Int` arm narrows the canonical
`i64` to a `u32`. It is harmless while nothing reads the value back — the type is `Int` either
way — and it is a wrong answer the moment the term is what code is generated from.
[`Int` is 64 bits](../spec/evaluation-semantics.md#numbers) ([`DEC-16`](../decisions/dec-16.md)).

**Approach:** `type_check` returns the module's solved types rather than `()`. Do not invent a
structure for them: `annotate` already produces a term carrying a type on every node, and
`unify` already produces the substitution — apply the final substitution to the annotated term
(the zonk) and return that, one per declaration. `check_module` holds what comes back.

The two skips stop being silent. Which they become is this ticket's call, and either is
defensible: a declaration the typer could not translate or could not resolve may be reported as
an error, or returned marked as un-typed so a later phase refuses to emit for it. What it may
not be is absent with nothing said, because that is indistinguishable from a declaration that
passed.

Widen `TermKind::Int` to `i64` here, with the truncation, rather than leaving it for the ticket
that first reads the value.

Keep `type_check`'s existing accumulation shape: it records an error per declaration and moves
on, so one broken declaration cannot hide the next (`CLAUDE.md`, *A pass that emitted an error
must not report success*). Returning types does not change that — a module with errors returns
its errors, as it does now.

**Not in this ticket:** reshaping the term into the backend IR, which is [`GEN-4`](gen-4.md) —
this ticket changes what `type_check` returns and not what a term *is*, beyond the `Int` width.
Closing the typer's coverage holes themselves is [`BUG-36`](bug-36.md) and whatever follows it;
this ticket only stops them being invisible.

**Acceptance:** a test in `tests/typer.rs` asserts the solved type a declaration comes back
with, and the solved type of at least one node *inside* its body — a whole-declaration type
alone would pass even if the substitution were never applied to the interior. A second test
takes a declaration the typer cannot type and asserts it is visibly so in the return value
rather than missing from it. A third asserts an `Int` literal above `u32::MAX` survives
translation with its value. `cargo run` still prints `parsed 8 modules`, lists all eight as
checked, and exits 0.

Neutralise-check each of those by reverting the one change it pins: hand back the un-zonked
term for the first, restore the bare `continue` for the second, restore `as u32` for the third.
