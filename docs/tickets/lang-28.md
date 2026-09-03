# LANG-28 · An `infix` declaration's function is never checked to take two arguments

**Sizing:** small if the check reads the annotation, medium if it waits on inference — see
*Approach*, which does not pick between the two.

**Location:** `src/compiler/canonical/mod.rs` — `do_infixes`, whose only condition on the
function is `functions.iter().find(|f| f.name == infix.function_name).is_some()`. The
`parser::Function` it finds already carries `tpe: Option<Type>`, the annotation as written.

**Decided by:** [`docs/spec/declarations.md`](../spec/declarations.md)'s *The function is
declared in the same module* — "The function must take two arguments."

**Problem:** an operator stands for a function of two arguments, so the name after an
`infix`'s `=` has to have a type of the shape `a -> b -> c`. Nothing checks it. `do_infixes`
asks whether a declaration by that name exists and stops there, so a nullary value is an
operator:

```
type Flag
  = On

zero : Flag
zero = On

infix left 6 (+) = zero
```

That module canonicalizes and type checks clean.

The use site is not a fallback. `On + On` in the same module also passes `check_module`,
because canonicalization leaves the operator as `VarTopLevel(Example.+)` rather than
rewriting it to `zero`, the typer's environment holds no `+`, and a declaration whose
inference hits `UnboundVariable` is skipped rather than reported — the gap
`src/compiler/typer/mod.rs`'s `type_check` doc comment describes, and the reason five
declarations in `std/core/src` go unchecked today. So an operator of the wrong shape is
caught nowhere, and the first thing that would notice is code generation.

**Approach:** two placements, with different reach, and this ticket does not choose.

*From the annotation, in `do_infixes`.* Count the arrows on the `parser::Type` the function
carries and raise a new `canonical::Error` variant when there are fewer than two, with the
caret on the `infix` declaration and a secondary label on the annotation. Cheap, and it
reports at the declaration that made the promise. Its limit is `tpe: None`: only an
[exposed declaration must be annotated](../spec/types.md#an-exposed-declaration-must-be-annotated),
so an `infix` may name a private unannotated function, which this check has to let through —
unless the language additionally requires an annotation on any function an `infix` names,
which is a spec question the chapter has not answered and should be settled before writing
the code.

*From the inferred type, in the typer.* Complete, no annotation needed, but the typer has no
notion of an infix declaration and does not resolve operator names at all today, so this
placement needs that plumbing first and inherits [TEST-2](test-2.md) for pinning the result
in the spec.

Whichever lands, an operator's arity check is close to [LANG-23](lang-23.md) — an operator
usable as a value needs its type on the same terms — and worth looking at together.

**Acceptance:** the *The function must take two arguments* paragraph in
[`docs/spec/declarations.md`](../spec/declarations.md) gains a block showing an operator whose
function takes fewer than two arguments, and that block is **red** before the fix — tagged
`expect=canonical-error:` with the new variant if the check lands in canonicalization, or
`expect=type-error:` (which needs [TEST-2](test-2.md) first) if it lands in the typer. The
**Not implemented:** paragraph beneath it is deleted. A `tests/compiler/canonical.rs` case —
or `tests/typer.rs`, following the placement — asserting the error on the nullary example
above, seen to fail before the fix.
