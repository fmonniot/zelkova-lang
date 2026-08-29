# LANG-4 · Prefix `-` is desugared to `0 - e`, so negating a `Float` mixes it with an `Int` literal

**Sizing:** small in the grammar; the interesting part is deciding what it desugars *to*.

**Location:** `src/compiler/parser/grammar.lalrpop`, the `Expr` production
`<l:@L> "-" <m:@R> <e: Expr>`, which builds `((-) 0) e` with `Literal::Int(0)`.

**Found:** while writing `docs/spec/lexical-structure.md` under `SPEC-2`.

**Decided (SPEC-2, by the language owner):** a `-` written directly before an expression,
where no left operand is available, is **negation** — `-e` is the negation of `e`, typed
`number -> number`. It is not subtraction against an implied zero.

**Problem:** the desugaring hard-codes an `Int` literal as the left operand of a subtraction.
For `-n` where `n` is an `Int`, or where the surrounding constraints leave the literal's type
open, this is invisible. For a `Float` it should not be: `-3.14` becomes `0 - 3.14`, an `Int`
zero subtracted from a `Float`.

Whether that is *observably* wrong today depends on how the typer treats an integer literal —
if `Literal::Int` is given a `number` type variable rather than `Int`, the unification
succeeds and the bug is latent rather than live. Establish which it is before writing the fix,
because it decides whether this ticket has a failing test to open with or is a correctness
argument about a shape that happens to work. Either way the shape is wrong: the spec says
negation, and a reader of `constraint.rs` should not have to know that `-x` is really a
subtraction.

There is a second reason to care, independent of types. `ERR-4` made the typer report the
*origin* of a failed constraint, and this desugaring invents an `Int` literal that appears
nowhere in the user's source while giving it the span of the `-`. A type error mentioning
`Int` and pointing at a `-` the author wrote as negation is exactly the kind of unactionable
label `ERR-4` set out to eliminate.

**Approach:** desugar to an application of `Basics.negate` rather than to a subtraction —
`negate` already exists, is already exposed, and is already typed `number -> number`. That
makes the desugaring name something real, so the invented node's span points at a function
the reader can look up.

The catch is that it makes the grammar depend on a name from `Basics`, which the `-` version
also does (it invents `Variable("-")`) but less visibly, since an operator reads as built-in.
Check how `Variable(Name::new("-"))` is resolved during canonicalization before assuming
`negate` can be resolved the same way; if `Basics` is not implicitly in scope, this ticket is
blocked on the implicit-import question the modules chapter will settle, and should say so
rather than inventing an answer.

**Acceptance:** `-3.14` type-checks as a `Float` with no `Int` anywhere in the constraints,
and a deliberate type error involving a negated expression produces a label that does not
mention a literal zero. `negate n = -n` in `std/core/src/Basics.zel` must still compile —
note it would then be defined in terms of itself, which is fine as a runtime matter but worth
a glance.

The syntax block in `docs/spec/lexical-structure.md`'s *Prefix negation* section stays green
across this fix by design — it pins the syntax, which does not change — so the
`**Known gap:**` paragraph beneath it will **not** be forced red. Delete it by hand as part of
this ticket; it is the one gap in that chapter with no test holding it to account.
