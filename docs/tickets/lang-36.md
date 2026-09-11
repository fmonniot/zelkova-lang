# LANG-36 · `std/core`'s `Basics` documents three semantics the language does not have

**Sizing:** small — three doc comments in one file, and no code changes.

**Location:** `std/core/src/Basics.zel` — the doc comments on `(&&)`/`and`, on `(||)`/`or`, on
`type Int`, and on `(==)`/`eq`.

**Decided ([`docs/spec/evaluation-semantics.md`](../spec/evaluation-semantics.md)):** three
rules, each contradicted by one of those comments.

- *Nothing short-circuits.* `&&` and `||` are names bound by an `infix` declaration to ordinary
  functions, and evaluation is strict, so both operands are evaluated at every call. Conditional
  evaluation is `if` and `case`, and nothing else.
- *`Int` is a 32-bit signed two's-complement integer*, wrapping on overflow, the same on every
  compilation target.
- *Functions are not comparable.* `Eq` is an ordinary class with no instance for a function type,
  so `f == g` is a type error rather than something that compiles and fails.

**Problem:** the file says otherwise, in prose that is what a reader of `std/core` trusts.

`and`'s comment: *"When used in the infix position, like `(left && right)`, the operator
short-circuits. This means if `left` is `False` we do not bother evaluating `right`"* — and
`or`'s says the same. The JavaScript underneath already agrees with the language rather than
with the comment: `Js/Basics.mjs` exports `and(a, b) { return a && b }`, a plain two-argument
function whose caller has evaluated both arguments before it is entered. So the comment
describes behaviour nothing anywhere implements.

`Int`'s comment: *"`Int` math is well-defined in the range `-2^31` to `2^31 - 1`. Outside of
that range, the behavior is determined by the compilation target."* The language now defines it
everywhere and identically, which is the point — a program that computes one answer on
JavaScript and another on WebAssembly is what that sentence licenses.

`eq`'s comment: *"Do not use `(==)` with functions … It does not work. It will crash if
possible"*, and the paragraph after it describes a future compiler detecting the case. There is
nothing to detect: no `Eq` instance exists for a function type, so a program that compares two
functions does not type-check, and the crash it warns about is unreachable.

All three are inherited from Elm's `Basics`, along with the surrounding text, and none was ever
a claim about Zelkova.

Found while writing [`docs/spec/evaluation-semantics.md`](../spec/evaluation-semantics.md)
(`SPEC-9`).

**Approach:** rewrite the three, and only them. Say what the language does: `&&` and `||`
evaluate both operands and `if` is how a program skips one; `Int` wraps at 32 bits everywhere;
comparing functions is rejected by the type checker because no instance covers them. Point each
at the chapter rather than restating the reasoning —
`docs/spec/evaluation-semantics.md` is the normative record and two records of one decision means
the unmaintained one is what someone eventually reads.

Two neighbours are already correct and should not be swept in. `eq`'s *first* note — that
equality is structural on tuples and user-defined union types — is what the language says
structural instances compute, so it stays. And the `(+)`-family comments about `a` not being the
restriction it looks like are [`BUG-20`](bug-20.md)'s and
[`docs/spec/type-classes.md`](../spec/type-classes.md)'s subject, not this ticket's.

**Note — this ticket has no red test behind it.** Every claim it corrects is in a doc comment,
which nothing in the test suite reads, and the chapter's own `**Known gap:**` for the
short-circuit claim is attached to a block that pins syntax only. That paragraph has to be
deleted by hand as part of this ticket; nothing will fail to remind you.

**Acceptance:** the four doc comments name the language's rules, with no sentence left claiming
short-circuiting, a target-determined `Int`, or a runtime crash on comparing functions. The
`**Known gap:**` paragraph in
[`docs/spec/evaluation-semantics.md`](../spec/evaluation-semantics.md)'s *Nothing
short-circuits* is deleted. `cargo run` still prints `parsed 8 modules` and lists all eight as
checked.
