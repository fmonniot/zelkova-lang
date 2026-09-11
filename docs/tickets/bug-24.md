# BUG-24 · Two `.mjs` companions call helpers no file defines, so `modBy 0` and comparing functions are `ReferenceError`s

**Severity:** medium (wrong behaviour under normal use — an arithmetic call fails with a
JavaScript error naming an identifier that appears nowhere in the tree, and a second one returns
`nan` where the language defines `0`). It is not high only because no code generator emits calls
into these files yet.

**Location:**

- `std/core/src/Js/Basics.mjs` — `modBy`, line 29: `? __Debug_crash(11)`.
- `std/core/src/Js/Utils.mjs` — `_Utils_eqHelp`, line 24:
  `typeof x === 'function' && __Debug_crash(5);`.
- `std/core/src/Js/Basics.mjs` — `remainderBy`, line 23: `return b % a`.

`append` in the same file used to be a fourth site, calling `__List_Cons` twice. That branch is
gone: [`BUG-20`](bug-20.md)'s runtime half replaced it with the explicit `throw` this ticket's
**Fix** asks for below, so nothing is left to do there.

`__Debug_crash` is defined in no file in the repository and imported by
neither module. Both files carry a header saying they were copied from Elm's implementation as
is; in Elm those names are placeholders a preprocessor substitutes, and Zelkova has no such
step — `docs/spec/interop.md` says a companion is an ordinary ES module and the compiler
never rewrites it.

**Problem:** three defects, one root.

`modBy 0 n` evaluates `__Debug_crash(11)` and raises `ReferenceError: __Debug_crash is not
defined`. [`docs/spec/evaluation-semantics.md`](../spec/evaluation-semantics.md) defines
`modBy 0 n` to be `0`, because a well-typed program has only two outcomes and a crash is not one
of them — so the correct behaviour and the current one differ twice over: it fails, and it fails
in a way whose message names nothing a reader can find.

`remainderBy 0 n` is `n % 0`, which in JavaScript is `nan`. The same chapter defines it to be
`0`. `nan` is not even an `Int`, so this one escapes the type the facade declares.

`Js/Utils.mjs`'s equality helper calls `__Debug_crash(5)` when handed a function. That path
becomes unreachable under the language's rules — `Eq` has no instance for a function type, so
comparing two functions does not type-check — but it is reachable today, because
`Js.Utils.equal` is declared `a -> a -> Bool` and accepts anything.

Note that `idiv` is already right: `(a / 0) | 0` is `0`, which is the value the chapter defines.

Found while writing [`docs/spec/evaluation-semantics.md`](../spec/evaluation-semantics.md)
(`SPEC-9`).

**Fix:** make each of the three sites do what the language says, with no undefined identifier
left in either file.

- `modBy`: return `0` when the modulus is `0`, and keep the existing sign-correcting arithmetic
  for every other modulus.
- `remainderBy`: return `0` when the divisor is `0`.
- `_Utils_eqHelp`'s function case: return `false` rather than calling a crash helper. The path is
  unreachable from well-typed source, and a comparison helper is not the place to invent a
  failure mode the language does not have.

`append`'s `__List_Cons` needed the same treatment and has already had it. The compiler
implements no list type at all, so there was no cons representation to write against
([`docs/spec/lexical-structure.md`](../spec/lexical-structure.md)'s note on brackets), and the
branch is now unimplemented *explicitly* — a `throw` naming what is missing rather than a call
to a name that does not exist, so the failure says which feature is absent instead of looking
like a typo.

[`BUG-20`](bug-20.md) covers a different defect in the same two functions — that their declared
types accept values their JavaScript cannot handle — and its first bullet asks them to fail
loudly rather than return nonsense. That bullet is done, which is where `append`'s `throw` came
from; the rest of that ticket waits on the type half and is not this ticket's work. This one is
about identifiers that resolve to nothing, and about two arithmetic results the language now
defines.

**Acceptance:** neither `.mjs` file mentions `__Debug_crash`, and nothing has reintroduced
`__List_Cons`: `grep -rn '__Debug_crash\|__List_Cons' std/` finds nothing. `modBy 0 n`, `remainderBy 0 n` and
`n // 0` each evaluate to `0`. The `**Known gap:**` paragraph in
[`docs/spec/evaluation-semantics.md`](../spec/evaluation-semantics.md)'s *Numbers* is deleted.
`cargo run` still prints `parsed 8 modules` and lists all eight as checked.

**Note — this ticket has no red test behind it.** [`BUG-20`](bug-20.md) added the repository's
first JavaScript harness — `std/core/tests/Js/UtilsChecks.mjs`, run with
`node --test 'std/core/tests/**/*.mjs'` — but it covers `Js/Utils.mjs` only, and nothing yet
covers the `Js/Basics.mjs` functions this ticket changes. Write
`std/core/tests/Js/BasicsChecks.mjs` alongside the fix, which is where
[*Testing a companion*](../spec/interop.md#testing-a-companion) puts it; until it exists the
check is the `grep` above, run by hand.
