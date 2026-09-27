# LANG-68 · An unmarked facade signature is not held to the `Task (Result Failure a)` result shape

**Sizing:** small-to-medium. The check is one more case in the same walk [`LANG-43`](README.md)
added — a shape match on the result piece left after arrows are stripped, guarded by
`marked_unsafe` — plus a new `Error` variant. The one open question in Approach below (whether
this ticket also has to reject `Task` appearing anywhere other than that result position) is
what could make it larger.

**Location:** `src/compiler/canonical/mod.rs` — `check_facade_admitted_type` and
`facade_signature_pieces`, both added by [`LANG-43`](README.md), and the `Error` enum in the
same file. `Value::TypedValue`'s `marked_unsafe` field already carries the flag this check
branches on.

**Problem:** [Foreign interoperability](../spec/interop.md#an-effectful-facade) settles that a
facade declares an effect by default, and an effectful facade's result type must be exactly
`Task (Result Failure a)` — any other result type is an error unless the signature is marked
[`unsafe`](../spec/interop.md#an-unsafe-facade)
([`DEC-12`](../decisions/dec-12.md) decision 1). Decision 7 makes `unsafe` the one surface the
compiler trusts rather than checks; everything else about the boundary, this shape included, is
meant to be enforced.

Nothing enforces it. `LANG-43` walks a facade's result, after stripping the signature's own
top-level arrows, for a bare type variable or a function type, and rejects both — but it never
asks what shape survives that walk. A facade with no `unsafe` keyword and a result type of, say,
a bare `Int`, or `Maybe Int`, canonicalizes cleanly today: `LANG-43`'s check only rules out the
two forms with no runtime predicate, and every other admitted type — including one that should
have required `unsafe` to write — passes. The two shapes an `unsafe` keyword is meant to tell
apart are read as the same thing, because the keyword itself is never read.

[`DEC-12`](../decisions/dec-12.md#what-nothing-checks) and
[`docs/spec/interop.md`](../spec/interop.md#an-effectful-facade) both already name this as the
compiler's own gap, without a ticket to point at: `DEC-12`'s "What nothing checks" section says
outright that "nothing checks the shape itself yet either," and [`GEN-16`](gen-16.md), the
wrapper this check is a prerequisite for, lists it in its own **Blocked on:** field as "a check,
not yet ticketed."

**Approach:**

1. Add a `canonical::Error` variant carrying the offending value's `Name` and its annotation's
   span (the same span `LANG-43`'s `FacadeTypeNotAdmitted` uses — a canonical `Type` carries no
   span of its own). Its `message()` states the required shape in the vocabulary of the source
   (`Task (Result Failure a)`), and a `notes()` line points at
   [An effectful facade](../spec/interop.md#an-effectful-facade).
2. In the same facade branch, after `check_facade_admitted_type` accepts a signature's result
   piece (the last one `facade_signature_pieces` returns), branch on `function.marked_unsafe`:
   - **Unmarked** (declares an effect): the result piece must be exactly
     `Type("Task", [Type("Result", [Type("Failure", []), a])])` for some type `a` the walk
     already admitted. Any other shape — including one that is itself perfectly admitted, like a
     bare `Int` — is the new error.
   - **`unsafe`**: unchanged. The result is walked as any other admitted type.
3. Push onto the branch's existing error accumulation, per `CLAUDE.md`'s *A pass that emitted an
   error must not report success* — the same discipline `LANG-43` followed.
4. **Open question this ticket does not decide:** [An effectful
   facade](../spec/interop.md#an-effectful-facade) also states "`Task` may appear only as the
   whole of a result type. Not as an argument; and not nested inside another type." Nothing
   enforces that sentence either, and it is a different question from the one this ticket
   answers — a `Task` written as a parameter, or nested inside a `Maybe`, reaches
   `check_facade_admitted_type` today as an ordinary `Type::Type` and is accepted as long as its
   own arguments are, since the walk has no notion of `Task` as a distinguished name outside the
   one result position this ticket adds a case for. Whether that is this ticket's scope too, or
   a sibling ticket's, is an open call — say which was picked, and why, before implementing.
5. `std/core` declares no `Task`, `Result` or `Failure` yet ([`GEN-1`](gen-1.md)), and every
   facade in the tree already carries `unsafe` ([`LANG-53`](README.md)), so this check accepts
   every signature `std/core` currently writes without requiring any rewrite.
   `cargo run -- compile std/core` should be unaffected.

**Tests:** `tests/compiler/canonical.rs`, alongside `LANG-43`'s fixtures. A synthetic interface
needs to declare `Task`, `Result` and `Failure` for a fixture to construct the admitted shape at
all — `tests/support/mod.rs`'s `basics_interface()`/`char_interface()` are the pattern to
follow, and building one is part of this ticket rather than something it can inherit, since
`std/core`'s own `Task` module does not compile ([`GEN-1`](gen-1.md)). Cases: an unmarked facade
over `Task (Result Failure a)` accepted; an unmarked facade over `Task (Result Failure Int)`
accepted; an unmarked facade over a bare `Int` rejected; an unmarked facade over `Task Int`
(wrong shape inside `Task`) rejected; an `unsafe` facade over a bare `Int` still accepted.
Neutralise the new branch and confirm each shape-specific case goes red before trusting it.

**Interactions:**

- **[`GEN-16`](gen-16.md)** names this check as one of its three blockers — `Task`/`Failure`
  existing at all is a separate blocker this ticket does not lift. Closing this ticket does not
  unblock `GEN-16` by itself.
- **[`LANG-43`](README.md)** is what this ticket extends — the walk, the span, and the
  error-accumulation discipline are all inherited from it rather than re-invented.

**Found:** while implementing `LANG-43` (`fmonniot/zelkova-lang#250`). `LANG-43`'s own
Interactions section described this shape check as something its Approach step 2 would "gain a
case" for, but the session working that ticket was explicitly scoped to the admitted-types walk
only — the two are a different check on a different question ("is this type admitted at all"
versus "does an unmarked facade's result have the one shape an effect is allowed to have") — and
left this gap for its own ticket rather than folding it in. Filed from that PR's own worktree
rather than from `main`, at the requester's direction, since the PR is what surfaced the gap and
the worktree it was found in could not itself commit to `main`.

**Acceptance:** an unmarked `module foreign` facade signature whose result type, after the
signature's own top-level arrows are stripped, is anything other than `Task (Result Failure a)`
is rejected with a diagnostic whose caret sits under that annotation. A facade marked `unsafe`
is unaffected. `docs/decisions/dec-12.md`'s "What nothing checks" section and
[`GEN-16`](gen-16.md)'s **Blocked on:** field are updated to point here instead of describing
the gap as unticketed. `cargo test --test spec` stays green.
