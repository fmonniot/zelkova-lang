# BUG-44 · `Float` arithmetic aborts at the `Int` facade's boundary check

**Severity:** medium (every `Float` `+`, `-`, `*` and `^` aborts the program — loud rather than
a wrong value, but it makes `Float` arithmetic unusable. Accepted by the language owner as the
price of landing [`GEN-2`](README.md) before type classes.)

**Location:** `std/core/src/Basics.zel` — `add`, `sub`, `mul` and `pow`, each annotated
`a -> a -> a` and bound to `Js.Basics.addInt`, `subInt`, `mulInt` and `powInt`; `append`, bound
to `Js.Utils.appendInt` the same way; `fromPolar`, `toPolar` and `degrees`, declared over `Float`
with no `a` in sight but calling `mul`/`add` internally, so they abort too.
`crates/zelkova-js/src/lib.rs` — `Emitter::facade_declaration`, which emits the check that
aborts. `std/core/tests/FloatTests.ignored` — the tests it breaks, disabled (via that
extension) so CI stays green until this closes.

**Depends on:** [`LANG-42`](lang-42.md), which gives `Basics` a `Number` class whose `Float`
instance can forward to `addFloat` while the `Int` instance forwards to `addInt`. That in turn
needs [`LANG-40`](lang-40.md) and [`LANG-41`](lang-41.md); see
[Active work: type classes](README.md#active-work-type-classes).

**Problem:** a facade signature may not name a type variable ([`LANG-43`](README.md)), so
`Js.Basics` declares each arithmetic operation twice — `addInt : Int -> Int -> Int` and
`addFloat : Float -> Float -> Float` — over one JavaScript function. `Basics.add` keeps its
`a -> a -> a` annotation, because the language has no way to write the real restriction yet,
and has to name one of the two. It names `addInt`, whatever `a` is instantiated at.

That was harmless while nothing checked the boundary. Since [`GEN-2`](README.md), every value a
companion hands back is run through the predicate of the type its signature declares
([Which types may cross the boundary](../spec/interop.md#which-types-may-cross-the-boundary)),
and an `unsafe` facade's failing check aborts the program. `0.1 + 0.2` calls `addInt` with two
JavaScript numbers, the companion returns a number, and `Int`'s predicate — a `bigint` the
64-bit range holds — rejects it:

```
$ cargo run -- test std/core
…
ERROR FloatTests.additionIsBinaryFloatingPoint: module failed to load: `Js.Basics.addInt`'s companion returned a value its declared type, `Int`, does not admit
ERROR FloatTests.divisionKeepsTheFraction: module failed to load: `Js.Basics.addInt`'s companion returned a value its declared type, `Int`, does not admit
…
25 tests: 23 passed, 0 failed, 2 errored
```

Both tests in the module error, not only the one that adds: a test is a parameterless binding,
evaluated when `FloatTests` loads, so the first abort takes the module with it. `FloatTests.zel`
is renamed to `FloatTests.ignored` — excluded from `cargo run -- test std/core`'s walk the same
way an unported `std/core/src/` module is — so the command exits 0 and CI stays green; the
transcript above is what running it still shows if you rename the file back by hand.

`sub`, `mul` and `pow` fail the same way on a `Float`, and so does anything built from them.
`fromPolar` and `toPolar` are declared `(Float,Float) -> (Float,Float)`, with no `a` anywhere
in their signature, but both call `mul` (and `toPolar` also `add`) internally, so they abort on
**every** call — a reader sees `mulInt`'s message, since `mul` is the argument evaluated first
in both. `degrees` calls `mul` too and aborts the same way. It also carries an independent
defect: its `180` is compiled as a `bigint`, and `fdiv` cannot divide the `Float` `mul` would
have produced by one — the same "a bare integer literal is typed `Number`, not `Int`" gap
[`LANG-41`](lang-41.md) is filed to close, which lists `std/core/src/` literals needing a point
as its expected diff. `degrees`'s `mul` call aborts first today, so that second defect is not
what a reader of the current error sees; fixing this ticket without also giving `180` a decimal
point would just trade one abort for the other. `append` names `Js.Utils.appendInt`, whose
companion concatenates two JavaScript strings and throws on anything else, so a `String` append
would abort at the same check; a string literal can be written since [`LANG-77`](README.md).
Comparisons are unaffected: `ltInt` and `equalInt` return a `Bool` whatever they are handed,
and only a result is checked.

**Fix:** once [`LANG-42`](lang-42.md) makes arithmetic a `Number` class member, the `Float`
instance forwards to `addFloat`, `subFloat`, `mulFloat` and `powFloat`, and specialisation
([`DEC-2` decision 7](../decisions/dec-2.md#7--dictionaries-are-erased-by-specialisation-not-passed))
calls the facade of the type each use is instantiated at. This is part of that ticket's work or
a small follow-up immediately after it; it is filed separately so that `LANG-42` landing without
it is visible. Weakening `Int`'s predicate to accept a number, or exempting `zelkova-core`'s
facades from the check, were both considered and rejected when `GEN-2` landed: the first
contradicts `Int`'s published representation, the second the chapter's rule that no facade is
privileged.

**Acceptance:** `std/core/tests/FloatTests.ignored` is renamed back to `FloatTests.zel`, and
`cargo run -- test std/core` reports every test passing, `FloatTests`' two included, and exits
0. The line in `CLAUDE.md`'s *Commands* section giving `23 tests: 23 passed` as that command's
expected result is updated to `25 tests: 25 passed`, and the comments on `add`, `sub`, `mul`,
`pow`, `fromPolar`, `toPolar` and `degrees` in `std/core/src/Basics.zel` stop naming this
ticket. `degrees` additionally needs `180` written `180.0` ([`LANG-41`](lang-41.md)) — check it
by hand, since nothing tests `degrees` today.

**Found:** while working [`GEN-2`](README.md), which escalated it rather than choosing; the
language owner chose to land the check with this regression rather than wait for type classes.
