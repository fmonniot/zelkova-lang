# LANG-42 · `std/core` declares `Eq`, `Comparable`, `Number` and `Appendable`

**Sizing:** large. No one signature is hard. It is the first time the package's over-general
types meet a checker that can reject them, it is the first real program the class mechanism
compiles and runs, and it reaches every package that compares two values — `std/test` and most
of `tests/fixtures/` among them.

**Location:** `std/core/src/Basics.zel` — the `exposing` list, the `infix` declarations, and
the thirteen declarations annotated over a bare `a` and bound to an `Int` facade (`add`, `sub`,
`mul`, `pow`, `eq`, `neq`, `lt`, `gt`, `le`, `ge`, `append`, `negate`, `abs`), plus `min`,
`max`, `clamp` and `compare`; `std/core/src/Js/Utils.zel` and `Utils.mjs`;
`std/core/src/Js/Basics.zel`, whose arithmetic facades already come in an `Int` and a `Float`
form; `std/core/src/Maybe.zel`, `Result.zel` and `Task.zel`, for the instances that live with
their types; `std/core/tests/`; `std/test/src/Test.zel` — `equal`; `tests/fixtures/`;
`crates/zelkova-compiler/src/scalars.rs`, for `Position`.

**Depends on:** [LANG-40](lang-40.md), for a solver;
[LANG-83](lang-83.md), for `derived`; and [GEN-24](gen-24.md), without which none of it can be
emitted and `cargo run -- test std/core` cannot pass.

**Closes:** [BUG-20](bug-20.md) — whose first half made the runtime say so, and whose second,
making the type say so, is this; [BUG-44](bug-44.md); and [BUG-46](bug-46.md).

**Decided (by the language owner):** four classes, `Eq` a superclass of `Comparable`
([`DEC-2` decision 9](../decisions/dec-2.md#9--stdcore-grows-four-classes)); a facade signature
may not carry a constraint, so the constraint lives one level above a monomorphic facade
([decision 6](../decisions/dec-2.md#6--a-module-javascript-facade-signature-may-not-carry-a-constraint));
and their members and instances
([`DEC-24` decision 7](../decisions/dec-24.md#7--stdcores-classes-one-member-where-the-class-is-derivable)),
which [*What the standard library declares*](../spec/type-classes.md#what-the-standard-library-declares)
states as two tables. Those tables are the specification of this ticket. The earlier version of
it left four questions open; all four are answered there.

**Problem:** `Basics` publishes six comparison functions, an equality pair, four arithmetic
operations and an append, all typed over a bare `a`, each bound to a facade that handles `Int`:

```zel
add : a -> a -> a
add =
  Js.Basics.addInt
```

The type accepts anything. The body handles one type. A `Float` sum aborts at `addInt`'s
boundary check (`BUG-44`), `min Red Blue` on a user's union type checks and throws at run time
(`BUG-20`), and comparing two functions type checks. None of it can be written honestly without
a class, and since [LANG-12](lang-12.md) will reject every one of these declarations, this
ticket is also what unblocks it.

**Approach:**

1. **The classes, in `Basics`**, as the chapter's table has them:

   ```zel
   class Eq a where
     eq : a -> a -> Bool

   class Eq a => Comparable a where
     compare : a -> a -> Order

   class Number a where
     add : a -> a -> a
     sub : a -> a -> a
     mul : a -> a -> a
     pow : a -> a -> a
     negate : a -> a
     abs : a -> a

   class Appendable a where
     append : a -> a -> a
   ```

   `Eq` and `Comparable` each carry the derivation the chapter shows for them, under *A class
   says how it is derived* and *What a derived instance computes*. `neq`, `lt`, `le`, `gt`,
   `ge`, `min`, `max` and `clamp` become ordinary functions constrained by the class, written
   over `eq` and `compare`. The `infix` declarations keep their operators and now name members
   and constrained functions.

2. **`Position`, in `Basics`**: a type with one constructor holding an `Int`, exposed without
   it, with `positionIndex : Position -> Int` and `Eq` and `Comparable` instances. It is the
   type [LANG-83](lang-83.md) made the compiler know by qualified name; this is its declaration.
   Both instances can be `derived` — a `Position` has one constructor, so neither walk ever
   reaches `differed`.

3. **The instances**, each in the module the chapter's rule allows. `Char` and `String` are
   scalar types a module of `zelkova-core` names without importing, so their instances sit
   beside the classes.

   | Where | Instances |
   |---|---|
   | `Basics` | `Eq`: `Int`, `Float`, `Char`, `String`, `Bool`, `Order`, `Position`, `(a, b)`, `(a, b, c)`, `()` |
   | `Basics` | `Comparable`: `Int`, `Float`, `Char`, `String`, `Position`, `(a, b)`, `(a, b, c)` |
   | `Basics` | `Number`: `Int`, `Float` |
   | `Basics` | `Appendable`: `String` |
   | `Maybe` | `Eq (Maybe a)` |
   | `Result` | `Eq (Result error value)` |
   | `Task` | `Eq Failure` |

   The scalar ones are written, each binding forwarding to a monomorphic facade. Every other
   one is `derived`. `Appendable`'s `List` instance waits on [LANG-46](lang-46.md). No module
   of `zelkova-core` receives a default import, so `Maybe`, `Result` and `Task` each import the
   class they instantiate.

   Write a scalar instance's bindings with their parameters — `add a b = Js.Basics.addInt a b`
   — so that a use is a direct call. A binding written without them has arity 0 and every call
   goes through `$curry`, which is [PERF-2](perf-2.md); that ticket stays open for the
   forwarding declarations this one does not touch.

4. **The facades, in `Js.Utils`.** Each scalar instance needs a facade at its own type, so the
   pairs that exist for `Int` and `Float` gain `Char` and `String` siblings where an instance
   calls them, and `appendInt` and `appendFloat`, which no type ever used, become
   `appendString`. What a `Comparable` instance forwards to is this ticket's to choose: the
   `compare` facades and an `Int`-to-`Order` step, or `lt` and `equal` and two tests. Either
   way `BUG-46` closes — its facades return a JavaScript number where they declare an `Int`,
   and they are either fixed to return a `bigint` or deleted with their checks. What `compare`
   answers when a `Float` is `nan` is [stated](../spec/evaluation-semantics.md#numbers); follow
   it.

   A facade no instance calls is removed, with its checks in `std/core/tests/Js/UtilsChecks.zel`
   and `.mjs` and its tests in `UtilsTests.zel`.

5. **The companion shrinks.** `Utils.mjs` compares two unions and two tuples structurally, in
   JavaScript, and guards against values it cannot read. All of that is now written in Zelkova,
   by derived instances, and the companion is left with the primitives a scalar instance calls.
   Keep each of those rejecting a value that is not of its type: the facades are the package's
   boundary, and a boundary that trusts its caller is one bad emission away from `BUG-20`.
   [TIDY-11](tidy-11.md) is about two comments in this file and a paragraph of `bug-20.md`; if
   what it cites is gone, close it here.

6. **Everything that compares.** `std/test`'s `equal : a -> a -> Test` becomes
   `Eq a => a -> a -> Test`. A test module or a fixture that compares values of its own union
   type — `std/core/tests/UnionTests.zel` compares two `Shape`s and two `Light`s — declares
   `instance Eq … where derived` beside the type. Build `std/core`, `std/test` and every
   fixture, and fix what the checker now rejects; each rejection is this ticket working.

7. **`FloatTests`.** Rename `std/core/tests/FloatTests.ignored` back to `FloatTests.zel`. Add
   test modules to `std/core/tests/` for what is new: each class at each of its instances, a
   derived instance in another module than its class, `min` and `max` at two types, `negate`
   and `abs` at `Int` and `Float`, a tuple compared element-wise, a `nan`.

**Acceptance:**

- `min Red Blue`, on a user union type with no `Comparable` instance, is a **type error** — the
  program `BUG-20` uses as its worked example, and the single check this ticket exists for.
  `eq` applied to two functions is a type error. `add 1 1.5` is a type error. Each is a test in
  `crates/zelkova/tests/pipeline.rs` against the real `std/core`, the way `check_std_core`
  supplies it, asserting the error's kind and where its caret is, and each is seen red.
- `cargo run -- compile std/core` prints `parsed 10 modules`, lists all ten as checked and
  exits 0.
- `cargo run -- test std/core` passes every test, `FloatTests`' two and the new modules'
  included, with none errored. The count changes; `CLAUDE.md`'s *Commands* section states it,
  and stops describing `FloatTests.ignored`.
- `cargo test --workspace` and `node --test 'tests/js/**/*.mjs'` pass, every fixture included.
- `grep -rn "BUG-44\|BUG-20\|BUG-46" std/ crates/` prints nothing.

**Closing it, and what closes with it.** `docs/tickets/bug-20.md`, `bug-44.md` and `bug-46.md`
are deleted and tombstoned with this one. `cargo test --test spec` goes red on a citation of a
deleted ticket, so:

- [`docs/spec/type-classes.md`](../spec/type-classes.md): the `**Not implemented:**` paragraph
  under *What the standard library declares* goes, keeping its last sentence about `List`; the
  `**Known gap:**` paragraph under *A constrained function may not be a foreign facade*, which
  cites `BUG-20`, goes; the chapter's opening `**Not implemented:**` paragraph has nothing left
  to say. Blocks showing a constrained standard-library signature that are still
  `expect=unimplemented` are retagged for what they now do.
- [`docs/spec/evaluation-semantics.md`](../spec/evaluation-semantics.md), *What structural
  equality computes*: the `**Not implemented:**` paragraph goes, and the `alike` block above it
  is `expect=ok`.
- [`docs/decisions/`](../decisions/README.md): `DEC-2`, `DEC-10` and `DEC-24` link to this
  ticket's file. Repoint each at [the index](README.md), as a closed ticket's citations are.
- `CLAUDE.md`, *Language notes*: the paragraph saying `number`, `comparable` and `appendable`
  are ordinary type variables and `std/core` spells all three `a` describes the library this
  ticket replaces. `std/core/src/Basics.zel`'s own doc comments say the same thing in several
  places — "the signature does not enforce it" — and each is now false.
