# GEN-25 · A record, a field access, an update, an accessor and a record pattern are not emitted

**Sizing:** medium. Five forms, each a few lines of JavaScript, and one new predicate. What could
make it bigger is the IR: if the typer's record expression has lost the order its fields were
written in, this ticket has to put it back before it can emit anything.

**Location:** `crates/zelkova-compiler/src/ir/mod.rs` — `TypedTermKind`, `TermPatternKind`, and
the module doc comment's *What this shape owes WebAssembly*;
`crates/zelkova-compiler/src/ir/decision.rs` — `Step`; `crates/zelkova-js/src/lib.rs` —
`Emitter`, `Construct`, `Predicates::test`, `Unpredicated`, and the *Representations*, *The
boundary check*, *A `case`* and *What is refused* sections of the module doc comment;
`crates/zelkova-compiler/src/canonical/mod.rs` — `check_facade_admitted_type`, which has an arm
per form of `canonical::Type` and none for a record; `tests/fixtures/` and `tests/js/`.

**Depends on:** [`LANG-51`](lang-51.md), hard: the typer is what produces the IR, and until it
has a record type no record reaches `ir::Module`. [`LANG-84`](lang-84.md) for the pattern half.

**Decided:** a construct that lands after the first emitter gets a sibling `GEN-` ticket and its
`LANG-` ticket grows no code-generation half
([`DEC-18` decision 7](../decisions/dec-18.md#7--the-program-covers-the-language-the-front-end-accepts-today)).
A record crossing a facade is held to a predicate — *the value is an object with exactly the
record's fields, each field satisfying its own predicate* — and is a WIT `record` of the same
fields ([Which types may cross the boundary](../spec/interop.md#which-types-may-cross-the-boundary)).
A field of type `()` is present, holding `undefined`
([`DEC-21`](../decisions/dec-21.md)).

**Problem:** nothing owns the emission of a record. After `LANG-51` a module holding one type
checks and cannot be built: `zelkova_js::emit` matches on `TypedTermKind` and has no arm for a
form that does not exist yet, so whatever `LANG-51` adds has to be refused there until this
lands — `Construct` is where the existing refusals are named. `check_facade_admitted_type` has
no record arm either, so a facade signature cannot name a record type and `Predicates::test`
builds nothing for one.

**What the result has to be:**

1. **A record is a plain JavaScript object keyed by its labels**, with no `$`. Three places
   already assume it and none of them decides it: the predicate row above, `DEC-21`'s
   `{ f: undefined }`, and the comment in `std/core/src/Js/Utils.mjs` that a record is "a plain
   object of the record's own field names". [Interop](../spec/interop.md) leaves the
   representation to code generation
   ([`DEC-6` decision 3](../decisions/dec-6.md#3--unions-cross-and-their-encoding-is-published-interop-interface)),
   so writing it into the *Representations* section is this ticket's. Whether the chapter then
   publishes it as interface, as it does a union's encoding, is the language owner's and is not
   done here.

2. **Fields are evaluated in the order they are written**, in a record and in an update, and an
   update's left operand before any of them
   ([Order of evaluation](../spec/evaluation-semantics.md#order-of-evaluation)). A record
   *type* is a set, and [`LANG-48`](lang-48.md) makes the field list order-independent in
   canonicalization. The *expression* must not go the same way: `{ b = f x, a = g y }` calls
   `f` first. If the term `LANG-51` built keeps only a map, restoring the written order is the
   first step here.

3. **An update evaluates the record it updates once** and answers a new object; the old one is
   untouched. `_Utils_update` in `Js/Utils.mjs` is Elm's kernel helper for this, exported by
   nothing; delete it or use it, and do not leave it as a second implementation.

4. **An access is a property read and an accessor is a one-parameter function**, so the
   *Calls* section needs no new rule: `.name` is called one argument at a time like any other
   function value.

5. **A record pattern tests nothing of its own.** `LANG-84` gives `Step` a field step;
   `occurrence_expr` reads it as a property, and a leaf binds from it.

6. **The predicate decides exactly the record's fields**: an object that is not an array and
   not `null`, whose own keys are the record's labels and no others, each field passing its own
   type's predicate. `check_facade_admitted_type` admits a record whose every field is admitted.

7. **A label is safe as a property name.** A label is a Zelkova lowercase identifier, which
   admits JavaScript reserved words (`class`, `new`) and names every object inherits
   (`constructor`, `toString`). Both are legal property names; the predicate's key check has to
   read *own* keys or `toString` is found on every object.

8. **The module doc comment in `ir/mod.rs` says what a record owes WebAssembly.** A WIT
   `record` reaches a field by position, and a record type is a set with no order of its own.
   The type is on the access node already; say which order a positional target reads off it,
   and that label order is the one two spellings of a type agree on
   ([Records and derivation](../spec/records.md#records-and-derivation) uses it for the same
   reason).

**Equality is not this ticket's**, but it runs through it. `Basics.eq` is annotated
`a -> a -> Bool` and forwards to `Js.Utils.equalInt` today, and only a facade's result is
checked, so `==` on two records type checks and reaches the companion's `_Utils_eqHelp`. That
walks an object's keys, so two records of the representation above compare field by field. Pin
it in the fixture, since it is what a record program relies on until [`LANG-42`](lang-42.md)
replaces the forwarding, after which a record's equality is [`LANG-85`](lang-85.md)'s walk.

**Acceptance:**

In `crates/zelkova-js/tests/javascript.rs`, each seen red: the text of a record, of an access,
of a chained access, of an update, of an accessor passed as an argument, and of a declaration
whose parameter is `{ x, y }`; a facade whose result is a record emits a predicate that names
each label; a facade signature naming a record with a function-typed field is refused.

A fixture package under `tests/fixtures/` with Zelkova tests, run by
`cargo run -- test <the fixture>` and by a file under `tests/js/` the way the fixtures there
already are. Its tests cover, by the value computed: building and reading a record; an update
leaving the original unchanged; an accessor applied through a higher-order function; a record
pattern in a parameter and in a `case`; a record nested in a record and in a constructor; two
records compared with `==`; a record returned by an `unsafe` facade, and one whose companion
returns an extra field aborting at the boundary; and field expressions observed to run in
written order.

`cargo test --workspace` is green, `node --test 'tests/js/**/*.mjs'` passes,
`cargo run -- compile std/core` still lists all ten modules as checked, and
`cargo run -- test std/core` still reports `98 tests: 98 passed`.

**Found:** while ordering the record tickets for *Active work: records* in
[the index](README.md): `LANG-47` through `LANG-52` end at the typer, and `DEC-18` decision 7
promises the ticket that picks up from there.
