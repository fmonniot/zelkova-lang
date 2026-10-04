# GEN-24 · A class member, an instance and a constrained function are not emitted

**Sizing:** large. It adds a step that reads the whole build at once, where every step before it
reads one module, and it is the first time the emitter writes a function nobody declared.

**Location:** `crates/zelkova-compiler/src/ir/` — a new pass beside `decision.rs`, and the
module doc comment in `mod.rs`; `crates/zelkova-js/src/lib.rs` — `emit`, its `Error`, the
*Exports* and *Calls* sections of the module doc comment, and `Unions`, the existing example of
a table of the whole build handed to `emit`; `crates/zelkova/src/lib.rs` — `compile` and
`emit_build`, which are where the build's checked modules are all in hand, and `BuildError`;
`tests/fixtures/` and `tests/js/`.

**Depends on:** [LANG-40](README.md), which puts a context on a declaration, an instantiated
context on each reference to one, and the instances with their checked bodies in the IR; and
[LANG-83](README.md), after which a derived instance has bodies like a written one and needs
nothing special here.

**Decided (by the language owner):** a constrained function is specialised per instantiation and
no dictionary is built or passed at run time
([`DEC-2` decision 7](../decisions/dec-2.md#7--dictionaries-are-erased-by-specialisation-not-passed),
[*A constrained function may not be a foreign facade*](../spec/type-classes.md#a-constrained-function-may-not-be-a-foreign-facade)).
A constrained function whose specialisations do not form a finite set is an error
([`DEC-24` decision 9](../decisions/dec-24.md#9--a-constrained-function-whose-specialisations-never-end-is-an-error)).
This is a ticket of its own, and not a half of `LANG-40`, because a construct that lands after
the backend gets its own emitter ticket
([`DEC-18` decision 7](../decisions/dec-18.md#7--the-program-covers-the-language-the-front-end-accepts-today)).

**Problem:** `zelkova_js::emit` refuses a module that holds a class or an instance, a declaration
with a context, and a reference that carries obligations — three refusals `LANG-39` and
`LANG-40` added so that nothing was written out with its classes missing. So after `LANG-40` a
program using a class type checks and cannot be built. There is nothing for `emit` to write
instead: a constrained function has no single JavaScript function, and which one a use needs is
a fact about the use.

**What the result has to be.** Each of these is a requirement, and the reason is given because
the obvious implementation breaks two of them.

1. **No dictionary.** The emitted JavaScript holds one ordinary function per instantiation.
   Nothing is passed to a function to tell it which instance it is running at.

2. **A specialisation is one declaration at one assignment of ground types to its constrained
   variables.** On this target only those variables matter: a variable with no constraint needs
   no copy, since nothing in the emitted code depends on it. Two uses at one key, in one module,
   are one function.

3. **A specialisation is emitted into the module that uses it**, and not into the module that
   declares the function. Its body is a copy of the declaring module's code that calls the
   members of some instance, and the instance may live in a module that *imports* the declaring
   one — `Basics.min` at `App.Colour` calls a `compare` that `App` declares. Emitted beside
   `min`, the copy would make `Basics` import `App`, and the build's modules would no longer be
   initialised in dependency order
   ([*A binding with no parameters is evaluated once*](../spec/evaluation-semantics.md#a-binding-with-no-parameters-is-evaluated-once)).
   The using module is downstream of everything the copy mentions.

   The cost is that two modules using `min` at `Colour` each carry a copy. For a function that
   is duplicated code and nothing a program can observe. For a constrained binding with **no
   parameters** it means one evaluation per using module where the chapter says "once"; a
   Zelkova value has no identity, so the difference is work done and not an answer changed.
   Record it in the module doc comment. If the chapter's promise is read to cover it, that is a
   `SPEC-` ticket to file, not a design to change here.

4. **A copied body can still reach what it mentions.** It was written in another module and may
   name a value that module does not expose. So a module's emitted `export` list is a JavaScript
   matter and not the `exposing` list: it holds what the header exposes, each instance member,
   and whatever a constrained declaration, an instance binding or a derivation's binding of that
   module mentions at top level. `exposing` is enforced by canonicalization and is no weaker for
   it. A constrained declaration itself has no function to export under its own name.

5. **An instance member is an ordinary function of the instance's module**, emitted once,
   under a name built from the class, the head and the member that no source name can collide
   with. A use of a member at a known instance is a direct reference to it. An instance with a
   context has members that are themselves constrained functions, specialised like any other.

6. **The call rule does not change.** A specialised function has the arity its declaration was
   written with, a saturated call of one is a direct call, and one used as a value is
   `$curry`'d — everything the *Calls* section already says.

7. **The specialisations are found by a pass over the IR of the whole build**, in
   `zelkova-compiler`, which `zelkova-js` then reads — not inside the JavaScript emitter. One IR
   serves both targets
   ([`DEC-18` decision 2](../decisions/dec-18.md#2--one-ir-serves-both-targets-and-javascript-is-written-first)),
   and [GEN-15](gen-15.md) needs the same pass for a reason of its own.

8. **A chain of specialisations has a limit.** `f` at `Int` needing `f` at `Maybe Int` needing
   `f` at `Maybe (Maybe Int)` is stopped at a fixed depth and reported, as a `BuildError` that
   names the declaration and the type that kept growing. Choose the limit generously and say
   what it is in the message. `CLAUDE.md`'s *A pass that emitted an error must not report
   success* applies: the error is pushed onto the build's accumulator and the build writes
   nothing.

9. **The output is deterministic.** Two runs over one unchanged build write the same text, in
   the same order, under the same names.

**Approach:** a worklist. The roots are every declaration with no context, and every member of
an instance with no context, in every module of the build. Reading a root's body, each reference
that carries obligations is resolved: each obligation's type is ground there, so its instance is
a lookup by class and head — the same lookup `LANG-40` did — and the reference becomes either a
direct reference to an instance member or a specialisation of a constrained declaration, which
is added to the work if its key is new. Reading a specialisation's body is the same with the
key's assignment applied to the body's types first; that is what turns an obligation on the
declaration's own variable, which `LANG-40` discharged as given, into one at a ground type.

The pass hands back, per module, the specialisations it must emit and what each reference
resolved to. How that is represented — new declarations in an `ir::Module`, a table beside the
modules the way `Unions` is — is this ticket's to choose within the nine requirements above;
none of them is a field name. `compile` emits a second tree for a build that compiled its tests,
and the pass has to cover both.

Remove the three refusals. Rewrite the `zelkova-js` module doc comment's *Exports* section, add
one for a specialisation and an instance member, and update *What is refused*; in
`ir/mod.rs`, *What is not here yet*.

**Acceptance:**

In `crates/zelkova-compiler/tests/ir.rs`, on the pass, each seen red:

- A constrained function used at two types in one module yields two specialisations; used
  twice at one type, one.
- A use inside a constrained function, at that function's own variable, resolves once the
  enclosing function is specialised, to the instance at that type.
- A use through an instance with a context (`eq` at `Box Colour`) yields the member specialised
  at `Colour`. A use of a superclass's member through a subclass's constraint resolves.
- A specialisation is assigned to the using module. Two modules using one key each get it.
- `f x = f (Box x)` under a constraint is the limit's error, naming `f`. So is the same loop
  through two functions.
- The result for one build is identical across two runs.

In `crates/zelkova-js/tests/javascript.rs`: the text of a specialised function takes exactly
its declared parameters and no more; a member reference at a known instance is a direct call of
the instance's function; a module exports what a constrained declaration of it mentions.

A fixture package under `tests/fixtures/` with Zelkova tests, run by
`cargo run -- test <the fixture>` and by a file under `tests/js/` the way the fixtures there
already are (`CLAUDE.md`, *Commands*, describes how each file compiles its own fixture). It
declares its own classes — `std/core` has none until [LANG-42](lang-42.md) — and its tests
cover, by the value computed:

- a member at an instance, and a constrained function at two types;
- an instance with a context, and a member of a superclass used through the subclass;
- a class in one module, the instance in the type's module, and the use in a third;
- a member behind an operator;
- a derived `Eq` and `Comparable` on a union with arguments, on a tuple, and on a recursive
  list-like type;
- two recursive lists of forty elements that differ only in the last compare unequal — which
  returns at once when each part is answered once, and does not return when `combine`'s first
  parameter is substituted.

`cargo test --workspace` is green, `node --test 'tests/js/**/*.mjs'` passes,
`cargo run -- compile std/core` still prints `parsed 10 modules` and lists all ten as checked,
and `cargo run -- test std/core` still reports `98 tests: 98 passed`.

**The documents that move with it.** In [`docs/spec/type-classes.md`](../spec/type-classes.md),
the `**Not implemented:**` paragraph under *A constrained function may not be a foreign facade*
is deleted; it cites this ticket, so `cargo test --test spec` is red until it goes. The `loop`
block above it stays `expect=fragment`: the spec harness runs no specialisation, so no tag can
hold it to account, and the fixture above is what does.
