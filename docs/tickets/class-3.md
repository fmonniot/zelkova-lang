# CLASS-3 · Resolve classes and instances, and enforce the orphan rule

**Sizing:** large. Three things that look separate and are not: what a class puts in the value
namespace, how an instance reaches another module, and which module is allowed to declare one.

**Location:** `src/compiler/canonical/environment.rs` — `RootEnvironment`,
`insert_union_type`, `process_import`; `src/compiler/canonical/mod.rs` — `Error`,
`from_parser_module`, `Module::to_interface`; `src/compiler/mod.rs` — `Interface`;
`src/compiler/dependencies.rs` — `ModuleWalker::check_in_order`, which is the driver that
builds each interface and hands it to the next module.

**Depends on:** [CLASS-2](class-2.md) for the declarations to exist at all;
[BUG-16](bug-16.md) and [BUG-17](bug-17.md), both of which would quietly sabotage instance-head
resolution and are worth reading together with this ticket:

- **BUG-17** — a type application's arguments are discarded when its head resolves, so
  `instance Comparable (Maybe Int)` and `instance Comparable (Maybe Char)` would canonicalize to
  the same head. Instance *lookup* is a match on the head, so this is not a cosmetic loss: it
  makes two distinct instances indistinguishable, and the duplicate-instance check below would
  then reject a legal program or accept an ambiguous one depending on which way it is written.
- **BUG-16** — an unresolved type name is invented rather than reported, so
  `instance Comparable Widgt` would fabricate `Widgt` and declare an instance for a type that
  does not exist. The orphan rule then passes it, because the fabricated type has no declaring
  module to compare against.

Neither is optional. Land both first.

**Decided (`SPEC-12`, by the language owner):** an `instance C T` declaration is legal in the
module declaring `C`, and in the module declaring `T`'s head, and nowhere else.

**Problem:** after `CLASS-2` a class and an instance reach `canonical::Module` and nothing looks
at them. Four things have to happen before a constraint can ever be discharged:

**A class puts its members in the value namespace.** `compare`, declared inside
`class Comparable a where`, is callable as `compare` — its type outside the class being
`Comparable a => a -> a -> Order`, the member's own signature with the class's variable bound by
the class. That is a new way for a name to enter the environment, alongside a top-level value
and a type constructor.

**An instance is not a name, so `exposing` cannot carry it.** Every other thing crossing a
module boundary is looked up by a name the importer wrote. An instance has none: the importer
never mentions it, and coherence means it must be in scope everywhere the class and the type
are, whether or not any module asked for it. So `Interface` gains instances, and they propagate
**transitively and unconditionally** — through a module that imports neither the class's module
nor the type's, and regardless of any `exposing` list. Getting this wrong is not a compile error
anywhere; it is a program that type checks in one module and not in another for reasons its
author cannot see.

**Two instances for one (class, type) pair is an error.** With the orphan rule above they can
only collide across two modules — the class's and the type's — which is exactly the case the
rule narrows to and does not eliminate. It wants the error `AmbiguousVariables` already models:
a primary label in the module doing the resolving, and one secondary label per candidate, each
in that candidate's own file. `ERR-5` built that machinery and `canonical::Error::AmbiguousVariables`
is the worked example to copy.

**An orphan is an error naming both alternatives.** *`Comparable` is declared in `Comparable`
and `Colour` in `Colour`; an instance may go in either.* That message is the whole value of the
rule to a reader, so it is worth writing before the check that produces it.

**Approach:**

1. `Interface` gains a class table and an instance table. `file` is already there (`ERR-5`), so
   an instance carries enough to be labelled in its own source; `Interface::source_span` is the
   existing shape for that pairing.

2. `to_interface` publishes **every** instance, not the exposed ones — it takes no `exposing`
   list today ([BUG-9](bug-9.md) is why, and that is convenient here rather than a thing to
   fix). Instances arriving from an *import* must be re-published too, which is what makes
   propagation transitive; that is a genuine change of shape, because `to_interface` currently
   publishes only what the module itself declared.

3. `RootEnvironment` gains the class and instance tables, filled from the module's own
   declarations and from every import's interface. `process_import` is where the second half
   lands.

4. The orphan check, the duplicate check, and the superclass-obligation check on an instance
   declaration. The third one — `instance Comparable Colour` requires `instance Eq Colour` — is
   a resolution question and belongs here rather than in the solver: it is about which
   declarations exist, not about a type the checker inferred.

5. New `canonical::Error` variants for each, every one with a `message()` in the reader's
   vocabulary and a span. `CLAUDE.md`'s *An error has to describe itself* applies without
   exception, and a group error flattens its members' labels — forgetting that silently drops
   every caret it swallowed.

Nothing in the typer changes in this ticket. A constrained annotation still validates and is
discarded; [CLASS-4](class-4.md) is what starts consuming it.

**Acceptance:** tests in `tests/compiler/canonical.rs`, using the `package=`-style multi-module
helpers in `tests/support/mod.rs`:

- An instance declared in the class's module resolves, and so does one declared in the type's
  module; one declared in a third module is the orphan error, asserted by variant and with a
  `diagnostic.labels[..]` assertion showing both alternatives named.
- An instance declared once in the class's module *and* once in the type's module is the
  duplicate error, with one secondary label per candidate, each carrying its own
  `SpanLabel::file`.
- An instance declared in module `A` is in scope in module `C`, where `C` imports `B` and `B`
  imports `A` and no `exposing` list mentions anything — the transitivity test, and the one
  most likely to be missed.
- `instance Comparable Colour` without `instance Eq Colour` is the superclass error.
- A class member is callable by its bare name in an importing module.

`cargo run` still prints `parsed 8 modules` and lists all eight as checked.

**The orphan block in [`docs/spec/type-classes.md`](../spec/type-classes.md) needs retagging and
will not go red to remind you.** It sits under *Where an instance may be declared*, is tagged
`expect=unimplemented`, and fails today because `Comparable` resolves to nothing. After this
ticket it fails because the instance is an orphan — the verdict the chapter actually claims, and
a different one. Retag it `expect=canonical-error:<the new variant>`.
