# LANG-39 · Resolve classes and instances, and enforce the orphan rule

**Sizing:** large. Four things that look separate and are not: what a class and an instance
are once canonicalized, what a class puts in the value namespace, how an instance reaches
another module, and which module is allowed to declare one.

**Location:** `crates/zelkova-compiler/src/canonical/mod.rs` — `Module`, `Error`,
`canonicalize_recovering`, `do_exports`, `Module::to_interface`;
`crates/zelkova-compiler/src/canonical/environment.rs` — `RootEnvironment`,
`insert_declared_type`, `insert_top_level_value`, `process_import`;
`crates/zelkova-compiler/src/lib.rs` — `Interface`;
`crates/zelkova-compiler/src/dependencies.rs` — `ModuleWalker::check_in_order`, the driver that
builds each interface and hands it to the next module; `crates/zelkova-js/src/lib.rs` — `emit`
and its `Error`.

**Depends on:** [LANG-38](lang-38.md), for the declarations to parse. That ticket leaves
canonicalization rejecting every class and instance with one error each; this one replaces the
rejection with the real thing.

**Decided (by the language owner):** an `instance C T` declaration is legal in the module
declaring `C`, and in the module declaring `T`, and nowhere else
([`DEC-2` decision 3](../decisions/dec-2.md#3--an-instance-lives-with-its-class-or-with-its-type)).
[DEC-24](../decisions/dec-24.md) settled the rest of what this ticket checks: what an instance
head may be and where a tuple's instance lives (decision 2), that a written instance may carry a
context (decision 3), that a member signature carries no context and mentions the class variable
(decision 4), and how a class is named, exposed and imported (decision 6). The chapter states
each as a rule — [*Declaring a class*](../spec/type-classes.md#declaring-a-class),
[*Exposing and importing a class*](../spec/type-classes.md#exposing-and-importing-a-class),
[*What an instance is declared for*](../spec/type-classes.md#what-an-instance-is-declared-for),
[*Where an instance may be declared*](../spec/type-classes.md#where-an-instance-may-be-declared) —
and those sections are what a check here is held to.

**Problem:** after `LANG-38` a class and an instance parse, and canonicalization turns each away.
Five things have to happen before a constraint can ever be discharged.

**A class and an instance get a canonical form.** `canonical::Module` gains its classes and its
instances. What each has to carry, whatever the fields are called:

- A class: its name, its one variable, its superclasses as resolved class names, each member's
  name, canonical `Type` and span, and each derivation — the member it is for and its bindings,
  canonicalized as ordinary values. Whether a derivation is *well formed* is
  [LANG-83](lang-83.md)'s; here it is carried and nothing more.
- An instance: the resolved class, its head, its context, where it was written, and its body —
  the member bindings canonicalized as ordinary values, or the fact that it is `derived`.
- A constraint — in a class head's superclass context and in an instance's context — as a
  resolved class name and the variable it is on. [LANG-70](lang-70.md) reuses the same type for
  an annotation's context, so write it to be reused.

The head `LANG-38` hands over is an unchecked `(Option<Context>, Type)`, and taking it apart is
where the shape errors come from. A class head is an uppercase name applied to exactly one type
variable; its context, if any, is constraints on that variable. An instance head is a class name
applied to exactly one type, and that type is one of the three forms the chapter lists: a
declared type applied to as many **distinct variables** as it has parameters, a tuple of
distinct variables, or `()`. `instance Eq (Maybe Int)`, `instance Eq (Pair a a)`,
`instance Eq (a -> b)` and `instance Eq a` are each an error at the head. An instance's context
constrains only variables its head binds.

A member signature that carries a context (`FunType::context` set), one marked `unsafe`, one
that never mentions the class variable, and two members of one name are each an error at the
class. An instance binding that names no member, a member with no binding, and a member bound
twice are each an error at the instance; the missing-member message names the member and the
class.

**A class name is a type-namespace name.** A class and a type of one name in one module is an
error, as two classes are. (Two *types* of one name is [LANG-32](lang-32.md), open and not this
ticket's.) A class name resolves wherever one is written: a superclass, an instance's class, a
constraint. One that resolves to nothing is an error naming it — the failure `BUG-16` was for a
type name — and `canonical::Error::TypeNotFound` is already what an instance head naming no type
raises.

**A class puts its members in the value namespace.** `compare`, declared inside
`class Comparable a where`, is a top-level value of its module: callable as `compare`, a name an
`infix` declaration in that module may bind an operator to, and a clash with a top-level
declaration of the same name. Its type outside the class is the member's signature with the
class's own constraint in front. Nothing in the typer reads that yet, so until
[LANG-40](lang-40.md) a declaration that mentions a member is left unchecked, the way one that
mentions an unannotated declaration is today.

**The `exposing` list and the import list know a class.** In a header, a bare uppercase entry
names a type *or a class*, and naming a class exposes its members with it. `Comparable(..)` on a
class and a member listed by itself are each an error. `(..)` exposes classes too. In an import
list, naming the class brings the class name and every member into scope unqualified; naming a
member by itself is allowed; a qualified `Module.member` resolves with neither. The default
imports ([Modules](../spec/modules.md#the-default-imports)) take `Basics` as `exposing (..)`,
so nothing changes there.

**An instance is not a name, so `exposing` cannot carry it.** Every other thing crossing a
module boundary is looked up by a name the importer wrote. An instance has none: the importer
never mentions it, and coherence means it must be in scope everywhere the class and the type
are, whether or not any module asked for it. So `Interface` gains classes and instances, and
instances propagate **transitively and unconditionally** — through a module that imports neither
the class's module nor the type's, and regardless of any `exposing` list. Getting this wrong is
not a compile error anywhere; it is a program that type checks in one module and not in another
for reasons its author cannot see.

**Approach:**

1. `Interface` gains a class table and an instance table. `file` is already there (`ERR-5`), so
   an instance carries enough to be labelled in its own source; `Interface::source_span` is the
   existing shape for that pairing. An instance entry records the module that declared it, so
   the same instance arriving by two import routes is recognised as one.

2. `to_interface` publishes the classes the header exposes, each with its members, and **every**
   instance. That is an explicit exception: since `BUG-9` closed, `to_interface` filters values,
   types and infixes against the module's `exposing` list, and the instance table is the one
   part of the interface that skips the filter. Instances arriving from an *import* are
   re-published too, which is what makes propagation transitive, and is a change of shape:
   `to_interface` currently publishes only what the module itself declared. `Interface` is what
   `ModuleWalker::check_in_order` hands the next module, and a package's public modules reach
   another package the same way, so nothing else has to carry them.

3. `RootEnvironment` gains the class and instance tables, filled from the module's own
   declarations and from every import's interface. `process_import` is where the second half
   lands. A class or instance declaration that fails leaves the scope incomplete
   (`set_incomplete`), as a failed `type` does, so a use of one of its members is not reported a
   second time.

4. The checks on an instance declaration, in the order a reader would want them reported:

   - **The orphan rule.** The instance's module is the class's or the head type's; for a tuple
     or `()` head, the class's only. The message names both alternatives — *`Comparable` is
     declared in `Comparable` and `Colour` in `Colour`; an instance may go in either* — which is
     the whole value of the rule to a reader, so write it before the check that produces it.
   - **The duplicate.** Two instances with one class and one head name. With the orphan rule
     above and imports that cannot form a cycle, the two legal modules can never *both* declare
     one — whichever of them names the other's declaration imports it — so the reachable case is
     two declarations in one module. Check against every instance in scope regardless, own and
     imported: the argument that it cannot happen leans on two other rules. Primary label on the
     second, secondary on the first.
   - **The superclass.** `instance Comparable Colour` requires an `Eq` instance with the same
     head name to be in scope. Whether that instance's *context* is satisfied by this one's is a
     question about constraints, and is [LANG-40](lang-40.md)'s.

5. New `canonical::Error` variants for each, every one with a `message()` in the reader's
   vocabulary and a span. `CLAUDE.md`'s *An error has to describe itself* applies without
   exception, and a group error flattens its members' labels.

6. **`zelkova_js::emit` refuses a module that holds a class or an instance**, with an error of
   its own, until [GEN-24](gen-24.md) emits them. Once this ticket lands such a module reaches
   emission, and the backend would otherwise write it out with its classes missing. `emit` is
   handed the `canonical::Module` beside the IR, which is where it reads them from.

Nothing in the typer changes. An instance's bindings and a derivation's are canonicalized and
not type checked; a constrained annotation still validates and is discarded here —
[LANG-70](lang-70.md) resolves it, and [LANG-40](lang-40.md) is what starts consuming any of
this.

**Acceptance:** tests in `crates/zelkova-compiler/tests/canonical.rs`, using
`canonicalize_with_interfaces` and building each imported module's `Interface` from its own
source rather than by hand, each error asserted by variant and by
`diagnostic.labels[..].range`, each seen red with its check neutralised:

- A class and an instance are in `canonical::Module`, with the member types, the superclass, the
  instance's context and its bindings asserted.
- Each head error: `Maybe Int`, a repeated variable, a function type, a bare variable, a class
  applied to two types. A context constraining a variable the head does not bind.
- Each member error: a signature with `=>`, one marked `unsafe`, one not mentioning the class
  variable; a missing member, naming it and the class; an extra binding; a member bound twice.
- A class and a type of one name. A superclass, an instance's class, naming no class.
- An instance declared in the class's module resolves, and so does one declared in the type's
  module; one declared in a third module is the orphan error, with a label assertion showing
  both alternatives named. A tuple instance outside the class's module is the orphan error.
- Two instances of one class for one type in one module is the duplicate error, with the
  secondary label on the first.
- An instance declared in module `A` is in scope in module `C`, where `C` imports `B` and `B`
  imports `A` and no `exposing` list mentions anything — the transitivity test, and the one
  most likely to be missed. Assert it on `C`'s `Interface` as well as on its environment.
- `instance Comparable Colour` without `instance Eq Colour` is the superclass error.
- A class member is callable by its bare name in an importing module that names the class in
  its import list, by its bare name where the import list names the member alone, and qualified
  with neither. A header listing a member on its own, and `Comparable(..)`, are each an error.
- An `infix` declaration naming a member of a class its module declares canonicalizes.

In `crates/zelkova-js/tests/javascript.rs`: a module holding a class is refused by `emit`.

`cargo test --workspace` is green. `cargo run -- compile std/core` still prints
`parsed 10 modules`, lists all ten as checked and exits 0.

**The chapter's blocks.** Run `cargo test --test spec -- --nocapture` and read what each
`expect=unimplemented` block in [`docs/spec/type-classes.md`](../spec/type-classes.md) now fails
on. Three kinds of change:

- The blocks that declare a class and use nothing else — the chapter's first block, the ones
  under *Declaring a class* and *Superclasses*, the derivations under *A class says how it is
  derived* and *A derivation over one value* — now canonicalize, go red, and are retagged
  `expect=ok`.
- **The orphan block under *Where an instance may be declared* will not go red to remind you.**
  It is `expect=unimplemented` and fails today because nothing parses; after this ticket it
  fails because `Comparable` resolves to nothing, which is still not the verdict the chapter
  claims. Give it the class and the type it needs — two more blocks in a `package=` group, which
  can hold them now that all three parse — and retag it `expect=canonical-error:<the orphan
  variant>`.
- A block that fails only because it names a class it does not declare gets the declaration
  added. Leave it `expect=unimplemented` if it still fails for the reason its prose gives.

Rewrite the chapter's opening `**Not implemented:**` paragraph, which says a class and an
instance do not parse.
