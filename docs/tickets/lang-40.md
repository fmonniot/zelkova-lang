# LANG-40 · Discharge class constraints in the type checker

**Sizing:** large. Hindley–Milner with a constraint set is a different solver from the one in
the tree: `unify` currently answers every constraint immediately, and a class obligation is one
it may not be able to answer yet.

**Location:** `crates/zelkova-compiler/src/typer/mod.rs` — `type_check_recovering`, which builds
the environment every declaration is checked against; `Types`, `Types::by_name` and
`Types::instantiate`; `infer_annotated`; `Constraint`, `Origin`, `Reason`, `ErrorKind`, `Solved`;
`canonical_type_to_typer_type`, `value_to_term_and_annotation`;
`crates/zelkova-compiler/src/typer/constraint.rs` — `collect`;
`crates/zelkova-compiler/src/typer/unifier.rs` — `unify`;
`crates/zelkova-compiler/src/ir/mod.rs` — `Module`, `Declaration`, the reference node and its
`ReferenceKind`, `build`, and the module doc comment's *What is not here yet*;
`crates/zelkova-js/src/lib.rs` — `emit`'s refusals.

**Depends on:** [LANG-39](README.md), for a class table and an instance table to discharge
against; [LANG-70](README.md), for an annotation's context on the canonical value and in the
`Interface`; and nothing else: an integer literal is an `Int`, so no obligation is raised at a type that is
neither `Int` nor `Float` and has no instance.

**Not on [LANG-12](lang-12.md), which this ticket used to call a hard prerequisite.** The order
was turned round ([`DEC-24` decision
10](../decisions/dec-24.md#10--lang-12-closes-the-order-instead-of-opening-it)): `LANG-12`'s
rigid variables reject thirteen declarations of `std/core`'s `Basics` that only the class
mechanism can make honest, so the solver lands first, on the flexible annotation variables the
tree has today, and `LANG-12` closes the order. What that costs here is stated under *Given
constraints* below, and it is a hole this ticket leaves open on purpose.

**Decided (by the language owner):** a caller supplying a type with no instance is an error at
the call ([*Constraining an annotation*](../spec/type-classes.md#constraining-an-annotation)); a
superclass is implied by its subclass ([*Superclasses*](../spec/type-classes.md#superclasses));
nothing defaults and the compiler knows no class by name
([*Numeric literals*](../spec/type-classes.md#numeric-literals)); and a constraint is never
inferred — a declaration with no annotation left needing a class of an undetermined type is an
error asking for the annotation
([`DEC-24` decision 5](../decisions/dec-24.md#5--a-constraint-is-never-inferred)).

**Problem:** the typer has no notion of an obligation. `Constraint` is a pair of types plus an
`Origin`, `unify` solves each one on sight, and the environment `type_check_recovering` builds
maps a name to a type and nothing else, so a constrained function is checked exactly as it would
be without its constraint and a class member is not in the environment at all. Nothing in the
solver resembles a class.

**Approach:**

1. **A name's type is a context and a type.** The environment's entries gain the context
   `LANG-70` put on `Value::TypedValue` and on `Interface` entries. A class member enters the
   environment here for the first time, from the module's own classes and from every imported
   interface's class table, typed as its signature with the class's constraint in front
   (`compare : Comparable a => a -> a -> Order`). `Types::by_name` instantiates the context with
   the same fresh variables it gives the type, and each instantiated constraint is an
   **obligation** of that use.

2. **A class obligation is a second kind of constraint**, not a new `Type` case.
   [`DEC-2` decision 5](../decisions/dec-2.md#5--no-higher-kinded-variables) — no higher-kinded
   variables — is what makes this simple: a class is always over a complete type, so an
   obligation is a (class name, type) pair and never a partial application. Keep obligations in
   order, in a `Vec`, for the reason `Constraint`'s own doc comment gives (*Why these are held
   in a `Vec` and not a `HashSet`*): deduplication drops provenance, and an unordered collection
   makes *which* error is reported vary between runs.

3. **Obligations are deferred, not solved on sight.** `Comparable t7` cannot be answered while
   `t7` is unsolved. Unification runs as it does today; then each obligation is read with the
   final substitution applied and discharged:

   - **Its type is a declared type, a tuple or `()`.** Look the instance up by class and the
     head's name, which [the head rule](../spec/type-classes.md#what-an-instance-is-declared-for)
     makes a plain lookup with at most one answer. No instance is an error naming the class and
     the type. An instance with a context turns into further obligations, the context
     instantiated at the type's arguments: `Eq (Maybe Colour)` asks for `Eq Colour`.
   - **Its type is a function.** No instance can exist; the same error.
   - **Its type is still a variable.** It is discharged if the declaration's own context
     provides it — see the next step — and is otherwise one of three errors, told apart because
     the fix differs. If the variable appears in the declaration's type and the declaration is
     annotated, the annotation is missing a constraint: say which, and that adding it is the
     fix. If the declaration has no annotation, it needs one: say what it has to state. If the
     variable appears nowhere in the declaration's type, nothing determines the type the class
     is needed at, and no annotation on this declaration can.

4. **Given constraints.** Inside a constrained declaration's body the annotation's context is
   *given*, not proved: `Comparable a` discharges an obligation `Comparable a` on that variable,
   and, through the superclass, `Eq a` — transitively, for a superclass of a superclass.

   On flexible variables this means: an annotation's variable is the unification variable
   `canonical_type_to_typer_type` made for it, a given is that variable with the final
   substitution applied, and an obligation still on a variable is discharged when a given is on
   the same variable. **If the body forced the variable to a concrete type, the given goes with
   it and the obligation is discharged by an instance** — `min : Comparable a => a -> a -> a`
   whose body solves `a := Int` proves `Comparable Int` and publishes `Comparable a`. That is
   the hole `LANG-12` closes, it is exactly as wide as the one every annotation has today, and
   this ticket does not close it. Do not add a partial rigidity check here to narrow it.

5. **Everything with a body is checked.** Besides value declarations:

   - **An instance's bindings.** Each is checked as a declaration whose annotation is the
     member's signature with the class variable replaced by the instance's head, and whose given
     context is the instance's own.
   - **An instance's superclasses.** `LANG-39` checked that `instance Comparable T` has an `Eq`
     instance with the same head in scope. Here the obligation `Eq T` is discharged with the
     instance's context as given, which is what catches `instance Comparable (Box a)` beside
     `instance Eq a => Eq (Box a)`: `Eq (Box a)` needs `Eq a`, and nothing provides it.
   - A `derived` instance and a derivation's bindings are [LANG-83](lang-83.md)'s. Treat a
     derived instance as an instance that exists, with the context `LANG-39` recorded for it —
     none, until `LANG-83` infers one.

6. **Provenance carries over unchanged, and must.** Every obligation records an `Origin` — the
   span of the use it came from and a `Reason` naming why. `Reason` gains at least one variant
   for *this use requires an instance*, and its `describes()` / `explains()` / `note()` arms are
   written for the reader. `Reason::describes` names no type deliberately, and that reasoning
   applies to obligations too: by the time one fails, substitution has moved types around.
   `ERR-4`'s labelling gives the caret for free once the obligation carries an `Origin`.

7. **New `ErrorKind` variants**, one per failure in step 3 — four of them — each with a
   `message()` in the user's vocabulary. Their names become `expect=type-error:Kind` tags in the
   chapter, so choose them to read well there.

8. **The typer hands back what it discharged.** A backend cannot re-derive an instantiation, and
   [GEN-24](gen-24.md) specialises on it. In the IR:

   - a reference to a name whose type has a context carries that context as instantiated at the
     use — one (class, type) pair per constraint, with the final substitution applied like every
     other type on the node;
   - a declaration carries its own context, as the variables of its solved type;
   - a module carries its instances, each with its class, head, context and one checked body per
     member, built the way a declaration's body is, and a declaration or instance binding that
     did not check is accounted for the way `ir::Module::unchecked` accounts for a value.

   Rewrite the IR module doc comment's *What is not here yet* and the paragraph on *A type on
   every node* for what is now there. The shape is this ticket's to choose within those three
   requirements; `GEN-24` is written against them and not against field names.

9. **`zelkova_js::emit` refuses what it cannot emit yet.** `LANG-39` made it refuse a module
   holding a class or an instance. A module that holds neither can still hold a constrained
   declaration or a use of an imported member, and after this ticket both reach the IR. `emit`
   answers an error for a declaration with a context and for a reference carrying obligations,
   until `GEN-24`.

**What this ticket does not reach.** Nothing in `std/core` declares a class, so nothing there
carries a constraint for this solver to discharge; [LANG-42](lang-42.md) is where that changes.
The solver is exercised by tests and by the chapter's blocks long before it is exercised by the
standard library. Do not read a green `cargo run -- compile std/core` as evidence this ticket
works.

**Acceptance:** tests in `crates/zelkova-compiler/tests/typer.rs`, each a source string
declaring its own classes and instances, each error asserted by kind and by
`diagnostic.labels[..].range`, each seen red with its check neutralised
(`CLAUDE.md`, *A green test proves nothing until you have seen it fail*):

- A use whose obligation is discharged by an instance checks. One through an instance with a
  context checks when the context's own obligation is discharged, and is an error when it is
  not, naming the inner class and type.
- A use at a type with no instance is an error naming the class and the type, with the caret
  under the use and not under the declaration. A use at a function type likewise.
- A constrained declaration whose body uses a member of its context's class checks; so does one
  using a member of that class's superclass. One using a class its context does not provide is
  the missing-constraint error, naming the constraint to add.
- A declaration with no annotation whose body needs a class of an undetermined type is the
  needs-annotation error. `isZero n = eq n 0`, with no annotation, checks.
- A use whose class is needed at a type nothing determines is the third error of step 3.
- An instance binding is checked against the member's signature at the instance's type: a
  binding of the wrong type is an error. `instance Comparable (Box a)` beside
  `instance Eq a => Eq (Box a)` is an error naming `Eq a`.
- A member and a constrained function imported from another module are checked against the
  context in that module's `Interface`, and an instance declared in a third module discharges
  the obligation.
- A use through an operator whose `infix` declaration names a member raises the same obligation
  the member does.
- `min : Comparable a => a -> a -> a` with a body that forces `a := Int` **checks**, and the
  test says why in a comment naming `LANG-12` — it pins the hole so that `LANG-12` has a test
  to turn round.

In `crates/zelkova-compiler/tests/ir.rs`: a reference to a constrained function carries its
instantiated context, a declaration carries its own, and a module carries its instances with
their bodies. In `crates/zelkova-js/tests/javascript.rs`: a constrained declaration and a
reference carrying obligations are each refused.

`cargo test --workspace` is green. `cargo run -- compile std/core` still prints
`parsed 10 modules`, lists all ten as checked and exits 0, and `cargo run -- test std/core`
still reports `98 tests: 98 passed`.

**The chapter's blocks.** Run `cargo test --test spec -- --nocapture`. In
[`docs/spec/type-classes.md`](../spec/type-classes.md):

- **The block under *A constraint is never inferred* will not go red.** It is
  `expect=unimplemented` and after this ticket fails for the reason the chapter gives. Retag it
  `expect=type-error:<the needs-annotation kind>`.
- The `**Not implemented:**` paragraph under *Constraining an annotation* says a constrained
  annotation asks nothing of a caller. It now does; delete the paragraph and its ticket links.
- A block that now type checks and is tagged `expect=unimplemented` goes red by itself; retag
  it `expect=ok` if its prose claims no more than that.

[SPEC-36](spec-36.md)'s block in `docs/spec/expressions.md` is not repaired by this ticket and
its paragraph should stop citing this one alone: what makes `double` an error is `LANG-12`.
