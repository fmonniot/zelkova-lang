# LANG-83 · A derivation is not checked, and a `derived` instance has no members

**Sizing:** large. Three pieces that only work together: the checks on a class's derivation, the
context a derived instance is inferred to need, and the member definitions it stands for. What
could make it bigger is a recursive type, which makes the second a fixed point.

**Location:** `crates/zelkova-compiler/src/canonical/mod.rs` — the canonical class and instance
[LANG-39](README.md) introduced, `canonicalize_recovering`, `Module::to_interface`, `Error`;
`crates/zelkova-compiler/src/lib.rs` — `Interface`'s class table;
`crates/zelkova-compiler/src/scalars.rs` — where a type the compiler knows by qualified name is
written down; `crates/zelkova-compiler/src/typer/mod.rs` — where [LANG-40](README.md) checks an
instance's bindings; `crates/zelkova-compiler/tests/support/mod.rs` — `basics_interface()`, the
stand-in `Basics` every spec block compiles against, which declares no `Position`.

**Depends on:** [LANG-39](README.md), which carries a derivation and a `derived` body into the
canonical module without reading either, and [LANG-40](README.md), whose solver is what checks
the definitions this ticket produces.

**Decided (`SPEC-14`, `SPEC-25` and `SPEC-27`, by the language owner):** every rule here is in
[`docs/spec/type-classes.md`](../spec/type-classes.md) — *An instance may be derived*, *A class
says how it is derived*, *What a derived instance computes*, *A derivation over one value*,
*Deriving for a tuple*, *The bindings are inlined, not called*, *What a derived instance
requires*. Read all seven before starting; none is re-argued here.
[DEC-24](../decisions/dec-24.md) added three things to them: that `combine`'s first parameter
names a value computed once and its second stands for the rest of the walk (decision 8), that a
tuple and `()` are derivable heads, and that derivation is part of this order (decision 11).
[DEC-10](../decisions/dec-10.md) is why nothing here checks that `combine` is associative, and
nothing should.

**Problem:** after `LANG-39` and `LANG-40` this type checks, and means nothing:

```zel
class Eq a where
  eq : a -> a -> Bool

  derived eq
    matched = True
    differed _ _ = False
    combine x y =
      y

type Colour
  = Red
  | Green

instance Eq Colour where
  derived

same : Bool
same =
  eq Red Green
```

The derivation is carried and never looked at, so one that names no member, or sits on a member
whose signature cannot be walked, or leaves out `combine`, is accepted. The instance exists as
far as the solver is concerned, so `eq Red Green` is discharged — against an instance with no
`eq` in it. The only thing standing between that program and a build is the emitter's refusal of
any module that holds a class.

**Approach:**

1. **A derivation is checked where it is written**, during canonicalization, each failure an
   error at the class:

   - it names a member of the class it sits in, and a member has at most one;
   - the member's signature is `a -> a -> R` or `a -> R`, `a` being the class variable and `R` a
     type that does not mention it — any other signature cannot carry a derivation;
   - its bindings are exactly `matched`, `differed` and `combine` for the first shape, exactly
     `atConstructor` and `combine` for the second; a missing one, an extra one and a repeated
     one are each named;
   - a class whose members are partly covered is an error naming the members left out.

   A class is **derivable** when every member carries a derivation. Record that on the class,
   and in its `Interface` entry.

2. **Its bindings are type checked**, with `LANG-40`'s machinery, each as a declaration
   annotated with the type the chapter gives it: `matched : R`,
   `differed : Position -> Position -> R`, `combine : R -> R -> R`,
   `atConstructor : Position -> R`. No context is given to them — none of them mentions the
   class variable — so an obligation in one is discharged by an instance or is an error.
   `Comparable`'s `differed i j = compare i j` is the case: it needs `Comparable Position`.

   `Position` is the one type the compiler has to know by name, to give those parameters a type
   before any class has been read. It is `Basics.Position`, known by its qualified name the way
   `scalars.rs` knows `Basics.Int`. [LANG-42](lang-42.md) declares it in `std/core`; until then
   it exists only where a test or the spec harness's stand-in declares it, so add it to
   `basics_interface()`, opaque.

3. **A `derived` instance is checked where it is written**, during canonicalization:

   - its class is derivable, or the error names the class;
   - its head has a shape to read. A declared union whose constructors are in scope — its own
     module's, or one imported with them — is walked constructor by constructor. A tuple is
     walked as one shape. `()` is walked by a two-value derivation and is an error for a class
     with a one-value member, which has no element to begin at. A scalar type (`scalars.rs`) and
     a type imported without its constructors are each an error;
   - every argument of every variant, or every element of a tuple, has what it needs, and that
     is where **the instance's context is inferred**. An argument whose type is one of the
     head's variables puts a constraint on that variable. One whose type is concrete needs an
     instance in scope, and the error for a missing one names the variant and the type; a
     function type is that error with no possible fix. One whose type is itself an application
     — `Maybe a`, `List (Maybe a)` — needs the instance for its head and whatever that
     instance's context asks of its arguments, down to the variables.

   A recursive type asks for its own instance, and a group of types deriving in one module may
   ask for each other's. The instance being derived counts as in scope, with the context being
   inferred: compute the contexts of a module's derived instances together, as a fixed point.
   `type List a = Nil | Cons a (List a)` deriving `Eq` infers `Eq a` and nothing more.

   The inferred context is recorded on the canonical instance and published in the `Interface`,
   which is why this step is canonicalization's: an interface is built from the canonical
   module and reads nothing the typer produces
   ([`DEC-23` decision 1](../decisions/dec-23.md#1--a-module-publishes-the-interface-it-has-whichever-phase-failed)).

4. **A `derived` instance is given its members.** For each member of the class, produce the
   definition the walk describes and put it where a written instance's binding would be, as
   canonical code. From there it is an ordinary instance binding: `LANG-40` checks it against
   the member's signature with the inferred context as given, and nothing downstream has a
   special case for an instance that was derived. An error in a generated definition is blamed
   on the word `derived`.

   **Its superclasses are discharged against that context too.** `LANG-40` skips the superclass
   obligations of a `derived` instance, because its context is none recorded until step 3 infers
   one and checking against none would reject instances the derivation makes sound. Once the
   context is inferred, a `derived` instance's superclass obligations are discharged at its head
   with the inferred context given, as a written instance's are: `derived Comparable (Phantom a)`
   for `type Phantom a = Phantom Int`, beside `instance Eq a => Eq (Phantom a)`, is an error
   naming `Eq a`, because the inferred context is none.

   For a union and a two-value member `m`: match the first value's constructor; when the second
   is the same constructor, fold over the arguments, left to right; when it is not, `differed`
   at the two constructors' positions. For a one-value member: `atConstructor` at the
   constructor's position, folded with the arguments' answers. A tuple has only the fold.

   The fold is right-nested: `combine a1 (combine a2 (… matched))` over two values, and
   `combine p (combine a1 (… an))` over one, `p` being `atConstructor`'s answer and each `a` the
   member applied to one argument, or one pair, at that argument's type. **`combine` is not
   called.**
   Its body is placed in the definition with its first parameter bound, once, to the value of
   the answer, and its second replaced by the rest of the fold, so that the rest is evaluated
   only where the body reaches it. A `case` with one branch binding its scrutinee does the
   first half with what the canonical AST has today, and no `let`. `matched`, `differed` and
   `atConstructor` are placed the same way, their parameters bound to the positions.

   Three things to get right:

   - **Capture.** The rest of the fold mentions the names the definition gave the arguments. A
     class author's `combine` may bind any name a source file can spell. Give the generated
     names spellings no source file can write and the emitter can still turn into an
     identifier.
   - **`Position` values.** The definition builds them, and `Position`'s constructor is exposed
     to no module. The compiler names it by its qualified name, not through scope.
   - **A class from another module.** A derived instance is usually written beside the type, in
     a different module from the class, so the class's `Interface` entry has to carry the
     derivation's bindings and not only the fact that there are some. They are canonical
     already, so every name in them is qualified and means the same thing wherever it is
     placed. That a placed body may then refer to a value its home module does not expose is
     [GEN-24](gen-24.md)'s to make work.

**Acceptance:** each error asserted by variant and by `diagnostic.labels[..].range`, each test
seen red with what it pins neutralised.

In `crates/zelkova-compiler/tests/canonical.rs`:

- Each class-side error of step 1: a derivation for a name that is not a member; on
  `add : a -> a -> a`; on `bottom : a`; missing `combine`; an extra binding; a class with two
  members and one derivation, naming the other.
- `derived` under a class with no derivation is an error naming the class. `derived` for
  `Int`, and for a type imported opaquely, are each an error. A one-value class deriving `()`
  is an error.
- The inferred context: `Box a` infers one constraint; a parameter no variant uses infers
  none; `Entry Key` with no instance for `Key` is an error naming the variant and the type; a
  variant holding a function is that error; a recursive `List a` infers `Eq a`; two mutually
  recursive types in one module each infer what the other needs.
- A derived instance in another module than its class has the context in its module's
  `Interface`.

In `crates/zelkova-compiler/tests/typer.rs`:

- A `combine` of the wrong type is a type error at the class. `differed i j = compare i j`
  checks only with a `Comparable Position` instance in scope.
- A module with a derived `Eq` and a derived `Comparable` on a union with arguments, on a
  recursive type and on a tuple type checks, and a use at a type whose argument has no instance
  is an error.
- A derived instance's superclasses are held to its inferred context. The test to turn round is
  `a_derived_instance_is_not_held_to_its_superclass_context_until_lang_83`, which pins the skip
  `LANG-40` has: `derived Comparable (Box a)` beside `instance Eq a => Eq (Box a)` checks
  there, and the `Phantom` case above is the one that is an error once the context is inferred.

In `crates/zelkova-compiler/tests/ir.rs`, on the generated bodies themselves:

- For `Comparable`'s `combine` — `case x of EQ -> y; _ -> x`, which mentions `x` twice — the
  definition applies `compare` to each pair of arguments **once**. This is the test
  [`DEC-24` decision 8](../decisions/dec-24.md#8--combines-first-parameter-is-a-value-and-its-second-is-the-rest-of-the-walk)
  exists for; substituting `x` passes every other test here and is exponential on a list.
- The rest of the fold sits inside the branch of `combine`'s body that reaches `y`, and not
  ahead of it.
- A `combine` that binds a name the generated definition also uses does not capture it.

`cargo test --workspace` is green. `cargo run -- compile std/core` still prints
`parsed 10 modules`, lists all ten as checked and exits 0. Nothing here runs under `node`: the
emitter refuses a module holding a class until [GEN-24](gen-24.md), whose fixture is where a
derived instance is first run.

**The chapter's blocks.** Run `cargo test --test spec -- --nocapture` and read what each block
under the seven sections above fails on.

- A block that declares everything it uses and now checks goes red and is retagged `expect=ok`.
- **The `Entry Key` block under *What a derived instance requires* will not go red**: it is
  `expect=unimplemented`, and it now fails for the reason the chapter gives. Give it the class
  it names and retag it `expect=canonical-error:<the variant>`.
- A block showing `std/core`'s own declaration that cannot check outside `Basics` — the
  `Comparable` derivation, whose `differed` needs `Comparable Position` — becomes
  `expect=fragment`. [LANG-42](lang-42.md) puts the real one under test.
- Every other block is made self-contained and tagged for what its prose claims.
