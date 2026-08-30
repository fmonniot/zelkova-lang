# SPEC-12 · Write the Type classes chapter, superseding Constrained type variables

**Sizing:** large. The mechanism is now designed (see **Decided**, below) but nothing of it
parses, so every example in the chapter is `expect=unimplemented` or `expect=fragment` until
the `CLASS-` program lands. The chapter also *removes* a sibling chapter, which means
rewriting every cross-reference into it.

**Location:** `docs/spec/type-classes.md` (new); `docs/spec/constrained-type-variables.md`
(deleted by this ticket); `docs/spec/README.md` — the chapter table and the per-chapter bullet
list; `docs/spec/types.md` — the *Type variables* section, which promises no variable is ever
applied; `docs/spec/js-interop.md` — the plain-parameter-list guarantee, which this chapter
constrains; `CLAUDE.md` — *Language notes*, whose closing paragraph currently says the
mechanism is undesigned.

**Depends on:** [TEST-2](test-2.md). Every claim a class mechanism makes is a type-level
claim — *this instance is not in scope*, *this constraint cannot be discharged*, *this
annotation promises more than its body gives*. The spec harness stops at canonicalization, so
a chapter written before `TEST-2` can tag none of them and would be prose nothing checks. The
`CLASS-` tickets below do **not** depend on `TEST-2`; only this chapter does.

**Supersedes:** `SPEC-11`'s [`constrained-type-variables.md`](../spec/constrained-type-variables.md).
That chapter answers a question — *are `number`, `comparable` and `appendable` special?* — that
this one answers as a paragraph of a larger story. Two chapters on one subject means the
unmaintained one is what someone eventually reads. Absorb these three things from it, which are
the parts that survive the mechanism landing:

- **The rule.** All three are ordinary type variables and always were; the compiler never
  special-cased them. Its `min Red Blue` example — a user union type flowing through
  `min : comparable -> comparable -> comparable` and checking clean — is the whole argument in
  one block and should be kept.
- **The spellings are not reserved.** A value may be named `number`; a `type` parameter may be
  spelled `comparable`; the annotation's `number` and the body's `number` live in different
  namespaces. Classes do not change this: `Comparable` is an ordinary uppercase name declared
  in a module, so the three lowercase spellings stay ordinary identifiers forever, not
  transitional ones.
- **Until then.** Passing the operation as an argument, and one function per type, are what a
  program does while the mechanism does not exist. Keep it until `CLASS-6` lands.

Do not carry over its *What is still open* list. This ticket's **Decided** section is what
replaced it.

## Decided (by the language owner, this session)

Eleven questions, settled together. Each is normative for the chapter and for every `CLASS-`
ticket below.

**1 — A constraint is written before the type, with `=>`.**

```zel
min : Comparable a => a -> a -> a
lookup : (Comparable k, Eq v) => k -> v -> Bool
```

`=>` becomes its own token. It is an ordinary `Operator` today and a legal user-defined infix
— probed: `infix left 5 (=>) = f` compiles on `main` — so this is a breaking change, with
nothing in the tree relying on it.

**2 — `class` and `instance` are hard keywords; `where` opens the body.** A class or instance
body is a block of members, one per line:

```zel
class Comparable a where
  compare : a -> a -> Order
  lt : a -> a -> Bool

instance Comparable Colour where
  compare a b =
    EQ
  lt a b =
    False
```

`class` and `instance` **cannot** be soft keywords the way `javascript` is, and the reason is
structural rather than stylistic — see `CLASS-2`. `where` is soft in every *value* position and
hard in the *type-variable* position only; that split is also `CLASS-2`'s, and it is narrower
than the owner was offered, because probing found the wider version ambiguous.

**3 — The orphan rule: an instance lives with its class or with its type.** An
`instance C T` declaration is legal in the module declaring `C`, and in the module declaring
`T`'s head, and nowhere else. This makes instance coherence a property of the source rather
than of what happened to be linked.

**4 — Classes may have superclasses, from the start.** `class Eq a => Comparable a where …`.
An `instance Comparable Colour` is then rejected unless `instance Eq Colour` is in scope. This
was chosen over flat classes deliberately: retrofitting superclasses changes both the surface
and the solver, and `Eq`/`Comparable` is the pair `std/core` needs on day one.

**5 — No higher-kinded variables.** Unchanged from `SPEC-11` and from
[Types](../spec/types.md#type-variables): a type variable stands for a complete type and is
never applied. A class is always over a complete type. `Functor` and `Monad` are out of reach,
and that is the price of not needing a kind system. The chapter must say so plainly rather than
leaving a reader to discover it by trying.

**6 — A `module javascript` facade signature may not carry a constraint.** This is the answer
to the sharpest question `SPEC-11` left open. [JS interop](../spec/js-interop.md) promises a
companion `.mjs` export a **plain parameter list**, and a dictionary passed as a hidden
argument is exactly the calling convention that file is promised it will never see. So the
constraint lives one level up, in an ordinary Zelkova function, and the facade underneath it is
monomorphic:

```zel
-- Js/Utils.zel — unconstrained, and now only callable at types the JS can handle
compareInt : Int -> Int -> Int

-- Basics.zel — the constraint lives here
instance Comparable Int where
  compare a b =
    orderOf (Js.Utils.compareInt a b)
```

**7 — Dictionaries are erased by monomorphisation, not passed.** Codegen specialises each
constrained function per instantiation; no dictionary is built or passed at runtime. There is
no ticket for this — code generation has not started — so it is recorded here and in
`js-interop.md` as a constraint the first codegen ticket inherits. Two consequences the chapter
should state: whole-package compilation is assumed (there is no separate compilation to
preserve), and polymorphic recursion over a constraint would not terminate, which decision 5
already rules out.

**8 — A numeric literal defaults to `Int`; nothing else defaults.** An otherwise-undetermined
`Number` constraint resolves to `Int`. Every other constraint the solver cannot discharge is an
error naming the class and the type. No `default` declaration form.

**9 — `std/core` grows four classes:** `Eq`, `Comparable` (with `Eq` as its superclass),
`Number`, `Appendable`. `Eq` is in the set because it is the one whose runtime genuinely
crashes today — `_Utils_eqHelp` calls `__Debug_crash(5)` on a function value.

**10 — `std/core` keeps `SPEC-11`'s rewrite to `a`.** The 25 signatures stay spelled `a` until
`CLASS-6` gives them real constraints. `a -> a -> a` is what those types *are*; a second pass
over the same lines is the cost of not shipping a signature that describes a restriction the
language cannot express.

**11 — `TEST-2` is not a prerequisite of the compiler work.** It gates this chapter and nothing
else. It adds `expect=type-error` and `expect=type-error:Variant`; it does **not** tighten
`expect=ok` to mean "and type checks".

## What the chapter has to cover

Beyond restating the eleven above in the language's own vocabulary:

- **A class declaration**, its members, and what a member's signature means — that `compare`'s
  type outside the class is `Comparable a => a -> a -> Order`, with the class's own variable
  bound by the class.
- **An instance declaration**, and the obligation a superclass puts on it.
- **What a constrained annotation promises a caller**, which is the same promise
  [Types](../spec/types.md#an-annotation-is-a-promise) already describes, narrowed: the body may
  use only the operations its own context provides. This is where `LANG-12`'s rigid variables
  become visible in the language rather than in the checker.
- **Where an instance may be declared**, with a rejected example, and a forward link to
  *Packages and source layout* ([SPEC-10](spec-10.md)) for what a package boundary adds.
- **What is not expressible**: no higher-kinded variables, and therefore what a reader coming
  from a language that has them should stop looking for.
- **The three inherited spellings**, absorbed from the superseded chapter per the list above.

**Approach:** follow `write-spec-chapter` in full. Steps 1–2 (read the conventions, then probe)
still apply even though the design is settled — the tags depend on what the compiler does, and
that moves as the `CLASS-` program lands. Step 4 (design questions) is largely discharged by
the **Decided** section: bring the owner anything it does not answer rather than re-opening what
it does.

Expect most blocks to be `expect=unimplemented` when the chapter is first written, and to be
retagged as `CLASS-1` … `CLASS-6` land. Each of those tickets names the blocks it turns red;
that is the mechanism keeping this chapter honest as the mechanism arrives underneath it.

**Acceptance:** `cargo test --test spec` green. `docs/spec/type-classes.md` exists with every
block tagged and each tag proven to fail. `docs/spec/constrained-type-variables.md` is deleted
and no file in the repository links to it — `grep -rn "constrained-type-variables" .` returns
nothing. `docs/spec/README.md`'s chapter table has one row for this chapter and none for the
superseded one, and its bullet list matches. `CLAUDE.md`'s *Language notes* closing paragraph
is rewritten: the mechanism is designed, and the sentence telling a future session not to
assume an answer to it is no longer true.
