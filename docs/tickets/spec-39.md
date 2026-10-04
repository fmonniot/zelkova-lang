# SPEC-39 · The chapter does not say what a context written on a `derived` instance means, or which constructors a `derived` instance needs in scope

**Sizing:** small. Two questions for the language owner, each answered in a sentence of
`docs/spec/type-classes.md`. It becomes larger if the answer to the first is not "an error", which
is code in `canonical/classes.rs`.

**Location:** `docs/spec/type-classes.md` — [*What a derived instance
requires*](../spec/type-classes.md#what-a-derived-instance-requires), which says the requirement is
"inferred rather than written" and says a derived instance for a type imported "without its
constructors" is an error; its *Open questions*. `crates/zelkova-compiler/src/canonical/classes.rs`
— `do_instances`, which raises `Error::DerivedInstanceWritesContext`;
`crates/zelkova-compiler/src/canonical/derivation.rs` — `plan`, which reads a type's constructors
off the declaring module's interface.

**Found while:** reviewing the PR for [`LANG-83`](README.md), which implemented both. Neither
choice is the implementer's.

**Problem:**

1. **A written context.** `instance Eq a => Eq (Box a) where derived` parses, since [`LANG-38`](README.md)
   made it so, and the chapter says what a derived instance's context *is* (inferred) and nothing
   about one that is written. The compiler rejects any written context on a `derived` instance, the
   one choice that no later answer can break. Other readings compile different programs:
   - a written context is kept beside the inferred one and the two are merged, so
     `instance Hash a => Eq (Box a) where derived` demands an unrelated `Hash a` of every use of
     `eq` at a `Box`;
   - a written context has to equal the inferred one;
   - a written context is an upper bound the inferred one has to fit in.
2. **Constructors in scope.** The chapter says a derived instance for a type imported "without its
   constructors" is an error. That reads as the import list (`import Colour exposing (Colour)`),
   and `modules.md` says a bare `import Colour` still makes `Colour.Red` reachable qualified when
   the declaring module exposes `Colour(..)`. The compiler reads it as the declaring module's
   interface: the derivation is accepted whenever the interface carries the union's variants, and
   rejected only for an opaque export. The two readings differ on `import Colour exposing (Colour)`
   where `Colour` exposes `Colour(..)`, which is accepted today.

**Approach:** the ticket does not pick. For the first, the readings above; for the second, the
interface reading (today's behaviour) or the import-list one, which needs the instance's module to
say what it imports.

**Acceptance:** the chapter states the answer to each in *What a derived instance requires*; the
entry for each leaves *Open questions*; the block tagged
`expect=canonical-error:DerivedInstanceWritesContext` is retagged to match the answer or deleted;
`cargo test --test spec` is green. If the answer to the first is not "an error", a test in
`crates/zelkova-compiler/tests/canonical.rs` pins the chosen reading, seen red against
the rejection; if the answer to the second is the import-list reading, one pins that
`import Colour exposing (Colour)` is rejected.
