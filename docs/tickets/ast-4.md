# AST-4 · A canonical type carries an unqualified name, so two types of one name are one type

**Sizing:** medium. Mechanical in shape and wide in reach — the variant, the environment entry
it is built from, every site in `canonical/` that constructs one, and the typer's translation
move together, and `tests/compiler/canonical.rs` and `tests/typer.rs` assert on the old shape.

**Location:** `src/compiler/canonical/mod.rs` — the `Type(Name, Vec<Type>)` variant of `enum
Type` at :302, and `Type::from_parser_type`, which is where a name is resolved and the
resolution discarded; `src/compiler/canonical/environment.rs` — `TypeArity` at :149, which is
what `find_type` hands back and which carries no module either;
`src/compiler/typer/mod.rs` — `canonical_type_to_typer_type` at :625, the consumer that needs
the missing half.

**Blocks:** [BUG-26](bug-26.md), and through it [LANG-53](lang-53.md) and
[LANG-54](lang-54.md).

**Problem:** canonicalization resolves a type name and then throws away what it resolved to.

`find_type` returns a `TypeArity { name, variables }` — the name as written and its parameters,
and nothing about where the declaration was. `Type::from_parser_type` builds a
`Type::Type(name, args)` out of that, so after canonicalization `Widget.Size` and `Gadget.Size`
are the same value, and so are `Basics`' `Int` and a module's own. That contradicts [every
declaration introducing a genuinely new type](../spec/types.md#type-declarations), and it is
`CLAUDE.md`'s rule that everything after parsing reaches for `QualName`.

It is also the one thing standing between the compiler and [`DEC-15` decision
1](../decisions/dec-15.md#1--a-scalar-type-is-known-by-its-qualified-name), which requires a
[scalar type](../spec/types.md#scalar-types) to be told apart from a module's own type of the
same name. [`BUG-26`](bug-26.md) named this shape as the cost of the fix it now takes; that
ticket is the defect and this is the prerequisite it waits on.

**Fix:** carry the declaring module from the point the name is resolved. `TypeArity` gains it —
`process_import` knows the interface an imported type came from, and the module's own
declarations know the module being canonicalized — and `Type::Type` holds a `QualName` built
from it.

An alias does not survive: `import Widget as W` followed by `W.Size` records `Widget.Size`,
because the qualified name names the declaration rather than the spelling that reached it. This
is the same rule [Name resolution](../spec/name-resolution.md) already states for values.

**Acceptance:** `canonical::Type::Type` holds a `QualName`. A canonicalized annotation naming a
type imported under an alias records the declaring module and not the alias, and two modules
declaring one type name produce two distinct canonical types — tests in
`tests/compiler/canonical.rs`, each checked by reverting the change and watching it go red.
`cargo run` still prints `parsed 8 modules`, lists all eight as checked, and exits 0.
`cargo test --workspace` and `cargo test --test spec` are green.
