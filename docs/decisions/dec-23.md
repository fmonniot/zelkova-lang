# DEC-23 · A module with errors still has a shape: seven decisions

**Settled:** 2026-10-01, by the language owner, in the session that settled the decisions
[`TOOL-8`](../tickets/README.md) had been filed with open.
**Status:** live.
**Where the rule lives:** the code sites each decision below names; `TOOL-8` through `TOOL-12`
implemented them, and the editor-support program they belonged to has been dropped from
[the ticket index](../tickets/README.md). None of them is a rule about the *language*, so no chapter changes;
each decision below names the code site it lands at, because the tickets carrying them are
deleted as they close.

Before these, a module either passed every phase or contributed nothing. A module that failed
was absent from the environment its importers were checked against, so each of them reported
`cannot find a module named …` about a module that exists, and the module itself had no typed
tree for an editor to read. On the command line that is one false error per importer. In an
editor it is the steady state, since a file being typed in has an error in it most of the time.

The seven decisions are one design in two layers. Decisions 1 to 5 are the *floor*: what
survives of a module when a declaration of it has no usable form at all. Decision 6 is a layer
on top of the floor, for the one case where the declaration does have a form and a single name
in it does not resolve. Decision 7 is what was left alone.

## 1 — A module publishes the interface it has, whichever phase failed

An `Interface` is built from the canonical module and reads nothing the typer produces, because
an exposed value must carry an annotation. So a module that fails type checking has exactly the
interface it would have had, and withholding it protects nobody. The same holds one phase
earlier for every declaration that did canonicalize, and one phase earlier again for every
declaration that did parse.

The alternative was to keep the rule and make the importer's error true: say "`A` did not
compile" where it said "`A` was not found". That fixes the wording and leaves the importer
unchecked, which is the half an editor needs.

Lands at `dependencies::ModuleWalker::check_in_order`, which inserts an interface for every
module its checker hands back, with or without errors.

## 2 — A broken declaration is left out and recorded, and an intact annotation still speaks for it

A declaration that did not parse, or that canonicalization rejected, has no canonical form. It
is left out of `canonical::Module::values` and recorded in a list beside it, with its annotation
when the annotation itself is sound.

**The annotation is enough for every caller.** The typer's environment holds a declared type
per name and is built from annotations alone, so a caller of `f : Int -> Int` is checked the
same whether `f`'s body is fine, is mid-edit, or names something misspelt. That is the
commonest state a file is in while it is being typed, and it costs the typer nothing.

**A caller of a broken declaration with no annotation is left unchecked, silently.** That is
what the typer already does to a caller of any unannotated top-level value, broken or healthy:
`Solved::UnboundName`, no error, no IR. So the floor adds no mechanism to the typer.

The alternative considered for the declaration as a whole was a *hole*: a canonical value with
a placeholder body, which the typer gives a fresh type variable. It was not chosen for a whole
declaration because it gives a broken unannotated value more than a healthy unannotated one
gets. Decision 6 takes the idea up where it does pay.

This also settles [`BUG-34`](../tickets/README.md). That ticket is the same all-or-nothing
shape one level down: a sub-pass of `canonicalize` that fails substitutes an empty map for
everything it did resolve. Handing back the partial map is the prerequisite for keeping
anything, so it is done here and not as a fix of its own.

Lands at `canonical::Module`, at `canonical::canonicalize_recovering`, and at
`typer::type_check_recovering`, which reads the recorded annotations into its environment.

## 3 — An error that restates a reported failure is dropped, by a flag on the scope

Once a declaration is missing, other errors follow that say nothing new: a reference to it is
a missing name, an `exposing` entry naming it is a missing export, an importer's entry naming
it is a missing import. Each restates an error the user has already been shown.

The rule is coarse on purpose. A scope is **incomplete** when a name could be missing from it
for a reason already reported: an import that did not resolve, a `type` or `infix` declaration
that failed, a chunk that did not parse and could not be named, or an import of a module whose
own interface is incomplete. In an incomplete scope a *not-found* error is dropped, and the
declaration it was raised in is left out as decision 2 says. An `Interface` carries the same
flag, and an import entry that is not found in an incomplete interface is dropped the same way.

Two alternatives were set aside:

- **Suppress per name.** Record each missing name and drop only the errors about it. The names
  are known after a canonicalization failure and are not known after a syntax error in a `type`
  or an `import`, so this needs the coarse flag as its fallback anyway, and is then two
  mechanisms where one answers.
- **Suppress nothing.** Every restating error is a true statement of the form "`A` does not
  expose `T`". True, and about neither file the user is looking at.

The cost is a deferral and never a loss: a real typo in a scope that is incomplete is reported
once the failure that made it incomplete is fixed. The build fails either way.

**No error is dropped without another one standing.** A scope becomes incomplete only through
a failure that was itself reported, or through a module of the same package whose interface is
incomplete, which bottoms out in one. A package with an error publishes nothing (decision 7),
so the flag never crosses a package boundary.

Lands at `canonical::canonicalize_recovering` and `canonical::environment::new_environment`,
with the flag on `Interface` and on `canonical::Module`.

## 4 — One name is read off a failed chunk: a value's

`TOOL-4` cuts a module into one chunk per top-level declaration and hands back the chunks that
failed with a span and an error. Decision 3 needs to know what a failed chunk would have
declared, and the tokens say so reliably in exactly one case: a chunk whose first token is a
lowercase identifier is the annotation or a binding of the value of that name, because those
are the only two declaration forms that open on one.

That case is also the one that matters. A body being typed is a failed chunk opening on its own
name, so with the name registered the scope stays complete and every other diagnostic in the
file stays live. Without it, the commonest editing state would defer every not-found error in
the file.

Every other failed chunk — a `type`, an `infix`, an `import`, an `unsafe` signature — is
unnamed, and makes the scope incomplete. Reading a type's name, arity and constructors off its
tokens was considered and set aside: past the first token the recovery is a guess, and a wrong
guess reports a false arity mismatch where the flag reports nothing.

Lands at `parser::Failure` and `parser::Module`.

## 5 — A module with errors has an IR, and nothing with an error behind it can be emitted

Hover and go-to-definition read the `ir::Module`, so a module with errors gets one: the typer
runs over the declarations that survived, and `check_package` hands the result back.

Two things keep that from ever reaching a backend, and the second does not depend on the first:

- **The driver is not handed it.** `PackageCheck` keeps its three lists of modules that
  checked, with their meaning unchanged, and gains a fourth for the modules of a package that
  did not. The driver reads the first three.
- **The IR says so itself.** Every declaration with no typed form — rejected by the typer,
  broken, or skipped — is in `ir::Module::unchecked`, and `zelkova_js::emit` already refuses a
  module that lists one. `ir::Unchecked` records whether an error stands behind the entry, so
  that the warning [`ERR-8`](../tickets/README.md) will give a declaration the typer merely
  could not reach is not also given to one the user has already been told about.

[DEC-18 decision 1](dec-18.md#1--the-backend-reads-a-typed-ir-and-the-typer-is-what-produces-it)
requires that no declaration be merely absent from what the typer answers with, and this keeps
to it: a rejected declaration has an entry that says it was rejected.

Lands at `PackageCheck`, `ir::Solved`, `ir::Unchecked` and `ir::build`.

## 6 — An unresolved name inside a sound body is a typed hole

The floor drops a whole declaration for one name that does not resolve. That is the declaration
being typed: `total = add subtotal ta`, with `ta` not yet a name, has no typed tree under the
floor, so it has no hover and none of its other type errors are shown.

So an unresolved value or constructor inside a body that otherwise canonicalizes is reported
(or dropped, under decision 3), replaced by a **hole**, and the declaration is kept. The typer
gives a hole a fresh type variable and constrains nothing by it, so it takes whatever type its
surroundings require and the rest of the body is checked. Unification is untouched. The type a
hole is solved to is the type expected at that position, which is what type-directed completion
would read.

It is a layer and not a replacement. A body that did not parse has no tree to put a hole in, an
unresolved operator leaves an infix chain with no precedence to associate by, and an unresolved
type in an annotation has no expression to replace. Each of those stays with decisions 2 and 3,
which is why the floor is built first and whole.

A hole is in the IR, so decision 5's second guarantee needs one more arm: the JavaScript
backend refuses a hole by name, as it refuses a `let`.

Lands at `canonical::ExpressionKind`, `canonical::PatternKind`, `ir::TermKind` and
`zelkova_js::Construct`.

## 7 — The package boundary and the test root stay all-or-nothing

Two places keep the old shape, deliberately:

- **A package with an error publishes nothing**, and a package depending on it is not checked:
  it reports `DependencyNotCompiled` once. That error is true and names the right package, so
  it is not the false error decision 1 removes. It is also what lets decision 3 say an
  incomplete interface never leaves its package.
- **A package's `tests/` are not checked when its `src/` has an error.** Decision 1 makes
  checking them against the interfaces `src/` did publish sound, and an editor open on a test
  file would want it. It was left out because it changes which errors a failing `zelkova test`
  prints, which is a question about the command line and not about the editor.

Neither is ticketed. Either is a small change on top of `TOOL-8` once something asks for it.
