# TOOL-12 · One unresolved name costs a declaration its whole typed tree

**Sizing:** large. Nothing is left to decide: the design is
[`DEC-23`](../decisions/dec-23.md) decision 6. It is large because a new node kind crosses
every layer between canonicalization and the JavaScript backend. Each arm it adds is a few
lines, and no arm changes what inference does to any other node: unification is untouched.

**Part of:** the *Active work: editor support* section of [the index](README.md), last of the
five tickets `TOOL-8` through `TOOL-12`. The four before it are the floor, which this one
builds on and does not replace.

**Depends on:** [`TOOL-10`](tool-10.md), for `without_restated`. It does not need
[`TOOL-11`](tool-11.md).

**Location:** `crates/zelkova-compiler/src/canonical/mod.rs` — `ExpressionKind`,
`PatternKind`, `Expression::from_parser`, `Pattern::from_parser`, `do_values` and
`collect_top_level_refs`; `crates/zelkova-compiler/src/canonical/environment.rs` —
`ScopedEnvironment::expose_pattern`; `crates/zelkova-compiler/src/ir/mod.rs` — `TermKind`,
`TypedTermKind`, `TermPatternKind` and `TermPattern::collect_bindings`;
`crates/zelkova-compiler/src/ir/decision.rs` — the pattern match that builds a `Decision`;
`crates/zelkova-compiler/src/typer/mod.rs` — `canonical_expr_to_term`, `translate_pattern`,
`Substitution::apply_term` and `Substitution::apply_pattern`;
`crates/zelkova-compiler/src/typer/annotate.rs` — `annotate`;
`crates/zelkova-compiler/src/typer/constraint.rs` — `collect` and its pattern half;
`crates/zelkova-js/src/lib.rs` — `Construct` and the emitter's expression and `case` arms.

**Problem:** after [`TOOL-9`](README.md), a body that names something unresolved is a
`Broken`: the error is reported and the declaration has no canonical form.

```zel
module Cart exposing (total)

add : Int -> Int -> Int
add a b = a

total : Int
total = add 1 ta
```

`ta` is not a name yet, because it is still being typed. `total` is reported once, which is
right, and then has no typed tree: hovering `add` inside it shows nothing, a second mistake in
the same body is not reported until the first is fixed, and nothing knows that the position
`ta` sits in expects an `Int`. This is the declaration under the cursor, which is the one an
editor is asked about most.

**Approach:** in one PR. An unresolved value or constructor inside a body that otherwise
canonicalizes becomes a *hole*. The error is still raised; the declaration is kept.

1. **The canonical nodes.** Add `ExpressionKind::Hole` and `PatternKind::Hole(Vec<Pattern>)`.
   The pattern form keeps the arguments written after the unresolved constructor, so the names
   they bind are still in scope in the branch.

2. **Three lookups answer with a hole.** `Expression::from_parser` and `Pattern::from_parser`
   take one more parameter, `unresolved: &mut Vec<Error>`. At exactly these three sites the
   error that is returned today is pushed onto it, and the node built is a hole:
   - `Expression::from_parser`'s `Variable` arm, when `find_value` finds nothing;
   - its `TypeConstructor` arm, when `find_type_constructor` finds nothing;
   - `Pattern::from_parser`'s `Constructor` arm, when `find_type_constructor` finds nothing.
     Its arguments are canonicalized as before and become the hole's.

   Every other failure still returns `Err` and still breaks the declaration:
   `AmbiguousVariables`, an operator `resolve_infix_operator` cannot resolve, and anything
   `reassociate_infix_chain` rejects.

3. **`do_values` keeps the declaration.** A body that comes back `Ok` is a `Value` whether or
   not `unresolved` is empty. Whatever is in `unresolved` joins the sub-pass's errors, and so
   goes through `without_restated` like any other: reported in a complete scope, dropped in an
   incomplete one. A body that comes back `Err` is a `Broken` as before, and its `unresolved`
   errors are reported beside the one that broke it.

4. **Scope and dependencies.** `ScopedEnvironment::expose_pattern` exposes a pattern hole's
   arguments. `collect_top_level_refs` finds no reference in a hole.

5. **The term language.** Add `TermKind::Hole`, `TypedTermKind::Hole` and
   `TermPatternKind::Hole { args: Vec<SubPattern> }`. `canonical_expr_to_term` translates the
   expression form. `translate_pattern` translates the pattern form, giving each argument a
   fresh type variable and translating it with `translate_sub_pattern`, so an argument that
   function refuses still makes the declaration `Solved::Untranslatable`, as under a real
   constructor. `TermPattern::collect_bindings` reads a hole's arguments as it reads a
   constructor's.

6. **Inference.** `annotate` gives a hole a fresh type variable. `constraint::collect` emits
   nothing for it, and its pattern half answers a pattern hole with no type for the matched
   value and the constraints of its arguments alone. `Substitution::apply_term` and
   `apply_pattern` each gain the arm. Nothing in `unifier.rs` changes: a hole's type is an
   ordinary variable, solved by whatever constrains the node around it.

7. **The decision tree.** A pattern hole is handled as `TermPatternKind::Anything` is. No tree
   built from one is emitted, because of step 8.

8. **The backend refuses a hole by name.** Add `Construct::Hole`, described as "a name that
   did not resolve". The emitter answers a `TypedTermKind::Hole` with `self.unsupported`, as
   it answers a `Let`, and answers a `Case` whose branch holds a pattern hole at any depth the
   same way, before building its decision tree.

9. **Say what the IR now holds.** The `ir` module doc's *A type on every node* still holds for
   a hole, whose type is the one inference solved for its position and may be an unsolved
   variable. `Solved::Typed`'s doc comment and `ir::Module::declarations`' gain one sentence
   each: a declaration here may hold a hole, and a backend refuses it.

**Acceptance:**

Tests in `crates/zelkova-compiler/tests/canonical.rs`, on `canonicalize_recovering`. `NodeSpan`
compares equal to everything, so each asserts the hole's `.span` range against the source.

- `f : Int -> Int` with `f x = x`, beside `g : Int` with `g = f nope`: the errors are exactly
  one `VariableNotFound`, `broken` is empty, and `g`'s body is an `Apply` whose argument is a
  `Hole` spanning `nope`. Mutation-checked by returning the error at the `Variable` site, which
  makes `g` a `Broken`.
- `g : Int` with `g = Nope`: exactly one `VariantNotFound`, and the body is a `Hole`.
- `h : Int -> Int` with `h x = case x of Nope y -> y`: exactly one `VariantNotFound` and no
  error about `y`. Mutation-checked by not exposing the hole's arguments.
- The control: `g = 1 <+> 2` with no `<+>` in scope is still a `Broken`.
- With `import Nope` in the module, `g : Int` with `g = f Nope.y` reports only the
  `EnvironmentErrors`, and `g` is a `Value` holding a `Hole`.

Tests in `crates/zelkova/tests/pipeline.rs`, through `check_module_recovering`:

- The `f`/`g` module: `g` is in `ir.declarations`, and the `Hole` in its body has a `tpe` that
  displays as `Int`. Mutation-checked by making `annotate` answer a hole with
  `ErrorKind::UnboundVariable`, which turns `g` into an `Unchecked`.
- `f : Int -> Int -> Int` with `g = f nope 'c'`: both a `CompilationError::Canonical` holding
  `VariableNotFound` and a `CompilationError::Type` are returned, for the one declaration.
- The `h` module: `h` is in `ir.declarations`.

A test in `crates/zelkova-js/tests/javascript.rs`:

- `zelkova_js::emit` on the `f`/`g` module's `CheckedModule` returns an
  `Error::Unsupported` with `Construct::Hole`. Mutation-checked by emitting `undefined` for a
  hole.

And `cargo test --workspace` is green, `cargo run -- compile std/core` still prints
`parsed 10 modules`, lists all ten as checked and exits 0, and `cargo run -- test std/core`
still reports `98 tests: 98 passed, 0 failed, 0 errored`.
