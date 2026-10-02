# ERR-19 · A module name used as a constructor is reported as a missing constructor of the current module

**Sizing:** small.

**Location:** `crates/zelkova-compiler/src/canonical/mod.rs` — `Error::VariantNotFound`, the `TypeConstructor` arm of `Expression::from_parser` that raises it, and the `VariantNotFound` arms of `message()` and `labels()`; `crates/zelkova-compiler/src/canonical/environment.rs` — `suggest_name`, and the environment's record of which modules are imported (the same set [`ERR-14`](err-14.md) wants recorded on `RootEnvironment`).

**Problem:** `Maybe .withDefault` in a module with `import Maybe` is `Maybe` applied to the accessor `.withDefault` (since `LANG-50`, which added the accessor; before it, `LANG-52` rejected the spacing in the parser; both are closed, see [the index](README.md)). `Maybe` is a module and no constructor, so canonicalization raises `VariantNotFound`, which renders as ``cannot find a type constructor named `App.Maybe` `` with the caret under `Maybe`. The message names a constructor of the *current* module (`env.module_name().qualify_name(name)` is what the arm builds), which the user never wrote, and says nothing about the spacing or about `Maybe` being an imported module. The case the user meant, `Maybe.withDefault`, is one edit away. `suggest_name` is not helping: it only ever suggests over `env.type_constructor_names()`, so the module name is never a candidate.

The test that pins today's behaviour is `a_module_name_applied_to_an_accessor_is_no_constructor` in `crates/zelkova-compiler/tests/canonical.rs`, which asserts only the variant and the span.

This is not covered by [`ERR-14`](err-14.md), which is about `Widget.label` with the prefix not imported (a *qualified* name), and keeps `VariantNotFound` for a prefix that resolves; nor by [`ERR-15`](err-15.md), which is `TypeNotFound`.

**Approach:**

1. At the `VariantNotFound` raise site for a bare (unqualified) constructor name, check whether the name is the name of an imported module. The environment knows the live module prefixes (see [`ERR-14`](err-14.md), whose `RootEnvironment` change this wants to share, so the two tickets should agree on one accessor for it).
2. When it is, add a note or label suffix to the diagnostic, in the way `suggestion_suffix` already appends a suggestion: a module name is not a constructor, and ``did you mean `Maybe.withDefault`?`` when the application's argument is an accessor. Whether that second part is worth the plumbing, which needs the parent application to be visible from the constructor arm, or a plain "`Maybe` is a module" note suffices, is undecided and this ticket does not pick.
3. Decide whether the message should still say `App.Maybe`: `Error::VariantNotFound` carries a `QualName` qualified with the current module, and `message()` renders `name.to_name()`, so the user sees a name they did not write. That applies to every unresolved constructor, not only this case.

**Acceptance:** a `crates/zelkova-compiler/tests/canonical.rs` case, next to `a_module_name_applied_to_an_accessor_is_no_constructor`, that renders the diagnostic for the source in the Problem section and asserts it mentions that `Maybe` is an imported module; a second case with a constructor name that is no import asserts the note is absent, so it cannot be added unconditionally. Mutation-check both by deleting the new branch and confirming the first goes red.

**Found by:** the review of the PR for `LANG-50`, which left it unfixed there because the rendering is pre-existing `VariantNotFound` code and `LANG-52`'s ticket decided only the phase the rejection moves to.
