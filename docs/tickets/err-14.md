# ERR-14 · A qualified name whose module is not imported is reported as a missing value

**Sizing:** small.

**Location:** `src/compiler/canonical/mod.rs` — the `Error::VariableNotFound` raised by the
`ExpressionKind::Variable` arm of `Expression::from_parser`, and the `Error::VariantNotFound`
raised beside it; `src/compiler/canonical/environment.rs` — `suggest_name`, which already
splits a target on its last `.` and so already knows which names are qualified.

**Problem:** `Widget.label` in a module that never imported `Widget` is reported as a value
named `Widget.label` that does not exist. That is true, and it is not the thing the reader has
to fix. The `import` is missing; the message never says the word, and the caret under the whole
dotted name gives no hint that the prefix is the half that failed.

The environment holds qualified names as flat keys — `Widget.label` is one entry, inserted by
`process_import` — which is why the lookup cannot tell "no such module" from "that module has
no such value". Both are one miss on one key.

The two cases want different sentences. A prefix that names no import should say so and name the
import that is missing; a prefix that does name an import, with a local part the module does not
expose, should say *that*, and is the case where a "did you mean …?" over the module's other
names is worth having.

**Fix:** the environment knows which prefixes are live — every `process_import` call registers
one — so recording that set on `RootEnvironment` is enough to split the two cases at the point
of failure. A new `canonical::Error` variant for the unimported-prefix case, naming the module
and carrying the span of the name; `VariableNotFound` and `VariantNotFound` keep the case where
the prefix resolves. `suggest_name`'s qualified branch already restricts candidates to the same
prefix, so it needs nothing for the second case and should not be reached in the first.

Worth checking at the same time whether the prefix set can suggest a module: a typo'd prefix
(`Widgt.label` where `Widget` is imported) is a near-miss over the live prefixes, and
`process_import` already suggests over `interfaces.keys()` for a bad `import` line
([`ERR-7`](err-7.md)'s pattern).

**Acceptance:** a `tests/compiler/canonical.rs` case asserting the new variant for
`Widget.label` with no `import Widget`, and one asserting that an imported module with a
missing member still reports `VariableNotFound` with its suggestion. The
`expect=canonical-error:VariableNotFound` block under *Unresolved names* in
[`docs/spec/name-resolution.md`](../spec/name-resolution.md) goes **red** when the new variant
is introduced, and its **Known gap:** paragraph is deleted in the same change — note that a fix
which only rewords the existing variant's message leaves that block **green**, so the paragraph
would have to be deleted by hand. Introducing the variant is what keeps the chapter honest.
