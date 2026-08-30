# Zelkova — Language specification

This is the specification of Zelkova the language, as distinct from `docs/tickets/`
(the compiler's own work log) and `cargo doc` (the compiler's Rust API). One markdown
file per chapter, sibling files in this directory.

**The spec is normative.** It describes Zelkova as designed, including constructs the
compiler does not implement yet — it is not a description of today's binary. Where a
chapter documents something unimplemented, it says so and tags its examples
accordingly (see the vocabulary below). `CLAUDE.md`'s *Language notes* section has the
compiler's current implemented/not-implemented split; this directory is where that
split gets explained rather than just listed.

**The spec is self-contained.** Zelkova began as a fork of Elm's surface syntax and owes
it most of its good ideas, but Elm is an *inspiration*, not a reference: no chapter here
may resolve a question by pointing at Elm's documentation. Where a rule is inherited it
is written out in full; where the two languages differ, this directory is the answer.

That rule is not fastidiousness. Deferring works only for as long as the two languages
agree, so it fails precisely when a divergence is wanted — at which point the spec has to
answer a question in vocabulary that lives in someone else's document, describing someone
else's language, and changing without notice. It also frames every deliberate divergence
as a defect. Writing the rules down here is what makes "we do this differently, and here
is why" a sentence this directory can hold (`SPEC-2`).

## Every example is checked

Every fenced ```` ```zel ```` block in a chapter carries an `expect=` tag in its info
string, and `cargo test --test spec` runs every one of them through the compiler. A
chapter claim the compiler disagrees with is a red test, not a stale sentence nobody
notices — which is the failure mode this directory exists to prevent: documentation
nothing checks drifts from the code it describes, silently and indefinitely.

A `zel` block with no `expect=` tag, or with a tag the harness does not recognise, is
a hard test failure — never a silent skip. When you add an example, tag it.

### The `expect=` vocabulary

| Tag | Meaning |
|---|---|
| `expect=ok` | Parses and canonicalizes with no errors. |
| `expect=parse-error` | Fails somewhere in the parser (tokenizer, layout or grammar). Which specific error is not pinned. Use when the chapter claims only that the source is rejected. |
| `expect=parse-error:Reason` | The same, and the reason must match. `Reason` is either the phase (`Tokenizer`, `Layout`) or a specific error (`IndentationError`, `TabError`, `LayoutError`, `UnexpectedToken`, `UnexpectedEOF`, `InvalidToken`, `ExtraToken`) — matched against the real enums in `src/compiler/parser/`. Use whenever the chapter's prose describes the error the reader will see. |
| `expect=canonical-error:VariantName` | Parses, then canonicalization returns a `Vec<canonical::Error>` containing at least one error of variant `VariantName` — matched against the real variant names in `src/compiler/canonical/mod.rs`'s `Error` enum. |
| `expect=unimplemented` | Must fail somewhere in parse-or-canonicalize, but deliberately does not pin *which* error: pinning would wire tokenizer/grammar internals into a prose document, and the tag's whole job is to go red the day the feature is actually implemented. On an expected failure the test run prints the error it observed, so a reviewer can eyeball that the block failed for the reason the chapter intends. |
| `expect=dependency-error` | The block's *package* (see below) has no valid module order — its imports form a cycle — so nothing in it is canonicalized at all. The one expectation that belongs to a group rather than to a module: every block of the package carries it, or none does. |
| `expect=fragment` | An illustrative fragment, deliberately not executed. The only opt-out, and it must be written explicitly — there is no implicit skip. |

A fenced block whose info string's first token is not `zel` (` ```sh `, a bare
` ``` `, prose) is not touched by the harness at all.

### More than one module: `package=`

A block holds a single module, and by default it is compiled alone, against no interfaces
at all. A block may also carry a second tag, `package=<label>`, beside its `expect=` — an
info string reading ```` ```zel expect=ok package=alias ````.

Blocks sharing one label, **within one chapter**, are one package. They are parsed
together, ordered by their imports, and canonicalized in that order against each other's
`Interface`s — which is how a chapter shows two modules at once. Each block keeps its
own `expect=`, so an example can show one module compiling and its importer failing, and
the failure is reported on the importer's line rather than on the group.

`SPEC-3` settled this, for the *Modules, exposing and imports* chapter, which cannot be
written one module at a time. Three alternatives were considered and rejected: adjacent
blocks sharing one expectation (a group can then only say "something failed", not which
module), a hand-built `Interface` in `tests/spec.rs` (the other module never appears in
the chapter, so the reader cannot see it), and writing the group to a temp directory for
`compile_package` (slow, touches disk, and prints status lines on every spec run).

Four things a group cannot do, each a hard failure rather than a skip, because none is
expressible once the group is compiled as a unit: hold a `parse-error` expectation (the
group is parsed as a whole before any of it is compiled), hold an `expect=fragment`,
contain a block that fails to parse, or declare one module name twice. A rejected-source
example belongs in a block with no `package=` label.

## Chapters

| Chapter | Status |
|---|---|
| [Lexical structure](lexical-structure.md) | written |
| [Layout (the offside rule)](layout.md) | written |
| [Modules, exposing and imports](modules.md) | written |
| Declarations | planned ([SPEC-4](../tickets/spec-4.md)) |
| [Types and type annotations](types.md) | written |
| Expressions | planned ([SPEC-6](../tickets/spec-6.md)) |
| Patterns | planned ([SPEC-7](../tickets/spec-7.md)) |
| Name resolution and scoping | planned ([SPEC-8](../tickets/spec-8.md)) |
| Evaluation semantics | planned ([SPEC-9](../tickets/spec-9.md)) |
| [JS interop](js-interop.md) | written |
| Packages and source layout | planned ([SPEC-10](../tickets/spec-10.md)) |
| [Type classes](type-classes.md) | written |

Together the planned chapters are meant to be the whole language, not a set of footnotes to
somebody else's manual — that is what *the spec is self-contained* costs, and it is the
point. They are listed in a reading order rather than a writing order; each already has its own
`SPEC-n` ticket (linked above), filed up front rather than one at a time as it is picked up —
`write-spec-chapter` refuses to file the ticket it would later close in the same run, so one
has to exist before that skill will touch a chapter. The `write-spec-chapter` skill
(`.claude/skills/`) carries the method — probe the compiler rather than reasoning about it,
settle the design questions with the owner before drafting, file what turns up instead of
fixing it. What each chapter has to cover:

- **Modules, exposing and imports** — the header, the four forms an `exposing` entry can
  take, `import … as … exposing`, how a module name maps to a file path, and whether any
  module is implicitly in scope. This is the chapter that answered the multi-module
  question below, since it cannot be written with one module at a time.
- **Declarations** — value and function declarations, `infix` declarations, and **multi-line
  function declarations with pattern matching**, which is a deliberate divergence: Elm has no
  equivalent, so there is no inherited rule even to restate. The two type-shaped declarations
  — the annotation `name : Type` and the `type` declaration — are in *Types and type
  annotations* instead; `SPEC-5` settled that split, on the grounds that both are made of
  type expressions and neither reads well away from them.
- **Types and type annotations** — type expressions, the function arrow, type variables,
  tuple types, and the fixed arity that makes a four-element tuple a *syntax* error rather
  than a type error; plus the annotation and `type` declarations, per the note above.
- **Expressions** — application, `if`/`then`/`else`, `case … of`, `let … in`, lambdas, and
  the operator table: precedence, associativity, and the fact that no operator's meaning is
  built in.
- **Patterns** — every pattern form, and where each may appear.
- **Name resolution and scoping** — qualified names, what shadows what, and what makes a
  reference ambiguous rather than merely unresolved.
- **Evaluation semantics** — order of evaluation, what `==` means structurally, which
  operators short-circuit, and what a function value is.
- **Packages and source layout** — the package directory, the `zelkova.json` manifest, and
  what a package boundary means for visibility.
- **Type classes** — the `class` and `instance` declarations, how a constraint is written in an
  annotation, superclasses, where an instance may be declared, and what a constrained function
  may *not* be: a `module javascript` facade. It also carries the rule that `number`,
  `comparable` and `appendable` are ordinary type variables and always were — which was a
  chapter of its own (`SPEC-11`) until `SPEC-12` found that two chapters on one subject means
  the unmaintained one is what someone eventually reads.

### Tag every claim the chapter makes

A chapter that describes a known-bad diagnostic is making **two** claims, and they have
different lifetimes. The rule — *this source is rejected* — is permanent. The sentence
describing today's error is temporary, and it becomes false the moment someone improves
the diagnostic.

The Layout chapter has two of these: a `case` branch indented deeper than its siblings
([ERR-11](../tickets/err-11.md)) and leading indentation before `module`
([ERR-12](../tickets/err-12.md)). Rejection is the intended behaviour in both cases *and*
what happens today, so a bare `expect=parse-error` stays green straight through those
fixes — and the prose explaining that the grammar "trips on the second `->`" would quietly
become a lie.

So pin the reason: those blocks are tagged `expect=parse-error:UnexpectedToken`, naming
the wrong-but-current error deliberately. When ERR-11 lands and the error becomes a proper
`LayoutError`, that block goes **red**, and whoever fixed the diagnostic has to update the
paragraph describing it in the same change.

Two consequences, both deliberate. **First**, this reads as being in tension with *A spec
change and a semantics change do not share a diff*, immediately below — pinning the reason
guarantees that whoever fixes ERR-11 edits `docs/spec/layout.md` in the same PR to get
green. That is the wanted outcome and not the shape that rule is aimed at. What it forces
is a small, prose-only edit, written by the one person who has just read the code the
paragraph describes; what the rule forbids is deciding what the language *is* inside the
diff that changes what the compiler *does*. A red block is the mechanism that makes the
first happen; the rule is what stops it becoming the second.

**Second**, the guarantee is narrower than "the stale sentence cannot survive". The pin is
on the error *variant* only, so an ERR-11 fix that produced a different but still
`UnexpectedToken` error would leave the block green with the stale paragraph intact.
Pinning the token as well is possible and is deliberately not done: wiring that much
grammar detail into a prose document costs more than the residual risk is worth.

The general form: **tag every claim you make, at the granularity you make it.** Claim only
rejection, and use the bare tag. Describe the diagnostic, and pin it. There is no manual
verification step here on purpose — a convention that depends on someone remembering to
check something by hand is one that will be skipped, and a spec whose examples are checked
by ritual is not checked at all.

A block tagged this way still looks, at a glance, like an ordinary green example —
`expect=ok` and `expect=parse-error:UnexpectedToken` are exactly the tags a correct example
would carry too, and the only thing marking the block as current-but-wrong is prose a
skimming reader can miss. So the sentence that says so opens with a fixed, bolded lead-in: **Known gap:**. That makes
it something a reader — or `grep -r "Known gap:" docs/spec/` — can find without reading
every paragraph, and it is what tells a future session not to treat the block's shown
behaviour as what the language should do. `expect=fragment` doesn't need it: the tag itself
already says the block isn't normative.

A `expect=unimplemented` block is the same kind of risk from the other direction: the tag
says the *example* doesn't compile yet, but prose right next to it can still describe design
intent — a rule the language will have once the feature exists — in a way that reads as
settled fact. That prose gets its own lead-in, **Not implemented:**, for the same reason:
`grep -r "Not implemented:" docs/spec/` finds every place a chapter is describing a feature
ahead of the compiler rather than behind it. The `let … in` section of `layout.md` is the
first example of both lead-ins living in one section.

### A spec change and a semantics change do not share a diff

Writing a chapter surfaces compiler behaviour nobody intended — that is much of the value.
When it does, file a ticket and specify the behaviour the language *should* have, tagging
the example for what the compiler does today. ERR-11 and ERR-12 were both found this way.
Fixing the compiler in the same diff that documents it makes the change unreviewable, and
it is unnecessary: a spec claim the compiler fails is a red test, which is a working
record rather than a lost one.
