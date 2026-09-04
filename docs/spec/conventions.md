# Conventions of this specification

This file is about the specification rather than about Zelkova. It is neither a
[chapter](README.md#chapters) nor an [appendix](README.md#appendices) — nothing in it says
what a program means — and it is not itself normative. What it holds is the set of rules a
chapter is held to, and the reasons those rules are what they are.

[The index](README.md) is what a reader of the language wants: what the spec is, the three
lead-ins that appear in chapter prose, and the chapters themselves.

## The `expect=` vocabulary

Every fenced ```` ```zel ```` block in a chapter carries an `expect=` tag in its info string,
and `cargo test --test spec` runs every one of them through the compiler. A `zel` block with
no tag, or with a tag the harness does not recognise, is a hard test failure — never a silent
skip. When you add an example, tag it.

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

## More than one module: `package=`

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

## Tag every claim the chapter makes

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
change and a semantics change do not share a diff*, below — pinning the reason
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
skimming reader can miss. So the sentence that says so opens with a fixed, bolded lead-in:
**Known gap:**. That makes it something a reader — or `grep -r "Known gap:" docs/spec/` —
can find without reading every paragraph, and it is what tells a future session not to treat
the block's shown behaviour as what the language should do. `expect=fragment` doesn't need it: the tag itself
already says the block isn't normative.

A `expect=unimplemented` block is the same kind of risk from the other direction: the tag
says the *example* doesn't compile yet, but prose right next to it can still describe design
intent — a rule the language will have once the feature exists — in a way that reads as
settled fact. That prose gets its own lead-in, **Not implemented:**, for the same reason:
`grep -r "Not implemented:" docs/spec/` finds every place a chapter is describing a feature
ahead of the compiler rather than behind it. The `let … in` section of `layout.md` is the
first example of both lead-ins living in one section.

The third lead-in, **Provisional:**, belongs to the appendices and a chapter never carries
it. It is what the `expect=` tags buy a chapter, done in prose: an appendix cannot be held
to account by a test, so it says in its own text which of its claims have nothing behind
them. A chapter has no need of it, because a language question with no settled answer is an
**Open question** at the foot of the chapter instead — a language may have questions it has
not answered, but it cannot have a rule that is only provisionally a rule and still be one
thing.

## A chapter says what the language is

Every sentence in a chapter describes Zelkova as designed, in the present tense. Three
things therefore never appear in one:

- **Project history.** How a rule came to be decided, which `SPEC-` ticket decided it, what a
  signature used to be spelled, which chapter superseded which, what a pass "found". A reader
  needs the rule; a rule that leans on its own past is one the chapter has not finished
  writing. `docs/tickets/` is the work log and keeps all of that.
- **Commentary on the document.** *This chapter is the record of that design*, *that block
  matters more than it looks*, *the chapter should say so*. Say the thing rather than
  announcing that you are about to.
- **Alternatives considered and dropped.** The language is what is written down. A road not
  taken belongs in the ticket that took the other one — this file and [the index](README.md)
  included, which is why the two of them may name `SPEC-2` and `SPEC-3` and a chapter may not.

Explaining *why* a rule is what it is stays in scope, and is much of what makes a chapter worth
reading — the test is whether the reason is a property of the language ("allowing it would need
a kind system") or a fact about the project ("`SPEC-11` found the spelling carried no meaning").

The two lead-ins above are the exception, and both are about the *compiler* rather than the
language: **Known gap:** describes behaviour that exists today and should not, **Not
implemented:** a rule the compiler does not have yet. Those measure the distance between the
spec and the binary, which is the one "not yet" a chapter is for.

## A spec change and a semantics change do not share a diff

Writing a chapter surfaces compiler behaviour nobody intended — that is much of the value.
When it does, file a ticket and specify the behaviour the language *should* have, tagging
the example for what the compiler does today. ERR-11 and ERR-12 were both found this way.
Fixing the compiler in the same diff that documents it makes the change unreviewable, and
it is unnecessary: a spec claim the compiler fails is a red test, which is a working
record rather than a lost one.

## Chapter or appendix

The line between the two is what a claim changes. A rule that decides what a program means —
what a name resolves to, what is visible across a boundary, what a manifest field obliges —
is a chapter's, however file-shaped it looks. A rule about how bytes arrive, where they are
kept, or what a command prints is an appendix's.

## Writing a chapter

The `write-spec-chapter` skill (`.claude/skills/`) carries the method: probe the compiler
rather than reasoning about it, settle the design questions with the owner before drafting,
and file what turns up instead of fixing it. Each chapter also has a `SPEC-n` ticket, filed
by a separate run before drafting starts, because that skill refuses to file the ticket it
would later close.
