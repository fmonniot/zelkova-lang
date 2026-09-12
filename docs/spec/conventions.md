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
| `expect=ok` | Parses, canonicalizes and type checks with no errors. The typer checks the declarations it can translate and skips the rest silently: a constructor or tuple pattern in a function head, a body reaching a [foreign](interop.md) value or an expression form the term language does not model, a name the module does not itself bind, and a `module foreign` block whole. So the tag promises that every declaration the typer *reached* honours its annotation, not that it reached every declaration — roughly one in ten across today's chapters is skipped. Exhaustiveness is not run at all: it is a stub that accepts every module. |
| `expect=parse-error` | Fails somewhere in the parser (tokenizer, layout or grammar). Which specific error is not pinned. Use when the chapter claims only that the source is rejected. |
| `expect=parse-error:Reason` | The same, and the reason must match. `Reason` is either the phase (`Tokenizer`, `Layout`) or one of the fifteen specific errors — `CharNotClosedError`, `StringError`, `UnicodeError`, `IndentationError`, `TabError`, `UnrecognizedToken`, `IntegerOverflow`, `MultipleDecimalPoints` and `NonAsciiDigit` from the tokenizer; `LayoutError` from layout; `InvalidToken`, `UnexpectedEOF`, `UnexpectedToken`, `ExtraToken` and `InfixPrecedenceOutOfRange` from the grammar — matched against the real enums in `src/compiler/parser/`. Use whenever the chapter's prose describes the error the reader will see. |
| `expect=canonical-error:VariantName` | Parses, then canonicalization returns a `Vec<canonical::Error>` containing at least one error of variant `VariantName` — matched against the real variant names in `src/compiler/canonical/mod.rs`'s `Error` enum. |
| `expect=type-error` | Parses and canonicalizes, and then the type checker returns at least one error. Which one is not pinned. Use when the chapter claims only that the declaration is a type error. |
| `expect=type-error:Kind` | The same, and the kind must match — `Kind` is one of the variant names of `ErrorKind` in `src/compiler/typer/mod.rs`, matched against the real enum. Use whenever the chapter's prose describes the error the reader will see. A block the parser or canonicalization rejects fails both of these tags rather than satisfying them: the tag names the phase that decides the rule being claimed. |
| `expect=unimplemented` | Must fail somewhere in parse, canonicalize or type check, but deliberately does not pin *which* error: pinning would wire tokenizer/grammar internals into a prose document, and the tag's whole job is to go red the day the feature is actually implemented. On an expected failure the test run prints the error it observed, so a reviewer can eyeball that the block failed for the reason the chapter intends. |
| `expect=dependency-error` | The block's *package* (see below) has no valid module order — its imports form a cycle — so nothing in it is canonicalized at all. The one expectation that belongs to a group rather than to a module: every block of the package carries it, or none does. |
| `expect=fragment` | An illustrative fragment, deliberately not executed. The only opt-out, and it must be written explicitly — there is no implicit skip. |

A fenced block whose info string's first token is not `zel` (` ```sh `, a bare
` ``` `, prose) is not touched by the harness at all.

The `expect=parse-error:Reason` row above is read by `tests/spec.rs`, which checks it against
the reasons the harness really accepts: keep every reason name in it backticked, and keep the
count of specific errors spelled out as a word.

## More than one module: `package=`

A block holds a single module, and by default it is compiled alone, against no interfaces
at all. A block may also carry a second tag, `package=<label>`, beside its `expect=` — an
info string reading ```` ```zel expect=ok package=alias ````.

Blocks sharing one label, **within one chapter**, are one package. They are parsed
together, ordered by their imports, canonicalized in that order against each other's
`Interface`s, and then type checked in the same order — which is how a chapter shows two
modules at once. Each block keeps its own `expect=`, so an example can show one module
compiling and its importer failing, and the failure is reported on the importer's line
rather than on the group.

A module that fails the **type** checker still publishes its interface to the rest of the
group, so an importer of it goes on resolving every name it imports. An interface carries
declared signatures and canonicalization is what validated them; withholding it would
turn a type error in one block into a wave of unresolved names in the next.

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
the block's shown behaviour as what the language requires. `expect=fragment` doesn't need it: the tag itself
already says the block isn't normative.

A `expect=unimplemented` block is the same kind of risk from the other direction: the tag
says the *example* doesn't compile yet, but prose right next to it can still describe design
intent — a rule the language will have once the feature exists — in a way that reads as
settled fact. That prose gets its own lead-in, **Not implemented:**, for the same reason:
`grep -r "Not implemented:" docs/spec/` finds every place a chapter is describing a feature
ahead of the compiler rather than behind it. The `let … in` section of `layout.md` is the
first example of both lead-ins living in one section.

The third lead-in, **Provisional:**, belongs to the appendix and a chapter never carries
it. It is what the `expect=` tags buy a chapter, done in prose: an appendix cannot be held
to account by a test, so it says in its own text which of its claims have nothing behind
them. A chapter has no need of it, because a language question with no settled answer is an
**Open question** at the foot of the chapter instead — a language may have questions it has
not answered, but it cannot have a rule that is only provisionally a rule and still be one
thing.

## The words a chapter uses

The rules above are about the half of a chapter a test reads. Nothing reads this half, so it
is written down here: the vocabulary the chapters already use, recorded rather than proposed.
Every rule below is one that reading the chapters would teach, and none of them makes an
existing paragraph wrong.

**A rule a program has to follow is written `must`, `cannot`, `may not` or `is an error`.**
Those are one force spelled four ways, and choosing between them is grammar rather than
degree. `must` and `cannot` take the construct as their subject — *leading whitespace on a
line must be an even number of spaces*, *a qualified name cannot be shadowed*. *Is an error*
takes the situation instead, which is what a sentence wants when the thing ruled out is a
combination rather than a construct: *repeating a name is an error*. *Is rejected* is the
same claim told from the compiler's side, and reads naturally as the line introducing a
block. Nothing separates the four in strength, because there is no weaker degree for them to
be stronger than: a chapter states rules, never advice a program may decline.

**`may` grants a permission** — *a name may mix scripts freely after its first character* —
and is the word for a latitude a reader would not otherwise assume. **`never`** is its
mirror, prohibiting something of the language rather than of one program: *thirteen words are
reserved and may never be used as identifiers*.

**`should` is reserved, and never states a requirement on a program.** It belongs to the
sentence a **Known gap:** or **Not implemented:** lead-in opens — *that block should be
rejected and is accepted* — and to the continuations of such a paragraph. That is prose about
the compiler: the language requires something and the binary does not do it. A rule for a
program is written with the words above, and reserving a common English word for the
compiler-facing case is worth its cost, because a rule written in that voice reads as one the
reader may weigh against others and this document holds no such rules.

Which is also why the obvious import — RFC 2119's MUST / SHOULD / MAY — would be wrong here,
worth writing down so that the next reader does not have to re-derive it. RFC 2119's SHOULD
means *recommended, and a valid implementation may decline*, the opposite of what `should`
means in a **Known gap:** sentence, where the thing is required and the compiler is at fault.
Taking the standard meaning would either invalidate every sentence of that shape or, worse,
leave them readable both ways. The other half of that convention's appeal is letting a reader
tell a guarantee from guidance, and a chapter has no guidance in it to tell apart.

**None of these words obliges an example.** Most rules here carry no block at all — whole
sections of [name resolution](name-resolution.md) and [packages](packages.md) state theirs in
prose alone, and a rule is no weaker for it. What holds is the converse: where a block *is*
the demonstration of a rejection claim, it carries a rejection tag — `expect=parse-error`,
`expect=parse-error:Reason`, `expect=canonical-error:Variant`, `expect=type-error`,
`expect=type-error:Kind`, `expect=unimplemented` or `expect=dependency-error` — and not
`expect=ok`.

The exception is the case the lead-ins exist for, and it is common: an `expect=ok` block sits
under a rejection claim in most chapters, and every one of those blocks has a **Known gap:**
or **Not implemented:** paragraph beside it — before or after — saying so. The rule is the
language, the green block is the binary, and the lead-in is the whole of what separates them.
Nothing else can be: `expect=ok` beside *is an error* is exactly as green as `expect=ok`
beside *is accepted*, which is the one contradiction in a chapter the harness cannot see.

## The sentences a chapter does not need

The section above decides which word carries a rule. This one is about the sentences standing
around it, and it is here because a first draft grows the same handful of them every time.
Nothing reads this half either — no test sees a paragraph that says its point twice, and the
patterns below are what a drafting session produces when it is thinking about being convincing
rather than about being read.

Two of the patterns belong to the next section, which owns them: commentary on the document,
and a case made for a rule rather than the rule itself. The rest are these.

**A sentence opens on its subject.** *A user can mark a Zelkova module as a JavaScript
interface. This is done by using the `javascript` modifier* is one sentence wearing two: *a user
marks a Zelkova module as a JavaScript interface with the `javascript` modifier*. A chapter has
no need to announce a claim before making it.

**A negation that only mirrors the first half is dropped.** A closing *rather than X*, *and not
X*, *instead of X* earns its place when X is a thing a reader might otherwise have assumed, and
not when it merely says the first half again in the negative: *an error at the boundary rather
than a wrong answer somewhere further on* is *an error at the boundary*. `rather than`, `not
as` and `instead of` are the three spellings, and they are greppable.

**A paragraph does not end on its own point.** *…which is the property this design exists to
preserve*, *which is what makes the rule worth having*. The paragraph made the point; a
restatement in the last clause reads as a chapter unsure the first one landed.

**A cost is stated and then left alone.** Naming what a rule costs is worth doing; following it
with the reason the cost is smaller than it looks is not. The reader weighs it.

**One shape per paragraph.** Three *is what* sentences in a row, or a *which means … which
means* chain, is a template rather than a thought. Vary the weakest, or cut it.

**A conclusion is stated in one place.** When two sections end on the same sentence, it stays
where the rule lives and the other links to it by anchor.

Structure carries the same bias: **what a construct can do comes first, what it cannot comes
after and shorter.** Separate sections that each say what is forbidden merge into one — the
three prohibitions [Foreign interoperability](interop.md#what-a-facade-signature-may-not-name)
makes about a facade signature are one section, and shorter than the three were. A chapter
written before this section was recorded may
still carry any of these; the `prose-pass` skill (`.claude/skills/`) is what takes them out, one
pattern swept at a time.

## A chapter says what the language is

Every sentence in a chapter describes Zelkova as designed, in the present tense. Three
things therefore never appear in one:

- **Project history.** How a rule came to be decided, which `SPEC-` ticket decided it, what a
  signature used to be spelled, which chapter superseded which, what a pass "found". A reader
  needs the rule; a rule that leans on its own past is one the chapter has not finished
  writing. `docs/tickets/` is the work log and keeps all of that.
- **Commentary on the document.** *This chapter is the record of that design*, *that block
  matters more than it looks*, *it is worth saying this twice*. Say the thing rather than
  announcing that you are about to.
- **Alternatives considered and dropped.** The language is what is written down. A road not
  taken belongs in [`docs/decisions/`](../decisions/README.md), or in the ticket that took the
  other one while that ticket is open — this file and [the index](README.md) included, which
  is why the two of them may name `SPEC-2` and `SPEC-3` and a chapter may not.

Explaining *why* a rule is what it is stays in scope, and is much of what makes a chapter worth
reading — the test is whether the reason is a property of the language ("allowing it would need
a kind system") or a fact about the project ("`SPEC-11` found the spelling carried no meaning").

The two lead-ins above are the exception, and both are about the *compiler* rather than the
language: **Known gap:** describes behaviour that exists today and should not, **Not
implemented:** a rule the compiler does not have yet. Those measure the distance between the
spec and the binary, which is the one "not yet" a chapter is for.

## A spec change and a semantics change do not share a diff

Writing a chapter surfaces compiler behaviour nobody intended — that is much of the value.
When it does, file a ticket and specify the behaviour the language *requires*, tagging
the example for what the compiler does today. ERR-11 and ERR-12 were both found this way.
Fixing the compiler in the same diff that documents it makes the change unreviewable, and
it is unnecessary: a spec claim the compiler fails is a red test, which is a working
record rather than a lost one.

## Chapter or appendix

The line between the two is what a claim changes. A rule that decides what a program means —
what a name resolves to, what is visible across a boundary, what a manifest field obliges —
is a chapter's, however file-shaped it looks. A rule about how bytes arrive, where they are
kept, or what a command prints is an appendix's.

Neither is where an *argument* goes. A chapter explains why a rule is what it is only as far
as the reason is a property of the language — "allowing it would need a kind system" — and
stops at the point where the reason becomes a comparison with what was rejected. That is a
third kind of document and it lives in [`docs/decisions/`](../decisions/README.md), which is
not normative, is not checked for examples, and is where a chapter's rule may be cited *from*
but never deferred *to*. The test is whether a later reader would otherwise re-open the
question: [Type classes](type-classes.md#a-class-says-how-it-is-derived) states one derivation
mechanism in full and is complete as it stands, and the survey of the eight it is not
([DEC-1](../decisions/dec-1.md)) is what stops someone proposing a generic representation
again.

## Writing a chapter

The `write-spec-chapter` skill (`.claude/skills/`) carries the method: probe the compiler
rather than reasoning about it, settle the design questions with the owner before drafting,
and file what turns up instead of fixing it. Each chapter also has a `SPEC-n` ticket, filed
by a separate run before drafting starts, because that skill refuses to file the ticket it
would later close.
