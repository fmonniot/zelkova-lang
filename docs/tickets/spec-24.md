# SPEC-24 · The conventions name every tag a chapter may write but not a single word it may write, and the tag table is four names short

**Sizing:** small. Prose in one file, no chapter is edited, no harness changes. The one thing
that could grow it is the `should` rule below, if the language owner wants a different answer
from the one the corpus already gives.

**Location:** [`docs/spec/conventions.md`](../spec/conventions.md) — *The `expect=`
vocabulary*, whose `expect=parse-error:Reason` row lists the reason names, and the file as a
whole, which has no section on chapter wording. `tests/spec.rs` — `parse_error_reasons`, which
holds the real list.

**Problem, first half — the table is wrong.** `parse_error_reasons` accepts thirteen names: the
two phase names `Tokenizer` and `Layout`, and **eleven** specific errors.
[`docs/spec/conventions.md`](../spec/conventions.md) lists both phase names and **seven** of the
eleven — `IndentationError`, `TabError`, `LayoutError`, `UnexpectedToken`, `UnexpectedEOF`,
`InvalidToken`, `ExtraToken`. The four it omits are `CharNotClosedError`, `StringError`,
`UnicodeError` and `UnrecognizedToken`.

`UnrecognizedToken` is not hypothetical — two blocks in
[`docs/spec/lexical-structure.md`](../spec/lexical-structure.md) are tagged
`expect=parse-error:UnrecognizedToken` and pass. So the file a chapter author is instructed to
read, by both [the index](../spec/README.md) and `write-spec-chapter`'s Step 1, understates
what the harness accepts, and a chapter is already using a name it does not list. The
asymmetry is structural rather than careless: `parse_error_reasons` is deliberately an explicit
match so that a **new enum variant** fails the file to compile, but nothing connects the name it
produces to the prose that is supposed to describe it. [SPEC-23](spec-23.md) is the check that
would close that loop; this ticket is the correct list it needs to check against.

**Problem, second half — no stated vocabulary.** *Tag every claim the chapter makes* is precise
about the machine-checked half of a chapter, down to why the pin is on the error variant and
not the token. It says nothing about the words. Across the twelve chapters and the appendix,
`may` appears 172 times, `never` 57, `cannot` 55, `must` 51, `is an error` 25, `is accepted` 16
and `is rejected` 15 — a real vocabulary, used consistently, written down nowhere. A reader
cannot tell whether the difference between "is rejected" and "is an error" carries meaning, and
a chapter author has to infer the register by reading two existing chapters, which is what Step
1 of the skill actually asks them to do.

The word that most needs pinning is **`should`**, because it already has a specific meaning
here that is *not* the ordinary one. Of its 35 occurrences, 28 sit on a **Known gap:** or
**Not implemented:** lead-in line, where it means *the language requires this and the compiler
does not do it* — as in
[`docs/spec/types.md`](../spec/types.md)'s "that block should be rejected and is accepted". The
remaining seven are all continuations of such a paragraph, one sentence inside an **Open
questions** entry, one rationale about what the compiler owes an interop author
([`docs/spec/js-interop.md`](../spec/js-interop.md)), and one idiom. **Not one of the 35 states
a requirement on a program.** So the rule is already held; it is simply not written, which
means the next chapter can break it without anyone noticing that it was a rule.

**What not to do:** import RFC 2119. The obvious move here is a MUST / SHOULD / MAY list, and it
would be actively wrong for this document. RFC 2119's SHOULD means *recommended, but a valid
implementation may decline* — the exact opposite of what `should` means in a **Known gap:**
sentence, where the thing is required and the compiler is at fault. Adopting the standard
meaning would either invalidate 35 existing sentences or, worse, leave them readable both ways.
The other half of RFC 2119's appeal — letting a reader tell a guarantee from guidance — is
already served better by whether a tagged block sits next to the claim, which is a stronger
signal than a capitalised verb.

**Approach:**

1. Correct the `expect=parse-error:Reason` row to list all eleven names the harness accepts.
   Keep the existing framing, which is right: `Reason` is either a phase or a specific error,
   "matched against the real enums in `src/compiler/parser/`".
2. Add a short section — *The words a chapter uses*, sibling to *Tag every claim the chapter
   makes* — recording the vocabulary as the corpus already uses it, not as an aspiration.
   Descriptive is the whole point: every rule in it should be one that reading the twelve
   chapters would already teach. At minimum it settles `should` (never a requirement on a
   program; it appears only where a chapter is describing the compiler falling short of one),
   and says whether `is rejected` / `is an error` / `cannot` differ or are one thing spelled
   three ways.
3. Say which of those words, if any, imply a checked block. "Is rejected" next to an
   `expect=ok` block is a contradiction the harness cannot see, and if the convention is that a
   rejection claim carries a rejection-tagged block, that belongs here beside the tag rules
   rather than being inferred.

**The one thing to settle with the language owner** is point 2's `should` rule as stated —
whether `should` is reserved to compiler-facing prose, or merely *usually* used that way and
free elsewhere. The corpus supports the strict reading and the strict reading is more useful,
but reserving a common English word is a real constraint on future prose and the person who
will write that prose gets to choose. Everything else in this ticket is transcription.

**Acceptance:** [`docs/spec/conventions.md`](../spec/conventions.md)'s reason list matches
`parse_error_reasons` name for name — checkable today by eye and, once [SPEC-23](spec-23.md)
lands, by `cargo test --test spec`. The file has a section stating the wording rules, and
`grep -n "\bshould\b" docs/spec/*.md` shows every hit conforming to whatever that section says,
with any that do not either rewritten or explicitly allowed by it. No chapter's `zel` blocks
change; `cargo test` and `cargo run` are green, the latter still printing `parsed 8 modules` and
exiting 0.

**Sequencing:** land before [SPEC-23](spec-23.md), which asserts against this table and would
otherwise land red. Also touches the same table as [`TEST-2`](test-2.md), which adds a
type-error tag to the `expect=` vocabulary — the two do not conflict, and after this and
[SPEC-23](spec-23.md), `TEST-2` documenting its new tag stops being something its author has to
remember.

**What this is not.** Not a style guide, and not a rewrite of any chapter's prose. The chapters
read the way they do on purpose — *A chapter says what the language is* explicitly keeps
rationale in scope — and this ticket adds no rule that would make an existing paragraph wrong.

**Found:** while assessing an outside review of `docs/spec/`'s conventions against the actual
directory, on 2026-09-05. The review proposed the RFC 2119 list; checking it against the corpus
is what turned up both the `should` collision and the four missing reason names.
