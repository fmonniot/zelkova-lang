# SPEC-23 · Nothing checks the spec's own cross-references, and 271 of them are one rename from silence

**Sizing:** small-to-medium. The walk over `docs/spec/*.md` already exists and reads each
chapter's full text; this is a second pass over the same string plus a fixture-backed
self-test. What could make it bigger is the scope question below — whether links out of the
directory are checked too — which is a decision, not an implementation cost.

**Dependency, already met:** the vocabulary this checker asserts against is the table in
[`docs/spec/conventions.md`](../spec/conventions.md)'s *The `expect=` vocabulary*, corrected by
`SPEC-24` (closed 2026-09-05 — see [the index](README.md)). Before that it listed seven of the
harness's eleven specific error names, so a checker written against it would have landed red.

**Location:** `tests/spec.rs` — `spec_chapters_pass`, which walks `docs/spec/*.md` and already
has each file's `content` in hand; `parse_error_reasons` and `variant_names`, which hold the
reason and variant vocabularies as explicit matches; the harness self-tests at the foot of the
file and their fixtures under `tests/fixtures/spec/`.

**Problem:** `docs/spec/` exists on one premise, stated in its own index: documentation nothing
checks drifts from what it describes, silently and indefinitely. The `expect=` harness honours
that premise for the *examples* — 291 blocks, every one executed. It does not honour it for the
directory's own connective tissue, and that tissue is now substantial:

- **156 anchor links between chapters** (108 cross-file, 48 same-file) of the form
  `[Layout](layout.md#tabs-are-legal-only-inside-a-comment)`. Every one resolves today. Every
  one is derived from a header's text, so renaming a header — an ordinary edit, and one the
  spec has already made several times — breaks each link that pointed at it with no error
  anywhere. `write-spec-chapter`'s Step 5 asks a chapter author to cross-link "by relative path
  and anchor", so the count grows with every chapter.
- **115 links from chapters into `docs/tickets/`**, cited from **Known gap:** and
  **Not implemented:** paragraphs. These point at files the ticket process is *designed to
  delete*: `docs/tickets/README.md`'s closing convention is "delete the ticket file, then
  rewrite its row as a tombstone". A closing `LANG-` usually turns some tagged block red and so
  forces the citing paragraph to be edited — but not always, and the skill's Step 7 already asks
  an author to flag "any **Known gap:** whose block stays green across its own fix"
  ([`LANG-4`](lang-4.md) is the worked example). For exactly those, nothing at all notices when
  the cited file stops existing.

Both are also [`SITE-1`](site-1.md)'s problem twice over: a broken anchor is invisible in a
terminal `grep` and loud on a rendered HTML page.

The second half of the same gap is the `expect=` vocabulary. `parse_error_reasons` is written
as an explicit match over the real enums specifically so that adding a variant "fails this file
to compile rather than silently producing a name no chapter can ever match" — it guards the
enum-to-name direction. Nothing guards the name-to-prose direction, and it had already drifted
once: the harness accepts **eleven** specific error names (plus the two phase names) and
[`docs/spec/conventions.md`](../spec/conventions.md)'s table documented **seven** of them, with
`UnrecognizedToken` among the four missing and in active use at two blocks in
[`docs/spec/lexical-structure.md`](../spec/lexical-structure.md). `SPEC-24` transcribed the
missing four, so the two lists agree today — by hand, and with nothing to keep them that way
the next time a variant is added or renamed.

**This is preventive, not a repair.** All 156 anchors and all 115 ticket links resolve on
`main` today, and the vocabulary table — the one thing that was actually wrong — has since been
corrected by hand. What this ticket buys is that they stay that way without anyone remembering to
look — the same trade the `expect=` tags already made for examples.

**The scope question this does not settle:** whether the `../tickets/*.md` links are checked
alongside the anchors. Both answers are defensible and they are not a matter of effort:

- **Check them.** The 115 citations are the spec's account of the distance between itself and
  the compiler, and a citation of a deleted file is a claim about a gap that may no longer
  exist — the most misleading kind of stale sentence the directory can hold. The cost is that
  `cargo test --test spec` starts failing on `main` the moment a cited ticket is tombstoned
  without its chapter being edited, which is the intended pressure but is pressure applied to
  whoever closes the ticket rather than to whoever wrote the paragraph.
- **Anchors only.** Keeps the spec harness's dependencies inside `docs/spec/`, and leaves the
  tombstone convention free to close a ticket without reaching into a chapter. The stale
  citation then survives, caught only by a red `expect=` block where one exists.

The ticket does not pick. The first is the likelier answer given what the directory is for, but
it changes what "closing a ticket" costs, and that is the ticket process's call rather than the
harness's.

**A second choice, smaller:** how the reason vocabulary is compared against
[`docs/spec/conventions.md`](../spec/conventions.md). Parsing that file's markdown table gives
an exact two-way check — every name the harness knows is documented, and every name the table
lists is real — at the cost of coupling the harness to one file's table formatting. Asserting
only that each known name appears verbatim somewhere in the *The `expect=` vocabulary* section
is looser (it cannot catch a name the table invents) but survives any reformatting. The same
choice applies to the `canonical::Error` variant names from `variant_names`, which
`conventions.md` describes by rule rather than by list and which may be out of scope for that
reason.

**Approach:**

1. Extract the vocabularies from `parse_error_reasons` and `variant_names` into named constants
   the checks can read, keeping the explicit match that makes a new enum variant a compile
   error. Nothing about the existing tag-matching behaviour changes.
2. Add a check over the already-loaded chapter text: collect every markdown header in every
   `docs/spec/*.md`, slugify by GitHub's rule (lowercase, strip punctuation, each space becomes
   one hyphen — note `## \`let … in\`` slugs to `let--in`, with two hyphens, and four chapters
   link to it), and assert every `](file.md#anchor)` and `](#anchor)` resolves to a header that
   exists in the target file. Report failures the way `spec_chapters_pass` already does:
   collected rather than short-circuited, each naming its file, line and the anchor it wanted.
3. Add the vocabulary check, in whichever of the two shapes above is chosen.
4. Decide the `../tickets/` question and implement it or write down, in the harness's own doc
   comment, that it was declined and why.
5. Whether this lives inside `spec_chapters_pass` or as a sibling `#[test]` is an
   implementation detail; a sibling reads better in a failure report, since a broken link is
   not a block failure and lumping the two makes the panic message harder to read.

**Acceptance:** `cargo test --test spec` fails when a header referenced by another chapter is
renamed, and names the referring file, its line, and the anchor that no longer resolves. Proven
by fixture, not by breaking a real chapter: new files under `tests/fixtures/spec/` in the
existing style — one holding a link to an anchor that does not exist, one holding a chapter-like
header set — with self-tests beside `block_with_no_expect_is_a_hard_failure` and its siblings,
each carrying the same doc comment those have, naming what it pins and how it was neutralised.
`cargo test --test spec` also fails when a chapter uses an `expect=parse-error:Reason` name that
`conventions.md` does not document. The whole suite is green on `main`, and `cargo run` still
prints `parsed 8 modules` and exits 0.

**What this is not.** Not a general markdown linter, and not a link checker for the repository —
`CLAUDE.md`, `docs/tickets/` and `README.md` are out of scope. The claim being defended is
narrow: the specification's internal references resolve, and its documented tag vocabulary is
the harness's real one.

**Found:** while assessing an outside review of `docs/spec/`'s conventions against the actual
directory, on 2026-09-05. The review proposed JLS-style stable per-rule anchors (`layout.md#L3`)
to make a rule addressable; the anchors already exist and are already used 156 times, so the
part worth doing is the part nothing was doing — checking them.
