# SPEC-2 · Make `docs/spec/` self-contained, and write the Lexical structure chapter

**Sizing:** medium. One chapter, plus a policy change to the index that every later chapter
inherits.

**Location:** `docs/spec/INDEX.md`, `docs/spec/lexical-structure.md` (new), and the two
existing chapters where they defer to Elm.

**Problem:** `docs/spec/INDEX.md` as written by [SPEC-1](INDEX.md) treats Elm's documentation
as the normative fallback. Its chapter list is explicitly "the places Zelkova **cannot** defer
to Elm's documentation, either because it diverges or because Elm never wrote the rule down",
and `js-interop.md` carries an open question about how much of Elm's interop story to pull in.

That is the wrong shape for a language that intends to diverge. Deferring works only while
Zelkova and Elm agree, and the moment a divergence is wanted the spec has to answer a question
it has no vocabulary for — because the vocabulary lives in someone else's document, describing
someone else's language, and changes without notice. It also makes every divergence feel like
a defect rather than a decision.

**Decided (by the language owner):** `docs/spec/` is self-contained. Elm is an inspiration and
a source of good ideas, not a reference: no chapter may resolve a question by pointing at Elm's
documentation. Where a rule is inherited from Elm it is written out here in full; where the two
differ, this directory is the answer.

**Approach:**

1. Rewrite `INDEX.md`'s framing and chapter list around that policy. The list stops being
   "the gaps Elm leaves" and becomes the whole language.
2. Remove the deferrals from `layout.md` and `js-interop.md`.
3. Write `lexical-structure.md`, the natural first chapter: every later one draws on its
   vocabulary (identifier, literal, operator, reserved word), and it holds the two questions
   `INDEX.md` already flagged as blocking — the soft-keyword split, and `true`/`false`.

**What the chapter settles.** Writing it forced a run of decisions that had never been made,
each taken by the language owner during the session. Recorded here because the chapter states
the rule but not that it was open:

- Booleans are an ordinary union type; there is no boolean literal syntax ([LANG-1](lang-1.md)).
- All four soft keywords behave alike ([LANG-2](lang-2.md)).
- Integer literals: decimal and hexadecimal, guaranteed over `-2^31 .. 2^31 - 1`, target-dependent
  beyond. Floats: a digit on each side of the point, with an optional exponent ([LANG-3](lang-3.md)).
- Characters and strings share a full escape set; strings have a `"""` multi-line form.
- Block comments nest, end exactly at `-}`, may appear anywhere a space may, and are an error
  if unterminated ([BUG-13](bug-13.md)).
- `{-|` is an ordinary comment; doc tooling is out of scope.
- An identifier begins with an uppercase or lowercase letter — not titlecase, not uncased
  ([LANG-3](lang-3.md)).
- Prefix `-` is negation, not subtraction against zero ([LANG-4](lang-4.md)).

**Found while writing it:** [BUG-12](bug-12.md) (four `unwrap()` panics reachable from source
text), [BUG-13](bug-13.md), and the three `LANG-` tickets above. None is fixed here, per
`INDEX.md`'s *A spec change and a semantics change do not share a diff*; each has a tagged
block in the chapter that goes red when it lands.

**Acceptance:** `cargo test --test spec` green, with every block in the new chapter carrying an
`expect=` tag, and each tag having been seen to fail — the chapter's claims are checked, not
asserted. No occurrence of Elm as a normative reference anywhere under `docs/spec/`
(`grep -rn "Elm" docs/spec/` should return only sentences that name it as an influence).
