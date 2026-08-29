# LANG-3 · The tokenizer accepts two forms the lexical rules reject: a titlecase-initial identifier, and a float with no digit after the point

**Sizing:** small. Two predicates, two tests each.

**Location:** `src/compiler/parser/tokenizer.rs` — `is_identifier_start` (and the
`first.is_uppercase()` classification at the end of `consume_identifier`), and
`consume_number`'s accumulation loop.

**Found:** while writing `docs/spec/lexical-structure.md` under `SPEC-2`. Both are places the
tokenizer is *more permissive* than the language, which is the direction that does not
announce itself: nothing fails, so nothing gets noticed.

**Decided (SPEC-2, by the language owner):**

1. An identifier begins with a character in Unicode general category `Lu` or `Ll`, and
   nothing else. Titlecase (`Lt`) may not begin one, for the same reason an uncased letter
   may not: Zelkova reads a name's first letter to decide whether it names a type or a value,
   and reads only two cases.
2. A float literal has at least one digit on each side of its `.`.

**Problem 1.** `is_identifier_start` admits any *cased* letter — `GeneralCategory::is_cased_letter()`
covers `Lu`, `Ll` **and** `Lt`. `consume_identifier` then classifies by `first.is_uppercase()`,
which is false for a titlecase character, so `ǅoo` is accepted and becomes a *lowercase*
identifier naming a value. The capital-looking name that makes a value is the visible symptom;
the underlying one is that the two predicates disagree about what the categories mean.

**Problem 2.** `consume_number` breaks only on a character that is neither numeric nor `.`,
so `1.` accumulates as `"1."`, which `f64::from_str` happily accepts as `1.0`. Requiring a
digit after the point is what keeps `.` usable as punctuation, and the spec says so.

**Approach:** for (1), replace the `is_cased_letter()` call with an explicit test for
`UppercaseLetter | LowercaseLetter`, and — since `Lt` can no longer reach it — the
`first.is_uppercase()` classification becomes exact rather than accidentally so. Leave
`is_identifier_continuation` alone: `Lt` stays a legal continuation character, and the spec's
example pins that.

For (2), the fix belongs with [`BUG-12`](bug-12.md), which is already rewriting that loop to
stop at a second `.` and to use `is_ascii_digit`. Requiring a digit after the first `.` is
one more condition in the same rewrite, and doing it separately means touching the loop
twice. **Take BUG-12 first**, or take both together.

**Acceptance:** `ǅoo = 1` is rejected with an error naming the character, and `xǅoo = 1` still
compiles. `f = 1.` is rejected; `f = 1.0` still compiles. Each seen to fail before the fix.

Two blocks in `docs/spec/lexical-structure.md` go red and are retagged, their `**Known gap:**`
paragraphs deleted: the titlecase one in *Identifiers*, and the trailing-point one in
*Floats*.

`cargo run` must still print `parsed 8 modules` and exit 0 — `std/core/src/` is ASCII
throughout and writes no bare trailing point, so neither rule should cost it anything.
