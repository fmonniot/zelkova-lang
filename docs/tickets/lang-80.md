# LANG-80 · The spec does not settle a string's unknown escape, surrogate escape or `\u{…}` digit count

**Sizing:** small. It is a spec edit plus two examples, and it changes no code unless the
digit-count choice below does. What could make it bigger is that character literals share the
escape table and are not tokenized with escapes yet, so a rule written for strings has to say
whether it is written for both.

**Part of:** no active program. Split out of [`LANG-77`](README.md), which landed single-line
strings. Its review found that the diff had written three new language rules into
[*Strings*](../spec/lexical-structure.md#strings) beside the compiler change that implements
them, which `conventions.md`'s *A spec change and a semantics change do not share a diff*
forbids. The sentence and its example were dropped from that PR and the tokenizer behaviour
kept, so the rules below are implemented and not specified.

**Location:** `docs/spec/lexical-structure.md` — [*Characters*](../spec/lexical-structure.md#characters)
(the escape table) and [*Strings*](../spec/lexical-structure.md#strings).
`crates/zelkova-syntax/src/parser/tokenizer.rs` — `consume_escape`, and the `InvalidEscape` and
`UnicodeError` variants of `TokenizerErrorType`. `docs/spec/conventions.md` — the
`expect=parse-error:Reason` row already lists both reasons.

**Problem:** the chapter says strings use "the same escapes as character literals" and gives
the table, but three inputs are left open, and `consume_escape` answers each one:

1. **An unknown escape**, such as `"C:\docs"`. The tokenizer rejects it with `InvalidEscape`.
2. **A surrogate**, `"\u{D800}"`. **Decided** (`SPEC-40`, by the language owner;
   [`DEC-28` decision 4](../decisions/dec-28.md#4--a-char-is-a-unicode-scalar-value-and-a-string-a-sequence-of-them)):
   a `Char` is a Unicode scalar value, so a surrogate escape is an error.
   [*Scalar types*](../spec/types.md#scalar-types) states the rule and *Characters* now says a
   literal holds one scalar value. The tokenizer already rejects it with `UnicodeError`. What
   is left for this ticket is the escape table's `\u{H…}` row, which still reads "the given
   hexadecimal code point", and the example pinning the rejection.
3. **How many digits a `\u{…}` may have.** The tokenizer accepts one to six and rejects a
   seventh, so `"\u{0000041}"` is a `UnicodeError` although it names U+0041. The review of
   `LANG-77` raised this as a contradiction with the (removed) sentence that only a
   non-scalar value is an error. With that sentence gone the chapter is silent, and the
   choice is open: cap the digits at six, as Rust and Elm do, or accept any number of digits
   and reject only a value above `0x10FFFF`, which has to watch `u32` overflow.

Elm and Rust reject 1 and cap 3 at six, so these are probably the right answers. This ticket
does not pick either: it is the place someone does, on purpose.

**Approach:**

1. Decide rules 1 and 3, in a `docs/decisions/` entry if the digit-count choice is argued
   rather than copied.
2. Write them into *Characters* and *Strings*, stating whether a character literal follows
   them once it tokenizes escapes.
3. Add an `expect=parse-error:InvalidEscape` example (`path = "C:\docs"`) and an
   `expect=parse-error:UnicodeError` example for each rule that is an error. If the digit
   count is decided differently from the tokenizer, change `consume_escape`'s digit loop and
   its doc comment in the same ticket, as a separate commit from the prose.

**Acceptance:** `cargo test --test spec` is green with the examples above present, each
tagged with the reason it pins; each example goes red if `consume_escape`'s matching rule is
neutralised; `cargo test --workspace` is green.
