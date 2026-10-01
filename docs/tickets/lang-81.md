# LANG-81 · A `Float` or `String` literal pattern is not checked by the typer and not emitted

**Sizing:** small-to-medium. The typer half is one arm each in `translate_pattern`. Emission
is the larger half: `ir::LiteralValue` has no `Float` or `String` variant, so the decision
tree and the JavaScript backend need one. What could make it bigger is `Float` equality,
whose meaning for `NaN` and `-0.0` the spec does not state.

**Part of:** no active program. Found while reviewing [`LANG-77`](README.md), which made a
string literal reachable in a pattern and left the pattern untranslated.

**Location:** `crates/zelkova-compiler/src/typer/mod.rs` — `translate_pattern`, whose arms cover `Bool`,
`Int`, `Char` and `Unit` and return `None` for every other literal pattern. `crates/zelkova-compiler/src/ir/mod.rs`
— `LiteralValue`. `crates/zelkova-js/src/lib.rs` — where a decision tree's literal test is
emitted. `docs/spec/patterns.md` — [*Literal patterns*](../spec/patterns.md#literal-patterns).

**Problem:** [*Literal patterns*](../spec/patterns.md#literal-patterns) says any literal
(integer, float, character or string) matches a value equal to it. `translate_pattern`
translates only integer, character, `Bool` and unit patterns. For a float or string pattern it
returns `None`, so the whole declaration is left unchecked without an error: a type error in
it, such as `case 1 of "a" -> …`, is never reported. The JavaScript backend then refuses the
declaration with "cannot be compiled to JavaScript, because the type checker could not check
it", which blames the typer for something the user wrote as the chapter allows.
`crates/zelkova-js/tests/javascript.rs`'s `a_string_pattern_is_refused` pins the refusal for strings. No test
covers floats.

**Approach:**

1. Add `Float` and `String` arms to `translate_pattern`, constraining the scrutinee to
   `Type::Literal(TypeLiteral::Float)` and `TypeLiteral::String`.
2. Add `LiteralValue::Float` and `LiteralValue::String`, and teach the decision-tree
   construction and the backend to test them. State what `Float` equality is for `NaN` and
   `-0.0` first; the ticket does not decide that.
3. Replace `a_string_pattern_is_refused` with a test that the declaration emits, and add the
   float counterpart. Add a `patterns.md` example that matches a string.

**Acceptance:** `case 1 of "a" -> …` is a type error under `cargo test --test typer`; a
declaration matching on a string literal emits and runs under `tests/js/`; each new test goes
red when its `translate_pattern` arm is removed; `cargo test --workspace` is green.
