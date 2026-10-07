# LANG-91 · A lone surrogate passes the `Char` and `String` boundary checks, and `Comparable` misorders it

**Sizing:** small. Two emitted predicates, two companion-side argument checks, and the tests
that pin the emitted text. What could make it bigger is the choice in step 3, if the remap in
`_Utils_lessByCodePoint` is rewritten instead of left alone.

**Location:** `crates/zelkova-js/src/lib.rs` — the `scalars::CHAR` and `scalars::STRING` arms of
the predicate emitter's `test`, and the comment above the `CHAR` arm.
`std/core/src/Js/Utils.mjs` — `_Utils_isChar`, `_Utils_isString`, `_Utils_lessByCodePoint` and
the comment above it. `crates/zelkova-js/tests/javascript.rs` — the tests whose expected text
holds `codePointAt(0) > 0xFFFF ? 2 : 1`. `docs/spec/interop.md` —
[*Which types may cross the boundary*](../spec/interop.md#which-types-may-cross-the-boundary),
its **Known gap:** paragraph.

**Found while:** working out [`SPEC-40`](README.md), which wrote the ordering of the scalar
types into [*Ordering*](../spec/evaluation-semantics.md#ordering). Left unfixed there because *A
spec change and a semantics change do not share a diff*.

**Decided (`SPEC-40`, by the language owner; [`DEC-28` decision 4](../decisions/dec-28.md#4--a-char-is-a-unicode-scalar-value-and-a-string-a-sequence-of-them)):**
a `Char` is a Unicode scalar value and a `String` a sequence of them
([*Scalar types*](../spec/types.md#scalar-types)). A surrogate is a value of neither.

**Problem:** a JavaScript string may hold a surrogate that is half of no pair, and both checks
admit it. The `String` predicate is `typeof v === "string"`. The `Char` predicate is
`typeof v === "string" && v.length === (v.codePointAt(0) > 0xFFFF ? 2 : 1)`, which a single
surrogate unit satisfies. So a companion can hand a program a value the language does not have.

The program can then see it. `_Utils_lessByCodePoint` remaps a surrogate above U+E000 to U+FFFF,
which is code point order when the surrogate is half of a pair and is not when it is alone:

```
$ node --input-type=module -e '
import { ltChar, ltString } from "./std/core/src/Js/Utils.mjs";
console.log(ltChar("\uD800", ""), ltString("", "\uD800"));'
false true
```

U+D800 is below U+E000, so code point order answers `true false`.

**Approach:**

1. Make the emitted `String` predicate ask for a well-formed string. `String.prototype.isWellFormed`
   is the direct spelling and exists on the node CI runs (`node-version: 24` in
   `.github/workflows/rust.yml`); this ticket does not decide whether an older runtime has to be
   supported, and a regular expression over lone surrogates is the fallback if one does.
2. Make the emitted `Char` predicate refuse a lone surrogate, and bring `_Utils_isChar` and
   `_Utils_isString` into line so a facade argument is held to the same rule as a result.
3. `_Utils_lessByCodePoint` then never sees a lone surrogate, and its remap is correct for every
   value that reaches it. Either leave it and say so in its comment, or make it order a lone
   surrogate by its code point as well. This ticket does not pick; the first is no code.
4. Delete the **Known gap:** paragraph in *Which types may cross the boundary*.

**Acceptance:** an effectful test facade under `std/core/tests/Js/` whose companion returns
`"\uD800"` as a `String`, and one returning it as a `Char`, each yield `Err (Malformed ..)`,
where both are `Ok` today; `"\u{1F600}"` still crosses as both; each new test goes red when
its predicate is reverted; `grep -n "Known gap" docs/spec/interop.md` no longer finds the surrogate
paragraph; `cargo test --workspace`, `cargo run -- test std/core` and
`node --test 'tests/js/**/*.mjs'` pass.
