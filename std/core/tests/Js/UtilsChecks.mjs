// The JavaScript checks over std/core/src/Js/Utils.mjs, the companion behind the
// Js.Utils facade (Js/Utils.zel).
//
// This is the companion of the test facade Js.UtilsChecks
// (tests/Js/UtilsChecks.zel), laid out as docs/spec/interop.md's "Testing a
// companion" describes. Each export is one check, declared there as
// `Task (Result Failure ())`: it returns nothing when the check holds, and a
// failed assertion throws, which the facade's wrapper turns into
// `Err (Threw ..)`. Js.UtilsTests (tests/Js/UtilsTests.zel) exposes one
// `Test` per check, so `zelkova test std/core` runs them.
//
// Js.Utils is a set of primitives, one per scalar type, that the instances of
// `Eq`, `Comparable` and `Appendable` in Basics.zel forward to. What an
// instance computes over a whole value is checked where it is written, in the
// Zelkova tests beside these (EqTests.zel, ComparableTests.zel, NumberTests.zel,
// AppendableTests.zel and DerivedTests.zel); what is checked here is each
// primitive on its own type, and that each one refuses a value that is not of
// it, because the facades are the package's boundary.
//
// Values are shaped the way docs/spec/interop.md says they cross to JavaScript.
// Utils.mjs is imported as a module of the target rather than reached through
// its facade, which is what lets these ask what it does with a value Js.Utils
// would have refused to pass it.

import assert from 'node:assert/strict';
import {
    equalInt, equalFloat, equalChar, equalString,
    ltInt, ltFloat, ltChar, ltString,
    appendString,
} from '../../src/Js/Utils.mjs';

// Stand-ins for `Colour = Red | Blue` and a three-argument constructor, encoded as
// docs/spec/interop.md#a-union-crosses-as-a-tagged-value specifies.
const Red = { $: 'Red' };
const Rgb = (r, g, b) => ({ $: 'Rgb', a: r, b: g, c: b });

// The values none of the nine primitives takes, whatever type it is for.
const strangers = [
    Red,
    Rgb(1n, 2n, 3n),
    [1n, 2n],
    { x: 1n },
    () => 1n,
    null,
    undefined,
    true,
];

const refusal = /can only be given two (Int|Float|Char|String)s, but was given/;

// EQUALITY

// equalInt answers on two Ints, the whole 64-bit range included
export function equalIntAnswersOnTwoInts() {
    assert.equal(equalInt(3n, 3n), true);
    assert.equal(equalInt(3n, 4n), false);
    assert.equal(equalInt(-(2n ** 63n), -(2n ** 63n)), true);
    assert.equal(equalInt(2n ** 63n - 1n, -(2n ** 63n)), false);
}

// equalFloat is IEEE 754's equality: nan equals nothing, and 0 equals -0
// (docs/spec/evaluation-semantics.md#what-structural-equality-computes)
export function equalFloatFollowsIEEE() {
    assert.equal(equalFloat(1.5, 1.5), true);
    assert.equal(equalFloat(1.5, 2.5), false);
    assert.equal(equalFloat(NaN, NaN), false);
    assert.equal(equalFloat(NaN, 1.5), false);
    assert.equal(equalFloat(0, -0), true);
    assert.equal(equalFloat(Infinity, Infinity), true);
    assert.equal(equalFloat(Infinity, -Infinity), false);
}

// equalChar and equalString compare by content, a Char above U+FFFF included
export function equalCharAndStringCompareByContent() {
    assert.equal(equalChar('a', 'a'), true);
    assert.equal(equalChar('a', 'b'), false);
    assert.equal(equalChar('\u{1F600}', '\u{1F600}'), true);
    assert.equal(equalChar('\u{1F600}', '\u{1F601}'), false);
    assert.equal(equalString('', ''), true);
    assert.equal(equalString('abc', 'abc'), true);
    assert.equal(equalString('abc', 'abd'), false);
    assert.equal(equalString('abc', 'ab'), false);
}

// every equality refuses what is not of its type, a number where an Int is
// asked for and a bigint where a Float is among them
export function equalityRefusesAValueOfAnotherType() {
    assert.throws(() => equalInt(1, 1), refusal);
    assert.throws(() => equalInt(1n, 1), refusal);
    assert.throws(() => equalFloat(1n, 1n), refusal);
    assert.throws(() => equalFloat(1.5, 'a'), refusal);
    assert.throws(() => equalChar('ab', 'ab'), refusal);
    assert.throws(() => equalChar('', ''), refusal);
    assert.throws(() => equalChar('a', 1n), refusal);
    assert.throws(() => equalString('a', 1n), refusal);
    assert.throws(() => equalString(1n, 'a'), refusal);
    for (const stranger of strangers) {
        for (const equal of [equalInt, equalFloat, equalChar, equalString]) {
            assert.throws(() => equal(stranger, stranger), refusal);
        }
    }
}

// ORDER

// ltInt orders two Ints, past the range a number is exact over
export function ltIntOrdersTwoInts() {
    assert.equal(ltInt(3n, 5n), true);
    assert.equal(ltInt(5n, 5n), false);
    assert.equal(ltInt(5n, 3n), false);
    assert.equal(ltInt(-(2n ** 63n), 2n ** 63n - 1n), true);
    assert.equal(ltInt(2n ** 53n, 2n ** 53n + 1n), true);
}

// ltFloat orders two Floats, and a nan is unordered against everything
// (docs/spec/evaluation-semantics.md#numbers)
export function ltFloatOrdersTwoFloatsAndNotNan() {
    assert.equal(ltFloat(1.5, 2.5), true);
    assert.equal(ltFloat(2.5, 2.5), false);
    assert.equal(ltFloat(2.5, 1.5), false);
    assert.equal(ltFloat(-Infinity, Infinity), true);
    assert.equal(ltFloat(-0, 0), false);
    assert.equal(ltFloat(0, -0), false);
    assert.equal(ltFloat(NaN, 1.5), false);
    assert.equal(ltFloat(1.5, NaN), false);
    assert.equal(ltFloat(NaN, NaN), false);
    assert.equal(ltFloat(-Infinity, NaN), false);
}

// ltChar and ltString order by code point, not by the UTF-16 units `<` compares:
// U+10000 is two units, both above U+E000's one
export function ltCharAndStringOrderByCodePoint() {
    assert.equal(ltChar('a', 'b'), true);
    assert.equal(ltChar('b', 'a'), false);
    assert.equal(ltChar('a', 'a'), false);
    assert.equal(ltChar('', '\u{10000}'), true);
    assert.equal(ltChar('\u{10000}', ''), false);
    assert.equal(ltChar('\u{1F600}', '\u{10000}'), false);
    assert.equal(ltString('abc', 'abd'), true);
    assert.equal(ltString('abc', 'abc'), false);
    assert.equal(ltString('ab', 'abc'), true);
    assert.equal(ltString('abc', 'ab'), false);
    assert.equal(ltString('', 'a'), true);
    assert.equal(ltString('a', 'a\u{10000}'), true);
    assert.equal(ltString('a\u{10000}', 'a'), false);
    assert.equal(ltString('\u{10000}a', '\u{10000}b'), true);
}

// every ordering refuses what is not of its type
export function orderingRefusesAValueOfAnotherType() {
    assert.throws(() => ltInt(1, 2), refusal);
    assert.throws(() => ltInt(1n, 2), refusal);
    assert.throws(() => ltFloat(1n, 2n), refusal);
    assert.throws(() => ltChar('ab', 'cd'), refusal);
    assert.throws(() => ltString(1n, 'a'), refusal);
    for (const stranger of strangers) {
        for (const lt of [ltInt, ltFloat, ltChar, ltString]) {
            assert.throws(() => lt(stranger, stranger), refusal);
        }
    }
}

// APPEND

// appendString concatenates two strings
export function appendStringConcatenatesTwoStrings() {
    assert.equal(appendString('ab', 'cd'), 'abcd');
    assert.equal(appendString('', ''), '');
    assert.equal(appendString('a', ''), 'a');
}

// appendString refuses a value that is not a string, naming it
export function appendStringRefusesWhatIsNotAString() {
    assert.throws(() => appendString('ab', 1n), refusal);
    assert.throws(() => appendString(1n, 'ab'), refusal);
    assert.throws(() => appendString('ab', Red), /a value of the user-defined constructor Red/);
    for (const stranger of strangers) {
        assert.throws(() => appendString(stranger, stranger), refusal);
    }
}
