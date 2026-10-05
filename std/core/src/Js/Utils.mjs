// The companion behind `Js.Utils` (`Utils.zel`): the primitives the instances of `Eq`,
// `Comparable` and `Appendable` in `Basics.zel` forward to, one per scalar type.
//
// Nothing here walks a value's structure. A union or a tuple is compared by the
// instance its type declares, written or derived in Zelkova, which hands this file
// only the scalars it ends in. What is left is four equalities, four orderings and
// an append, each exported under the name of the type it handles.
//
// Every export refuses a value that is not of its type. The facades are the package's
// boundary, and a boundary that trusts its caller is one bad emission away from
// handing `<` two things that have no order. The check is by the representation
// `docs/spec/interop.md#which-types-may-cross-the-boundary` gives each type: an `Int`
// is a `bigint`, a `Float` a number, a `Char` a string of one character and a
// `String` a string.

// Names a value the way the errors below talk about values, so a failure says what
// it was handed rather than only that it refused.
function _Utils_describe(v) {
    if (v === null) {
        return 'null';
    }
    if (Array.isArray(v)) {
        return 'an array';
    }
    if (typeof v === 'object') {
        return '$' in v
            ? 'a value of the user-defined constructor ' + String(v.$)
            : 'an object';
    }
    return 'a ' + typeof v;
}

function _Utils_isInt(v) { return typeof v === 'bigint'; }
function _Utils_isFloat(v) { return typeof v === 'number'; }
function _Utils_isString(v) { return typeof v === 'string'; }

// A `Char` is one code point, which is one UTF-16 unit or, above U+FFFF, a pair.
function _Utils_isChar(v) {
    return typeof v === 'string'
        && (v.length === 1 || (v.length === 2 && v.codePointAt(0) > 0xFFFF));
}

// The function `operation` over two values `admits` accepts, and a throw naming both
// arguments for anything else.
function _Utils_binary(name, type, admits, operation) {
    return function (x, y) {
        if (!admits(x) || !admits(y)) {
            throw new Error(
                name + ': can only be given two ' + type + 's, but was given ' +
                _Utils_describe(x) + ' and ' + _Utils_describe(y)
            );
        }
        return operation(x, y);
    };
}


// EQUALITY

// `===` is IEEE 754's equality on two numbers, which is the one place structural
// equality is not the everyday one: `nan` equals nothing, itself included, and `0`
// equals `-0` (docs/spec/evaluation-semantics.md#what-structural-equality-computes).
function _Utils_same(x, y) { return x === y; }

export const equalInt = _Utils_binary('equalInt', 'Int', _Utils_isInt, _Utils_same);
export const equalFloat = _Utils_binary('equalFloat', 'Float', _Utils_isFloat, _Utils_same);
export const equalChar = _Utils_binary('equalChar', 'Char', _Utils_isChar, _Utils_same);
export const equalString = _Utils_binary('equalString', 'String', _Utils_isString, _Utils_same);


// ORDER

// `<` on two strings compares UTF-16 units, which puts a character above U+FFFF
// (two units, each at least 0xD800) before U+E000 to U+FFFF. A `Char` is a code
// point, and so is each element of a `String`, so the units are put back in code
// point order: a surrogate sorts above every other unit, as the pair it belongs to
// is above every unit it could be compared with.
function _Utils_lessByCodePoint(x, y) {
    var shortest = Math.min(x.length, y.length);
    for (var i = 0; i < shortest; i++) {
        var a = x.charCodeAt(i);
        var b = y.charCodeAt(i);
        if (a !== b) {
            if (a >= 0xD800 && b >= 0xD800) {
                a = a >= 0xE000 ? a - 0x800 : a + 0x2000;
                b = b >= 0xE000 ? b - 0x800 : b + 0x2000;
            }
            return a < b;
        }
    }
    return x.length < y.length;
}

// `Basics.zel` writes every other comparison over these and the equalities above;
// how a `nan` is ordered is decided there, on `Comparable`.
export const ltInt = _Utils_binary('ltInt', 'Int', _Utils_isInt, function (x, y) { return x < y; });
export const ltFloat = _Utils_binary('ltFloat', 'Float', _Utils_isFloat, function (x, y) { return x < y; });
export const ltChar = _Utils_binary('ltChar', 'Char', _Utils_isChar, _Utils_lessByCodePoint);
export const ltString = _Utils_binary('ltString', 'String', _Utils_isString, _Utils_lessByCodePoint);


// APPEND

export const appendString = _Utils_binary(
    'appendString', 'String', _Utils_isString,
    function (xs, ys) { return xs + ys; }
);
