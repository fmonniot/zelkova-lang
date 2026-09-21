

/*
Elm has a few trick to convert between curried functions and non-curried one (F2 to F9):
https://github.com/elm/compiler/blob/51e20357137ebc9c3f6136cf0a3fe21c24027f39/compiler/src/Generate/JavaScript/Functions.hs#L19-L93
For now, I'm assuming we can instead use parameter list instead. Let's see how
much more difficult it's going to make our life :)

Also note that Zelkova use JS modules instead of magic constant names. Not sure
if we will be able to keep this design decision.
It's not a language designed for high-perf or high-reach applications :)
*/


// MATH

// An `Int` reaches this file as a `BigInt`
// (docs/spec/interop.md#which-types-may-cross-the-boundary), because a
// JavaScript number is a binary64 and is exact on integers only to `2^53`.
// `Int` arithmetic wraps at 64 bits
// (docs/spec/evaluation-semantics.md#numbers) and `BigInt` is arbitrary
// precision, so an `Int` result whose arithmetic can leave the range is
// brought back into it with `BigInt.asIntN(64, ..)`, the way `| 0` used to
// bring one back into 32 bits.
//
// `add`, `sub` and `mul` back both `Int` and `Float` arithmetic: `Basics.zel`
// declares each of them `a -> a -> a` and means either. The operand's own
// JavaScript type is what tells the two apart — an `Int` is a `bigint`, a
// `Float` a number — and a `Float` keeps IEEE's answer, which is what the
// bare operator already computes.
export function add(a, b) { return typeof a === 'bigint' ? BigInt.asIntN(64, a + b) : a + b }
export function sub(a, b) { return typeof a === 'bigint' ? BigInt.asIntN(64, a - b) : a - b }
export function mul(a, b) { return typeof a === 'bigint' ? BigInt.asIntN(64, a * b) : a * b }
export function fdiv(a, b) { return a / b }

// `pow` is declared `a -> a -> a` the same way, and dispatches the same way:
// a `Float` operand keeps `Math.pow`, IEEE's own answer, and a `bigint`
// operand uses `**`, masked back into 64 bits the way `add`/`sub`/`mul` are,
// since a power can leave the range that arithmetic can't reach on its own.
//
// A negative `Int` exponent is not handled here. `2 ^ -1` has a real answer
// (`0.5`) that `Int` has no room for — unlike `n // 0`
// (docs/spec/evaluation-semantics.md#an-operation-with-no-answer), no value
// has been chosen to stand in for it, and choosing one is `LANG-66`. `**`
// itself throws on a negative `BigInt` exponent, so `pow` still throws on
// that case until `LANG-66` settles it.
export function pow(a, b) {
  return typeof a === 'bigint' ? BigInt.asIntN(64, a ** b) : Math.pow(a, b);
}

// docs/spec/evaluation-semantics.md#an-operation-with-no-answer defines
// `n // 0` to be `0`. `BigInt` division by zero throws, so the divisor is
// tested rather than the answer falling out of the arithmetic.
//
// `BigInt` division truncates toward zero, which is what `//` computes. The
// mask covers the one division that leaves the range: `-2^63 // -1` is `2^63`,
// which wraps to `-2^63`.
export function idiv(a, b) {
  if (b === 0n) {
    return 0n;
  }
  return BigInt.asIntN(64, a / b);
}

// docs/spec/evaluation-semantics.md#an-operation-with-no-answer defines
// `remainderBy 0 n` to be `0`. `%` on a zero divisor throws for a `BigInt` the
// way `/` does, so this guard is the same one `idiv` needs.
//
// A remainder is smaller in magnitude than the divisor, so it is in range by
// construction and nothing is masked here.
export function remainderBy(a, b) { return a === 0n ? 0n : b % a }

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
//
// docs/spec/evaluation-semantics.md#an-operation-with-no-answer defines
// `modBy 0 n` to be `0`.
//
// The correction adds a remainder to a modulus of the opposite sign, so it
// lands between the two and the mask never fires; it is there so that no `Int`
// arithmetic in this file stands unwrapped.
export function modBy(modulus, x) {
  if (modulus === 0n) {
    return 0n;
  }
  let answer = x % modulus;
  return ((answer > 0n && modulus < 0n) || (answer < 0n && modulus > 0n))
    ? BigInt.asIntN(64, answer + modulus)
    : answer;
}

// MORE MATH

// `Basics.zel` types `toFloat` `Int -> Float`, and a `Float` is a JavaScript
// number, not a `BigInt`. `Number` on a `BigInt` in `Int`'s admitted range
// never loses precision differently than the `2^53` limit a `Float` already
// has (`DEC-16` decision 2).
export function toFloat(x) { return Number(x) }
export function isInfinite(n) { return n === Infinity || n === -Infinity }

// docs/spec/evaluation-semantics.md#converting-a-float-to-an-int defines a
// conversion to `Int` as rounding and then wrapping into 64 bits, with `nan`
// and both infinities landing on 0.
//
// The four conversions below are that rule in two steps: a `Math` function
// that rounds the way the conversion's name says, then this helper for the
// wrap. `BigInt` throws on a value that is not an integer, and on `nan` and
// both infinities, so the non-finite cases are answered before it is reached.
function _Basics_wrapToInt(rounded) {
  if (!Number.isFinite(rounded)) {
    return 0n;
  }
  return BigInt.asIntN(64, BigInt(rounded));
}

export function truncate(n) { return _Basics_wrapToInt(Math.trunc(n)) }
export function ceiling(n) { return _Basics_wrapToInt(Math.ceil(n)) }
export function floor(n) { return _Basics_wrapToInt(Math.floor(n)) }
export function round(n) { return _Basics_wrapToInt(Math.round(n)) }
export const sqrt = Math.sqrt;
export const log = Math.log;
export const isNotANumber = isNaN;

// TRIGONOMETRY

export const pi = Math.PI;
export const e = Math.E;
export const cos = Math.cos;
export const sin = Math.sin;
export const tan = Math.tan;
export const acos = Math.acos;
export const asin = Math.asin;
export const atan = Math.atan;
export const atan2 = Math.atan2;

// BOOLEANS

export function not(bool) { return !bool }
export function and(a, b) { return a && b }
export function or(a, b) { return a || b }
export function xor(a, b) { return a !== b }
