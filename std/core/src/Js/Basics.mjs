

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
export function add(a, b) { return a + b }
export function sub(a, b) { return a - b }
export function mul(a, b) { return a * b }
export function fdiv(a, b) { return a / b }
export function idiv(a, b) { return (a / b) | 0 }
export const pow = Math.pow

// docs/spec/evaluation-semantics.md#an-operation-with-no-answer defines
// `remainderBy 0 n` to be `0`, keeping the operation total the same way `idiv`
// already is: `(a / 0) | 0` is `0`.
export function remainderBy(a, b) { return a === 0 ? 0 : b % a }

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
//
// docs/spec/evaluation-semantics.md#an-operation-with-no-answer defines
// `modBy 0 n` to be `0`.
export function modBy(modulus, x) {
  if (modulus === 0) {
    return 0;
  }
  let answer = x % modulus;
  return ((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
    ? answer + modulus
    : answer;
}

// MORE MATH

export function toFloat(x) { return x }
export function truncate(n) { return n | 0 }
export function isInfinite(n) { return n === Infinity || n === -Infinity }

// docs/spec/evaluation-semantics.md#converting-a-float-to-an-int defines a
// conversion to `Int` as rounding and then wrapping into 32 bits, with `nan`
// and both infinities landing on 0. `Math.ceil`/`Math.floor`/`Math.round`
// hand back a JavaScript number with none of that, so the `| 0` here is the
// wrap `truncate` already gets from `n | 0` doing double duty as both the
// rounding and the wrap.
export function ceiling(n) { return Math.ceil(n) | 0 }
export function floor(n) { return Math.floor(n) | 0 }
export function round(n) { return Math.round(n) | 0 }
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
