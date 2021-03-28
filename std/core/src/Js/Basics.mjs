

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

export function remainderBy(a, b) { return b % a }

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
export function modBy(modulus, x) {
  let answer = x % modulus;
  return modulus === 0
		? __Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
}

// MORE MATH

export function toFloat(x) { return x }
export function truncate(n) { return n | 0 }
export function isInfinite(n) { return n === Infinity || n === -Infinity }

export const ceiling = Math.ceil;
export const floor = Math.floor;
export const round = Math.round;
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
