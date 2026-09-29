// The companion of `Effects.zel`. Each export returns the bare payload its signature
// names — never a `Result` and never a `Task` — or misbehaves on purpose, for the wrapper
// the compiler emits around it to catch.

export function throwsSynchronously(n) {
  throw new Error(`refused ${n}`);
}

export async function rejects(n) {
  throw new Error(`rejected ${n}`);
}

// A string where the signature says `Int`.
export function wrongShape(n) {
  return String(n);
}

export async function correctAfterAwait(n) {
  await Promise.resolve();
  return n + 1n;
}

export function correctNow(n) {
  return n + 1n;
}

// Ends on a call it does not mean to return, which is a number here.
export function discards(n) {
  return [n].push(n);
}

export function throwsWhenDiscarding(n) {
  throw new Error(`refused ${n}`);
}

// Counts how often it is called, so a check can tell building a `Task` from running one.
export const calls = { counted: 0, clock: 0 };

export function counted(n) {
  calls.counted += 1;
  return n;
}

// A facade constant naming a `Task` exports a function.
export function clock() {
  calls.clock += 1;
  return BigInt(calls.clock);
}
