// The companion of `Effects.zel`. Each export returns the bare payload its signature
// names — never a `Result` and never a `Task` — or misbehaves on purpose. The same five sit
// in `tests/fixtures/package_effectful_facade`, where `tests/js/EffectChecks.mjs` asserts
// them on the JavaScript the compiler emits; `EffectsTests.zel` asserts them as Zelkova.

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

// Ends on a call it does not mean to return, which is a number here.
export function discards(n) {
  return [n].push(n);
}
