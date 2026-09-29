// The companion of `Effects.zel`: `boom` throws, so a module that calls it as it loads aborts.

export function boom(n) {
  throw new Error(`load boom ${n}`);
}
