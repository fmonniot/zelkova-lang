// The companion of `Effects.zel`: `say` writes to standard output when its `Task` is run.

export function say(n) {
  console.log(`hello ${n}`);
}
