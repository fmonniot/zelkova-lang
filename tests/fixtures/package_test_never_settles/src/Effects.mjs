// The companion of `Effects.zel`. `never` returns a promise nothing settles and keeps
// nothing on the event loop alive, so a run waiting on it has no way to be woken.

export function never() {
  return new Promise(() => {});
}
