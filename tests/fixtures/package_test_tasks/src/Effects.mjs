// The companion of `Effects.zel`. The three effects are facade constants naming a `Task`, so
// each export is a function, called each time its `Task` is run.

export function throws() {
  throw new Error("the check did not hold");
}

// `slow` finishes on a later macrotask, and `afterSlow` fails unless it has: run one at a
// time in the order they are listed, `slow` is judged before `afterSlow` starts.
let slowFinished = false;

export async function slow() {
  await new Promise((resolve) => setTimeout(resolve, 20));
  slowFinished = true;
}

export function afterSlow() {
  if (!slowFinished) {
    throw new Error("started before the test listed ahead of it finished");
  }
}

// A `String`, which no Zelkova source can write yet.
export function reason(n) {
  return `reason ${n}`;
}

// Called inside a running `Task`, an `unsafe` facade that throws aborts that run.
export function aborts(n) {
  throw new Error(`aborted on ${n}`);
}
