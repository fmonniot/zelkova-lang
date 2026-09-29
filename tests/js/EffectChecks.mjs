// The JavaScript checks over the wrapper `javascript::emit` puts around an effectful facade's
// companion (src/compiler/javascript.rs, "A facade"; docs/spec/interop.md#an-effectful-facade):
// running the `Task` it builds calls the companion, and yields `Ok` for a value of the declared
// type, `Err (Threw _)` for a throw or a rejection, and `Err (Malformed _)` for a value of the
// wrong shape. `cargo test` pins the text it emits as (tests/javascript.rs) and does not run
// `node`
// (docs/decisions/dec-18.md#6--the-generated-code-is-checked-in-two-halves-and-cargo-test-does-not-run-node),
// so this file compiles its fixture itself, with the compiler built from this checkout, and
// runs the emitted `Task`s under the runtime's `$runTask`:
//
//   node --test 'tests/js/**/*.mjs'
//
// The fixture is tests/fixtures/package_effectful_facade. `Effects` is the facade and
// `Effects.mjs` its companion, which throws, rejects, answers the wrong shape and answers
// correctly on purpose; `App` is ordinary Zelkova building a `Task` from one of them.
//
// The properties of `$effect` no emitted program can reach — a continuation that throws after
// the companion returned, a step run twice — are checked beside the runtime, in
// runtime/js/tests/zelkovaChecks.mjs.
//
// Mutation-checked in `Emitter::facade_declaration`: passing the companion's result to
// `$effect` where its thunk goes (`$effect($companion$x(a), ...)`) turns every test below red
// bar none of the constant's; replacing the predicate with `null` turns the `Ok` and
// `Malformed` tests red; replacing the `()` payload's `null` with a predicate for `()` turns
// both `()` tests red.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { dirname, join } from "node:path";
import { before, describe, test } from "node:test";
import { fileURLToPath, pathToFileURL } from "node:url";

const repository = join(dirname(fileURLToPath(import.meta.url)), "..", "..");
const fixture = join(repository, "tests", "fixtures", "package_effectful_facade");
const output = join(fixture, "build", "out", "js");

let Effects;
let Companion;
let App;
let $runTask;

before(async () => {
  execFileSync("cargo", ["run", "--quiet", "--", "compile", fixture], {
    cwd: repository,
    stdio: ["ignore", "ignore", "inherit"],
  });
  const load = (...path) => import(pathToFileURL(join(output, ...path)));
  ({ $runTask } = await load("zelkova.mjs"));
  const dir = "package-effectful-facade";
  Effects = await load(dir, "Effects.mjs");
  Companion = await load(dir, "Effects.companion.mjs");
  App = await load(dir, "App.mjs");
});

describe("a Task built from an effectful facade", () => {
  test("yields Ok with the value a synchronous companion returns", async () => {
    assert.deepEqual(await $runTask(Effects.correctNow(41n)), { $: "Ok", a: 42n });
  });

  test("yields Ok with the value a companion returns after an await", async () => {
    assert.deepEqual(await $runTask(Effects.correctAfterAwait(41n)), { $: "Ok", a: 42n });
  });

  test("yields Err (Threw _) for a companion that throws, carrying the host's description", async () => {
    assert.deepEqual(await $runTask(Effects.throwsSynchronously(7n)), {
      $: "Err",
      a: { $: "Threw", a: "Error: refused 7" },
    });
  });

  test("yields Err (Threw _) for a companion whose promise rejects, carrying the host's description", async () => {
    assert.deepEqual(await $runTask(Effects.rejects(7n)), {
      $: "Err",
      a: { $: "Threw", a: "Error: rejected 7" },
    });
  });

  test("yields Err (Malformed _) naming the export for a value of the wrong shape", async () => {
    assert.deepEqual(await $runTask(Effects.wrongShape(7n)), {
      $: "Err",
      a: {
        $: "Malformed",
        a: "`Effects.wrongShape`'s companion returned a value its declared type does not admit",
      },
    });
  });
});

describe("a Task whose payload is ()", () => {
  test("yields Ok () whatever the companion returns", async () => {
    assert.deepEqual(await $runTask(Effects.discards(1n)), { $: "Ok", a: undefined });
  });

  test("still yields Err (Threw _) for a companion that throws", async () => {
    const result = await $runTask(Effects.throwsWhenDiscarding(1n));
    assert.equal(result.$, "Err");
    assert.equal(result.a.$, "Threw");
  });
});

describe("when the companion is called", () => {
  test("building a Task calls nothing, and running it calls the companion once", async () => {
    const before = Companion.calls.counted;
    const task = App.built(5n);
    assert.equal(Companion.calls.counted, before);

    assert.deepEqual(await $runTask(task), { $: "Ok", a: 5n });
    assert.equal(Companion.calls.counted, before + 1);
  });

  test("running the same Task twice calls the companion twice", async () => {
    const task = Effects.counted(1n);
    const before = Companion.calls.counted;
    await $runTask(task);
    await $runTask(task);
    assert.equal(Companion.calls.counted, before + 2);
  });

  test("a facade constant is one Task whose companion export is called each time it runs", async () => {
    const first = await $runTask(Effects.clock);
    const second = await $runTask(Effects.clock);
    assert.equal(first.$, "Ok");
    assert.equal(second.a, first.a + 1n);
  });
});
