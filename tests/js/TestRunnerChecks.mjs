// The JavaScript checks over what `zelkova test` does with a `Test` that holds a `Task`
// (src/compiler/test_runner.rs, "The entry point"): the `run.mjs` it generates runs the
// `Task`, judges the `Test` it produces, prints a failure's reason, reports an aborted run as
// errored, runs the tests one at a time in the order they are listed, and exits 1 when any
// did not pass. `cargo test` pins the text of `run.mjs` (test_runner's own tests) and does
// not run `node`
// (docs/decisions/dec-18.md#6--the-generated-code-is-checked-in-two-halves-and-cargo-test-does-not-run-node),
// so this file runs `zelkova test` on its fixture itself, with the compiler built from this
// checkout, and reads what it printed:
//
//   node --test 'tests/js/**/*.mjs'
//
// The fixture is tests/fixtures/package_test_tasks. `tests/TaskTest.zel` holds the tests;
// `src/Effects.zel` is the facade they reach a throw, a slow effect, a `String` and an abort
// through, and `src/Effects.mjs` its companion.
//
// Mutation-checked in `TAIL`, each one turning the first test red: dropping the `while` loop
// over `Awaiting`; turning it into an `if`, which fails `jNestedPasses`; dropping the `catch`
// around it, which ends the run at `hAborts`; dropping the branch that prints a `Just`
// reason; and starting every test's `Task` before awaiting any, which fails `gAfterSlow`.
// Deleting `process.exitCode = 1;` turns the second red.

import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { dirname, join } from "node:path";
import { before, describe, test } from "node:test";
import { fileURLToPath } from "node:url";

const repository = join(dirname(fileURLToPath(import.meta.url)), "..", "..");
const fixture = join(repository, "tests", "fixtures", "package_test_tasks");

let run;
let lines;

before(() => {
  run = spawnSync("cargo", ["run", "--quiet", "--", "test", fixture], {
    cwd: repository,
    encoding: "utf8",
  });
  lines = run.stdout
    .split("\n")
    .filter((line) => /^(pass |FAIL |ERROR )/.test(line) || /^\d+ tests?:/.test(line));
});

describe("zelkova test over Tests that hold a Task", () => {
  test("reports each test by the verdict its Task produces, one at a time in order", () => {
    assert.deepEqual(lines, [
      "pass  TaskTest.aPasses",
      "FAIL  TaskTest.bFails",
      "pass  TaskTest.cSucceedsOnOk",
      "FAIL  TaskTest.dFailsWithTheReason: reason 4",
      "FAIL  TaskTest.eFailsOnThrew: Error: the check did not hold",
      "pass  TaskTest.fSlow",
      "pass  TaskTest.gAfterSlow",
      "ERROR TaskTest.hAborts: aborted on 1",
      "FAIL  TaskTest.iNestedFails",
      "pass  TaskTest.jNestedPasses",
      "10 tests: 5 passed, 4 failed, 1 errored",
    ]);
  });

  test("exits 1 when a test built from a Task did not pass", () => {
    assert.equal(run.status, 1, run.stderr);
  });
});
