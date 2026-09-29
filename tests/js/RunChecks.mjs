// The JavaScript checks over the entry point `zelkova run` writes (src/compiler/program_runner.rs,
// "The entry point"): the `main.mjs` that runs the manifest's `main`, reports an abort on
// standard error with exit code 1, and reports a `Task` that never finishes. `cargo test`
// pins the text of `main.mjs` (program_runner's own tests) and runs `zelkova run` only
// against a stub `node`
// (docs/decisions/dec-18.md#6--the-generated-code-is-checked-in-two-halves-and-cargo-test-does-not-run-node),
// so this file runs `zelkova run` on its fixtures itself, with the compiler built from this
// checkout and the real `node`:
//
//   node --test 'tests/js/**/*.mjs'
//
// The fixtures are tests/fixtures/package_run_says (an effectful facade whose companion
// writes to standard output), package_run_aborts (a module whose companion throws as it
// loads) and package_run_never_settles (a `Task` nothing settles).
//
// Mutation-checked in `TAIL`: deleting `process.exitCode = 1;` from the `catch` turns the
// aborts test red; deleting the `process.on("exit", ...)` handler turns the never-settles
// test red (node's exit code 13, and no `aborted:` line).

import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { dirname, join } from "node:path";
import { describe, test } from "node:test";
import { fileURLToPath } from "node:url";

const repository = join(dirname(fileURLToPath(import.meta.url)), "..", "..");

function run(fixture) {
  return spawnSync(
    "cargo",
    ["run", "--quiet", "--", "run", join(repository, "tests", "fixtures", fixture)],
    { cwd: repository, encoding: "utf8" },
  );
}

describe("zelkova run", () => {
  test("runs main, lets an effectful facade write to stdout, and exits 0", () => {
    const result = run("package_run_says");
    assert.match(result.stdout, /^hello 1$/m);
    assert.equal(result.status, 0, result.stderr);
  });

  test("reports a module that aborts as it loads, and exits 1", () => {
    const result = run("package_run_aborts");
    assert.match(result.stderr, /^aborted: load boom 1$/m);
    assert.equal(result.status, 1, result.stderr);
  });

  test("reports a Task that never finishes, and exits 1", () => {
    const result = run("package_run_never_settles");
    assert.match(result.stderr, /^aborted: the program's Task never finished$/m);
    assert.equal(result.status, 1, result.stderr);
  });
});
