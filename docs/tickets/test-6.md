# TEST-6 · The `tests/cli.rs` tests that run `zelkova` on a shared fixture write one `build/` between them

**Sizing:** small. It moves a helper that `tests/cli.rs` already has and points more tests at
it. What could make it bigger is a fixture whose manifest holds a path that a copy has to
rewrite.

**Location:** `tests/cli.rs` — `compile_defaults_to_current_directory` and
`compile_routes_explicit_dir_to_compile_package`, both of which run `zelkova compile` on
`tests/fixtures/package_checks` and then `remove_dir_all` its `build/`;
`test_on_a_package_with_no_tests_exits_0_without_running_node` (`package_no_tests`) and
`test_without_node_on_path_fails_naming_node` (`package_test_run`), which each write and remove
their fixture's `build/`. The helper to reuse is `scratch_test_run_package`, which already gives
the tests that put a stub `node` on `PATH` a copy of `package_test_run` under
`CARGO_TARGET_TMPDIR`. `src/compiler/output.rs` — the module doc's account of `WRITE_LOCK`.

**Problem:** every one of these tests starts a `zelkova` **process**. `WRITE_LOCK` serializes
writes within a process, and its module doc records why it exists: write-then-prune "is not
safe to run twice at once", because one call's prune can remove a file the other is in the
middle of writing. Two processes are not covered by it. So two `cli.rs` tests that share a
fixture, running on parallel threads of the test binary, are the race the lock was added to
close, with no lock in front of it.

Today exactly one pair overlaps: the two `package_checks` tests. The other two tests above
write to a fixture nothing else in the file uses. The `LANG-69` PR's tests that put a stub
`node` on `PATH` were given scratch copies for this reason, so the file now handles the same
hazard two ways.

The failure has not been seen. Running `compile_defaults_to_current_directory` and
`compile_routes_explicit_dir_to_compile_package` together 60 times on `main` passed all 60. The
ticket is about a hazard that the file's own comment names for the newer tests and leaves in
the older ones, and a new test that shares a fixture with one of them would make it likely.

Found while reviewing the PR for `LANG-69`.

**Approach:** give every `cli.rs` test that runs a command which writes `build/` its own copy of
the fixture, and stop running `remove_dir_all` on a shared fixture's `build/`. Two ways, and the
ticket does not pick:

1. Generalise `scratch_test_run_package` into a helper that takes the fixture's name, copies
   `src/` and `tests/` (and whichever else the manifest needs), and rewrites the relative
   `path =` entries to absolute ones, and use it in all four tests.
2. Only move the `package_checks` pair. That closes the one overlap that exists and leaves the
   other two tests depending on being the only user of their fixture, which is true only
   until someone adds a test.

**Acceptance:**

- No test in `tests/cli.rs` runs `zelkova` with a working directory, or an argument, inside
  `tests/fixtures/`, except for a fixture the command cannot write a build for
  (`package_type_error`, whose compile fails first). `grep -n "fixture_package" tests/cli.rs`
  shows only `package_type_error` and the helper's own source of the copy.
- After `cargo test --test cli`, `find tests/fixtures -name build -type d` prints nothing.
- Each moved test is neutralised as its own doc comment already describes and still goes red.
- `cargo test --workspace` is otherwise unchanged and does not invoke `node`.
