# TEST-5 · Two `manifest` unit tests can be handed the same temporary directory, so the suite fails intermittently

**Sizing:** small. It changes one test helper. What could make it bigger is finding that a
third helper shares the scheme (see **Location**).

**Location:** `crates/zelkova-compiler/src/manifest.rs` — the test module's `tempdir()` and its `TempDir`,
whose `Drop` runs `remove_dir_all`. `crates/zelkova-js/src/output.rs` — the test module's `fresh_dir`,
which builds its name the same way and is not affected in practice (see **Problem**). A search
of `src/`, `tests/` and `tools/` for `SystemTime` found only these two.

**Problem:** `tempdir()` names its directory `zelkova-manifest-test-<pid>-<nanoseconds since
the epoch>`. The tests of that module run on parallel threads of one process, so the pid is the
same and the timestamp is the only thing telling two calls apart. On the macOS host this was
found on, `SystemTime::now()` advances in steps of 1000 ns. Eight threads taking 20,000
readings each produced 1,000 to 1,800 distinct values and saw almost every one of them more
than once. Two tests that call `tempdir()` within the same microsecond therefore share a path.
One creates its manifest there, and the other's `Drop` removes the whole directory from under
it, or writes its own manifest over it.

That is the intermittent failure. On `main`, 15 runs of `cargo test --lib manifest` failed
twice, on a different test each time:

```
test compiler::manifest::tests::an_unknown_key_is_rejected ... FAILED
test compiler::manifest::tests::missing_manifest_names_the_file ... FAILED
```

Full `cargo test --workspace` runs during the review of the `LANG-69` PR failed
`illegal_name_is_rejected` once and `a_bare_version_string_is_malformed` once, and each passed
on rerun. Four tests in one module failing at random is what a shared directory looks like,
and the timestamp collision above is the mechanism. The failing assertions were not read to
confirm that each one is this and not something else.

`output.rs`'s `fresh_dir` puts the test's own name in the path, so two *different* tests cannot
collide. It would still collide if a test called it twice with one name.

Found while reviewing the PR for `LANG-69`. Unrelated to it, and left alone there.

**Approach:**

1. Give `tempdir()` a suffix nothing can repeat within a process: a `static AtomicUsize`
   counter, incremented on every call, in the name beside the pid. The timestamp can stay,
   or go.
2. Decide on purpose whether `fresh_dir` takes the counter too. It is not failing, and one
   scheme in two places is easier to keep true than two. The ticket does not decide it.

**Acceptance:**

- A shell loop running `cargo test --lib manifest` 200 times passes every run. Run the same
  loop before the change and record how many of the 200 failed.
- Neutralise it: make the counter return a constant, and the loop fails again.
- `cargo test --workspace` is otherwise unchanged and does not invoke `node`.
