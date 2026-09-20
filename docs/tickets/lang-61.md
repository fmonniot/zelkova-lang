# LANG-61 · A `git` dependency is not fetched, and nothing writes or reads `zelkova.lock`

**Sizing:** large. The two halves are one ticket because neither is useful alone: a lock file
records what a fetch resolved to, and a fetch with nothing recording it is not reproducible.
Sequence it after `LANG-14`, which has landed and is what resolves a build at all. What could
make it bigger: version-constraint solving across several packages asking for one package is
listed here as a decided rule and is a solver, not a lookup.

**Location:** `src/compiler/resolve.rs` — `Resolver::obtain`, which matches on
`manifest::Source` and returns `Error::UnsupportedSource` for `Source::Git`; `Resolver::visit`,
which walks manifests and has no lock file to consult. `src/compiler/manifest.rs` —
`Source`, `Version`, and the entry shape `dependencies` takes.

**Decided ([`docs/spec/packages.md`](../spec/packages.md),
[`docs/spec/toolchain.md`](../spec/toolchain.md)):**

- A dependency entry carries exactly one source, `git` or `path`. A `git` entry is fetched,
  kept in a cache shared by every package on the machine, and checked for identity against the
  name the entry was written under — [*Where a dependency comes
  from*](../spec/toolchain.md#where-a-dependency-comes-from).
- `zelkova.lock` sits beside `zelkova.toml`, is written by the toolchain, and records what each
  entry resolved to. The manifest says what is acceptable and the lock file says what was
  chosen, so a build is reproducible without the manifest being rewritten to pin it —
  [*Resolution and `zelkova.lock`*](../spec/toolchain.md#resolution-and-zelkovalock).
- A `path` source is read where it lies, never copied into the cache, and is **not** recorded
  as a resolved location.
- One version of each package in a build. Where several packages ask for one package under
  compatible constraints, one version satisfying all of them is chosen.

**Problem:** none of it exists. `Resolver::obtain` reads and validates a `git` entry and then
reports it as a source this compiler cannot obtain, so a build holding one compiles nothing —
the entry is named rather than silently skipped, which is the whole of what is implemented.
Nothing writes or reads a lock file and no version constraint is checked anywhere. A `path`
entry carries no constraint to check and is the only source obtained today, so every package
in a build is at whatever version its own manifest happens to declare, and two packages asking
for incompatible versions of a third is not a question the compiler can be asked.

**Approach:**

1. Fetch a `git` source into the cache, honouring `version` / `rev` / `branch` as the appendix
   describes, and check the fetched package's declared name against the key the entry was
   written under — `Resolver::obtain` already does that check for a `path` source
   (`Error::NameMismatch`) and it is the same check.
2. Resolve constraints across the whole build rather than taking each manifest's word, and
   report a set of entries with no common version the way `Error::ConflictingSources` reports a
   name with two directories.
3. Write `zelkova.lock` with what was resolved, and use it on the next build when it satisfies
   the manifest. `path` entries are not recorded.

**What is not in this ticket:** vendoring, offline builds, publishing, and the cache's own
eviction policy. Each is its own **Provisional:** section of the appendix, and none of them is
needed for a `git` dependency to compile.

**Where this came from:** filed in review of #226, which closed `LANG-14`. `LANG-14` listed
both halves under **Decided** and deliberately implemented neither; closing it without a
successor would have left them as spec text with nothing tracking them. The **Not
implemented:** paragraphs in [*Where a dependency comes
from*](../spec/packages.md#where-a-dependency-comes-from), [*One version of
each*](../spec/packages.md#one-version-of-each) and [*The compiler's
interface*](../spec/toolchain.md#the-compilers-interface) name this ticket and are deleted when
it lands.

**Acceptance:** `tests/fixtures/package_git_dependency` compiles instead of being refused, and
`a_git_dependency_is_reported_as_one_this_compiler_cannot_obtain` in `tests/pipeline.rs` —
which pins today's refusal — is replaced by a test that pins the fetch. A `zelkova.lock` is
written beside the fixture's manifest naming what the `git` entry resolved to, and a second
build reads it rather than resolving again. `Error::UnsupportedSource` has no constructor left
and goes with it. `cargo run` must still print `parsed 8 modules`, list all eight as checked,
and exit 0.
