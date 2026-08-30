# Appendix: the toolchain

This appendix describes the toolchain around Zelkova rather than the language: how a
dependency is obtained, how a build is made reproducible, how a package is published, how a
package's tests are run, and what the compiler is pointed at. None of it changes what a
program means, which is why it is here and not in a chapter.

It is **not normative.** A chapter of the specification says what Zelkova is, and a claim in
one is either checked by `cargo test --test spec` or marked as a gap between the language and
the compiler. Nothing here is checked by anything: a toolchain is free to do this differently
and still compile Zelkova. What is written here is written because most of these questions have
a sensible answer once the language's own answers are fixed, and leaving them
unwritten means every reader invents that answer privately and slightly differently.

Two chapters carry the parts that *are* normative, and they are not restated below:
[Packages and source layout](packages.md) has the manifest, the two source roots, the
dependency rules and what a package boundary means, and [Modules](modules.md) has how an
import resolves.

## **Provisional:** — what it marks

Much of what follows is designed here rather than recorded from something already built. A
paragraph describing such a mechanism opens with a bolded **Provisional:**, so that
`grep -r "Provisional:" docs/spec/` finds every place the toolchain has been invented and
nothing yet holds it to account. A provisional mechanism is one to argue with and replace, not
one to build against. Keep the owner in the loop on any argue/replace action.

The marker belongs to this appendix alone. A chapter never carries it: a language question
with no settled answer is an **Open question** at the foot of the chapter, because a language
cannot have a provisional rule and still be one thing. The chapters' two other lead-ins,
**Known gap:** and **Not implemented:**, keep their meanings here — they measure the distance
between what is written and what the compiler does today.

## Where a dependency comes from

Every entry in `dependencies` and `test-dependencies` names one source, `git` or `path`, and
the manifest is the only place a package's location is written. There is no index and no
registry, so nothing has to be running anywhere for a build to resolve.

That is a deliberate trade. An index buys short manifests, one place to search, and a name
that means the same thing to everyone; it costs an operator. Sources in the manifest cost a
longer entry and give up any global notion of who owns a name, and buy a language whose
package ecosystem depends on no service at all. A registry can be added later as a third kind
of source without invalidating a manifest written today, which is why an entry names its
source explicitly rather than defaulting to one.

### `git`

**Provisional:** the URL is handed to `git` unchanged, so whatever the local `git` can clone
is a legal source: `https://`, `ssh://`, `git@host:path`, and a local repository by absolute
path. Authentication is `git`'s: a private dependency works if the user's `git` can already
fetch it, and the toolchain neither stores nor asks for a credential.

An entry says which commit it wants in one of two ways, and which two is the language's
([Packages](packages.md#where-a-dependency-comes-from)): a `version` constraint, resolved
through the repository's version tags, or a `rev`.

| Field | Names | Moves when |
|---|---|---|
| `version` | the highest tagged version satisfying the constraint | a satisfying tag is pushed, until the lock file names a commit |
| `rev` | one commit | never |

**Provisional:** resolving a `version` means listing the source's tags, and a tag is read as a
version when it is `v` followed by three dot-separated integers and nothing else. Every other
tag is invisible to resolution, so a repository is free to tag whatever else it likes. A tag
is fetched as a tag and never as a branch, so a repository with both under one name resolves
to the tag.

**Provisional:** a `rev` is a whole commit hash and is never abbreviated. An abbreviation is
unambiguous only against the repository as it stands, so one that resolves today can stop
resolving as the repository grows, and a prefix is a weaker statement about which bytes are
wanted than the hash it was taken from. Nothing is gained by shortening a field written once
and read by a machine.

**Provisional:** a toolchain offers to write a `rev` rather than making the user find one:
given a branch, it resolves that branch's tip, rewrites the entry with the commit it found,
and leaves the branch it followed and the date in a comment beside it — the provenance a bare
hash loses, put where nothing can resolve against it. Following a branch is an act with a
date, and this is where it belongs: between edits of the manifest, producing a diff, rather
than inside an entry that would move on its own afterwards.

The two differ in how firmly they pin, and the difference matters only until the lock file
exists — after that, the lock names a commit and the entry is consulted again just to check
that commit still satisfies it.

### `path`

A `path` dependency is a directory holding a package, relative to the manifest that names it.
It is for a package being developed alongside the one that depends on it: a fix can be made and
used in the same edit, with no publishing step in between.

**Provisional:** a `path` dependency is read where it lies, never copied into the cache, and is
recompiled whenever its files change. It is also not recorded in `zelkova.lock` as a resolved
location — a path is meaningful only on the machine that wrote it, and a lock file that carried
one would not be reproducible anywhere else. The version it resolves to is recorded, so the
lock file still says what was built against.

A package with a `path` dependency in `dependencies` is not publishable, because the directory
it points at will not exist for anyone else. In `test-dependencies` it is fine: nobody but this
package's own tests will ever resolve it.

### Checking a fetched package's identity

Two checks happen before a fetched package is used at all, and both are about identity rather
than correctness.

The package's own `name` must equal the key that asked for it. A manifest that fetched
`https://github.com/acme/widgets` under the key `todo-widgets` would derive the namespace
`TodoWidgets` for modules the package itself has never heard of, and a diagnostic naming the
package would name one that does not exist.

The package's own `version` must agree with how the package was found. A `git` source reached
through the tag `v1.2.4` declares `1.2.4` at that commit, or the tag and the manifest disagree
about what was published — a hard error, because everything downstream, the lock file
included, records the declared version. A `path` source and a `rev` pin name their bytes
directly rather than by version, and so are checked the other way round: whatever version the
package declares there must satisfy every constraint the build places on that name. The fact that a human chose
that commit does not make it satisfy them.

## Resolution and `zelkova.lock`

Resolution turns the manifest into the exact set of packages a build is made from. It reads
the manifest, obtains each source, reads each obtained package's manifest, and repeats, until
nothing new appears. No metadata service is involved because every package carries its own
dependency list.

The language fixes three properties of the result, and they are in
[Packages](packages.md#one-version-of-each): the graph is acyclic, at most one version of each
package is in the build, and only direct dependencies are usable. Everything else is this
appendix's.

**Provisional:** when several packages ask for one package under compatible constraints, the
highest version satisfying all of them is chosen. When their constraints have no common
version, resolution fails and the error names each constraint and the package that wrote it —
there is nothing to fall back on, because choosing one of them would build a package against a
version it declared it could not work with. An entry pinned to a `rev` has nothing chosen for
it: it contributes the version its own manifest declares, and if that version fails somebody
else's constraint, resolution fails exactly as a conflict between two constraints does.

**Provisional:** `zelkova.lock` is written beside `zelkova.toml`, by the toolchain, and is
checked into version control. It is TOML like the manifest, and records one entry per resolved
package:

```toml
lock-version = 1

[packages.acme-widgets]
version = "1.2.4"
git = "https://github.com/acme/widgets"
commit = "a3f9c1d84b2e57906fbbb0a4c1d2e3f4a5b6c7d8"
hash = "sha256-9f1c…"

[packages.acme-parser]
version = "2.4.0"
```

`commit` is what makes a `git` source reproducible: a `version` entry resolves through a tag
once and then stays on that commit until something asks it to move, whatever is tagged later. `hash` is over the fetched package's files
and is what makes it verifiable — a source that later serves different bytes under the same
commit is a mismatch and a hard error, not a silent update. An entry with a version and no
source is a `path` dependency, recorded for what it resolved to and not for where it was.

**Provisional:** a lock file is used when it satisfies the manifest and is regenerated when it
does not. Adding or tightening a constraint is what makes it not satisfy; a package already
locked at a version the new constraint admits keeps that version, so changing one dependency
does not quietly upgrade the rest. Regenerating everything on purpose is an explicit request.

A lock file is a property of the package being built and not of its dependencies. A dependency's
own `zelkova.lock` is ignored: it locked a build of *that* package, and this build has different
constraints to satisfy.

## The dependency cache

**Provisional:** a fetched package is kept in a cache shared by every package on the machine,
keyed by its source and resolved commit, so a dependency shared by two projects is fetched once.
The cache is content-addressed and treated as immutable: an entry is written once and never
modified, so a build can read one without coordinating with any other build, and deleting the
cache is always safe.

**Provisional:** nothing in a build ever writes into a cached package. Compilation output goes
beside the package being built, not beside its sources, which is what lets the cache be shared
by builds that disagree about compiler version or flags.

## Vendoring and offline builds

**Provisional:** a build that has a lock file and a populated cache needs no network. Resolution
is the only step that fetches, and a lock file that satisfies the manifest means there is nothing
to resolve. A build that would have to fetch, and is told it may not, fails naming what it would
have fetched rather than proceeding without it.

**Provisional:** vendoring copies every resolved package into a directory inside the package
being built, and rewrites nothing — the manifest keeps its `git` sources and the lock file keeps
its commits, and the vendor directory is consulted before the cache. That way a vendored tree
and a fetched one build the same thing, and un-vendoring is deleting a directory. A vendored
package is verified against the lock file's `hash` like any other, so a tree that has been edited
in place is detected rather than silently built.

## Publishing a package

**Provisional:** publishing is tagging. A package is published by pushing a commit, on which
`zelkova.toml` declares the version being published, under the tag `v` followed by that
version — a commit declaring `1.2.4` is published as `v1.2.4`, which is the tag a dependent's
`^1.2.0` finds. There is
nothing to upload and no account to hold, which is what having no registry means in the one place
it is most visible.

Three things are worth checking before that tag is pushed, and a toolchain that offers a publish
command is checking them:

- The version in `zelkova.toml` is one no existing tag already names. A published version is
  immutable — a lock file holds a commit and a hash, so a moved tag is a mismatch rather than an
  update — and re-using a number is the one mistake that cannot be corrected afterwards.
- No entry in `dependencies` has a `path` source. The directory will not exist for anyone else.
- The package compiles, and its tests pass, from a clean checkout of that commit alone.

**Provisional:** a package's discoverability is not the toolchain's problem. With sources in the
manifest, finding a package is finding a repository, by whatever means people find repositories.
That is a real cost of having no index, and it is the cost being accepted rather than one being
denied.

## Running a package's tests

`tests/` is a source root of the package and its modules are compiled exactly like `src/`'s,
against the union of `dependencies` and `test-dependencies`
([Packages](packages.md#tests)). A test module may import the package's private modules, and
nothing may import a test module.

**Provisional:** running a package's tests compiles both roots and then runs whatever the runner
finds in `tests/`. What it finds — an exposed value of a particular type, a naming convention, a
declaration form the language does not have yet — is the open question at the foot of the
[Packages chapter](packages.md#open-questions), and this section cannot be finished until it is
answered. What can be said now is what does not depend on that answer: a package's tests are run
by that package, dependencies' tests are never run, and a failing test is a non-zero exit.

**Not implemented:** there is no `tests/` root, no `test-dependencies`, and no runner
([`docs/tickets/lang-15.md`](../tickets/lang-15.md)).

## The compiler's interface

**Known gap:** the compiler today takes a source directory rather than a package root, reads no
manifest, and has the package it compiles hardcoded — `cargo run` compiles `std/core/src` and
nothing else. It prints one line per module it parsed and one per module that checked, and exits
non-zero if any phase reported an error, which is the one part of its behaviour that matches
what a toolchain needs of it ([`docs/tickets/lang-13.md`](../tickets/lang-13.md)).

**Provisional:** what it becomes is a compiler pointed at a package root — the directory holding
`zelkova.toml` — which resolves, compiles every module of `src/`, and writes its output beside
that directory rather than beside any source it read. Errors are reported with a caret in the
file they came from, in the format `codespan_reporting` already produces, and a build that
emitted any error exits non-zero and writes no output.
