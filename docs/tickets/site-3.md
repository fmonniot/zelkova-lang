# SITE-3 · A doc comment's link into `docs/` resolves nowhere, and nothing checks it

**Sizing:** small-to-medium. The check is small. The size is in the 144 links, all of which
are rewritten under two of the three options below. It grows if the links' anchors turn out
not to match what the rendered spec emits.

**Location:** every doc comment under `crates/` holding a link of the form `](../docs/…)`,
`](../../docs/…)` or `](../../../docs/…)` — 144 of them, 105 into `docs/spec/`, 35 into
`docs/decisions/` and 4 into `docs/tickets/`. `tools/spec-site/src/main.rs` — `build_site`,
which decides what the site holds. `.github/workflows/rustdoc.yml` — the *Mount the rustdoc*
step. `crates/zelkova-compiler/tests/spec.rs` — `spec_cross_references_resolve` and
`decision_cross_references_resolve`, the checks these links are outside.

**Found while** settling [`TOOL-5`](README.md)'s open decisions, which had to say what depth
these links take after the move and so had to establish what they are relative to. Left
unfixed there because `TOOL-5` moves files and changes no behaviour.

**Problem:** the links are written relative to the page rustdoc renders, with one `../` per
segment of the module's path, so that each climbs to the directory rustdoc writes into and
then names `docs/`. `crates/zelkova-compiler/src/program.rs` writes
`../../docs/spec/packages.md#programs`, and `cargo doc --no-deps -p zelkova-compiler` renders it
unchanged into `target/doc/zelkova_compiler/program/index.html`. Three things are wrong with that:

- **Nothing is there.** The link resolves to `target/doc/docs/spec/packages.md` locally and
  to `api/docs/spec/packages.md` on the published site. `cargo run -p spec-site -- --out site`
  writes `site/index.html`, `site/style.css`, `site/spec/` and `site/api/index.html`, and
  `rustdoc.yml` copies `target/doc/` into `site/api/`. Neither creates `api/docs/`. The
  chapter the link means is at `site/spec/packages.html`.
- **One link cannot be right on two pages.** rustdoc repeats a module's first paragraph as
  its summary on the parent's page. `target/doc/zelkova_compiler/index.html`, one
  directory shallower, carries `program`'s `../../docs/spec/packages.md#programs` verbatim,
  where it climbs out of rustdoc's directory altogether.
- **Nothing checks any of it.** `crates/zelkova-compiler/tests/spec.rs` walks `docs/spec/` and `docs/decisions/` and
  no `.rs` file, so a renamed chapter header or a deleted ticket file breaks a doc comment's
  link silently. `RUSTDOCFLAGS="-D warnings"` does not help: rustdoc checks intra-doc links and
  passes a relative URL through.

**Approach:** two parts. The check is settled; where the links point is not.

1. **Check them.** A test in `crates/zelkova-compiler/tests/spec.rs` beside `spec_cross_references_resolve` collects
   every `docs/` link in a doc comment under `crates/` and asserts that the file exists and, for
   a `.md` target with a fragment, that the anchor does, with the same anchor rule the two
   existing tests use. It reads the link's target after stripping whatever prefix part 2
   settles on, so it holds under any of the three options.
2. **Make them resolve.** This ticket does not pick between:
   - **Absolute URLs.** A spec link names the published chapter,
     `https://francois.monniot.eu/zelkova-lang/spec/packages.html#programs`, and a link into
     `docs/decisions/` or `docs/tickets/` names the file on GitHub, the way
     `tools/spec-site/src/render.rs`'s `GITHUB_BLOB_BASE` does for the rendered spec. Right on
     every page, the summary on a parent's page included, and right in a local `cargo doc`.
     The site's address is then written into the source 105 times, and a local build links to
     the published spec, not to the working tree's.
   - **Rewrite at publish time.** The comments keep a repository-relative spelling, and a
     step after `cargo doc` rewrites each rendered `href` to `spec/<chapter>.html` relative to
     the site root, or to GitHub, reusing `rewrite_link`'s rule. The source stays free of the
     site's address. A local `cargo doc` stays broken, and the step has to compute a different
     prefix for each page's depth, which is the summary problem again.
   - **Mount `docs/` under `api/`.** `rustdoc.yml` copies `docs/` to `site/api/docs/`, so the
     links as written land on a file. That file is raw Markdown the browser shows as text, the
     summary copies stay wrong, and a local build stays broken.

   The first is the only one that fixes all three symptoms. It is not chosen here because it
   ties the source to one host.

[`TOOL-5`](README.md) rewrote the depth of every one of these links to the rule above when the
crates split, and changed nothing else about them.

**Acceptance:**

- The new test in `crates/zelkova-compiler/tests/spec.rs` is green, and goes red when one doc comment's link is
  pointed at a chapter anchor that does not exist, and again at a ticket file that does not
  exist.
- After `cargo doc --no-deps -p zelkova-compiler`, `cargo run -p spec-site -- --out site` and
  `cp -r target/doc/. site/api/`, the link to *Programs* in
  `site/api/zelkova_compiler/program/index.html` and its copy in
  `site/api/zelkova_compiler/index.html` both resolve: to a file that exists under
  `site/`, or to an absolute URL whose path names a file the build produced or the repository
  holds.
- `cargo test --workspace` is green, and `docs/tickets/README.md`'s *Closing a ticket*
  section says that deleting a ticket a doc comment cites turns the suite red, as it already
  says for a chapter.
