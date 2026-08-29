# SITE-1 · Publish a landing page and the rendered spec alongside the rustdoc on GitHub Pages

**Sizing:** large (a workflow restructure, a new landing page, and a markdown→HTML renderer
for `docs/spec/` that has to invent its own `expect=` styling — none of which exists today).

**Location:** `.github/workflows/rustdoc.yml`; new files under a to-be-decided site source
directory (see Approach); `docs/spec/INDEX.md` and its chapter files, as the renderer's input.

**Problem:** the only thing published to GitHub Pages today is `cargo doc`'s output, deployed
by `.github/workflows/rustdoc.yml` straight from `target/doc`:

```yaml
- name: Build Documentation
  run: cargo doc --all --no-deps

- name: Deploy Docs
  uses: peaceiris/actions-gh-pages@v3
  with:
    github_token: ${{ secrets.GITHUB_TOKEN }}
    publish_branch: gh-pages
    publish_dir: ./target/doc
    force_orphan: true
```

Two consequences follow from that:

1. There is no landing page. The only way to reach the docs is to already know
   `https://francois.monniot.eu/zelkova-lang/zelkova_lang/` — `cargo doc`'s crate-root index,
   with no framing of what Zelkova is or where else to look.
2. `docs/spec/` — the normative language specification, checked chapter-by-chapter against the
   compiler by `cargo test --test spec` (`docs/spec/INDEX.md`) — is not published anywhere. It
   only exists as markdown in the repo.
3. `force_orphan: true` with `publish_dir: ./target/doc` means the workflow can only ever
   publish exactly one directory tree, replaced wholesale on every push to `main`. Adding a
   landing page or a rendered spec means assembling a combined site directory (rustdoc output
   plus the new pages) *before* the deploy step, not adding a second deploy step — two
   `peaceiris/actions-gh-pages` runs with `force_orphan: true` in the same workflow would each
   wipe out what the other just published.

**Wanted:** a landing page at the site root that frames the project and links to the rustdoc
and the spec, and `docs/spec/`'s chapters rendered as HTML on the same site — each chapter's
```` ```zel ```` blocks styled by their `expect=` tag (`docs/spec/INDEX.md`'s vocabulary:
`ok`, `parse-error[:Reason]`, `canonical-error:Variant`, `unimplemented`, `fragment`) so a
reader can see at a glance which examples the compiler accepts today and which are aspirational.

**Approach — open design questions, resolve before implementing:**

1. **Markdown → HTML for the spec.** Nothing in the repo does this today (`find . -iname
   "*mdbook*" -o -iname "book.toml"` turns up nothing, and `Cargo.toml` has no markdown or
   site-generation dependency). Candidates: a small custom script (Rust, reusing
   `tests/spec.rs`'s existing block-parsing logic so the `expect=` tag is parsed the same way
   once rather than twice; or any language available in the CI image) against a markdown crate;
   `mdBook`, which has first-class GitHub Pages support and a plugin/preprocessor model that
   could hang the `expect=` badge rendering off of; or an external static-site generator. This
   ticket does not pick one — `tests/spec.rs`'s block-parsing is worth checking first since a
   second, drifting implementation of the `expect=` grammar is exactly the failure mode
   `docs/spec/INDEX.md`'s *every example is checked* section exists to prevent.
2. **What the `expect=` styling actually looks like.** At minimum, a reader should be able to
   tell `expect=ok` from `expect=unimplemented`/`expect=parse-error*` at a glance (e.g. a
   colored badge or border — green for accepted, amber/red for rejected, with the tag's literal
   text shown so the distinction is never hidden behind color alone). `expect=fragment` blocks
   are not run by `cargo test --test spec` and should look visually distinct from both — a
   fragment carries no claim about what the compiler does with it, so styling it as either
   "works" or "doesn't" would misrepresent it.
3. **Site assembly.** However the spec gets rendered, `rustdoc.yml` needs a step that builds a
   combined output directory (landing page + rendered spec + `target/doc`, e.g. rustdoc mounted
   at `/rustdoc/` or `/api/`) and points `publish_dir` at that combined directory instead of
   `./target/doc` directly.
4. **Landing page content and stack.** Plain hand-written HTML is enough to link the two other
   pieces together; this ticket does not require a framework. What it should say (project
   description, a link to the GitHub repo, links to rustdoc and the spec) is left to whoever
   picks this up, informed by `CLAUDE.md`'s framing of the project.

**Acceptance:** pushing to `main` deploys a `gh-pages` site where the root serves a landing
page linking to both the rustdoc (still reachable, at whatever path it's mounted at) and the
rendered spec; each spec chapter's `zel` blocks are visually tagged by their `expect=` value,
verified by eye against at least one chapter that has a block of each tag currently in use
(`docs/spec/lexical-structure.md` has `ok`, several `parse-error*` variants, and
`unimplemented`; `docs/spec/js-interop.md` or `layout.md` for `fragment`, if either uses it —
otherwise add one). No second `force_orphan` deploy step; a single `actions-gh-pages` step
publishes the combined directory.
