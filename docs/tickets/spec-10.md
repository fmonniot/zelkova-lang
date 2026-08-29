# SPEC-10 · Write the Packages and source layout chapter

**Sizing:** medium. The manifest is entirely undesigned (see Problem), so part of this ticket's
work is a real design decision, not just documentation.

**Location:** `src/compiler/source/mod.rs` — `load_package_sources`;
`src/compiler/source/files.rs` — `SourceFile`, `SourceFile::load`/`load_private`,
`SourceFileId`; `src/compiler/mod.rs` — `PackageName` (`author`/`project`), `ModuleName`, and
its `// TODO Ultimately we will pass a manifest content instead of a raw path` comment.

**Grounding note:** the above came from one quick pass done only to scope this ticket, not from
`write-spec-chapter`'s Step 2 probing. Treat it as a lead to re-verify, not as settled — and
don't let this ticket's Approach cap what the chapter ends up covering. Steps 1–2 and Step 4
(design questions) are what actually decide that, especially here: the manifest shape is
undesigned, not merely undocumented.

**Problem:** the package directory, the `zelkova.json` manifest, and what a package boundary
means for visibility have never been written down — and the manifest does not exist at all
yet, beyond two `TODO`-flavoured comments in `src/compiler/mod.rs`. This chapter may also touch
`LANG-6` (a module's declared name is unrelated to the file it lives in), which looks like it
sits at the boundary between this chapter and the already-written Modules chapter — confirm
that overlap while drafting rather than assuming it.

**Approach:** follow `write-spec-chapter` in full. Document what exists today (directory walk,
path-to-module-name derivation) as settled; bring the `zelkova.json` manifest shape and
package-boundary visibility to the language owner as design questions rather than assuming an
answer from this ticket; cross-link `LANG-6` only if drafting confirms the overlap above.

**Acceptance:** `cargo test --test spec` green, `docs/spec/packages.md` contributing its blocks
with every block tagged and each tag proven to fail, `docs/spec/README.md`'s row for this
chapter moved to `written`.
