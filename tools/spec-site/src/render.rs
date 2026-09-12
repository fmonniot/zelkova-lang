//! Renders one `docs/spec/*.md` chapter to HTML.
//!
//! Reuses `spec-doc`'s scanner rather than `pulldown-cmark`'s own fence and header
//! detection for anything the `expect=` badge or an anchor link depends on: the point
//! of sharing that crate with `tests/spec.rs` is that a chapter's badges and its
//! checked examples come from the exact same read of the file, so the two can never
//! quietly disagree about what a block or a header is.

use std::fmt;

use pulldown_cmark::{CodeBlockKind, CowStr, Event, Options, Parser, Tag, TagEnd};
use spec_doc::{extract_zel_blocks, header_anchors, Block, Expect};

const GITHUB_BLOB_BASE: &str = "https://github.com/fmonniot/zelkova-lang/blob/main/";

/// A chapter that could not be rendered: an `expect=` tag this harness cannot read, or
/// the block scan and `pulldown-cmark`'s own fence scan disagreeing about where a fence
/// is. Either is a hard failure — a silently-unstyled block is the drift this crate
/// exists to prevent.
#[derive(Debug)]
pub struct RenderError(pub String);

impl fmt::Display for RenderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Render one chapter's markdown to a full HTML fragment (no `<html>`/`<body>`
/// wrapper — the caller lays that around it).
///
/// `label` is the chapter's display name, used only in a [`RenderError`] to say which
/// file a bad block came from.
pub fn render_chapter(label: &str, content: &str) -> Result<String, RenderError> {
    let anchors = header_anchors(content);
    let zel_blocks = extract_zel_blocks(content, label);

    // A block this harness cannot read is a hard failure here exactly as it is in
    // `tests/spec.rs` — checked up front, before any HTML is produced, so the file and
    // line named in the error are the only output this run produces.
    for block in &zel_blocks {
        if let Err(reason) = &block.expect {
            return Err(RenderError(format!(
                "{}:{}: {}",
                block.file, block.line, reason
            )));
        }
    }

    let mut options = Options::empty();
    options.insert(Options::ENABLE_TABLES);

    let mut anchor_iter = anchors.into_iter();
    let mut block_iter = zel_blocks.into_iter();
    let mut events: Vec<Event> = Vec::new();
    let mut source = Parser::new_ext(content, options);

    while let Some(event) = source.next() {
        match event {
            Event::Start(Tag::Heading {
                level,
                id: _,
                classes,
                attrs,
            }) => {
                let anchor = anchor_iter.next().map(CowStr::from);
                events.push(Event::Start(Tag::Heading {
                    level,
                    id: anchor,
                    classes,
                    attrs,
                }));
            }
            Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(ref info))) if is_zel_fence(info) => {
                // The block's text was already extracted by `extract_zel_blocks`, the
                // same scan `tests/spec.rs` runs the compiler against, so the events
                // between here and the matching `End` are dropped rather than
                // re-rendered from `pulldown-cmark`'s own `Text` events.
                for ev in source.by_ref() {
                    if matches!(ev, Event::End(TagEnd::CodeBlock)) {
                        break;
                    }
                }
                let block = block_iter.next().ok_or_else(|| {
                    RenderError(format!(
                        "{}: a `zel` fence in the rendered output has no matching block \
                         from the shared scanner — the two disagree about where a fence is",
                        label
                    ))
                })?;
                events.push(Event::Html(render_zel_block(&block).into()));
            }
            Event::Start(Tag::Link {
                link_type,
                dest_url,
                title,
                id,
            }) => {
                let rewritten = rewrite_link(&dest_url);
                events.push(Event::Start(Tag::Link {
                    link_type,
                    dest_url: rewritten.into(),
                    title,
                    id,
                }));
            }
            other => events.push(other),
        }
    }

    let mut html = String::new();
    pulldown_cmark::html::push_html(&mut html, events.into_iter());
    Ok(html)
}

/// A fence opens a `zel` block when the first whitespace-delimited token of its info
/// string is exactly `zel` — the same rule [`spec_doc::extract_zel_blocks`] uses, so a
/// fence this function skips is a fence that scan skips too.
fn is_zel_fence(info: &str) -> bool {
    info.split_whitespace().next() == Some("zel")
}

/// The badge family a `zel` block's tag renders as: which visual language the reader
/// sees, of the four this feature exists to distinguish.
///
/// An exhaustive match over [`Expect`], deliberately: a new variant fails this file to
/// compile rather than rendering unstyled, the same guarantee `tests/spec.rs` gets from
/// its own explicit matches over `canonical::Error` and `typer::ErrorKind`.
fn expect_family(expect: &Expect) -> &'static str {
    match expect {
        Expect::Ok => "expect-accepted",
        Expect::ParseError(_)
        | Expect::CanonicalError(_)
        | Expect::TypeError(_)
        | Expect::DependencyError => "expect-rejected",
        Expect::Unimplemented => "expect-pending",
        Expect::Fragment => "expect-unchecked",
    }
}

/// The badge's literal text — always the tag exactly as a chapter author would write
/// it, so the distinction a reader sees never rests on colour alone.
///
/// Exhaustive for the same reason [`expect_family`] is.
fn expect_badge_text(expect: &Expect) -> String {
    match expect {
        Expect::Ok => "expect=ok".to_string(),
        Expect::ParseError(None) => "expect=parse-error".to_string(),
        Expect::ParseError(Some(reason)) => format!("expect=parse-error:{}", reason),
        Expect::CanonicalError(variant) => format!("expect=canonical-error:{}", variant),
        Expect::TypeError(None) => "expect=type-error".to_string(),
        Expect::TypeError(Some(kind)) => format!("expect=type-error:{}", kind),
        Expect::Unimplemented => "expect=unimplemented".to_string(),
        Expect::DependencyError => "expect=dependency-error".to_string(),
        Expect::Fragment => "expect=fragment".to_string(),
    }
}

/// Render one `zel` block as the badge-wrapped HTML `render_chapter` splices in place
/// of `pulldown-cmark`'s own code-block rendering.
fn render_zel_block(block: &Block) -> String {
    // Checked by `render_chapter` before any block reaches here.
    let expect = block
        .expect
        .as_ref()
        .expect("a block with an unreadable tag is rejected before rendering starts");

    let mut html = String::new();
    html.push_str("<div class=\"zel-block ");
    html.push_str(expect_family(expect));
    html.push_str("\">");
    html.push_str("<div class=\"zel-block-tags\">");
    html.push_str("<span class=\"zel-tag\">");
    html.push_str(&escape_html(&expect_badge_text(expect)));
    html.push_str("</span>");
    if let Some(package) = &block.package {
        html.push_str("<span class=\"zel-package\">package=");
        html.push_str(&escape_html(package));
        html.push_str("</span>");
    }
    html.push_str("</div>");
    html.push_str("<pre><code>");
    html.push_str(&escape_html(&block.source));
    html.push_str("</code></pre>");
    html.push_str("</div>");
    html
}

fn escape_html(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    for c in text.chars() {
        match c {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            _ => out.push(c),
        }
    }
    out
}

/// Rewrite one link destination as written in a chapter to its address on the site.
///
/// A same-directory `.md` link (a chapter, or `README.md`) becomes a sibling `.html`
/// page; anything reached only by walking `..` out of `docs/spec/` — a ticket, a
/// decision entry — becomes an absolute link to that file's GitHub blob, since the site
/// renders neither directory. An anchor carries through unchanged either way. A bare
/// `#anchor`, a scheme link (`https://…`) or a `mailto:` link is left exactly as
/// written.
fn rewrite_link(dest: &str) -> String {
    if dest.starts_with('#') || dest.contains("://") || dest.starts_with("mailto:") {
        return dest.to_string();
    }

    let (path, anchor) = match dest.split_once('#') {
        Some((path, anchor)) => (path, Some(anchor)),
        None => (dest, None),
    };

    let resolved = resolve_relative(path);

    let rewritten = match resolved.strip_prefix("docs/spec/") {
        Some(sibling) => {
            let stem = sibling.strip_suffix(".md").unwrap_or(sibling);
            let stem = if stem == "README" { "index" } else { stem };
            format!("{}.html", stem)
        }
        None => format!("{}{}", GITHUB_BLOB_BASE, resolved),
    };

    match anchor {
        Some(anchor) => format!("{}#{}", rewritten, anchor),
        None => rewritten,
    }
}

/// Resolve `path`, written relative to `docs/spec/`, to a path rooted at the repo.
///
/// Every chapter lives directly under `docs/spec/` with no subdirectories, so this
/// only ever has to walk `..` segments off that one fixed base — `../tickets/lang-47.md`
/// becomes `docs/tickets/lang-47.md`.
fn resolve_relative(path: &str) -> String {
    let mut parts: Vec<&str> = vec!["docs", "spec"];
    for segment in path.split('/') {
        match segment {
            "" | "." => {}
            ".." => {
                parts.pop();
            }
            other => parts.push(other),
        }
    }
    parts.join("/")
}

#[cfg(test)]
mod tests {
    use super::*;

    fn render(content: &str) -> String {
        render_chapter("chapter.md", content).expect("chapter should render")
    }

    /// Each of the seven `Expect` variants renders its own family class and its
    /// literal tag text — the whole point of the badge is that the family is never
    /// the only thing distinguishing two blocks.
    #[test]
    fn each_expect_variant_renders_its_own_family_and_literal_tag() {
        let cases: &[(&str, &str, &str)] = &[
            ("expect=ok", "expect-accepted", "expect=ok"),
            (
                "expect=parse-error",
                "expect-rejected",
                "expect=parse-error",
            ),
            (
                "expect=parse-error:IndentationError",
                "expect-rejected",
                "expect=parse-error:IndentationError",
            ),
            (
                "expect=canonical-error:VariantNotFound",
                "expect-rejected",
                "expect=canonical-error:VariantNotFound",
            ),
            ("expect=type-error", "expect-rejected", "expect=type-error"),
            (
                "expect=type-error:UnificationFailed",
                "expect-rejected",
                "expect=type-error:UnificationFailed",
            ),
            (
                "expect=dependency-error",
                "expect-rejected",
                "expect=dependency-error",
            ),
            (
                "expect=unimplemented",
                "expect-pending",
                "expect=unimplemented",
            ),
            ("expect=fragment", "expect-unchecked", "expect=fragment"),
        ];

        for (tag, class, text) in cases {
            let source = format!("```zel {}\nmodule Main exposing (..)\n```\n", tag);
            let html = render(&source);
            assert!(
                html.contains(&format!("zel-block {}", class)),
                "tag `{}` should render family `{}`, got: {}",
                tag,
                class,
                html
            );
            assert!(
                html.contains(&format!("<span class=\"zel-tag\">{}</span>", text)),
                "tag `{}` should render its literal text `{}`, got: {}",
                tag,
                text,
                html
            );
        }
    }

    /// A header renders the `id` `header_anchors` would give it — `let … in` among the
    /// cases, because it is the one that slugs to two hyphens rather than one.
    #[test]
    fn header_renders_the_shared_scanners_anchor() {
        let html = render("# let … in\n\nsome prose\n");
        assert!(
            html.contains("<h1 id=\"let--in\">"),
            "expected the `let--in` anchor, got: {}",
            html
        );
    }

    /// A same-directory chapter link becomes a sibling `.html` page, anchor carried
    /// through.
    #[test]
    fn sibling_chapter_link_becomes_sibling_html() {
        let html = render("[see](types.md#some-anchor)\n");
        assert!(
            html.contains("href=\"types.html#some-anchor\""),
            "got: {}",
            html
        );
    }

    /// `README.md` is the chapter directory's index, so it becomes `index.html`.
    #[test]
    fn readme_link_becomes_index_html() {
        let html = render("[see](README.md)\n");
        assert!(html.contains("href=\"index.html\""), "got: {}", html);
    }

    /// A link that walks out of `docs/spec/` becomes an absolute GitHub blob link,
    /// since the site renders neither `docs/tickets/` nor `docs/decisions/`.
    #[test]
    fn ticket_link_becomes_a_github_blob_link() {
        let html = render("[see](../tickets/lang-47.md)\n");
        assert!(
            html.contains(
                "href=\"https://github.com/fmonniot/zelkova-lang/blob/main/docs/tickets/lang-47.md\""
            ),
            "got: {}",
            html
        );
    }

    /// A decision-entry link carries its anchor through to the blob link too.
    #[test]
    fn decision_link_with_anchor_becomes_a_github_blob_link_with_anchor() {
        let html = render("[see](../decisions/dec-2.md#7-dictionaries)\n");
        assert!(
            html.contains(
                "href=\"https://github.com/fmonniot/zelkova-lang/blob/main/docs/decisions/dec-2.md#7-dictionaries\""
            ),
            "got: {}",
            html
        );
    }

    /// A bare `#anchor` link is left exactly as written.
    #[test]
    fn bare_anchor_link_is_unchanged() {
        let html = render("[see](#some-anchor)\n");
        assert!(html.contains("href=\"#some-anchor\""), "got: {}", html);
    }

    /// An absolute URL is left exactly as written.
    #[test]
    fn absolute_url_is_unchanged() {
        let html = render("[see](https://example.com/page)\n");
        assert!(
            html.contains("href=\"https://example.com/page\""),
            "got: {}",
            html
        );
    }

    /// A `zel` fence with an unrecognised tag is a hard failure that names the file
    /// and line, exactly as it is in `tests/spec.rs`.
    ///
    /// Pins: the fence below carries `expect=bogus`. Neutralised by making
    /// `is_zel_fence` return `false` unconditionally (so the block never reaches the
    /// tag check): with that change this test goes red because the block instead
    /// renders as a plain, unstyled code block and `render_chapter` returns `Ok`.
    /// Restored afterwards.
    #[test]
    fn unrecognised_tag_is_a_hard_failure_naming_file_and_line() {
        let source = "prose\n\n```zel expect=bogus\nmodule Main exposing (..)\n```\n";
        let err = render_chapter("chapter.md", source).expect_err("should fail to render");
        assert!(
            err.0.contains("chapter.md:3"),
            "error should name the file and line, got: {}",
            err.0
        );
        assert!(
            err.0.contains("bogus"),
            "error should name the unrecognised value, got: {}",
            err.0
        );
    }

    /// A non-`zel` fence renders as an ordinary code block, with no badge — the badge
    /// is `expect=`'s, and a `sh` fence makes no claim it exists to carry.
    #[test]
    fn non_zel_fence_renders_plain_with_no_badge() {
        let html = render("```sh\necho hello\n```\n");
        assert!(!html.contains("zel-block"), "got: {}", html);
        assert!(html.contains("<pre><code"), "got: {}", html);
        assert!(html.contains("echo hello"), "got: {}", html);
    }
}
