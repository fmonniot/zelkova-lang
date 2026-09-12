//! Renders `docs/spec/` — the normative Zelkova language specification — to the
//! static site published on GitHub Pages: a chapter per page, each `zel` block styled
//! by the `expect=` tag [`spec_doc`] gives meaning to, plus the hand-written landing
//! page under `assets/`. The rustdoc and the deploy step live in
//! `.github/workflows/rustdoc.yml`; this binary only ever writes to `--out`.
//!
//! ```sh
//! cargo run -p spec-site -- --out site
//! ```

mod render;

use std::fs;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

/// `site/api/index.html`: `cargo doc --no-deps` writes no root index for the crate it
/// documents, so a reader who trims the URL down to `/api/` needs somewhere to land.
const API_REDIRECT: &str = "<!DOCTYPE html>\n\
<meta charset=\"utf-8\">\n\
<meta http-equiv=\"refresh\" content=\"0; url=zelkova_lang/index.html\">\n\
<title>Redirecting…</title>\n\
<a href=\"zelkova_lang/index.html\">The rustdoc has moved here.</a>\n";

fn main() -> ExitCode {
    let mut args = std::env::args().skip(1);
    let mut out_dir = None;
    loop {
        match args.next() {
            Some(flag) if flag == "--out" => {
                out_dir = args.next();
            }
            Some(other) => {
                eprintln!("spec-site: unrecognised argument `{}`", other);
                return ExitCode::FAILURE;
            }
            None => break,
        }
    }
    let Some(out_dir) = out_dir else {
        eprintln!("spec-site: usage: spec-site --out <dir>");
        return ExitCode::FAILURE;
    };

    // `tools/spec-site` sits two levels under the repo root.
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let Some(repo_root) = manifest_dir.parent().and_then(Path::parent) else {
        eprintln!("spec-site: CARGO_MANIFEST_DIR is not two levels under the repo root");
        return ExitCode::FAILURE;
    };

    match build_site(
        &repo_root.join("docs/spec"),
        &manifest_dir.join("assets"),
        Path::new(&out_dir),
    ) {
        Ok(()) => ExitCode::SUCCESS,
        Err(message) => {
            eprintln!("spec-site: {}", message);
            ExitCode::FAILURE
        }
    }
}

fn io_err(context: impl Into<String>) -> impl FnOnce(std::io::Error) -> String {
    let context = context.into();
    move |e| format!("{}: {}", context, e)
}

fn build_site(spec_dir: &Path, assets_dir: &Path, out_dir: &Path) -> Result<(), String> {
    fs::create_dir_all(out_dir).map_err(io_err("creating the output directory"))?;
    fs::create_dir_all(out_dir.join("spec")).map_err(io_err("creating site/spec"))?;
    fs::create_dir_all(out_dir.join("api")).map_err(io_err("creating site/api"))?;

    for entry in fs::read_dir(assets_dir).map_err(io_err("reading assets/"))? {
        let entry = entry.map_err(io_err("reading an entry under assets/"))?;
        fs::copy(entry.path(), out_dir.join(entry.file_name()))
            .map_err(io_err("copying a static asset"))?;
    }

    fs::write(out_dir.join("api/index.html"), API_REDIRECT)
        .map_err(io_err("writing site/api/index.html"))?;

    // The sidebar every chapter page shares is not a second copy of the chapter list:
    // it is read straight out of `docs/spec/README.md`'s own "Chapters" and
    // "Appendices" tables, so the two can never drift the way a hand-maintained
    // duplicate would.
    let readme_content = fs::read_to_string(spec_dir.join("README.md"))
        .map_err(io_err("reading docs/spec/README.md"))?;
    let chapter_links = extract_table_links(&readme_content, "Chapters")?;
    let appendix_links = extract_table_links(&readme_content, "Appendices")?;

    let mut chapters: Vec<PathBuf> = fs::read_dir(spec_dir)
        .map_err(io_err("reading docs/spec/"))?
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.extension().is_some_and(|ext| ext == "md"))
        .collect();
    chapters.sort();

    for path in &chapters {
        let stem = path
            .file_stem()
            .ok_or_else(|| format!("{}: a `.md` file has no stem", path.display()))?
            .to_string_lossy()
            .into_owned();
        let label = format!("docs/spec/{}.md", stem);
        let content = fs::read_to_string(path).map_err(io_err(format!("reading {}", label)))?;
        let body = render::render_chapter(&label, &content).map_err(|e| e.to_string())?;

        let out_name = if stem == "README" {
            "index".to_string()
        } else {
            stem.clone()
        };
        let toc_headers = spec_doc::headers(&content);
        let page = chapter_page(
            &titleize(&stem),
            &out_name,
            &chapter_links,
            &appendix_links,
            &toc_headers,
            &body,
        );
        fs::write(
            out_dir.join("spec").join(format!("{}.html", out_name)),
            page,
        )
        .map_err(io_err(format!("writing site/spec/{}.html", out_name)))?;
    }

    Ok(())
}

/// One entry of the sidebar: the link text a chapter's own table row carries, and the
/// `.html` stem it targets.
struct NavLink {
    title: String,
    stem: String,
}

/// Pull the ordered list of chapter (or appendix) links out of one markdown table in
/// `docs/spec/README.md` — the same table a reader sees, so the sidebar can never name
/// a chapter, or an order, the index page itself does not.
///
/// `section_heading` is the exact text of the `## ` heading that owns the table (e.g.
/// `"Chapters"`). Fails loudly rather than rendering an empty or partial sidebar: a
/// heading this can't find, or a row whose first cell isn't a markdown link, means the
/// table's shape changed and this scanner needs to change with it.
fn extract_table_links(content: &str, section_heading: &str) -> Result<Vec<NavLink>, String> {
    let heading_line = format!("## {}", section_heading);
    let mut lines = content.lines();
    let found = lines.by_ref().any(|line| line.trim() == heading_line);
    if !found {
        return Err(format!(
            "docs/spec/README.md: no `{}` heading",
            heading_line
        ));
    }

    let rows: Vec<&str> = lines
        .take_while(|line| !line.trim_start().starts_with("## "))
        .map(str::trim)
        .filter(|line| line.starts_with('|'))
        .collect();

    // The first table row is the header (`| Chapter | Covers |`), the second is the
    // `|---|---|` separator; neither carries a link.
    let mut links = Vec::new();
    for row in rows.into_iter().skip(2) {
        let first_cell = row.trim_matches('|').split('|').next().unwrap_or("").trim();
        let (title, target) = parse_markdown_link(first_cell).ok_or_else(|| {
            format!(
                "docs/spec/README.md: expected a markdown link in the `{}` row `{}`",
                section_heading, row
            )
        })?;
        let stem = target.strip_suffix(".md").unwrap_or(&target).to_string();
        links.push(NavLink { title, stem });
    }

    if links.is_empty() {
        return Err(format!(
            "docs/spec/README.md: the `{}` table has no linked rows",
            section_heading
        ));
    }

    Ok(links)
}

/// Pull the text and target out of one `[text](target)` markdown link — the only
/// inline markup a chapter-table cell carries, so this stays a hand-rolled scan rather
/// than pulling in a markdown parser for one field.
fn parse_markdown_link(cell: &str) -> Option<(String, String)> {
    let cell = cell.trim();
    let text_start = cell.find('[')?;
    let text_end = text_start + cell[text_start..].find(']')?;
    let text = cell[text_start + 1..text_end].to_string();

    let rest = &cell[text_end + 1..];
    let target_start = rest.find('(')?;
    let target_end = target_start + rest[target_start..].find(')')?;
    let target = rest[target_start + 1..target_end].to_string();

    Some((text, target))
}

/// A chapter's file stem, as a title: `type-classes` becomes `Type classes`, and the
/// index page's own stem, `README`, becomes `Specification`.
fn titleize(stem: &str) -> String {
    if stem == "README" {
        return "Specification".to_string();
    }
    let mut words = stem.split('-');
    let mut title = String::new();
    if let Some(first) = words.next() {
        let mut chars = first.chars();
        if let Some(c) = chars.next() {
            title.extend(c.to_uppercase());
        }
        title.push_str(chars.as_str());
    }
    for word in words {
        title.push(' ');
        title.push_str(word);
    }
    title
}

/// Wrap one chapter's rendered body in the page shell every chapter shares: the
/// sidebar (site home link, the standing chapter/appendix nav, and this chapter's own
/// table of contents), the shared stylesheet, and a `<title>` naming the chapter.
///
/// `out_name` is the chapter's own `.html` stem — `chapter_links` and
/// `appendix_links` mark whichever of their entries has that stem `current`.
fn chapter_page(
    title: &str,
    out_name: &str,
    chapter_links: &[NavLink],
    appendix_links: &[NavLink],
    toc_headers: &[spec_doc::Header],
    body: &str,
) -> String {
    let checkbox_id = format!("sidebar-toggle-{}", out_name);
    let sidebar = render_sidebar(
        &checkbox_id,
        out_name,
        chapter_links,
        appendix_links,
        toc_headers,
    );

    format!(
        "<!DOCTYPE html>\n\
         <html lang=\"en\">\n\
         <head>\n\
         <meta charset=\"utf-8\">\n\
         <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n\
         <title>{title} — Zelkova specification</title>\n\
         <link rel=\"stylesheet\" href=\"../style.css\">\n\
         </head>\n\
         <body>\n\
         <div class=\"page-shell\">\n\
         <div class=\"sidebar\">\n\
         {sidebar}\
         </div>\n\
         <main class=\"chapter\">\n\
         {body}\n\
         </main>\n\
         </div>\n\
         </body>\n\
         </html>\n",
        title = title,
        sidebar = sidebar,
        body = body,
    )
}

/// The sidebar's full contents: the mobile toggle, the site home link, the
/// "Specification" section (the index page and the conventions chapter — the two
/// chapters no table lists, since neither is itself a chapter or an appendix), the
/// "Chapters" and "Appendix" sections from `docs/spec/README.md`'s own tables, and,
/// when `toc_headers` has any level 2 or 3 header, an "On this page" section built
/// from them.
fn render_sidebar(
    checkbox_id: &str,
    current_stem: &str,
    chapter_links: &[NavLink],
    appendix_links: &[NavLink],
    toc_headers: &[spec_doc::Header],
) -> String {
    let specification_links = [
        NavLink {
            title: "Overview".to_string(),
            stem: "index".to_string(),
        },
        NavLink {
            title: "Conventions".to_string(),
            stem: "conventions".to_string(),
        },
    ];

    let mut html = String::new();
    html.push_str(&format!(
        "<input type=\"checkbox\" id=\"{id}\" class=\"sidebar-toggle-input\">\n\
         <label for=\"{id}\" class=\"sidebar-toggle-label\"></label>\n\
         <div class=\"sidebar-inner\">\n\
         <a href=\"../\" class=\"sidebar-home\">Zelkova</a>\n\
         <span class=\"sidebar-label\">Specification</span>\n\
         <ul>\n{specification}</ul>\n\
         <span class=\"sidebar-label\">Chapters</span>\n\
         <ul>\n{chapters}</ul>\n\
         <span class=\"sidebar-label\">Appendix</span>\n\
         <ul>\n{appendices}</ul>\n",
        id = checkbox_id,
        specification = nav_list_items(&specification_links, current_stem),
        chapters = nav_list_items(chapter_links, current_stem),
        appendices = nav_list_items(appendix_links, current_stem),
    ));

    let toc = toc_list_items(toc_headers);
    if !toc.is_empty() {
        html.push_str("<span class=\"sidebar-label\">On this page</span>\n<ul>\n");
        html.push_str(&toc);
        html.push_str("</ul>\n");
    }

    html.push_str("</div>\n");
    html
}

/// One `<li><a>` per nav link, `current` marking whichever targets `current_stem`.
fn nav_list_items(links: &[NavLink], current_stem: &str) -> String {
    let mut html = String::new();
    for link in links {
        let current = if link.stem == current_stem {
            " class=\"current\""
        } else {
            ""
        };
        html.push_str(&format!(
            "<li><a href=\"{stem}.html\"{current}>{title}</a></li>\n",
            stem = link.stem,
            current = current,
            title = render::escape_html(&link.title),
        ));
    }
    html
}

/// One `<li><a>` per level 2 or 3 header, in document order — a level 3 header gets
/// `class="nested"` so the stylesheet can indent it under the level 2 header before it.
/// A chapter's single level 1 title, and any deeper header (none exist in `docs/spec/`
/// today), are left out.
fn toc_list_items(headers: &[spec_doc::Header]) -> String {
    let mut html = String::new();
    for header in headers {
        if header.level != 2 && header.level != 3 {
            continue;
        }
        let nested = if header.level == 3 {
            " class=\"nested\""
        } else {
            ""
        };
        html.push_str(&format!(
            "<li{nested}><a href=\"#{anchor}\">{text}</a></li>\n",
            nested = nested,
            anchor = header.anchor,
            text = render::escape_html(&header.text),
        ));
    }
    html
}
