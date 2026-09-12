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
    let repo_root = manifest_dir
        .parent()
        .and_then(Path::parent)
        .expect("CARGO_MANIFEST_DIR is tools/spec-site under the repo root");

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
            .expect("a `.md` file has a stem")
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
        let page = chapter_page(&titleize(&stem), &body);
        fs::write(
            out_dir.join("spec").join(format!("{}.html", out_name)),
            page,
        )
        .map_err(io_err(format!("writing site/spec/{}.html", out_name)))?;
    }

    Ok(())
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

/// Wrap one chapter's rendered body in the page shell every chapter shares: a link
/// back to the site, the shared stylesheet, and a `<title>` naming the chapter.
fn chapter_page(title: &str, body: &str) -> String {
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
         <nav class=\"site-nav\"><a href=\"../\">Zelkova</a> / <a href=\"./\">Specification</a></nav>\n\
         <main class=\"chapter\">\n\
         {body}\n\
         </main>\n\
         </body>\n\
         </html>\n",
        title = title,
        body = body,
    )
}
