//! The `expect=` grammar and the markdown scanning it depends on, shared by
//! `tests/spec.rs` (which runs each `zel` block through the compiler) and
//! `spec-site` (which renders chapters to HTML and needs the same badge and
//! anchor rules the harness checks by). A second, drifting implementation of
//! this grammar is the exact failure mode
//! [*Every example is checked*](https://github.com/fmonniot/zelkova-lang/blob/main/docs/spec/README.md#every-example-is-checked)
//! exists to prevent — so this crate has no dependencies of its own, and
//! nothing here runs the compiler; that stays in `tests/spec.rs`.

/// What a `zel` block's `expect=` tag asks the harness to check.
#[derive(Debug, PartialEq, Eq)]
pub enum Expect {
    Ok,
    /// `None` claims only that the parser rejected the block. `Some(reason)` also
    /// pins *why*, against the names `parse_error_reasons` (in `tests/spec.rs`)
    /// returns.
    ///
    /// Pin the reason whenever the chapter's prose describes the error the reader
    /// will see — especially when that error is a known-bad one with a ticket
    /// against it. The pin is what makes the chapter go red when the diagnostic
    /// improves, so the sentence describing the old behaviour cannot outlive it.
    ParseError(Option<String>),
    CanonicalError(String),
    /// `None` claims only that the type checker rejected the block. `Some(kind)` also
    /// pins *which* `typer::ErrorKind` it raised, against the names
    /// `error_kind_names` (in `tests/spec.rs`) returns.
    ///
    /// The bare form is for a chapter that claims only "this is a type error"; pin the
    /// kind when the prose names the diagnostic the reader will see.
    TypeError(Option<String>),
    Unimplemented,
    /// The group this block belongs to has no valid module order — its imports form
    /// a cycle — so nothing in it is canonicalized at all. Group-wide by nature:
    /// `evaluate_group` (in `tests/spec.rs`) requires every block of the group to
    /// agree on it.
    DependencyError,
    Fragment,
}

/// One ```` ```zel ```` block extracted from a chapter (or a fixture).
pub struct Block {
    /// Display label for the source file (a path, or a fixture name) — not read
    /// from disk again, just carried through to failure messages.
    pub file: String,
    /// 1-indexed line number of the opening fence (the ` ```zel... ` line itself).
    pub line: usize,
    /// The info string exactly as written, past the leading `zel` token — kept for
    /// failure messages so an unrecognised tag can be quoted back at the author.
    pub info_rest: String,
    /// `Err` when the info string carried no `expect=`, an unrecognised one, or an
    /// unrecognised key beside it.
    pub expect: Result<Expect, String>,
    /// The `package=<label>` this block belongs to, if any. Blocks sharing a label
    /// within one chapter are compiled together, in dependency order. `None` is a
    /// package of one.
    pub package: Option<String>,
    pub source: String,
}

/// Parse one info string's `key=value` tokens (already stripped of the leading `zel`).
/// `rest` is the trimmed remainder of the info string after `zel`.
///
/// Returns the expectation and the `package=` label separately, because a malformed
/// `expect=` still has to produce a `Block` — a chapter author who mistypes a tag gets
/// a named failure rather than a silently skipped example.
pub fn parse_info(rest: &str) -> (Result<Expect, String>, Option<String>) {
    let mut expect = None;
    let mut package = None;

    for token in rest.split_whitespace() {
        if let Some(value) = token.strip_prefix("expect=") {
            if expect.is_some() {
                return (
                    Err("more than one `expect=` in the info string".to_string()),
                    package,
                );
            }
            expect = Some(parse_expect(value));
        } else if let Some(value) = token.strip_prefix("package=") {
            if value.is_empty() {
                return (Err("`package=` names no label".to_string()), None);
            }
            if package.is_some() {
                return (
                    Err("more than one `package=` in the info string".to_string()),
                    None,
                );
            }
            package = Some(value.to_string());
        } else {
            return (
                Err(format!("unrecognised `{}` in the info string", token)),
                package,
            );
        }
    }

    match expect {
        Some(e) => (e, package),
        None => (Err("no `expect=` in the info string".to_string()), package),
    }
}

/// Parse the value of one `expect=` token.
pub fn parse_expect(value: &str) -> Result<Expect, String> {
    match value {
        "ok" => Ok(Expect::Ok),
        "parse-error" => Ok(Expect::ParseError(None)),
        "type-error" => Ok(Expect::TypeError(None)),
        "unimplemented" => Ok(Expect::Unimplemented),
        "dependency-error" => Ok(Expect::DependencyError),
        "fragment" => Ok(Expect::Fragment),
        _ if value.starts_with("parse-error:") => {
            let reason = &value["parse-error:".len()..];
            if reason.is_empty() {
                Err("`parse-error:` names no reason".to_string())
            } else {
                Ok(Expect::ParseError(Some(reason.to_string())))
            }
        }
        _ if value.starts_with("canonical-error:") => {
            let variant = &value["canonical-error:".len()..];
            if variant.is_empty() {
                Err("`canonical-error:` names no variant".to_string())
            } else {
                Ok(Expect::CanonicalError(variant.to_string()))
            }
        }
        _ if value.starts_with("type-error:") => {
            let kind = &value["type-error:".len()..];
            if kind.is_empty() {
                Err("`type-error:` names no kind".to_string())
            } else {
                Ok(Expect::TypeError(Some(kind.to_string())))
            }
        }
        other => Err(format!("unrecognised `expect={}`", other)),
    }
}

/// The info string of the fenced code block `line` opens, if it opens one, paired with
/// the length of its backtick run.
///
/// Two halves, and the second is the one worth stating. A fence is three or more
/// backticks — and CommonMark forbids a backtick anywhere in a *backtick* fence's info
/// string, precisely so that a line beginning with an inline code span is not read as
/// opening a block. That case is not hypothetical here: prose that names a fence writes
/// it as an inline span (```` ```zel ````), and `docs/tickets/site-1.md` already opens a
/// line that way. Without the rule such a line swallows every header and every link
/// after it, to the next backticks-only line or to the end of the file, and a checker
/// whose whole premise is that unchecked things drift silently acquires a silent-stop
/// mode of its own.
///
/// Shared by `extract_zel_blocks` and `prose_lines`, which have to agree about what
/// a fence is or the block scan and the prose scan disagree about the same file.
pub fn fence_open(line: &str) -> Option<(usize, &str)> {
    let trimmed = line.trim_start();
    let ticks = trimmed.chars().take_while(|&c| c == '`').count();
    let info = &trimmed[ticks..];
    if ticks >= 3 && !info.contains('`') {
        Some((ticks, info.trim()))
    } else {
        None
    }
}

/// Whether `line` closes a fence opened with `open` backticks: at least as many
/// backticks, and nothing else on the line.
pub fn fence_close(line: &str, open: usize) -> bool {
    let trimmed = line.trim_start();
    let ticks = trimmed.chars().take_while(|&c| c == '`').count();
    ticks >= open && trimmed[ticks..].trim().is_empty()
}

/// Extract every ```` ```zel ```` fenced block from `content`, by hand — no markdown
/// dependency, per `SPEC-1`. A fence is what `fence_open` says it is; the block runs
/// until a line whose trimmed text is *only* backticks, at least as many as the opener.
/// Only fences whose info string's first whitespace-delimited token is exactly `zel`
/// become a `Block`; anything else (`sh`, bare fences, prose) is skipped over without
/// being inspected.
pub fn extract_zel_blocks(content: &str, file_label: &str) -> Vec<Block> {
    let mut blocks = Vec::new();
    let lines: Vec<&str> = content.lines().collect();
    let mut i = 0;
    while i < lines.len() {
        let Some((fence_len, info)) = fence_open(lines[i]) else {
            i += 1;
            continue;
        };
        let open_line = i + 1; // 1-indexed
        let mut tokens = info.split_whitespace();
        let is_zel = tokens.next() == Some("zel");
        let info_rest = tokens.collect::<Vec<_>>().join(" ");

        // Find the matching close fence.
        let mut j = i + 1;
        let mut close = None;
        while j < lines.len() {
            if fence_close(lines[j], fence_len) {
                close = Some(j);
                break;
            }
            j += 1;
        }
        let close = close.unwrap_or(lines.len());

        if is_zel {
            let source = lines[(i + 1)..close.min(lines.len())].join("\n");
            let (expect, package) = parse_info(&info_rest);
            blocks.push(Block {
                file: file_label.to_string(),
                line: open_line,
                info_rest,
                expect,
                package,
                source,
            });
        }

        i = close + 1;
    }
    blocks
}

/// Every line of `content` that is not inside a fenced code block, paired with its
/// 1-indexed line number. The fence lines themselves are excluded too.
///
/// Both the header scan and the link scan need this and neither is correct without it:
/// `docs/spec/packages.md` holds a ```` ```toml ```` block whose comment lines begin
/// with `#`, and reading one of those as a markdown header would invent an anchor no
/// reader can reach. The fence rule is `fence_open`'s and `fence_close`'s, shared
/// with `extract_zel_blocks`; nothing here cares what a block contains, only where it
/// ends.
pub fn prose_lines(content: &str) -> Vec<(usize, &str)> {
    let mut lines = Vec::new();
    let mut fence: Option<usize> = None;
    for (i, line) in content.lines().enumerate() {
        match fence {
            None => match fence_open(line) {
                Some((ticks, _)) => fence = Some(ticks),
                None => lines.push((i + 1, line)),
            },
            Some(open) if fence_close(line, open) => fence = None,
            Some(_) => {}
        }
    }
    lines
}

/// GitHub's header-anchor rule: lowercase the text, drop every character that is not
/// alphanumeric, a hyphen or an underscore, and turn each remaining space into one
/// hyphen.
///
/// Punctuation vanishes without leaving a separator behind, which is the part worth
/// stating: a header whose text is `let … in` slugs to `let--in` — two hyphens, one
/// for each space around the ellipsis — and seven links across six chapters name
/// exactly that.
pub fn slugify(header: &str) -> String {
    let mut slug = String::with_capacity(header.len());
    for c in header.to_lowercase().chars() {
        if c.is_alphanumeric() || c == '-' || c == '_' {
            slug.push(c);
        } else if c == ' ' {
            slug.push('-');
        }
    }
    slug
}

/// The anchors `content`'s headers define, in document order.
///
/// A repeated header does not shadow the one before it: GitHub appends `-1`, `-2`, …
/// to the second and later spellings, so both are addressable and a link to the bare
/// slug reaches the first.
pub fn header_anchors(content: &str) -> Vec<String> {
    let mut anchors: Vec<String> = Vec::new();
    for (_, line) in prose_lines(content) {
        let trimmed = line.trim_start();
        let hashes = trimmed.chars().take_while(|&c| c == '#').count();
        if hashes == 0 || hashes > 6 || !trimmed[hashes..].starts_with(' ') {
            continue;
        }
        let base = slugify(trimmed[hashes..].trim());
        let mut anchor = base.clone();
        let mut repeat = 0;
        while anchors.contains(&anchor) {
            repeat += 1;
            anchor = format!("{}-{}", base, repeat);
        }
        anchors.push(anchor);
    }
    anchors
}
