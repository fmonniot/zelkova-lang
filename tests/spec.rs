//! Executable-example harness for `docs/spec/`.
//!
//! Every fenced ```` ```zel ```` block in a `docs/spec/*.md` chapter carries an
//! `expect=` tag, and this binary is what makes that tag mean something: it walks the
//! chapters, extracts each such block, and runs it through the phase its tag implies.
//! An example that does not match its tag — or carries no tag at all — is a test
//! failure, not a skip, because an unchecked example in a spec is the exact defect
//! this harness exists to prevent (`SPEC-1`; `docs/spec/README.md` carries the reasoning
//! and is the document chapter authors read).
//!
//! The vocabulary is fixed, and `docs/spec/README.md` documents it for chapter authors —
//! keep the two in step. Chapters are written against it independently of this file:
//!
//! - `zel expect=ok` — parses and canonicalizes with no errors.
//! - `zel expect=parse-error` — fails in the parser (tokenizer, layout or grammar).
//!   Which error is not pinned.
//! - `zel expect=parse-error:Reason` — the same, but the reason must match one of the
//!   names [`parse_error_reasons`] returns for the actual error (a phase, `Tokenizer`
//!   or `Layout`, or a specific one like `TabError` or `UnexpectedToken`). Pin the
//!   reason whenever a chapter's prose describes the error the reader will see. That
//!   matters most where the described diagnostic is a known-bad one with a ticket
//!   against it: rejection is the same before and after such a fix, so a bare
//!   `parse-error` stays green across it and the prose describing the old behaviour
//!   silently rots. The pin is what turns that into a red test.
//! - `zel expect=canonical-error:VariantName` — parses, then canonicalization returns
//!   a `Vec<canonical::Error>` containing at least one error of that variant.
//!   `VariantName` is matched against the real `canonical::Error` variant names.
//! - `zel expect=unimplemented` — must fail somewhere in parse-or-canonicalize, but
//!   deliberately does not pin which error: pinning would wire tokenizer/grammar
//!   internals into a prose document, and the tag's whole job is to go red the day
//!   the feature lands. On an expected failure the harness prints the actual error
//!   observed, so a human reviewing a chapter can eyeball that it failed for the
//!   intended reason.
//! - `zel expect=dependency-error` — the block's *group* (see below) fails before any
//!   module is canonicalized, because its imports have no valid order: a cycle. It is
//!   the one expectation that belongs to a group rather than to a module, so every
//!   block of the group carries it or none does.
//! - `zel expect=fragment` — an illustrative fragment, deliberately not executed. Only
//!   opt-out, and it must be explicit in the source. Counted and reported at the end
//!   of a run.
//!
//! A block may also carry `package=<label>`. Blocks sharing one label, within one
//! chapter, are one package: they are parsed together, ordered by their imports, and
//! canonicalized in that order against each other's `Interface`s — which is how a
//! chapter shows two modules at once (`SPEC-3`, settling the question
//! `docs/spec/README.md` left open). Each block keeps its **own** `expect=`, so an
//! example can show one module compiling and its importer failing. A block with no
//! `package=` is a package of one, compiled with no interfaces at all, exactly as
//! before.
//!
//! A `zel` block with no `expect=`, an unrecognised `expect=` value, or an
//! unrecognised key in its info string, is a hard failure. The extraction and evaluation logic below is written to take an arbitrary
//! path or string rather than being hardcoded to `docs/spec/`, so the harness's own
//! failure modes can be pinned against fixtures under `tests/fixtures/spec/` instead
//! of committing a deliberately-broken example to a real chapter.

use std::path::Path;

use std::collections::HashMap;

use codespan_reporting::files::SimpleFile;
use zelkova_lang::compiler::canonical;
use zelkova_lang::compiler::dependencies::ModuleWalker;
use zelkova_lang::compiler::name::Name;
use zelkova_lang::compiler::parser;
use zelkova_lang::compiler::parser::tokenizer::TokenizerErrorType;
use zelkova_lang::compiler::Interface;

mod support;

use support::test_package;

// ── Block extraction ──────────────────────────────────────────────────────────

/// What a `zel` block's `expect=` tag asks the harness to check.
#[derive(Debug, PartialEq, Eq)]
enum Expect {
    Ok,
    /// `None` claims only that the parser rejected the block. `Some(reason)` also
    /// pins *why*, against the names in [`parse_error_reasons`].
    ///
    /// Pin the reason whenever the chapter's prose describes the error the reader
    /// will see — especially when that error is a known-bad one with a ticket
    /// against it. The pin is what makes the chapter go red when the diagnostic
    /// improves, so the sentence describing the old behaviour cannot outlive it.
    ParseError(Option<String>),
    CanonicalError(String),
    Unimplemented,
    /// The group this block belongs to has no valid module order — its imports form
    /// a cycle — so nothing in it is canonicalized at all. Group-wide by nature:
    /// [`evaluate_group`] requires every block of the group to agree on it.
    DependencyError,
    Fragment,
}

/// One ```` ```zel ```` block extracted from a chapter (or a fixture).
struct Block {
    /// Display label for the source file (a path, or a fixture name) — not read
    /// from disk again, just carried through to failure messages.
    file: String,
    /// 1-indexed line number of the opening fence (the ` ```zel... ` line itself).
    line: usize,
    /// The info string exactly as written, past the leading `zel` token — kept for
    /// failure messages so an unrecognised tag can be quoted back at the author.
    info_rest: String,
    /// `Err` when the info string carried no `expect=`, an unrecognised one, or an
    /// unrecognised key beside it.
    expect: Result<Expect, String>,
    /// The `package=<label>` this block belongs to, if any. Blocks sharing a label
    /// within one chapter are compiled together, in dependency order. `None` is a
    /// package of one.
    package: Option<String>,
    source: String,
}

/// Parse one info string's `key=value` tokens (already stripped of the leading `zel`).
/// `rest` is the trimmed remainder of the info string after `zel`.
///
/// Returns the expectation and the `package=` label separately, because a malformed
/// `expect=` still has to produce a `Block` — a chapter author who mistypes a tag gets
/// a named failure rather than a silently skipped example.
fn parse_info(rest: &str) -> (Result<Expect, String>, Option<String>) {
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
fn parse_expect(value: &str) -> Result<Expect, String> {
    match value {
        "ok" => Ok(Expect::Ok),
        "parse-error" => Ok(Expect::ParseError(None)),
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
        other => Err(format!("unrecognised `expect={}`", other)),
    }
}

/// Extract every ```` ```zel ```` fenced block from `content`, by hand — no markdown
/// dependency, per `SPEC-1`. A fence is a line whose trimmed text starts with three
/// or more backticks; the block runs until a line whose trimmed text is *only*
/// backticks, at least as many as the opener. Only fences whose info string's first
/// whitespace-delimited token is exactly `zel` become a [`Block`]; anything else
/// (`sh`, bare fences, prose) is skipped over without being inspected.
fn extract_zel_blocks(content: &str, file_label: &str) -> Vec<Block> {
    let mut blocks = Vec::new();
    let lines: Vec<&str> = content.lines().collect();
    let mut i = 0;
    while i < lines.len() {
        let trimmed = lines[i].trim_start();
        let fence_len = trimmed.chars().take_while(|&c| c == '`').count();
        if fence_len < 3 {
            i += 1;
            continue;
        }
        let info = trimmed[fence_len..].trim();
        let open_line = i + 1; // 1-indexed
        let mut tokens = info.split_whitespace();
        let is_zel = tokens.next() == Some("zel");
        let info_rest = tokens.collect::<Vec<_>>().join(" ");

        // Find the matching close fence.
        let mut j = i + 1;
        let mut close = None;
        while j < lines.len() {
            let ct = lines[j].trim_start();
            let ct_fence_len = ct.chars().take_while(|&c| c == '`').count();
            if ct_fence_len >= fence_len && ct[ct_fence_len..].trim().is_empty() {
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

// ── Evaluation ─────────────────────────────────────────────────────────────────

/// The outcome of running one [`Block`].
enum Verdict {
    Pass,
    Fragment,
    Fail(String),
}

fn parse(source: &str) -> Result<parser::Module, parser::Error> {
    let file = SimpleFile::new("Spec.zel".to_string(), source.to_string());
    parser::parse(&file)
}

fn canonicalize(module: &parser::Module) -> Result<canonical::Module, Vec<canonical::Error>> {
    let interfaces = std::collections::HashMap::new();
    canonical::canonicalize(&test_package(), &interfaces, module)
}

/// The names an `expect=parse-error:<reason>` tag may pin, for one actual error.
///
/// Returns every name that matches, coarse first: a tab used for indentation is both
/// `Tokenizer` and `TabError`, so a chapter can pin either the phase that rejected the
/// block or the exact reason, depending on which one its prose actually claims.
///
/// Written as an explicit match over the real enums — `parser::Error`,
/// `TokenizerErrorType` — rather than by formatting with `Debug` and splitting the
/// string, so that adding a variant fails this file to compile rather than silently
/// producing a name no chapter can ever match.
fn parse_error_reasons(error: &parser::Error) -> Vec<&'static str> {
    match error {
        parser::Error::Tokenizer(e) => {
            let specific = match e.error.value {
                TokenizerErrorType::CharNotClosedError(_) => "CharNotClosedError",
                TokenizerErrorType::StringError => "StringError",
                TokenizerErrorType::UnicodeError => "UnicodeError",
                TokenizerErrorType::IndentationError => "IndentationError",
                TokenizerErrorType::TabError => "TabError",
                TokenizerErrorType::UnrecognizedToken { .. } => "UnrecognizedToken",
            };
            vec!["Tokenizer", specific]
        }
        parser::Error::Layout(_) => vec!["Layout", "LayoutError"],
        parser::Error::InvalidToken(_) => vec!["InvalidToken"],
        parser::Error::UnexpectedEOF { .. } => vec!["UnexpectedEOF"],
        parser::Error::UnexpectedToken { .. } => vec!["UnexpectedToken"],
        parser::Error::ExtraToken { .. } => vec!["ExtraToken"],
    }
}

/// The `canonical::Error` variant names present in `errors`, flattening `Error::Many`
/// since it is a grouping construct rather than a kind of failure a chapter would
/// tag against. Written as an explicit match rather than reaching for `Debug` and
/// string-splitting, so a new variant fails this file to compile instead of silently
/// never matching.
fn variant_names(errors: &[canonical::Error]) -> Vec<&'static str> {
    fn one(e: &canonical::Error) -> Vec<&'static str> {
        use canonical::Error::*;
        match e {
            Many(inner) => inner.iter().flat_map(one).collect(),
            ExportNotFound(..) => vec!["ExportNotFound"],
            EnvironmentErrors(..) => vec!["EnvironmentErrors"],
            InfixReferenceInvalidValue(..) => vec!["InfixReferenceInvalidValue"],
            BindingPatternsInvalidLen(..) => vec!["BindingPatternsInvalidLen"],
            NoBindings(..) => vec!["NoBindings"],
            VariableNotFound(..) => vec!["VariableNotFound"],
            AmbiguousVariables(..) => vec!["AmbiguousVariables"],
            VariantNotFound(..) => vec!["VariantNotFound"],
            AmbiguousVariants(..) => vec!["AmbiguousVariants"],
            InvalidTupleSize(..) => vec!["InvalidTupleSize"],
            MultipleBindingsUnsupported(..) => vec!["MultipleBindingsUnsupported"],
            InfixDeclared(..) => vec!["InfixDeclared"],
            TypeDeclared(..) => vec!["TypeDeclared"],
            NoTypeInBinding(..) => vec!["NoTypeInBinding"],
        }
    }
    errors.iter().flat_map(one).collect()
}

/// Run one block's source through the phases its `expect` tag implies.
fn evaluate(block: &Block) -> Verdict {
    let expect = match &block.expect {
        Ok(e) => e,
        Err(reason) => return Verdict::Fail(reason.clone()),
    };

    match expect {
        Expect::Fragment => Verdict::Fragment,
        Expect::DependencyError => Verdict::Fail(
            "`expect=dependency-error` is about a group of modules having no valid \
             import order, so it needs a `package=` label naming the other modules \
             it cycles with"
                .to_string(),
        ),
        Expect::Ok => match parse(&block.source) {
            Err(e) => Verdict::Fail(format!("expected `ok`, but the parser rejected it: {:?}", e)),
            Ok(module) => match canonicalize(&module) {
                Err(errors) => Verdict::Fail(format!(
                    "expected `ok`, but canonicalization failed: {:?}",
                    errors
                )),
                Ok(_) => Verdict::Pass,
            },
        },
        Expect::ParseError(wanted) => match parse(&block.source) {
            Ok(_) => Verdict::Fail(format!(
                "expected `{}`, but the block parsed successfully",
                expect_label(block)
            )),
            Err(e) => match wanted {
                None => Verdict::Pass,
                Some(wanted) => {
                    let found = parse_error_reasons(&e);
                    if found.contains(&wanted.as_str()) {
                        Verdict::Pass
                    } else {
                        Verdict::Fail(format!(
                            "expected the parser to reject this for `{}`, but it rejected it \
                             for {:?} ({:?}).\nIf the diagnostic was deliberately improved, the \
                             chapter's prose about it needs updating in this same change — that \
                             is what this pin is for.",
                            wanted, found, e
                        ))
                    }
                }
            },
        },
        Expect::CanonicalError(wanted) => match parse(&block.source) {
            Err(e) => Verdict::Fail(format!(
                "expected `canonical-error:{}`, but the parser rejected it before canonicalization ran: {:?}",
                wanted, e
            )),
            Ok(module) => match canonicalize(&module) {
                Ok(_) => Verdict::Fail(format!(
                    "expected `canonical-error:{}`, but the module canonicalized with no errors",
                    wanted
                )),
                Err(errors) => {
                    let found = variant_names(&errors);
                    if found.contains(&wanted.as_str()) {
                        Verdict::Pass
                    } else {
                        Verdict::Fail(format!(
                            "expected a canonical error of variant `{}`, got {:?} ({:?})",
                            wanted, found, errors
                        ))
                    }
                }
            },
        },
        Expect::Unimplemented => match parse(&block.source) {
            Err(e) => {
                println!(
                    "{}:{} (expect=unimplemented) failed in the parser, as expected: {:?}",
                    block.file, block.line, e
                );
                Verdict::Pass
            }
            Ok(module) => match canonicalize(&module) {
                Err(errors) => {
                    println!(
                        "{}:{} (expect=unimplemented) failed in canonicalization, as expected: {:?}",
                        block.file, block.line, errors
                    );
                    Verdict::Pass
                }
                Ok(_) => Verdict::Fail(
                    "expected `unimplemented`, but the block parsed and canonicalized \
                     successfully — this feature looks implemented now; update the chapter"
                        .to_string(),
                ),
            },
        },
    }
}

/// Canonicalize one module, tagging any errors with the module they came from.
///
/// [`ModuleWalker::check_in_order`] hands back one flat error list for the whole
/// package, so without the tag there is no way to say *which* block of a group failed
/// — which is the entire point of letting each block carry its own `expect=`. Written
/// as a free function rather than a closure because `check_in_order` takes a `fn`
/// pointer.
fn canonicalize_tagged(
    package: &zelkova_lang::compiler::PackageName,
    interfaces: &HashMap<Name, Interface>,
    source: &parser::Module,
) -> Result<canonical::Module, (Name, Vec<canonical::Error>)> {
    canonical::canonicalize(package, interfaces, source)
        .map_err(|errors| (source.name.clone(), errors))
}

/// Run one `package=` group: every block of it is a module of the same package, and
/// they are canonicalized in dependency order against each other's `Interface`s.
///
/// Returns one [`Verdict`] per block, in the order given. The group is compiled once;
/// each block is then judged against its own `expect=`, so an example can show a
/// module compiling and its importer failing in the same package.
///
/// Three whole-group failure modes, each reported on every block rather than on one,
/// because none of them is any single block's fault:
///
/// - a `parse-error` expectation, which a group cannot express — the group has to
///   parse before any of it can be compiled, so a rejected-source example belongs in
///   a package-less block;
/// - a block that fails to parse, which leaves the rest of the group with a module
///   missing;
/// - two blocks declaring the same module name, which would make the mapping from
///   module back to block ambiguous. (The language forbids it too — see
///   `docs/spec/modules.md` — but here it is the harness protecting its own bookkeeping.)
fn evaluate_group(blocks: &[&Block]) -> Vec<Verdict> {
    let group_failure = |reason: String| -> Vec<Verdict> {
        blocks
            .iter()
            .map(|_| Verdict::Fail(reason.clone()))
            .collect()
    };

    // A malformed tag is the block's own failure, not the group's, but it also means
    // there is no expectation to judge it against — so the group stops here and every
    // block says why.
    let mut expects = Vec::new();
    for block in blocks {
        match &block.expect {
            Ok(e) => expects.push(e),
            Err(reason) => {
                return group_failure(format!(
                    "{}:{} has a tag this harness cannot read ({}), so the package \
                     could not be compiled",
                    block.file, block.line, reason
                ))
            }
        }
    }

    if let Some(i) = expects
        .iter()
        .position(|e| matches!(e, Expect::ParseError(_)))
    {
        return group_failure(format!(
            "{}:{} expects a parse error inside a `package=` group. A group is parsed \
             as a whole before anything is compiled, so a block showing rejected \
             source has to stand on its own, without a `package=` label",
            blocks[i].file, blocks[i].line
        ));
    }

    // `expect=fragment` is an opt-out from being executed, and a group is executed as
    // a unit — so a fragment cannot sit in one.
    if let Some(i) = expects.iter().position(|e| matches!(e, Expect::Fragment)) {
        return group_failure(format!(
            "{}:{} is an `expect=fragment` inside a `package=` group. A fragment is \
             never executed and a group is compiled as a unit; drop the `package=` \
             label",
            blocks[i].file, blocks[i].line
        ));
    }

    let mut modules = Vec::new();
    for block in blocks {
        match parse(&block.source) {
            Ok(module) => modules.push(module),
            Err(e) => {
                return group_failure(format!(
                    "{}:{} failed to parse, so the whole package could not be \
                     compiled: {:?}",
                    block.file, block.line, e
                ))
            }
        }
    }

    for (i, module) in modules.iter().enumerate() {
        if let Some(j) = modules[..i].iter().position(|m| m.name == module.name) {
            return group_failure(format!(
                "{}:{} and {}:{} both declare `module {}`; a package holds one module \
                 per name",
                blocks[j].file, blocks[j].line, blocks[i].file, blocks[i].line, module.name
            ));
        }
    }

    // No files on disk behind these modules, so no `SourceFileId` for any of them:
    // an `Interface` built here carries `file: None`, and a cross-module label falls
    // back on the module under check the way it did before `ERR-5`. Nothing the
    // harness asserts on depends on that.
    let module_files = HashMap::new();
    let walker = match ModuleWalker::new(&modules, &module_files) {
        Ok(walker) => walker,
        Err(err) => {
            return blocks
                .iter()
                .zip(&expects)
                .map(|(block, expect)| match expect {
                    Expect::DependencyError => Verdict::Pass,
                    _ => Verdict::Fail(format!(
                        "expected `{}`, but the package has no valid module order and \
                         so was never compiled: {:?}",
                        expect_label(block),
                        err
                    )),
                })
                .collect()
        }
    };

    let mut interfaces: HashMap<Name, Interface> = HashMap::new();
    let (_checked, failures) = walker.check_in_order(
        &test_package(),
        &mut interfaces,
        &module_files,
        canonicalize_tagged,
    );
    let failures: HashMap<Name, Vec<canonical::Error>> = failures.into_iter().collect();

    blocks
        .iter()
        .zip(&expects)
        .zip(&modules)
        .map(|((block, expect), module)| {
            let errors = failures.get(&module.name);
            match (expect, errors) {
                (Expect::Ok, None) => Verdict::Pass,
                (Expect::Ok, Some(errors)) => Verdict::Fail(format!(
                    "expected `ok`, but canonicalization failed: {:?}",
                    errors
                )),
                (Expect::CanonicalError(wanted), None) => Verdict::Fail(format!(
                    "expected `canonical-error:{}`, but the module canonicalized with \
                     no errors",
                    wanted
                )),
                (Expect::CanonicalError(wanted), Some(errors)) => {
                    let found = variant_names(errors);
                    if found.contains(&wanted.as_str()) {
                        Verdict::Pass
                    } else {
                        Verdict::Fail(format!(
                            "expected a canonical error of variant `{}`, got {:?} ({:?})",
                            wanted, found, errors
                        ))
                    }
                }
                (Expect::Unimplemented, Some(errors)) => {
                    println!(
                        "{}:{} (expect=unimplemented) failed in canonicalization, as \
                         expected: {:?}",
                        block.file, block.line, errors
                    );
                    Verdict::Pass
                }
                (Expect::Unimplemented, None) => Verdict::Fail(
                    "expected `unimplemented`, but the block parsed and canonicalized \
                     successfully — this feature looks implemented now; update the chapter"
                        .to_string(),
                ),
                (Expect::DependencyError, _) => Verdict::Fail(
                    "expected `dependency-error`, but the package had a valid module \
                     order — nothing here forms an import cycle"
                        .to_string(),
                ),
                // Both are refused above, before anything is parsed.
                (Expect::ParseError(_), _) | (Expect::Fragment, _) => unreachable!(),
            }
        })
        .collect()
}

fn expect_label(block: &Block) -> String {
    match &block.expect {
        Ok(Expect::Ok) => "expect=ok".to_string(),
        Ok(Expect::ParseError(None)) => "expect=parse-error".to_string(),
        Ok(Expect::ParseError(Some(r))) => format!("expect=parse-error:{}", r),
        Ok(Expect::CanonicalError(v)) => format!("expect=canonical-error:{}", v),
        Ok(Expect::Unimplemented) => "expect=unimplemented".to_string(),
        Ok(Expect::DependencyError) => "expect=dependency-error".to_string(),
        Ok(Expect::Fragment) => "expect=fragment".to_string(),
        Err(_) => format!("`{}`", block.info_rest),
    }
}

// ── The real chapters ─────────────────────────────────────────────────────────

/// `cargo test --test spec`: every `zel` block under `docs/spec/` must match its tag.
///
/// Walks `docs/spec/*.md` (top level, sorted for a deterministic run order),
/// extracts every `zel` block, and evaluates it. Any failure is collected — not
/// short-circuited — so one bad block does not hide the next, and the panic message
/// names every one, with its file, line and expectation, the way `CLAUDE.md`'s
/// standing invariants ask a phase error to describe itself. `expect=fragment`
/// blocks are counted and reported at the end, per `SPEC-1`.
#[test]
fn spec_chapters_pass() {
    let manifest = std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    let spec_dir = Path::new(&manifest).join("docs/spec");

    let mut entries: Vec<_> = std::fs::read_dir(&spec_dir)
        .unwrap_or_else(|e| panic!("failed to read {:?}: {}", spec_dir, e))
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().is_some_and(|ext| ext == "md"))
        .collect();
    entries.sort_by_key(|e| e.path());

    assert!(
        !entries.is_empty(),
        "expected at least one chapter under {:?}",
        spec_dir
    );

    let mut failures = Vec::new();
    let mut pass_count = 0usize;
    let mut fragment_count = 0usize;

    for entry in entries {
        let path = entry.path();
        let content = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("failed to read {:?}: {}", path, e));
        let label = path
            .strip_prefix(&manifest)
            .unwrap_or(&path)
            .to_string_lossy()
            .to_string();

        let blocks = extract_zel_blocks(&content, &label);

        // A block with no `package=` is judged on its own; blocks sharing a label are
        // one package, compiled together and judged individually against their own
        // tags. Groups are keyed per chapter, so two chapters may reuse a label.
        let mut verdicts: Vec<Option<Verdict>> = Vec::with_capacity(blocks.len());
        let mut groups: Vec<(&str, Vec<usize>)> = Vec::new();
        for (i, block) in blocks.iter().enumerate() {
            match &block.package {
                None => verdicts.push(Some(evaluate(block))),
                Some(label) => {
                    verdicts.push(None);
                    match groups.iter_mut().find(|(l, _)| *l == label.as_str()) {
                        Some((_, members)) => members.push(i),
                        None => groups.push((label.as_str(), vec![i])),
                    }
                }
            }
        }
        for (_, members) in &groups {
            let group: Vec<&Block> = members.iter().map(|&i| &blocks[i]).collect();
            for (&i, verdict) in members.iter().zip(evaluate_group(&group)) {
                verdicts[i] = Some(verdict);
            }
        }

        for (block, verdict) in blocks.iter().zip(verdicts) {
            match verdict.expect("every block is judged exactly once") {
                Verdict::Pass => pass_count += 1,
                Verdict::Fragment => fragment_count += 1,
                Verdict::Fail(reason) => {
                    failures.push(format!(
                        "{}:{} ({}): {}",
                        block.file,
                        block.line,
                        expect_label(block),
                        reason
                    ));
                }
            }
        }
    }

    println!(
        "spec: {} block(s) passed, {} fragment(s) skipped",
        pass_count, fragment_count
    );

    assert!(
        failures.is_empty(),
        "{} spec block(s) failed their expectation:\n{}",
        failures.len(),
        failures.join("\n")
    );
}

// ── Harness self-tests ────────────────────────────────────────────────────────
//
// These prove the harness can fail, against fixtures under `tests/fixtures/spec/`
// rather than against a real chapter — `SPEC-1`'s explicit request, so a red run
// never has to be manufactured by breaking `docs/spec/` on purpose. Each fixture
// holds exactly one `zel` block, isolating the one behaviour its test pins.

fn read_fixture(name: &str) -> String {
    let manifest = std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    let path = Path::new(&manifest).join("tests/fixtures/spec").join(name);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("failed to read {:?}: {}", path, e))
}

fn only_block(name: &str) -> Block {
    let content = read_fixture(name);
    let mut blocks = extract_zel_blocks(&content, name);
    assert_eq!(
        blocks.len(),
        1,
        "fixture {} should hold exactly one zel block",
        name
    );
    blocks.remove(0)
}

/// A block with no `expect=` at all is a hard failure, not a skip.
///
/// Pins: `tests/fixtures/spec/missing_expect.md` has a bare ` ```zel ` fence around
/// an otherwise-fine module. Neutralised by making `parse_expect` return
/// `Ok(Expect::Ok)` for an empty `rest` instead of `Err(..)`: with that change this
/// test goes red because `evaluate` then actually compiles the (valid) module and
/// reports `Verdict::Pass`. Restored afterwards.
#[test]
fn block_with_no_expect_is_a_hard_failure() {
    let block = only_block("missing_expect.md");
    assert!(
        block.expect.is_err(),
        "expected extraction to reject a missing `expect=`, got {:?}",
        block.expect
    );
    match evaluate(&block) {
        Verdict::Fail(_) => {}
        _ => panic!("a block with no `expect=` must evaluate to a failure"),
    }
}

/// An `expect=` value the harness does not recognise is a hard failure.
///
/// Pins: `tests/fixtures/spec/unrecognized_expect.md` tags its block
/// `expect=bogus`. Neutralised by adding a catch-all `_ => Ok(Expect::Ok)` arm ahead
/// of `parse_expect`'s final `other => Err(..)` arm: with that change this test goes
/// red the same way as the one above. Restored afterwards.
#[test]
fn unrecognised_expect_value_is_a_hard_failure() {
    let block = only_block("unrecognized_expect.md");
    assert!(
        block.expect.is_err(),
        "expected extraction to reject `expect=bogus`, got {:?}",
        block.expect
    );
    match evaluate(&block) {
        Verdict::Fail(reason) => assert!(
            reason.contains("bogus"),
            "failure message should name the unrecognised value, got {:?}",
            reason
        ),
        _ => panic!("a block with an unrecognised `expect=` must evaluate to a failure"),
    }
}

/// `expect=ok` on a block that fails to canonicalize is a failure, not a pass.
///
/// Pins: `tests/fixtures/spec/ok_block_fails_to_compile.md` tags `expect=ok` a
/// module referencing an undefined variable. Neutralised by making the `Expect::Ok`
/// arm of `evaluate` return `Verdict::Pass` unconditionally (skipping the
/// `canonicalize` call): with that change this test goes red because the block
/// reports `Pass` despite `x = y` never resolving. Restored afterwards.
#[test]
fn ok_block_that_fails_to_compile_is_a_failure() {
    let block = only_block("ok_block_fails_to_compile.md");
    assert_eq!(block.expect, Ok(Expect::Ok));
    match evaluate(&block) {
        Verdict::Fail(reason) => assert!(
            reason.contains("ok"),
            "failure message should say what was expected, got {:?}",
            reason
        ),
        _ => panic!("an `expect=ok` block that fails to canonicalize must fail"),
    }
}

/// `expect=canonical-error:X` on a block that fails with a *different* variant is a
/// failure — pinning the wrong reason must not pass.
///
/// Pins: `tests/fixtures/spec/canonical_error_wrong_variant.md` tags
/// `expect=canonical-error:VariantNotFound` a module whose actual failure is
/// `VariableNotFound` (an undefined value, not an undefined constructor).
/// Neutralised by making the `Expect::CanonicalError` arm of `evaluate` accept any
/// non-empty `errors` regardless of `variant_names` (i.e. dropping the
/// `found.contains(&wanted.as_str())` check): with that change this test goes red
/// because the mismatched variant reports `Pass`. Restored afterwards.
#[test]
fn canonical_error_wrong_variant_is_a_failure() {
    let block = only_block("canonical_error_wrong_variant.md");
    assert_eq!(
        block.expect,
        Ok(Expect::CanonicalError("VariantNotFound".to_string()))
    );
    match evaluate(&block) {
        Verdict::Fail(reason) => {
            assert!(
                reason.contains("VariantNotFound"),
                "failure message should name what was wanted, got {:?}",
                reason
            );
            assert!(
                reason.contains("VariableNotFound"),
                "failure message should name what was actually found, got {:?}",
                reason
            );
        }
        _ => panic!("a canonical error of the wrong variant must fail, not pass"),
    }
}

/// `expect=parse-error:<reason>` must check the reason, not just that the parse failed.
///
/// This is what lets a chapter describe a *known-bad* diagnostic in prose and be forced
/// to update that prose when the diagnostic improves — see `ERR-11` and `ERR-12`, whose
/// blocks in `docs/spec/layout.md` pin the wrong-but-current error deliberately. Without
/// the reason check, both the bad and the improved diagnostic satisfy a bare
/// `parse-error`, and the stale sentence survives.
///
/// Pins: `tests/fixtures/spec/parse_error_wrong_reason.md` tags
/// `expect=parse-error:TabError` a block whose actual failure is `IndentationError`
/// (three-space indentation, no tab anywhere). Neutralised by making the
/// `Expect::ParseError` arm return `Verdict::Pass` for any `Err(_)` regardless of
/// `wanted` — i.e. reverting it to the bare pre-pin behaviour: with that change this
/// test goes red because the mismatched reason reports `Pass`. Restored afterwards.
#[test]
fn parse_error_wrong_reason_is_a_failure() {
    let block = only_block("parse_error_wrong_reason.md");
    assert_eq!(
        block.expect,
        Ok(Expect::ParseError(Some("TabError".to_string())))
    );
    match evaluate(&block) {
        Verdict::Fail(reason) => {
            assert!(
                reason.contains("TabError"),
                "failure message should name what was wanted, got {:?}",
                reason
            );
            assert!(
                reason.contains("IndentationError"),
                "failure message should name what was actually found, got {:?}",
                reason
            );
        }
        _ => panic!("a parse error for the wrong reason must fail, not pass"),
    }
}

/// A bare `expect=parse-error` still claims only that the parser rejected the block,
/// and must keep passing whatever the reason — chapters that do not describe the
/// diagnostic should not be forced to track it.
#[test]
fn bare_parse_error_does_not_pin_the_reason() {
    let mut block = only_block("parse_error_wrong_reason.md");
    block.expect = Ok(Expect::ParseError(None));
    assert!(
        matches!(evaluate(&block), Verdict::Pass),
        "a bare parse-error must accept any parser rejection"
    );
}

/// `expect=unimplemented` on a block that parses *and* canonicalizes successfully is
/// a failure: the tag's whole point is to go red the day the feature lands.
///
/// Pins: `tests/fixtures/spec/unimplemented_block_that_compiles.md` tags
/// `expect=unimplemented` a perfectly ordinary, compiling module. Neutralised by
/// making the `Expect::Unimplemented` arm's `Ok(_)` case return `Verdict::Pass`
/// instead of `Verdict::Fail(..)`: with that change this test goes red because a
/// compiling block is reported as passing. Restored afterwards.
#[test]
fn unimplemented_block_that_compiles_is_a_failure() {
    let block = only_block("unimplemented_block_that_compiles.md");
    assert_eq!(block.expect, Ok(Expect::Unimplemented));
    match evaluate(&block) {
        Verdict::Fail(_) => {}
        _ => panic!(
            "an `expect=unimplemented` block that compiles cleanly must fail, \
             not pass silently"
        ),
    }
}

/// A `package=` group is compiled as one package, and each block is judged on its own
/// `expect=` — the property that makes multi-module examples worth having at all.
///
/// Pins: `tests/fixtures/spec/package_group_ok_fails.md` holds two blocks labelled
/// `package=fixture`. `Widget` compiles; `Main` imports a name `Widget` does not
/// declare and so must fail, despite carrying the same `expect=ok`. Neutralised by
/// having `evaluate_group` return `Verdict::Pass` for every block whenever *any*
/// module in the group checked — the "one verdict for the group" reading this test
/// exists to rule out: with that change `Main` reports `Pass` and this goes red.
/// Restored afterwards.
#[test]
fn package_group_judges_each_block_separately() {
    let content = read_fixture("package_group_ok_fails.md");
    let blocks = extract_zel_blocks(&content, "package_group_ok_fails.md");
    assert_eq!(blocks.len(), 2, "fixture should hold two zel blocks");
    assert!(
        blocks
            .iter()
            .all(|b| b.package.as_deref() == Some("fixture")),
        "both blocks should share one package label"
    );

    let group: Vec<&Block> = blocks.iter().collect();
    let verdicts = evaluate_group(&group);

    assert!(
        matches!(verdicts[0], Verdict::Pass),
        "the exporting module compiles and must pass"
    );
    match &verdicts[1] {
        Verdict::Fail(reason) => assert!(
            reason.contains("ValueNotFound"),
            "the failure should name what actually went wrong, got {:?}",
            reason
        ),
        _ => panic!("the importing module fails to canonicalize and must not pass"),
    }
}

/// `expect=dependency-error` passes exactly when the group has no valid module order.
///
/// Pins: `tests/fixtures/spec/package_group_cycle.md` holds two modules importing each
/// other. Neutralised by giving the `Err` arm of `evaluate_group`'s `ModuleWalker::new`
/// match the same treatment as a parse failure — a `group_failure(..)` for every block:
/// with that change the cycle is reported as a failure rather than as the expected
/// outcome and this goes red. Restored afterwards.
#[test]
fn package_group_cycle_is_a_dependency_error() {
    let content = read_fixture("package_group_cycle.md");
    let blocks = extract_zel_blocks(&content, "package_group_cycle.md");
    assert_eq!(blocks.len(), 2, "fixture should hold two zel blocks");

    let group: Vec<&Block> = blocks.iter().collect();
    assert!(
        evaluate_group(&group)
            .iter()
            .all(|v| matches!(v, Verdict::Pass)),
        "both blocks of a cyclic package must pass their `dependency-error` tag"
    );
}

/// `expect=dependency-error` on a group that *does* have a valid order is a failure —
/// the tag must not become a way of saying "something went wrong somewhere".
///
/// Reuses the two-module fixture from
/// [`package_group_judges_each_block_separately`], whose imports do not cycle, and
/// retags both blocks in memory.
#[test]
fn dependency_error_without_a_cycle_is_a_failure() {
    let content = read_fixture("package_group_ok_fails.md");
    let mut blocks = extract_zel_blocks(&content, "package_group_ok_fails.md");
    for block in &mut blocks {
        block.expect = Ok(Expect::DependencyError);
    }

    let group: Vec<&Block> = blocks.iter().collect();
    for verdict in evaluate_group(&group) {
        match verdict {
            Verdict::Fail(reason) => assert!(
                reason.contains("import cycle"),
                "the failure should say the package had a valid order, got {:?}",
                reason
            ),
            _ => panic!("`dependency-error` must fail when the package orders fine"),
        }
    }
}

/// `expect=parse-error` cannot live in a `package=` group, and saying so is a failure
/// rather than a silent pass: a group has to parse as a whole before any of it is
/// compiled, so a block showing rejected source has to stand alone.
#[test]
fn parse_error_inside_a_group_is_a_failure() {
    let content = read_fixture("package_group_ok_fails.md");
    let mut blocks = extract_zel_blocks(&content, "package_group_ok_fails.md");
    blocks[1].expect = Ok(Expect::ParseError(None));

    let group: Vec<&Block> = blocks.iter().collect();
    for verdict in evaluate_group(&group) {
        match verdict {
            Verdict::Fail(reason) => assert!(
                reason.contains("stand on its own"),
                "the failure should say why, got {:?}",
                reason
            ),
            _ => panic!("a parse-error expectation inside a group must fail"),
        }
    }
}

/// `expect=fragment` is skipped — never parsed, never canonicalized — and counted.
///
/// Pins: `tests/fixtures/spec/fragment_skipped.md` tags `expect=fragment` text that
/// is not even syntactically valid Zelkova. If `evaluate` executed it anyway it
/// would fail (a parse error, not a pass), so a green `Verdict::Fragment` here is
/// only possible if the block was genuinely never run. Neutralised by making the
/// `Expect::Fragment` arm of `evaluate` fall through to the `Expect::Ok` behaviour
/// (parse-and-canonicalize) instead of returning `Verdict::Fragment` directly: with
/// that change this test goes red because the garbage text fails to parse. Restored
/// afterwards.
#[test]
fn fragment_block_is_skipped_and_counted() {
    let block = only_block("fragment_skipped.md");
    assert_eq!(block.expect, Ok(Expect::Fragment));
    match evaluate(&block) {
        Verdict::Fragment => {}
        other => panic!(
            "an `expect=fragment` block must be skipped, not evaluated, got a {}",
            match other {
                Verdict::Pass => "Pass",
                Verdict::Fail(_) => "Fail",
                Verdict::Fragment => unreachable!(),
            }
        ),
    }
}
