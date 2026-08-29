//! Executable-example harness for `docs/spec/`.
//!
//! Every fenced ```` ```zel ```` block in a `docs/spec/*.md` chapter carries an
//! `expect=` tag, and this binary is what makes that tag mean something: it walks the
//! chapters, extracts each such block, and runs it through the phase its tag implies.
//! An example that does not match its tag — or carries no tag at all — is a test
//! failure, not a skip, because an unchecked example in a spec is the exact defect
//! this harness exists to prevent (`SPEC-1`; `docs/spec/INDEX.md` carries the reasoning
//! and is the document chapter authors read).
//!
//! The vocabulary is fixed, and `docs/spec/INDEX.md` documents it for chapter authors —
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
//! - `zel expect=fragment` — an illustrative fragment, deliberately not executed. Only
//!   opt-out, and it must be explicit in the source. Counted and reported at the end
//!   of a run.
//!
//! A `zel` block with no `expect=`, or an unrecognised `expect=` value, is a hard
//! failure. The extraction and evaluation logic below is written to take an arbitrary
//! path or string rather than being hardcoded to `docs/spec/`, so the harness's own
//! failure modes can be pinned against fixtures under `tests/fixtures/spec/` instead
//! of committing a deliberately-broken example to a real chapter.

use std::path::Path;

use codespan_reporting::files::SimpleFile;
use zelkova_lang::compiler::canonical;
use zelkova_lang::compiler::parser;
use zelkova_lang::compiler::parser::tokenizer::TokenizerErrorType;

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
    /// `Err` when the info string carried no `expect=`, or an unrecognised one.
    expect: Result<Expect, String>,
    source: String,
}

/// Parse an `expect=` value (already stripped of the `zel` token). `rest` is the
/// trimmed remainder of the info string after `zel`.
fn parse_expect(rest: &str) -> Result<Expect, String> {
    if rest.is_empty() {
        return Err("no `expect=` in the info string".to_string());
    }
    let Some(value) = rest.strip_prefix("expect=") else {
        return Err(format!(
            "info string `{}` does not start with `expect=`",
            rest
        ));
    };
    match value {
        "ok" => Ok(Expect::Ok),
        "parse-error" => Ok(Expect::ParseError(None)),
        "unimplemented" => Ok(Expect::Unimplemented),
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
            let expect = parse_expect(&info_rest);
            blocks.push(Block {
                file: file_label.to_string(),
                line: open_line,
                info_rest,
                expect,
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

fn expect_label(block: &Block) -> String {
    match &block.expect {
        Ok(Expect::Ok) => "expect=ok".to_string(),
        Ok(Expect::ParseError(None)) => "expect=parse-error".to_string(),
        Ok(Expect::ParseError(Some(r))) => format!("expect=parse-error:{}", r),
        Ok(Expect::CanonicalError(v)) => format!("expect=canonical-error:{}", v),
        Ok(Expect::Unimplemented) => "expect=unimplemented".to_string(),
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

        for block in extract_zel_blocks(&content, &label) {
            match evaluate(&block) {
                Verdict::Pass => pass_count += 1,
                Verdict::Fragment => fragment_count += 1,
                Verdict::Fail(reason) => {
                    failures.push(format!(
                        "{}:{} ({}): {}",
                        block.file,
                        block.line,
                        expect_label(&block),
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
