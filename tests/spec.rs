//! Executable-example harness for `docs/spec/`.
//!
//! Every fenced ```` ```zel ```` block in a `docs/spec/*.md` chapter carries an
//! `expect=` tag, and this binary is what makes that tag mean something: it walks the
//! chapters, extracts each such block, and runs it through the phase its tag implies.
//! An example that does not match its tag — or carries no tag at all — is a test
//! failure, not a skip, because an unchecked example in a spec is the exact defect
//! this harness exists to prevent (`SPEC-1`; `docs/spec/conventions.md` carries the
//! reasoning and is the document chapter authors read).
//!
//! The vocabulary is fixed, and `docs/spec/conventions.md` documents it for chapter
//! authors — keep the two in step. Chapters are written against it independently of
//! this file:
//!
//! - `zel expect=ok` — parses, canonicalizes and type checks with no errors. The typer
//!   is part of it deliberately (`TEST-2`): a block that canonicalizes and then
//!   contradicts its own annotation is not an example of anything the language allows,
//!   and leaving it green meant a chapter's **Known gap:** about the type checker had
//!   to be deleted by hand on the day its ticket landed instead of going red on its
//!   own. What the tag does *not* promise is that every declaration was checked:
//!   [`typer::type_check`] skips silently — a bare `continue`, not an error — any
//!   declaration whose function head holds a constructor or tuple pattern, any body
//!   reaching a `VarForeign` or an expression form its term language does not model,
//!   any `ErrorKind::UnboundVariable`, and any `binding_javascript` module whole. Its
//!   own doc comment is the account of why. Across `docs/spec/` that is roughly one
//!   declaration in ten, so a green `expect=ok` block may still hold an annotation its
//!   body contradicts — `docs/spec/conventions.md`'s row carries the same caveat for
//!   chapter authors. Exhaustiveness is not run at all — it is a stub that accepts
//!   every module.
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
//! - `zel expect=type-error` — parses and canonicalizes, then `typer::type_check`
//!   returns at least one error. Which one is not pinned.
//! - `zel expect=type-error:Kind` — the same, and the kind must match one of the names
//!   [`error_kind_names`] returns, which are the real `typer::ErrorKind` variant names.
//!   A block whose *earlier* phases reject it fails either tag rather than satisfying
//!   it: the point of a separate tag is that the chapter names the phase that decides
//!   the rule it is claiming.
//! - `zel expect=unimplemented` — must fail somewhere in parse-or-canonicalize, but
//!   deliberately does not pin which error: pinning would wire tokenizer/grammar
//!   internals into a prose document, and the tag's whole job is to go red the day
//!   the feature lands. On an expected failure the harness prints the actual error
//!   observed, so a human reviewing a chapter can eyeball that it failed for the
//!   intended reason. The typer counts as somewhere, for the reason `expect=ok`
//!   includes it: a block no phase rejects is the one thing this tag rules out.
//! - `zel expect=dependency-error` — the block's *group* (see below) fails before any
//!   module is canonicalized, because its imports have no valid order: a cycle. It is
//!   the one expectation that belongs to a group rather than to a module, so every
//!   block of the group carries it or none does.
//! - `zel expect=fragment` — an illustrative fragment, deliberately not executed. Only
//!   opt-out, and it must be explicit in the source. Counted and reported at the end
//!   of a run.
//!
//! A block may also carry `package=<label>`. Blocks sharing one label, within one
//! chapter, are one package: they are parsed together, ordered by their imports,
//! canonicalized in that order against each other's `Interface`s and then type checked
//! in the same order — which is how a chapter shows two modules at once (`SPEC-3`,
//! settling the question
//! `docs/spec/conventions.md` records). Each block keeps its **own** `expect=`, so an
//! example can show one module compiling and its importer failing. A block with no
//! `package=` is a package of one, compiled with no interfaces at all, exactly as
//! before.
//!
//! A `zel` block with no `expect=`, an unrecognised `expect=` value, or an
//! unrecognised key in its info string, is a hard failure. The extraction and evaluation logic below is written to take an arbitrary
//! path or string rather than being hardcoded to `docs/spec/`, so the harness's own
//! failure modes can be pinned against fixtures under `tests/fixtures/spec/` instead
//! of committing a deliberately-broken example to a real chapter.
//!
//! # The prose, as far as it is checkable
//!
//! Three sibling tests hold the rest of `docs/` to the same premise the `expect=` tags
//! hold the examples to — documentation nothing checks drifts from what it describes.
//!
//! [`spec_cross_references_resolve`] resolves every inline markdown link a chapter
//! makes. An anchor is checked against the headers of the file it names, slugified by
//! GitHub's rule, and a relative path is checked for existing at all. A broken anchor
//! is invisible to `grep` and loud on a rendered page, which is why nothing was
//! catching it. Targets outside `docs/spec/` are checked the same way, `docs/tickets/`
//! and `docs/decisions/` included, so closing a ticket a chapter cites turns this test
//! red until the citing paragraph is edited.
//!
//! [`decision_cross_references_resolve`] runs that same check over `docs/decisions/`,
//! which holds no `zel` block and has no `expect=` vocabulary. It is checked because it
//! holds citations, and a citation nothing keeps alive is the failure that directory
//! exists to prevent.
//!
//! [`spec_tag_vocabulary_is_documented`] holds `docs/spec/conventions.md`'s
//! *The `expect=` vocabulary* table to the names this file actually accepts, in both
//! directions: every name [`parse_error_reasons`] can produce is documented, and every
//! name the table documents is one of those. [`parse_error_reasons`]'s explicit match
//! guards the enum-to-name direction — a new variant fails this file to compile — and
//! it reads the table's `expect=parse-error:Reason` row rather than searching the
//! section name by name, so that row's formatting is load-bearing: keep every reason
//! name in it backticked, and the count spelled out as a word.
//!
//! Why each of those three is scoped the way it is — why ticket citations are checked
//! at all, why an anchor is resolved in whatever file names it, why the table is parsed
//! rather than searched, why `docs/decisions/` is checked for links and not for
//! examples, and what is deliberately left unchecked (the `canonical::Error` variant
//! names of [`variant_names`] among it) — is `docs/decisions/dec-3.md`, with
//! `docs/decisions/dec-4.md` for the last of those. Read it before narrowing any of
//! them: each cost was weighed, and the one that looks gratuitous is the one that had
//! already caught a drift.

use std::path::Path;

use std::collections::HashMap;

use codespan_reporting::files::SimpleFile;
use zelkova_lang::compiler::canonical;
use zelkova_lang::compiler::dependencies::ModuleWalker;
use zelkova_lang::compiler::name::Name;
use zelkova_lang::compiler::parser;
use zelkova_lang::compiler::parser::tokenizer::TokenizerErrorType;
use zelkova_lang::compiler::typer;
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
    /// `None` claims only that the type checker rejected the block. `Some(kind)` also
    /// pins *which* [`typer::ErrorKind`] it raised, against the names in
    /// [`error_kind_names`].
    ///
    /// The bare form is for a chapter that claims only "this is a type error"; pin the
    /// kind when the prose names the diagnostic the reader will see.
    TypeError(Option<String>),
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
/// Shared by [`extract_zel_blocks`] and [`prose_lines`], which have to agree about what
/// a fence is or the block scan and the prose scan disagree about the same file.
fn fence_open(line: &str) -> Option<(usize, &str)> {
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
fn fence_close(line: &str, open: usize) -> bool {
    let trimmed = line.trim_start();
    let ticks = trimmed.chars().take_while(|&c| c == '`').count();
    ticks >= open && trimmed[ticks..].trim().is_empty()
}

/// Extract every ```` ```zel ```` fenced block from `content`, by hand — no markdown
/// dependency, per `SPEC-1`. A fence is what [`fence_open`] says it is; the block runs
/// until a line whose trimmed text is *only* backticks, at least as many as the opener.
/// Only fences whose info string's first whitespace-delimited token is exactly `zel`
/// become a [`Block`]; anything else (`sh`, bare fences, prose) is skipped over without
/// being inspected.
fn extract_zel_blocks(content: &str, file_label: &str) -> Vec<Block> {
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

/// The phases are called one at a time rather than through
/// [`check_module`](zelkova_lang::compiler::check_module), and that is a deliberate
/// cost: `check_module` collapses every phase into one `CompilationError`, and the
/// `expect=canonical-error:` / `expect=type-error:` distinction the tags exist to draw
/// would have to be recovered from it by matching on the variant anyway. Calling the
/// phases directly means this file knows the pipeline's shape — canonicalize, then type
/// check — which is the price of keeping the two tags apart.
/// Exhaustiveness is deliberately not run: it is a stub that accepts every module, so
/// running it would only let a future chapter tag a block against a phase that inspects
/// nothing.
fn type_check(module: &canonical::Module) -> Result<(), Vec<typer::Error>> {
    typer::type_check(module)
}

/// The `typer::ErrorKind` names present in `errors`.
///
/// Written as an explicit match over the real enum, for the reason [`variant_names`] is:
/// a new `ErrorKind` variant fails this file to compile rather than silently becoming a
/// name no chapter can ever match. There is no grouping variant to flatten — a
/// `typer::Error` carries exactly one kind, and [`typer::type_check`] hands back at most
/// one `Error` per declaration it *rejected*. A declaration it could not check at all is
/// skipped silently and contributes no error, so an empty list is not evidence that every
/// declaration was looked at; see this file's module documentation.
fn error_kind_names(errors: &[typer::Error]) -> Vec<&'static str> {
    errors
        .iter()
        .map(|e| match e.kind {
            typer::ErrorKind::UnificationFailed { .. } => "UnificationFailed",
            typer::ErrorKind::CircularType { .. } => "CircularType",
            typer::ErrorKind::UnboundVariable { .. } => "UnboundVariable",
        })
        .collect()
}

/// The two phase names an `expect=parse-error:<reason>` tag may pin instead of naming
/// a specific error — the coarse half of what [`parse_error_reasons`] returns.
const PARSE_ERROR_PHASES: &[&str] = &["Tokenizer", "Layout"];

/// The specific error names an `expect=parse-error:<reason>` tag may pin: one per
/// `TokenizerErrorType` variant, one for layout, and one per grammar-level
/// `parser::Error` variant.
///
/// This list and [`parse_error_reasons`]'s match move together, and
/// [`parse_error_reasons`] asserts that every name it hands back is in it — so a name
/// added to the match by the one edit a new enum variant forces cannot reach a chapter
/// without also reaching this list, and from here
/// [`spec_tag_vocabulary_is_documented`] carries it on to `conventions.md`'s table.
const PARSE_ERROR_SPECIFICS: &[&str] = &[
    "CharNotClosedError",
    "StringError",
    "UnicodeError",
    "IndentationError",
    "TabError",
    "UnrecognizedToken",
    "LayoutError",
    "InvalidToken",
    "UnexpectedEOF",
    "UnexpectedToken",
    "ExtraToken",
];

/// Every name an `expect=parse-error:<reason>` tag may pin, phases and specific errors
/// together.
fn parse_error_vocabulary() -> Vec<&'static str> {
    PARSE_ERROR_PHASES
        .iter()
        .chain(PARSE_ERROR_SPECIFICS)
        .copied()
        .collect()
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
    let names = match error {
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
    };

    // A name the match can produce but the constants above do not list is a name
    // `spec_tag_vocabulary_is_documented` never carries to `conventions.md`, so it
    // would be usable in a chapter and documented nowhere. Caught here rather than
    // there because only an actual error can say what the match produces.
    let vocabulary = parse_error_vocabulary();
    for name in &names {
        assert!(
            vocabulary.contains(name),
            "`parse_error_reasons` returned `{}`, which `PARSE_ERROR_PHASES` and \
             `PARSE_ERROR_SPECIFICS` do not list — add it there, and to the \
             `expect=parse-error:Reason` row of `docs/spec/conventions.md`",
            name
        );
    }

    names
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
            TypeArityMismatch(..) => vec!["TypeArityMismatch"],
        }
    }
    errors.iter().flat_map(one).collect()
}

/// How an `expect=type-error[:Kind]` tag is written, for a failure message.
fn type_error_label(wanted: &Option<String>) -> String {
    match wanted {
        None => "expect=type-error".to_string(),
        Some(kind) => format!("expect=type-error:{}", kind),
    }
}

/// Judge one `expect=type-error[:Kind]` block against what the typer actually returned.
///
/// `errors` is empty when the module type checked. Shared by [`evaluate`] and
/// [`evaluate_group`] so a block means the same thing inside a `package=` group as
/// outside one.
fn judge_type_error(wanted: &Option<String>, errors: &[typer::Error]) -> Verdict {
    if errors.is_empty() {
        return Verdict::Fail(format!(
            "expected `{}`, but the module type checked with no errors",
            type_error_label(wanted)
        ));
    }

    match wanted {
        None => Verdict::Pass,
        Some(wanted) => {
            let found = error_kind_names(errors);
            if found.contains(&wanted.as_str()) {
                Verdict::Pass
            } else {
                Verdict::Fail(format!(
                    "expected a type error of kind `{}`, got {:?} ({:?})",
                    wanted, found, errors
                ))
            }
        }
    }
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
                Ok(canonical) => match type_check(&canonical) {
                    Err(errors) => Verdict::Fail(format!(
                        "expected `ok`, but type checking failed: {:?}",
                        errors
                    )),
                    Ok(()) => Verdict::Pass,
                },
            },
        },
        Expect::TypeError(wanted) => match parse(&block.source) {
            Err(e) => Verdict::Fail(format!(
                "expected `{}`, but the parser rejected it before the typer ran: {:?}",
                type_error_label(wanted),
                e
            )),
            Ok(module) => match canonicalize(&module) {
                Err(errors) => Verdict::Fail(format!(
                    "expected `{}`, but canonicalization rejected it before the typer \
                     ran: {:?}",
                    type_error_label(wanted),
                    errors
                )),
                Ok(canonical) => {
                    judge_type_error(wanted, &type_check(&canonical).err().unwrap_or_default())
                }
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
                Ok(canonical) => match type_check(&canonical) {
                    Err(errors) => {
                        println!(
                            "{}:{} (expect=unimplemented) failed in the typer, as expected: {:?}",
                            block.file, block.line, errors
                        );
                        Verdict::Pass
                    }
                    Ok(()) => Verdict::Fail(
                        "expected `unimplemented`, but the block compiled cleanly — this \
                         feature looks implemented now; update the chapter"
                            .to_string(),
                    ),
                },
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
    let (checked, failures) = walker.check_in_order(
        &test_package(),
        &mut interfaces,
        &module_files,
        canonicalize_tagged,
    );
    let failures: HashMap<Name, Vec<canonical::Error>> = failures.into_iter().collect();

    // The typer runs per module, over the modules that canonicalized, in the order
    // `check_in_order` produced them — the same dependency order canonicalization used.
    //
    // A module whose *type* check fails still published its `Interface` above, because
    // `check_in_order` built that interface out of the canonical module and the typer
    // does not touch it: an interface carries declared signatures, and those are what
    // canonicalization already validated. So a group can show one module failing the
    // typer and its importer still resolving every name it imports, which is what a
    // chapter demonstrating a type error inside a two-module example needs. The typer
    // reads one module at a time (`typer::type_check` takes no interfaces), so the
    // order is bookkeeping rather than a dependency here.
    let type_failures: HashMap<Name, Vec<typer::Error>> = checked
        .iter()
        .filter_map(|m| {
            type_check(m)
                .err()
                .map(|errors| (m.name.name().clone(), errors))
        })
        .collect();

    blocks
        .iter()
        .zip(&expects)
        .zip(&modules)
        .map(|((block, expect), module)| {
            let errors = failures.get(&module.name);
            let type_errors = type_failures
                .get(&module.name)
                .map(Vec::as_slice)
                .unwrap_or_default();
            match (expect, errors) {
                (Expect::Ok, None) if type_errors.is_empty() => Verdict::Pass,
                (Expect::Ok, None) => Verdict::Fail(format!(
                    "expected `ok`, but type checking failed: {:?}",
                    type_errors
                )),
                (Expect::Ok, Some(errors)) => Verdict::Fail(format!(
                    "expected `ok`, but canonicalization failed: {:?}",
                    errors
                )),
                (Expect::TypeError(wanted), None) => judge_type_error(wanted, type_errors),
                (Expect::TypeError(wanted), Some(errors)) => Verdict::Fail(format!(
                    "expected `{}`, but canonicalization rejected it before the typer \
                     ran: {:?}",
                    type_error_label(wanted),
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
                (Expect::Unimplemented, None) if !type_errors.is_empty() => {
                    println!(
                        "{}:{} (expect=unimplemented) failed in the typer, as expected: {:?}",
                        block.file, block.line, type_errors
                    );
                    Verdict::Pass
                }
                (Expect::Unimplemented, None) => Verdict::Fail(
                    "expected `unimplemented`, but the block compiled cleanly — this \
                     feature looks implemented now; update the chapter"
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
        Ok(Expect::TypeError(wanted)) => type_error_label(wanted),
        Ok(Expect::Unimplemented) => "expect=unimplemented".to_string(),
        Ok(Expect::DependencyError) => "expect=dependency-error".to_string(),
        Ok(Expect::Fragment) => "expect=fragment".to_string(),
        Err(_) => format!("`{}`", block.info_rest),
    }
}

// ── Cross-references ──────────────────────────────────────────────────────────

/// Every line of `content` that is not inside a fenced code block, paired with its
/// 1-indexed line number. The fence lines themselves are excluded too.
///
/// Both the header scan and the link scan need this and neither is correct without it:
/// `docs/spec/packages.md` holds a ```` ```toml ```` block whose comment lines begin
/// with `#`, and reading one of those as a markdown header would invent an anchor no
/// reader can reach. The fence rule is [`fence_open`]'s and [`fence_close`]'s, shared
/// with [`extract_zel_blocks`]; nothing here cares what a block contains, only where it
/// ends.
fn prose_lines(content: &str) -> Vec<(usize, &str)> {
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
fn slugify(header: &str) -> String {
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
fn header_anchors(content: &str) -> Vec<String> {
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

/// One inline markdown link found in a chapter.
struct Link {
    /// Display label for the file the link was written in.
    file: String,
    /// 1-indexed line the link sits on.
    line: usize,
    /// The target exactly as written, before it is split on `#`.
    target: String,
}

/// Extract every inline `[text](target)` link from `content`, by hand and one line at a
/// time — the same no-markdown-dependency choice [`extract_zel_blocks`] makes.
///
/// Only the inline form is recognised, because it is the only one `docs/spec/` uses. A
/// link title (`](file.md "Title")`) is not part of the target and is dropped. Links
/// inside a fenced block are not links and are not collected.
fn extract_links(content: &str, file_label: &str) -> Vec<Link> {
    let mut links = Vec::new();
    for (number, line) in prose_lines(content) {
        let bytes = line.as_bytes();
        let mut i = 0;
        while i + 1 < bytes.len() {
            if bytes[i] == b']' && bytes[i + 1] == b'(' {
                let after = &line[i + 2..];
                if let Some(end) = after.find(')') {
                    if let Some(target) = after[..end].split_whitespace().next() {
                        links.push(Link {
                            file: file_label.to_string(),
                            line: number,
                            target: target.to_string(),
                        });
                    }
                    i += 2 + end + 1;
                    continue;
                }
            }
            i += 1;
        }
    }
    links
}

/// One markdown file the checks read, kept as a label plus its text.
struct Chapter {
    /// Path relative to the crate root — what a failure names, and what a relative
    /// link is resolved against.
    label: String,
    content: String,
}

/// Read every `*.md` directly under `dir`, sorted, labelled relative to `root`.
fn load_chapters(root: &Path, dir: &Path) -> Vec<Chapter> {
    let mut paths: Vec<_> = std::fs::read_dir(dir)
        .unwrap_or_else(|e| panic!("failed to read {:?}: {}", dir, e))
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.extension().is_some_and(|ext| ext == "md"))
        .collect();
    paths.sort();

    paths
        .into_iter()
        .map(|path| {
            let content = std::fs::read_to_string(&path)
                .unwrap_or_else(|e| panic!("failed to read {:?}: {}", path, e));
            let label = path
                .strip_prefix(root)
                .unwrap_or(&path)
                .to_string_lossy()
                .replace('\\', "/");
            Chapter { label, content }
        })
        .collect()
}

/// Resolve `.` and `..` lexically, so a failure can name `docs/tickets/lang-4.md`
/// rather than `docs/spec/../tickets/lang-4.md`.
fn normalize(path: &Path) -> std::path::PathBuf {
    let mut out = std::path::PathBuf::new();
    for component in path.components() {
        match component {
            std::path::Component::ParentDir => {
                out.pop();
            }
            std::path::Component::CurDir => {}
            other => out.push(other.as_os_str()),
        }
    }
    out
}

/// Every link in `chapters` that does not land, one message per failure.
///
/// Collected rather than short-circuited, the way [`spec_chapters_pass`] collects block
/// failures: one renamed header usually breaks several links and a reader wants all of
/// them. Each message names the file the link was written in, its line, and the target
/// it wanted.
///
/// `root` is the directory the chapters' labels are relative to; a link's target is
/// resolved against the directory of the file that wrote it, which is what lets
/// `docs/spec/types.md`'s `../tickets/bug-17.md` and a fixture's sibling link go
/// through the same code.
fn cross_reference_failures(root: &Path, chapters: &[Chapter]) -> Vec<String> {
    let mut anchors: HashMap<String, Vec<String>> = chapters
        .iter()
        .map(|c| (c.label.clone(), header_anchors(&c.content)))
        .collect();
    let mut failures = Vec::new();

    for chapter in chapters {
        for link in extract_links(&chapter.content, &chapter.label) {
            let (file_part, anchor) = match link.target.split_once('#') {
                Some((file, anchor)) => (file, Some(anchor)),
                None => (link.target.as_str(), None),
            };

            // Anything addressed by scheme is somebody else's to keep alive.
            if file_part.contains("://") || file_part.starts_with("mailto:") {
                continue;
            }

            let target = if file_part.is_empty() {
                chapter.label.clone()
            } else {
                let dir = Path::new(&chapter.label).parent().unwrap_or(Path::new(""));
                normalize(&dir.join(file_part))
                    .to_string_lossy()
                    .replace('\\', "/")
            };

            if !root.join(&target).exists() {
                failures.push(format!(
                    "{}:{} links to `{}`, but `{}` does not exist",
                    link.file, link.line, link.target, target
                ));
                continue;
            }

            let Some(anchor) = anchor else { continue };
            if anchor.is_empty() {
                failures.push(format!(
                    "{}:{} links to `{}`, whose `#` names no anchor",
                    link.file, link.line, link.target
                ));
                continue;
            }

            if !anchors.contains_key(&target) {
                // A file outside the set under check — a ticket, say. Read it and
                // slugify its headers by the same rule.
                match std::fs::read_to_string(root.join(&target)) {
                    Ok(content) => {
                        anchors.insert(target.clone(), header_anchors(&content));
                    }
                    Err(e) => {
                        failures.push(format!(
                            "{}:{} links to `{}`, but `{}` could not be read: {}",
                            link.file, link.line, link.target, target, e
                        ));
                        continue;
                    }
                }
            }

            if !anchors[&target].iter().any(|a| a == anchor) {
                failures.push(format!(
                    "{}:{} links to `{}`, but `{}` has no header whose anchor is \
                     `#{}` — a header it names was renamed, or the anchor was \
                     mistyped",
                    link.file, link.line, link.target, target, anchor
                ));
            }
        }
    }

    failures
}

// ── The documented tag vocabulary ─────────────────────────────────────────────

/// The English words for the counts a vocabulary is plausibly ever going to have —
/// enough to check the one `conventions.md` writes out in prose.
const NUMBER_WORDS: &[&str] = &[
    "zero",
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
    "ten",
    "eleven",
    "twelve",
    "thirteen",
    "fourteen",
    "fifteen",
    "sixteen",
    "seventeen",
    "eighteen",
    "nineteen",
    "twenty",
];

/// The row of `conventions.md`'s *The `expect=` vocabulary* table that documents
/// `expect=parse-error:Reason`, or the reason it could not be found.
///
/// Located by slug rather than by line number, and by the tag the row is about rather
/// than by its position in the table, so reordering the rows or rewording the heading
/// does not move it.
fn parse_error_reason_row(conventions: &str) -> Result<String, String> {
    let lines = prose_lines(conventions);
    let start = lines
        .iter()
        .position(|(_, line)| {
            line.starts_with("## ") && slugify(line[3..].trim()) == "the-expect-vocabulary"
        })
        .ok_or(
            "`docs/spec/conventions.md` has no section whose anchor is `#the-expect-vocabulary`",
        )?;

    lines[start + 1..]
        .iter()
        .take_while(|(_, line)| !line.starts_with("## "))
        .map(|(_, line)| line.trim())
        .find(|line| line.starts_with('|') && line.contains("parse-error:Reason"))
        .map(|line| line.to_string())
        .ok_or_else(|| {
            "the `expect=` vocabulary section of `docs/spec/conventions.md` has no \
             table row documenting `expect=parse-error:Reason`"
                .to_string()
        })
}

/// The backticked spans of `text`, in order.
fn backticked(text: &str) -> Vec<&str> {
    let mut spans = Vec::new();
    let mut rest = text;
    while let Some(open) = rest.find('`') {
        let after = &rest[open + 1..];
        match after.find('`') {
            Some(close) => {
                spans.push(&after[..close]);
                rest = &after[close + 1..];
            }
            None => break,
        }
    }
    spans
}

/// The reason names a table row documents: its backticked spans that are bare
/// UpperCamel identifiers.
///
/// The filter is what lets the whole row be scanned rather than one of its cells:
/// `expect=parse-error:Reason` and `src/compiler/parser/` are backticked too and carry
/// characters no error name has. `Reason` itself is the tag's own placeholder — the
/// row is titled with it — and is not one of the names it documents.
fn documented_reason_names(row: &str) -> Vec<&str> {
    backticked(row)
        .into_iter()
        .filter(|span| {
            *span != "Reason"
                && span.starts_with(|c: char| c.is_ascii_uppercase())
                && span.chars().all(|c| c.is_ascii_alphabetic())
        })
        .collect()
}

/// Compare the names `conventions.md` documents against the ones this harness accepts,
/// in both directions, plus the count its prose writes out in words.
///
/// Returns one message per disagreement, each saying which side to edit. `documented`
/// is what [`documented_reason_names`] read out of the row; `phases` and `specifics`
/// are [`PARSE_ERROR_PHASES`] and [`PARSE_ERROR_SPECIFICS`], taken as parameters so
/// the self-test can hand in a deliberately-wrong pair.
fn vocabulary_failures(
    row: &str,
    documented: &[&str],
    phases: &[&str],
    specifics: &[&str],
) -> Vec<String> {
    let mut failures = Vec::new();

    for name in phases.iter().chain(specifics) {
        if !documented.contains(name) {
            failures.push(format!(
                "`{}` is a reason a chapter may pin, and the \
                 `expect=parse-error:Reason` row of `docs/spec/conventions.md` does \
                 not document it — a chapter can write a tag no reader can look up",
                name
            ));
        }
    }

    for name in documented {
        if !phases.contains(name) && !specifics.contains(name) {
            failures.push(format!(
                "the `expect=parse-error:Reason` row of `docs/spec/conventions.md` \
                 documents `{}`, which this harness does not accept — a chapter \
                 written against the table would fail",
                name
            ));
        }
    }

    match NUMBER_WORDS.get(specifics.len()) {
        Some(word) if row.contains(&format!(" {} ", word)) => {}
        Some(word) => failures.push(format!(
            "there are {} specific error names, and the \
             `expect=parse-error:Reason` row of `docs/spec/conventions.md` does not \
             say `{}` anywhere — it counts them out in words, so the count is the \
             word to fix",
            specifics.len(),
            word
        )),
        None => failures.push(format!(
            "there are now {} specific error names, more than `NUMBER_WORDS` can \
             spell — extend it, or drop the count from the row",
            specifics.len()
        )),
    }

    failures
}

/// Every `expect=parse-error:<reason>` a chapter pins that `documented` does not carry,
/// one message per chapter and name.
///
/// The direction of the check a chapter can break on its own, and the counterpart of
/// [`vocabulary_failures`]: that one compares the table against the harness, this one
/// compares the chapters against the table. It takes `documented` as a parameter for the
/// same reason — so the self-test can hand in a chapter and a vocabulary that disagree,
/// and exercise the code the real test runs rather than a copy of it.
fn undocumented_pins(chapters: &[Chapter], documented: &[&str]) -> Vec<String> {
    let mut failures = Vec::new();
    for chapter in chapters {
        let blocks = extract_zel_blocks(&chapter.content, &chapter.label);
        for name in pinned_reason_names(&blocks) {
            if !documented.contains(&name.as_str()) {
                failures.push(format!(
                    "{} pins `expect=parse-error:{}`, a reason the \
                     `expect=parse-error:Reason` row of `docs/spec/conventions.md` \
                     does not document",
                    chapter.label, name
                ));
            }
        }
    }
    failures
}

/// The `expect=parse-error:<reason>` names `blocks` actually write, deduplicated.
fn pinned_reason_names(blocks: &[Block]) -> Vec<String> {
    let mut names: Vec<String> = Vec::new();
    for block in blocks {
        if let Ok(Expect::ParseError(Some(reason))) = &block.expect {
            if !names.contains(reason) {
                names.push(reason.clone());
            }
        }
    }
    names
}

// ── The real chapters ─────────────────────────────────────────────────────────

/// The crate root, and every `docs/spec/*.md` under it — top level only, sorted for a
/// deterministic run order.
fn spec_chapters() -> (std::path::PathBuf, Vec<Chapter>) {
    let manifest = std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    let root = std::path::PathBuf::from(manifest);
    let chapters = load_chapters(&root, &root.join("docs/spec"));
    assert!(
        !chapters.is_empty(),
        "expected at least one chapter under {:?}",
        root.join("docs/spec")
    );
    (root, chapters)
}

/// The crate root, and every `docs/decisions/*.md` under it.
///
/// Read by [`load_chapters`] like a chapter, because a link is a link: the only thing
/// separating the two directories here is that nothing extracts `zel` blocks from these
/// files. `docs/decisions/dec-4.md` is why.
fn decision_records() -> (std::path::PathBuf, Vec<Chapter>) {
    let manifest = std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    let root = std::path::PathBuf::from(manifest);
    let records = load_chapters(&root, &root.join("docs/decisions"));
    assert!(
        !records.is_empty(),
        "expected at least one entry under {:?}",
        root.join("docs/decisions")
    );
    (root, records)
}

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
    let (_root, chapters) = spec_chapters();

    let mut failures = Vec::new();
    let mut pass_count = 0usize;
    let mut fragment_count = 0usize;

    for chapter in &chapters {
        let blocks = extract_zel_blocks(&chapter.content, &chapter.label);

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

/// `cargo test --test spec`: every link `docs/spec/` writes must land.
///
/// A sibling of [`spec_chapters_pass`] rather than part of it: a broken link is not a
/// block failure, and lumping the two makes a panic message that is harder to read than
/// either on its own.
///
/// The two scope decisions — that `../tickets/*.md` citations are checked, and that an
/// anchor is checked in whatever file names it — are argued in
/// `docs/decisions/dec-3.md`.
///
/// Two liveness assertions come before the check itself, because every failure mode of
/// the scan is silent: it reports what it read, and a file it read nothing out of looks
/// exactly like a file with nothing in it. The per-chapter one is the sharper of the
/// two — a single swallowed chapter is invisible in a whole-directory total.
#[test]
fn spec_cross_references_resolve() {
    let (root, chapters) = spec_chapters();

    let mute: Vec<&str> = chapters
        .iter()
        .filter(|c| {
            header_anchors(&c.content).is_empty() || extract_links(&c.content, &c.label).is_empty()
        })
        .map(|c| c.label.as_str())
        .collect();
    assert!(
        mute.is_empty(),
        "every chapter under `docs/spec/` writes headers and links to its neighbours, so \
         a chapter the scan reads none of one or the other out of is one it stopped \
         reading part way — most likely a line it mistook for a code fence: {}",
        mute.join(", ")
    );

    let link_count: usize = chapters
        .iter()
        .map(|c| extract_links(&c.content, &c.label).len())
        .sum();
    assert!(
        link_count > 100,
        "expected `docs/spec/` to be densely cross-linked, found {} link(s) — the \
         extractor is probably reading nothing",
        link_count
    );

    let failures = cross_reference_failures(&root, &chapters);

    println!("spec: {} cross-reference(s) checked", link_count);

    assert!(
        failures.is_empty(),
        "{} cross-reference(s) in `docs/spec/` do not resolve:\n{}",
        failures.len(),
        failures.join("\n")
    );
}

/// `cargo test --test spec`: every link `docs/decisions/` writes must land.
///
/// The design-decision track is not normative and holds no `zel` block, so none of the
/// `expect=` machinery reaches it. Its *citations* are checked, and for the reason the
/// directory exists: `SPEC-12`'s decision list was promoted nowhere when its ticket was
/// deleted, and five files went on citing decisions of it that resolved to nothing. A
/// directory created out of that failure that did not check its own links would be
/// reproducing it one directory over (`docs/decisions/dec-4.md`).
///
/// The liveness assertion is the per-file one [`spec_cross_references_resolve`] carries
/// and not its whole-directory count: an entry always names the rule it decided —
/// that is the `Where the rule lives` field the README requires of every one — so an
/// entry the scan reads no link out of is an entry it stopped reading part way.
#[test]
fn decision_cross_references_resolve() {
    let (root, records) = decision_records();

    let mute: Vec<&str> = records
        .iter()
        .filter(|c| {
            header_anchors(&c.content).is_empty() || extract_links(&c.content, &c.label).is_empty()
        })
        .map(|c| c.label.as_str())
        .collect();
    assert!(
        mute.is_empty(),
        "every file under `docs/decisions/` writes headers, and every entry links to the \
         rule it decided, so a file the scan reads none of one or the other out of is one \
         it stopped reading part way — most likely a line it mistook for a code fence: {}",
        mute.join(", ")
    );

    let link_count: usize = records
        .iter()
        .map(|c| extract_links(&c.content, &c.label).len())
        .sum();

    let failures = cross_reference_failures(&root, &records);

    println!("decisions: {} cross-reference(s) checked", link_count);

    assert!(
        failures.is_empty(),
        "{} cross-reference(s) in `docs/decisions/` do not resolve:\n{}",
        failures.len(),
        failures.join("\n")
    );
}

/// `cargo test --test spec`: `conventions.md`'s tag table and this harness name the
/// same reasons.
///
/// Three claims, all about the same vocabulary: every name [`parse_error_reasons`] can
/// produce is documented, every name the table documents is one this harness accepts,
/// and every `expect=parse-error:Reason` a chapter actually writes is one of them.
#[test]
fn spec_tag_vocabulary_is_documented() {
    let (_root, chapters) = spec_chapters();

    let conventions = chapters
        .iter()
        .find(|c| c.label.ends_with("conventions.md"))
        .expect("`docs/spec/conventions.md` should be one of the chapters");
    let row = parse_error_reason_row(&conventions.content).expect("the vocabulary row");
    let documented = documented_reason_names(&row);

    let mut failures =
        vocabulary_failures(&row, &documented, PARSE_ERROR_PHASES, PARSE_ERROR_SPECIFICS);

    // And the direction a chapter can break on its own: a tag naming a reason the
    // table does not carry is one a reader cannot look up, whether or not the block
    // it sits on happens to be green.
    failures.extend(undocumented_pins(&chapters, &documented));

    assert!(
        failures.is_empty(),
        "the documented `expect=` vocabulary and the harness's disagree:\n{}",
        failures.join("\n")
    );
}

// ── Harness self-tests ────────────────────────────────────────────────────────
//
// These prove the harness can fail, against fixtures under `tests/fixtures/spec/`
// rather than against a real chapter — `SPEC-1`'s explicit request, so a red run
// never has to be manufactured by breaking `docs/spec/` on purpose. Each fixture
// holds exactly one `zel` block, isolating the one behaviour its test pins. The
// cross-reference fixtures — `anchor_targets.md` and `broken_anchor.md` — hold no
// `zel` block at all and are read as a two-file `docs/spec/` of their own.

fn read_fixture(name: &str) -> String {
    let manifest = std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    let path = Path::new(&manifest).join("tests/fixtures/spec").join(name);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("failed to read {:?}: {}", path, e))
}

/// One real chapter's text, for the self-tests that are about `docs/spec/` itself
/// rather than about a fixture.
fn read_chapter(name: &str) -> String {
    let manifest = std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    let path = Path::new(&manifest).join("docs/spec").join(name);
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

/// `expect=type-error:Kind` passes when the typer raises that kind, and fails when it
/// raises a different one — the whole point of pinning a kind rather than writing the
/// bare tag.
///
/// Pins: `tests/fixtures/spec/type_error_unification.md`, whose module canonicalizes
/// cleanly and whose body hands an integer back where its annotation promised a `Size`.
/// The wrong-kind direction retags the same block `CircularType` in memory, which is a
/// real `ErrorKind` the typer can raise and did not raise here. Neutralised two ways,
/// each turning one half of this test red on its own: dropping the
/// `found.contains(&wanted.as_str())` check in `judge_type_error` (the mismatched kind
/// reports `Pass`, and the second half goes red), and having `evaluate`'s
/// `Expect::TypeError` arm skip the `type_check` call and judge against no errors at
/// all (the first half goes red). Restored afterwards.
#[test]
fn type_error_of_the_wrong_kind_is_a_failure() {
    let block = only_block("type_error_unification.md");
    assert_eq!(
        block.expect,
        Ok(Expect::TypeError(Some("UnificationFailed".to_string())))
    );
    assert!(
        matches!(evaluate(&block), Verdict::Pass),
        "the fixture's annotation and body really do disagree, so the pinned kind passes"
    );

    let mut wrong = only_block("type_error_unification.md");
    wrong.expect = Ok(Expect::TypeError(Some("CircularType".to_string())));
    match evaluate(&wrong) {
        Verdict::Fail(reason) => {
            assert!(
                reason.contains("CircularType"),
                "failure message should name what was wanted, got {:?}",
                reason
            );
            assert!(
                reason.contains("UnificationFailed"),
                "failure message should name what was actually found, got {:?}",
                reason
            );
        }
        _ => panic!("a type error of the wrong kind must fail, not pass"),
    }
}

/// A bare `expect=type-error` claims only that the typer rejected the block, and accepts
/// whichever kind it raised — the counterpart of
/// [`bare_parse_error_does_not_pin_the_reason`].
///
/// Neutralised by the same change as the first half of
/// [`type_error_of_the_wrong_kind_is_a_failure`]: have `evaluate`'s `Expect::TypeError`
/// arm judge against no errors rather than calling `type_check`, and this goes red.
/// Restored afterwards.
#[test]
fn bare_type_error_does_not_pin_the_kind() {
    let mut block = only_block("type_error_unification.md");
    block.expect = Ok(Expect::TypeError(None));
    assert!(
        matches!(evaluate(&block), Verdict::Pass),
        "a bare type-error must accept any rejection by the typer"
    );
}

/// `expect=type-error` on a block that type checks is a failure, and so is one the
/// *earlier* phases rejected — a tag naming the typer must not be satisfiable by a
/// module the typer never saw.
///
/// The second half is what keeps `canonical-error:` and `type-error:` apart. Without it
/// the two tags would be interchangeable on any module that fails early, and a chapter
/// could claim a type-level rule while demonstrating a name that does not resolve.
///
/// Pins: `unimplemented_block_that_compiles.md`, an ordinary compiling module, and
/// `ok_block_fails_to_compile.md`, which fails canonicalization on an undefined
/// variable. Neutralised by having `evaluate`'s `Expect::TypeError` arm treat a
/// canonicalization failure as the expected outcome — i.e. judging against `errors` from
/// whichever phase produced them: with that change the second half goes red. Restored
/// afterwards.
#[test]
fn type_error_needs_the_typer_to_be_the_phase_that_failed() {
    let mut clean = only_block("unimplemented_block_that_compiles.md");
    clean.expect = Ok(Expect::TypeError(None));
    match evaluate(&clean) {
        Verdict::Fail(reason) => assert!(
            reason.contains("type checked with no errors"),
            "the failure should say the module type checked, got {:?}",
            reason
        ),
        _ => panic!("`type-error` on a module that type checks must fail"),
    }

    let mut early = only_block("ok_block_fails_to_compile.md");
    early.expect = Ok(Expect::TypeError(Some("UnificationFailed".to_string())));
    match evaluate(&early) {
        Verdict::Fail(reason) => assert!(
            reason.contains("before the typer ran"),
            "the failure should say the typer never ran, got {:?}",
            reason
        ),
        _ => panic!("`type-error` on a module canonicalization rejects must fail"),
    }
}

/// `expect=ok` means the block type checks too, not only that it canonicalizes.
///
/// This is what makes a `**Known gap:**` about the type checker go red on the day its
/// ticket lands, instead of having to be deleted by hand — `LANG-12`'s block in
/// `docs/spec/types.md` is the case the tightening was made for.
///
/// Pins: the same fixture as [`type_error_of_the_wrong_kind_is_a_failure`], retagged
/// `expect=ok` in memory, because the property under test is that canonicalizing
/// cleanly is no longer enough. Neutralised by returning `Verdict::Pass` from
/// `evaluate`'s `Expect::Ok` arm as soon as `canonicalize` succeeds, which is what that
/// arm did before this test existed: with that change this goes red. Restored
/// afterwards.
#[test]
fn ok_block_that_fails_the_typer_is_a_failure() {
    let mut block = only_block("type_error_unification.md");
    block.expect = Ok(Expect::Ok);
    match evaluate(&block) {
        Verdict::Fail(reason) => assert!(
            reason.contains("type checking failed"),
            "the failure should say which phase rejected it, got {:?}",
            reason
        ),
        _ => panic!("an `expect=ok` block that fails the typer must fail"),
    }
}

/// A module that fails the type checker still publishes its `Interface` to the rest of
/// its `package=` group.
///
/// The alternative — withholding the interface — would make every importer of a
/// type-error example fail canonicalization on names that resolve perfectly well, so a
/// chapter could not show a type error in one module of a group and a working importer
/// beside it. An interface is built from the canonical module, and the typer does not
/// touch it.
///
/// Pins: `tests/fixtures/spec/package_group_type_error.md`, whose `Widget` fails the
/// typer and whose `Main` imports `Widget.Size` and `Widget.small`. Neutralised by
/// making the group's type check gate the interface — folding the typer into
/// `canonicalize_tagged`, so a type failure returns `Err` and `check_in_order` never
/// inserts the interface: with that change `Main` no longer resolves `Widget` and the
/// importer's assertion below goes red. `Widget`'s own verdict goes red with it, since
/// under that design its type error is reported as a canonicalization failure and stops
/// matching its tag — which is the second thing wrong with it. Restored afterwards.
#[test]
fn a_type_error_does_not_withhold_the_modules_interface() {
    let content = read_fixture("package_group_type_error.md");
    let blocks = extract_zel_blocks(&content, "package_group_type_error.md");
    assert_eq!(blocks.len(), 2, "fixture should hold two zel blocks");

    let group: Vec<&Block> = blocks.iter().collect();
    let verdicts = evaluate_group(&group);

    match &verdicts[1] {
        Verdict::Pass => {}
        Verdict::Fail(reason) => panic!(
            "the importer must still resolve against the failed module's interface, \
             got {:?}",
            reason
        ),
        Verdict::Fragment => panic!("neither block is a fragment"),
    }
    assert!(
        matches!(verdicts[0], Verdict::Pass),
        "the module with the type error must pass its `type-error` tag"
    );
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
        Verdict::Fail(reason) => assert!(
            reason.contains("compiled cleanly"),
            "the failure has to claim the whole pipeline, not just the two phases \
             that used to run — got {:?}",
            reason
        ),
        _ => panic!(
            "an `expect=unimplemented` block that compiles cleanly must fail, \
             not pass silently"
        ),
    }
}

/// A failure in the **typer** satisfies `expect=unimplemented`, the same way a failure
/// in the parser or in canonicalization does.
///
/// This is a deliberate broadening and it has a cost worth naming, which is why it gets
/// a test of its own rather than riding on a chapter block. Before `TEST-2` the tag went
/// red the moment its construct parsed and canonicalized; it now stays green if the
/// block is rejected anywhere, including for a type error unrelated to the feature the
/// chapter says is missing. `BUG-26` is a live example of such an incidental error, and
/// the day `LANG-48` lands a `records.md` block carrying one would stay green as
/// `unimplemented` instead of announcing that records arrived.
///
/// It is still the right reading: `expect=ok` now means "and type checks", so the tag
/// that is its negation has to mean "and is not rejected by the typer either", or a
/// block would exist that satisfies neither. What keeps the cost bounded is that the
/// harness prints the error it observed on every expected failure, so the phase and the
/// reason are in the run output for a reviewer to eyeball against the chapter's claim.
///
/// Pins: `tests/fixtures/spec/type_error_unification.md`, retagged `expect=unimplemented`
/// in memory — its module canonicalizes cleanly and only the typer rejects it, so it
/// reaches this branch and nothing earlier. Neutralised by making `evaluate`'s
/// `Expect::Unimplemented` arm report `Verdict::Fail` on a type error rather than
/// `Verdict::Pass` — which is what it did before `TEST-2`: with that change this goes
/// red. Restored afterwards.
#[test]
fn unimplemented_block_may_fail_in_the_typer() {
    let mut block = only_block("type_error_unification.md");
    block.expect = Ok(Expect::Unimplemented);
    match evaluate(&block) {
        Verdict::Pass => {}
        Verdict::Fail(reason) => panic!(
            "a block only the typer rejects must satisfy `expect=unimplemented`, \
             got {:?}",
            reason
        ),
        Verdict::Fragment => panic!("`expect=unimplemented` is not a fragment"),
    }
}

/// The same, inside a `package=` group: `evaluate_group` reads the typer's verdict from
/// its own `type_failures` map rather than from `check_in_order`'s errors, so it needs
/// its own branch and its own test.
///
/// Pins: `tests/fixtures/spec/package_group_type_error.md` with `Widget` retagged
/// `expect=unimplemented` in memory. `Widget` canonicalizes, publishes its interface and
/// fails only the typer; `Main` imports it and must stay green, which is what separates
/// this from the single-block case. Neutralised by deleting the
/// `(Expect::Unimplemented, None) if !type_errors.is_empty()` arm, leaving the group to
/// fall through to the "compiled cleanly" failure: with that change `Widget`'s verdict
/// goes red. Restored afterwards.
#[test]
fn unimplemented_inside_a_group_may_fail_in_the_typer() {
    let content = read_fixture("package_group_type_error.md");
    let mut blocks = extract_zel_blocks(&content, "package_group_type_error.md");
    assert_eq!(blocks.len(), 2, "fixture should hold two zel blocks");
    blocks[0].expect = Ok(Expect::Unimplemented);

    let group: Vec<&Block> = blocks.iter().collect();
    let verdicts = evaluate_group(&group);

    match &verdicts[0] {
        Verdict::Pass => {}
        Verdict::Fail(reason) => panic!(
            "a group member only the typer rejects must satisfy \
             `expect=unimplemented`, got {:?}",
            reason
        ),
        Verdict::Fragment => panic!("`expect=unimplemented` is not a fragment"),
    }
    assert!(
        matches!(&verdicts[1], Verdict::Pass),
        "the importer must be unaffected by how its dependency is tagged"
    );
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

/// The two cross-reference fixtures, read as a two-file `docs/spec/` of their own.
///
/// Labelled by their real path under the crate root, because that is what a relative
/// link is resolved against: `anchor_targets.md` from `broken_anchor.md` has to reach
/// its sibling exactly the way `layout.md` reaches `expressions.md`.
fn link_fixtures() -> (std::path::PathBuf, Vec<Chapter>) {
    let manifest = std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    let chapters = ["anchor_targets.md", "broken_anchor.md"]
        .iter()
        .map(|name| Chapter {
            label: format!("tests/fixtures/spec/{}", name),
            content: read_fixture(name),
        })
        .collect();
    (std::path::PathBuf::from(manifest), chapters)
}

/// The one failure of `failures` mentioning `needle`, or a panic naming what was found
/// instead.
fn only_failure_about<'a>(failures: &'a [String], needle: &str) -> &'a str {
    let mut matching = failures.iter().filter(|f| f.contains(needle));
    let found = matching
        .next()
        .unwrap_or_else(|| panic!("no failure mentioned `{}`, got {:?}", needle, failures));
    assert!(
        matching.next().is_none(),
        "expected exactly one failure about `{}`, got {:?}",
        needle,
        failures
    );
    found.as_str()
}

/// Header anchors are slugified by GitHub's rule, headers inside a fenced block are not
/// headers, and a repeated header does not shadow the one before it.
///
/// Pins: `tests/fixtures/spec/anchor_targets.md` holds one header per rule the slug has
/// to get right — an ellipsis and the two spaces around it (`let--in`, which seven links
/// across six real chapters name), a dot, an apostrophe, a `#` comment line inside a
/// ```` ```toml ```` block, and a header repeated after it. Neutralised three ways, each of which turns
/// this test red on its own: dropping the `c == ' '` arm of `slugify` (every multi-word
/// anchor loses its hyphens); giving `header_anchors` the raw `content.lines()` instead
/// of `prose_lines` (the toml comment becomes a third `two-outcomes`); and returning
/// `base` unconditionally from the repeat loop (the second `## Two outcomes` collides
/// rather than becoming `two-outcomes-1`). Restored afterwards.
#[test]
fn header_anchors_follow_githubs_slug_rule() {
    let anchors = header_anchors(&read_fixture("anchor_targets.md"));
    assert_eq!(
        anchors.iter().map(String::as_str).collect::<Vec<_>>(),
        vec![
            "anchor-targets",
            "let--in",
            "resolution-and-zelkovalock",
            "the-annotation-and-the-declarations-parameters",
            "two-outcomes",
            "two-outcomes-1",
            "a-line-beginning-with-an-inline-code-span",
            "still-a-header",
        ]
    );
}

/// A prose line that *begins* with an inline code span opens no fenced block.
///
/// CommonMark's rule for a backtick fence is that its info string may not contain a
/// backtick, and that rule exists for exactly this line: prose naming a fence writes it
/// as an inline span (```` ```zel ````), which is what `docs/spec/`'s own conventions
/// chapter and several tickets do. Reading such a line as an opener swallows every
/// header and every link after it, silently — the failure mode this whole file exists to
/// prevent, turned on the file itself.
///
/// Pins: the closing section of both cross-reference fixtures, each of which writes such
/// a line and then something the scan has to still see — a header in
/// `anchor_targets.md`, a link to a missing anchor in `broken_anchor.md`. Neutralised by
/// dropping the ``!info.contains('`')`` half of `fence_open`'s condition: with that change
/// the ```` ```zel ```` line opens a fence that never closes, `still-a-header` is not
/// collected and the link below is never checked, and both halves of this test go red.
/// Restored afterwards.
#[test]
fn a_line_beginning_with_an_inline_code_span_is_not_a_fence() {
    let anchors = header_anchors(&read_fixture("anchor_targets.md"));
    assert!(
        anchors.iter().any(|a| a == "still-a-header"),
        "a header written after an inline code span is still a header, got {:?}",
        anchors
    );

    let (root, chapters) = link_fixtures();
    let failures = cross_reference_failures(&root, &chapters);
    let after = only_failure_about(&failures, "neither-does-this-one");
    assert!(
        after.contains("has no header whose anchor is"),
        "a link written after an inline code span is still checked, got {:?}",
        after
    );
}

/// A link to an anchor no header defines is a failure, and the failure names the file
/// the link was written in, its line, and the anchor it wanted.
///
/// This is the whole point of the check: renaming a header is an ordinary edit, and
/// every link that pointed at it breaks with no error anywhere — invisible to a
/// terminal `grep`, loud on a rendered page.
///
/// Pins: `tests/fixtures/spec/broken_anchor.md` holds one link of every shape — two
/// that resolve (one same-file, one into `anchor_targets.md`), two that name anchors
/// neither file defines, one naming a file that does not exist, and one inside a fenced
/// block that is not a link at all. Neutralised by making
/// `cross_reference_failures` `continue` instead of pushing when
/// `anchors[&target]` does not hold the anchor: with that change both anchor failures
/// vanish and this goes red on the `no-such-section` lookup. Restored afterwards.
#[test]
fn broken_anchor_is_a_failure() {
    let (root, chapters) = link_fixtures();
    let failures = cross_reference_failures(&root, &chapters);

    let content = read_fixture("broken_anchor.md");
    let line = content
        .lines()
        .position(|l| l.contains("#no-such-section"))
        .expect("the fixture links to `#no-such-section`")
        + 1;

    let same_file = only_failure_about(&failures, "no-such-section");
    assert!(
        same_file.starts_with(&format!("tests/fixtures/spec/broken_anchor.md:{} ", line)),
        "the failure should name the referring file and its line, got {:?}",
        same_file
    );

    let cross_file = only_failure_about(&failures, "no-such-header");
    assert!(
        cross_file.contains("tests/fixtures/spec/anchor_targets.md"),
        "the failure should name the file whose header was wanted, got {:?}",
        cross_file
    );
}

/// A link to a file that does not exist is a failure too — the shape a chapter's
/// `../tickets/*.md` citation takes once the ticket process deletes the ticket it
/// cites.
///
/// Pins: the `no_such_chapter.md` link in `tests/fixtures/spec/broken_anchor.md`.
/// Neutralised by making the `!root.join(&target).exists()` arm of
/// `cross_reference_failures` `continue` without pushing: with that change this goes
/// red because nothing reports the missing file. Restored afterwards.
#[test]
fn link_to_a_missing_file_is_a_failure() {
    let (root, chapters) = link_fixtures();
    let failures = cross_reference_failures(&root, &chapters);

    let missing = only_failure_about(&failures, "no_such_chapter.md");
    assert!(
        missing.contains("does not exist"),
        "the failure should say the target does not exist, got {:?}",
        missing
    );
}

/// A link that resolves is not reported, and a link inside a fenced code block is not a
/// link — without both, the check would be a test that always fails.
///
/// Pins: `tests/fixtures/spec/broken_anchor.md`'s two resolving links and its fenced
/// `[not a link](no_such_chapter.md#no-such-anchor)`, which names both a missing file
/// and a missing anchor and so would contribute a failure if it were collected.
/// Neutralised by giving `extract_links` the raw `content.lines()` instead of
/// `prose_lines`: with that change the fenced link is collected, the count reaches five
/// and this goes red. Restored afterwards.
#[test]
fn resolving_and_fenced_links_are_not_reported() {
    let (root, chapters) = link_fixtures();
    let failures = cross_reference_failures(&root, &chapters);

    assert!(
        !failures.iter().any(|f| f.contains("no-such-anchor")),
        "a link inside a fenced block is not a link, got {:?}",
        failures
    );
    assert_eq!(
        failures.len(),
        4,
        "the fixture holds exactly four links that do not land, got {:?}",
        failures
    );
}

/// The documented vocabulary and the harness's are compared in both directions: a name
/// the table omits is a failure, and so is a name the table invents.
///
/// The first direction is the one that had already drifted — the table documented seven
/// of the eleven specific errors while `UnrecognizedToken` was in use at two blocks in
/// `docs/spec/lexical-structure.md`. The second is what reading the table row buys over
/// searching the section for each name.
///
/// Pins: the real `expect=parse-error:Reason` row, compared against a deliberately
/// wrong pair of lists rather than against a fixture, because the row is the artefact
/// under test and a copy of it would prove nothing about the real one. Neutralised by
/// dropping either loop of `vocabulary_failures`: each direction's assertion below goes
/// red with its own loop removed. Restored afterwards.
#[test]
fn a_vocabulary_disagreement_is_a_failure() {
    let conventions = read_chapter("conventions.md");
    let row = parse_error_reason_row(&conventions).expect("the vocabulary row");
    let documented = documented_reason_names(&row);

    let mut invented: Vec<&str> = PARSE_ERROR_SPECIFICS.to_vec();
    invented.push("NoSuchError");
    let omitted = vocabulary_failures(&row, &documented, PARSE_ERROR_PHASES, &invented);
    let omitted = only_failure_about(&omitted, "NoSuchError");
    assert!(
        omitted.contains("does not document it"),
        "an undocumented reason should say the table is missing it, got {:?}",
        omitted
    );

    let mut over_documented = documented.clone();
    over_documented.push("Fictional");
    let invented = vocabulary_failures(
        &row,
        &over_documented,
        PARSE_ERROR_PHASES,
        PARSE_ERROR_SPECIFICS,
    );
    let invented = only_failure_about(&invented, "Fictional");
    assert!(
        invented.contains("this harness does not accept"),
        "a documented-but-unreal reason should say so, got {:?}",
        invented
    );
}

/// The count `conventions.md` writes out in words is checked against the real one.
///
/// The row says "one of the eleven specific errors", which is a claim about the same
/// vocabulary the names are and is one added variant away from being wrong.
///
/// Pins: the real row, compared against a `specifics` list one name short, so the
/// expected word becomes `ten` and the row still says `eleven`. Neutralised by deleting
/// the `NUMBER_WORDS` match at the foot of `vocabulary_failures`: with that change the
/// short list produces no failure at all and this goes red. Restored afterwards.
#[test]
fn a_stale_count_in_the_vocabulary_row_is_a_failure() {
    let conventions = read_chapter("conventions.md");
    let row = parse_error_reason_row(&conventions).expect("the vocabulary row");
    let documented = documented_reason_names(&row);

    let short = &PARSE_ERROR_SPECIFICS[..PARSE_ERROR_SPECIFICS.len() - 1];
    let failures = vocabulary_failures(&row, &documented, PARSE_ERROR_PHASES, short);
    let stale = only_failure_about(&failures, "the count is the word to fix");
    assert!(
        stale.contains(NUMBER_WORDS[short.len()]),
        "the failure should name the word the row ought to say, got {:?}",
        stale
    );
}

/// A chapter pinning a reason the table does not document is a failure, whether or not
/// the block it sits on is green.
///
/// A tag naming a real error the conventions never wrote down passes its block and
/// leaves a reader with a word they cannot look up — the direction
/// [`spec_tag_vocabulary_is_documented`]'s second half exists for.
///
/// Pins: [`undocumented_pins`], the function [`spec_tag_vocabulary_is_documented`] runs
/// for this direction, given `tests/fixtures/spec/parse_error_wrong_reason.md` twice —
/// once as written, whose `TabError` the real row documents, and once with its tag
/// rewritten to `NoSuchReason`, which it does not. Neutralised by having
/// `undocumented_pins` return an empty vector: with that change the retagged chapter
/// produces no failure and this goes red on `only_failure_about`. Restored afterwards.
#[test]
fn a_chapter_pinning_an_undocumented_reason_is_a_failure() {
    let conventions = read_chapter("conventions.md");
    let row = parse_error_reason_row(&conventions).expect("the vocabulary row");
    let documented = documented_reason_names(&row);
    assert!(
        documented.contains(&"TabError"),
        "the fixture's own reason should be a documented one"
    );

    let label = "tests/fixtures/spec/parse_error_wrong_reason.md";
    let content = read_fixture("parse_error_wrong_reason.md");
    let as_written = Chapter {
        label: label.to_string(),
        content: content.clone(),
    };
    assert!(
        undocumented_pins(std::slice::from_ref(&as_written), &documented).is_empty(),
        "the fixture as written pins a documented reason, so it is not a failure"
    );

    let retagged = Chapter {
        label: label.to_string(),
        content: content.replace(
            "expect=parse-error:TabError",
            "expect=parse-error:NoSuchReason",
        ),
    };
    assert_ne!(
        retagged.content, content,
        "the retagging should have found the fixture's tag"
    );

    let failures = undocumented_pins(std::slice::from_ref(&retagged), &documented);
    let failure = only_failure_about(&failures, "NoSuchReason");
    assert!(
        failure.starts_with(label) && failure.contains("does not document"),
        "the failure should name the chapter and say the table is missing the reason, \
         got {:?}",
        failure
    );
}
