//! `BUG-10`: a `case` branch level with, or left of, the `case` keyword must be
//! rejected. `docs/spec/layout.md`'s *The first branch fixes the column for
//! all of them* section states the rule; these tests pin the two examples it
//! gives of violations, running the source through `parser::parse` (tokenizer
//! + layout + grammar) the way the compiler actually does.
//!
//! `NodeSpan`'s `PartialEq` is blind (see its doc comment and
//! `CLAUDE.md`'s *Standing invariants*), so a whole-value comparison on the
//! error would prove nothing about *where* it points. Both tests assert on
//! the `LayoutError` variant directly and on the rendered diagnostic's
//! primary label range, which is built from real byte offsets.
use codespan_reporting::files::SimpleFile;
use zelkova_lang::compiler::parser;
use zelkova_lang::compiler::parser::layout::LayoutError;
use zelkova_lang::compiler::parser::tokenizer::Token;
use zelkova_lang::compiler::parser::Error;

/// Parse `source` and return the layout error it must produce, together with
/// the byte range of its primary label. Panics (test failure) if parsing
/// succeeds or fails with anything other than `Error::Layout`.
fn layout_error(source: &str) -> (LayoutError, std::ops::Range<usize>) {
    let file = SimpleFile::new("test".to_owned(), source.to_owned());

    let err = match parser::parse(&file) {
        Ok(module) => panic!(
            "expected a layout error, but parsing succeeded: {:?}",
            module
        ),
        Err(err) => err,
    };

    let layout_err = match &err {
        Error::Layout(layout_err) => layout_err.clone(),
        other => panic!("expected a layout error, got {:?}", other),
    };

    let diagnostic = err.diagnostic(());
    let range = diagnostic
        .labels
        .first()
        .expect("layout error diagnostic has no primary label")
        .range
        .clone();

    (layout_err, range)
}

/// A branch level with `case` itself is rejected: the branches sit on the
/// same column as `case`, which reads as belonging to the enclosing
/// declaration rather than to the `case … of`.
///
/// Verified to fail by reverting the fix — restoring the old
/// `Context::CaseBlock(None)` / unchecked-first-token behaviour in
/// `src/compiler/parser/layout.rs` — which turns this into `Ok(_)` and
/// panics the `Err(...)` match arm above.
#[test]
fn branch_level_with_case_is_rejected() {
    let source = indoc::indoc! {"
        module Example exposing (describe)

        type Flag
          = On
          | Off

        describe f =
          case f of
          On -> 1
          Off -> 0
    "};

    let (error, range) = layout_error(source);

    assert!(
        matches!(error, LayoutError::LayoutError { .. }),
        "expected LayoutError::LayoutError, got {:?}",
        error
    );

    let LayoutError::LayoutError { token, .. } = error;
    assert_eq!(token.value, Token::UpperIdentifier("On".to_string()));

    // The first `On` belongs to the type declaration (`= On`); the second is
    // the offending branch pattern the diagnostic must point at.
    let expected_start = source.rfind("On ->").expect("fixture must contain `On ->`");
    let expected_end = expected_start + "On".len();
    assert_eq!(range, expected_start..expected_end);
}

/// A branch level *left* of `case` is the worse variant of the same bug: the
/// branches read as belonging to something enclosing the `case … of` itself.
///
/// Verified to fail the same way as `branch_level_with_case_is_rejected`.
#[test]
fn branch_level_left_of_case_is_rejected() {
    let source = indoc::indoc! {"
        module Example exposing (describe)

        type Flag
          = On
          | Off

        describe f =
            case f of
          On -> 1
          Off -> 0
    "};

    let (error, range) = layout_error(source);

    assert!(
        matches!(error, LayoutError::LayoutError { .. }),
        "expected LayoutError::LayoutError, got {:?}",
        error
    );

    let LayoutError::LayoutError { token, .. } = error;
    assert_eq!(token.value, Token::UpperIdentifier("On".to_string()));

    let expected_start = source.rfind("On ->").expect("fixture must contain `On ->`");
    let expected_end = expected_start + "On".len();
    assert_eq!(range, expected_start..expected_end);
}
