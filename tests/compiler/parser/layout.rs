//! `BUG-10`: a `case` branch level with, or left of, the `case` keyword must be
//! rejected. `docs/spec/layout.md`'s *The first branch fixes the column for
//! all of them* section states the rule; these tests pin the two examples it
//! gives of violations, plus the shape the rule must *not* claim, running each
//! source through `parser::parse` (tokenizer + layout + grammar) the way the
//! compiler actually does.
//!
//! `NodeSpan`'s `PartialEq` is blind (see its doc comment and
//! `CLAUDE.md`'s *Standing invariants*), so a whole-value comparison on the
//! error would prove nothing about *where* it points. The violation tests
//! assert on the offending token and on the rendered diagnostic's primary
//! label range, which is built from real byte offsets.
use super::support::*;
use codespan_reporting::files::SimpleFile;
use zelkova_lang::compiler::parser;
use zelkova_lang::compiler::parser::layout::LayoutError;
use zelkova_lang::compiler::parser::tokenizer::Token;
use zelkova_lang::compiler::parser::Error;

/// Assert that `source` is rejected by the layout pass, and that the caret
/// lands on the *last* `On ->` in it — the offending branch pattern. (The
/// fixtures below also contain an `On` in a type declaration, which is why
/// this looks for the last occurrence rather than the first.)
fn assert_branch_rejected(source: &str, description: &str) {
    let (error, range) = layout_error(source);

    let LayoutError::LayoutError { token, .. } = error;
    assert_eq!(
        token.value,
        Token::UpperIdentifier("On".to_string()),
        "{}: the error must blame the branch pattern",
        description
    );

    let expected_start = source.rfind("On ->").expect("fixture must contain `On ->`");
    assert_eq!(
        range,
        expected_start..expected_start + "On".len(),
        "{}: the caret must sit on the offending branch pattern",
        description
    );
}

/// The two rejected shapes: a branch level with `case` reads as belonging to
/// the enclosing declaration rather than to the `case … of`, and a branch
/// *left* of `case` is the worse variant of the same mistake.
///
/// Verified to fail by reverting the fix — restoring the unchecked-first-token
/// behaviour in `src/compiler/parser/layout.rs` — which turns both sources
/// into `Ok(_)` and panics inside `layout_error`.
#[test]
fn a_branch_not_strictly_right_of_case_is_rejected() {
    assert_branch_rejected(
        indoc::indoc! {"
            module Example exposing (describe)

            type Flag
              = On
              | Off

            describe f =
              case f of
              On -> 1
              Off -> 0
        "},
        "branch level with `case`",
    );

    assert_branch_rejected(
        indoc::indoc! {"
            module Example exposing (describe)

            type Flag
              = On
              | Off

            describe f =
                case f of
              On -> 1
              Off -> 0
        "},
        "branch left of `case`",
    );
}

/// A `case … of` with no branches at all is a *grammar* error, not a layout
/// one: the token that follows is a new top level declaration, so blaming it
/// for a misindented branch would point the caret and the advice at the wrong
/// thing. The BUG-10 check therefore only fires on a token which was not
/// already going to close the branch block.
///
/// Verified to fail by dropping the `column > offside.indent` guard from that
/// check in `src/compiler/parser/layout.rs`: this then reports
/// `Error::Layout` against `other` instead.
#[test]
fn an_empty_case_block_is_left_to_the_grammar() {
    let source = indoc::indoc! {"
        module Example exposing (describe)

        describe f =
          case f of
        other = 1
    "};

    let file = SimpleFile::new("test".to_owned(), source.to_owned());

    match parser::parse(&file) {
        Err(Error::UnexpectedToken { token, .. }) => {
            assert_eq!(token.value, Token::CloseBlock)
        }
        other => panic!(
            "expected the grammar to reject the branchless `case`, got {:?}",
            other.map(|_| "Ok(_)")
        ),
    }
}
