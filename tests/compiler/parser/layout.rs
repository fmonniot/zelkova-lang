//! Layout rules checked through `parser::parse`.
//!
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
//!
//! The tests at the end pin *Top-level declarations* from the same chapter: a
//! declaration whose first token is not in column 1 continues the one above,
//! and when that one was already complete the error names the indentation.
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

    let LayoutError::LayoutError { token, .. } = error else {
        panic!(
            "{}: expected an offside violation, got {:?}",
            description, error
        );
    };
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

/// Assert that `source` is rejected as an indented top-level declaration,
/// blaming the `f` on its last line, read as a continuation of the
/// declaration on line 1.
fn assert_indented_declaration(source: &str, description: &str) {
    let (error, range) = layout_error(source);

    let LayoutError::IndentedDeclaration {
        token,
        declaration_line,
    } = error
    else {
        panic!(
            "{}: expected an indented declaration, got {:?}",
            description, error
        );
    };
    assert_eq!(
        token.value,
        Token::LowerIdentifier("f".to_string()),
        "{}: the error must blame the indented line's first token",
        description
    );
    assert_eq!(declaration_line, 1, "{}", description);

    let expected_start = source.rfind("f =").expect("fixture must contain `f =`");
    assert_eq!(
        range,
        expected_start..expected_start + 1,
        "{}: the caret must sit on the indented line's first token",
        description
    );
}

/// A top-level declaration whose first token is not in column 1 continues
/// the declaration above it. After a complete module header nothing can
/// continue it, and the error says the line is indented instead of reporting
/// the grammar's `UnexpectedToken` against `f`. The three sources put `f` off
/// column 1 by leading spaces, by a comment before it, and by a comment
/// before it with the body on the next line.
///
/// Verified to fail by making `Layout::explain` return its argument
/// unchanged: all three then come back as `Error::UnexpectedToken` on `f`,
/// expecting `close block`, and `layout_error` panics.
#[test]
fn a_declaration_not_in_column_1_after_a_complete_one_is_an_indentation_error() {
    assert_indented_declaration(
        indoc::indoc! {"
            module Example exposing (f)

              f = 1
        "},
        "declaration indented by two spaces",
    );

    assert_indented_declaration(
        indoc::indoc! {"
            module Example exposing (f)

            {- a note -} f = 1
        "},
        "declaration after a block comment",
    );

    assert_indented_declaration(
        indoc::indoc! {"
            module Example exposing (f)

            {- a note -} f =
              1
        "},
        "declaration after a block comment, body on the next line",
    );
}

/// The rendered diagnostic names the indentation and the column rule. Its
/// wording is pinned verbatim, because it is the whole of what the fix
/// changes for the reader.
///
/// Verified to fail by making `Layout::explain` return its argument
/// unchanged (the message is then the grammar's "unexpected token").
#[test]
fn an_indented_declaration_diagnostic_names_the_indentation() {
    let source = indoc::indoc! {"
        module Example exposing (f)

          f = 1
    "};
    let file = SimpleFile::new("test".to_owned(), source.to_owned());

    let error = match parser::parse(&file) {
        Err(error) => error,
        Ok(_) => panic!("expected the indented declaration to be rejected"),
    };
    let diagnostic = error.diagnostic(());

    assert_eq!(
        diagnostic.message,
        "this line is indented, so it continues the declaration above it"
    );
    assert_eq!(
        diagnostic.labels[0].message,
        "this token starts at column 3, so it is read as part of the declaration on line 1, \
         which was already complete"
    );
    assert_eq!(
        diagnostic.notes,
        vec![
            "a top-level declaration begins in column 1; move this line there if it starts a \
             new declaration"
                .to_string()
        ]
    );
}

/// An indented line that continues a declaration the grammar can still
/// extend is ordinary source and parses. The layout pass records every such
/// line, so this guards against that record turning into a rejection.
///
/// A guard, not a regression test: it passes against the code before the
/// fix too. Verified to go red by making `Layout::handle_next_token` return
/// an error wherever it records `top_level_continuation`.
#[test]
fn an_indented_continuation_line_still_parses() {
    let source = indoc::indoc! {"
        module Example
          exposing
            (f)

        f x =
          x
    "};
    let file = SimpleFile::new("test".to_owned(), source.to_owned());

    if let Err(error) = parser::parse(&file) {
        panic!("expected the continuation lines to parse, got {:?}", error);
    }
}

/// A file whose top level is indented as a whole is `ERR-12`'s concern: its
/// first declaration does not begin in column 1 either, so an indented line
/// after it is not reported as an indented declaration.
///
/// Verified to fail by dropping the `offside.indent == 1` condition where
/// `Layout::handle_next_token` records `top_level_continuation`: `f` is then
/// reported as an `IndentedDeclaration`.
#[test]
fn an_indented_file_is_not_reported_as_an_indented_declaration() {
    // Not `indoc!`: it would strip the leading indentation this is about.
    let source = "  module Example exposing (f)\n\n  f x =\n    1\n";
    let file = SimpleFile::new("test".to_owned(), source.to_owned());

    match parser::parse(&file) {
        Err(Error::UnexpectedToken { token, .. }) => {
            assert_eq!(token.value, Token::LowerIdentifier("f".to_string()))
        }
        other => panic!(
            "expected the grammar's error on `f`, got {:?}",
            other.map(|_| "Ok(_)")
        ),
    }
}

/// An indented line the grammar rejects while the declaration above is still
/// incomplete is an ordinary syntax error, and stays the grammar's: after
/// `x +` the declaration cannot end, so the line is not a finished
/// declaration's continuation.
///
/// Verified to fail by dropping the `expected.iter().any(|e| e == "close
/// block")` guard from `Layout::explain`: `)` is then reported as an
/// `IndentedDeclaration`.
#[test]
fn an_indented_line_inside_an_incomplete_declaration_is_left_to_the_grammar() {
    let source = indoc::indoc! {"
        module Example exposing (f)

        f x =
          x +
          )
    "};
    let file = SimpleFile::new("test".to_owned(), source.to_owned());

    match parser::parse(&file) {
        Err(Error::UnexpectedToken { token, expected }) => {
            assert_eq!(token.value, Token::RPar);
            assert!(!expected.iter().any(|e| e == "close block"));
        }
        other => panic!(
            "expected the grammar's error on `)`, got {:?}",
            other.map(|_| "Ok(_)")
        ),
    }
}
