use super::layout::LayoutError;
use super::tokenizer::{Token, TokenizerError, TokenizerErrorType};
use crate::compiler::position::{BytePos, Spanned};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use lalrpop_util::ParseError;
use std::ops::Range;

// TODO Make integration tests for errors. Can be inspired from insta with generate-tests crate

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    Tokenizer(TokenizerError),
    Layout(LayoutError),
    // Errors coming from the parser
    InvalidToken(BytePos),
    UnexpectedEOF {
        position: BytePos,
        expected: Vec<String>, // The kind of token the parser was expecting
    },
    UnexpectedToken {
        token: Spanned<BytePos, Token>,
        expected: Vec<String>, // The kind of token the parser was expecting
    },
    ExtraToken {
        token: Spanned<BytePos, Token>,
    },
}

impl Error {
    pub fn diagnostic<Id: Copy>(&self, name: Id) -> Diagnostic<Id> {
        match self {
            Error::UnexpectedToken { token, expected } => {
                Diagnostic::error()
                    .with_message(format!("unexpected token: `{:?}`", token.value)) // TODO display instead of debug
                    .with_labels(vec![Label::primary(name, token.span.to_range())
                        .with_message("unexpected token")])
                    .with_notes(vec![format!(
                        "we were expecting one of the following tokens: {:?}",
                        expected
                    )
                    .to_owned()])
            }
            Error::Tokenizer(err) => {
                /*
                CharNotClosedError:
                let err = Diagnostic::error()
                    .with_message("my message")
                    .with_labels(vec![
                        Label::primary((), 2..2).with_message("expected quote here"),
                        Label::secondary((), 0..0).with_message("for char started here")
                    ]);
                */
                let diag = Diagnostic::error();
                match err.error.value {
                    TokenizerErrorType::CharNotClosedError(None) => {
                        diag.with_message("char sequence opened but never closed")
                            .with_labels(vec![
                                Label::primary(name, err.error.span.to_range())
                                    .with_message("The char is declared here but not closed")
                            ])
                    }
                    TokenizerErrorType::CharNotClosedError(Some(_)) => {
                        let open = err.error.span.start;
                        let close = err.error.span.end;
                        diag.with_message("char sequence opened but never closed")
                            .with_labels(vec![
                                Label::primary(name, open.to_range())
                                    .with_message("We were expecting a single quote here"),
                                Label::secondary(name, close.to_range())
                                    .with_message("For the opening quote here")

                            ])
                    }
                    // String literals are not implemented in the language yet, so
                    // nothing constructs these two today. They still have to render:
                    // `diagnostic` is on the path every parse error takes to the
                    // user, and a `todo!()` there is a compiler panic (`BUG-6`).
                    TokenizerErrorType::StringError => diag
                        .with_message("this string literal could not be read")
                        .with_labels(vec![Label::primary(name, err.error.span.to_range())
                            .with_message("the string starts here")])
                        .with_notes(vec![
                            "Zelkova does not support string literals yet".to_owned()
                        ]),
                    TokenizerErrorType::UnicodeError => diag
                        .with_message("this unicode escape sequence could not be read")
                        .with_labels(vec![Label::primary(name, err.error.span.to_range())
                            .with_message("the escape sequence starts here")])
                        .with_notes(vec![
                            "Zelkova does not support string literals, and so unicode escape sequences, yet".to_owned()
                        ]),
                    TokenizerErrorType::IndentationError => {
                        diag.with_message("Invalid indentation level")
                            .with_labels(vec![
                                Label::primary(name, err.error.span.to_range())
                            ])
                            .with_notes(vec![
                                "Zelkova use exclusively two spaces to denote indentation but an odd number of spaces was found".to_owned()
                            ])
                    }
                    TokenizerErrorType::TabError => {
                        diag.with_message("Tab found")
                        .with_labels(vec![
                            Label::primary(name, err.error.span.to_range())
                        ])
                        .with_notes(vec!["Zelkova use exclusively two spaces to denote indentation and forbid the usage of tabs".to_owned()])
                    }
                    TokenizerErrorType::UnrecognizedToken { tok } => {
                        Diagnostic::error()
                        .with_message("Unexpected token found")
                        .with_labels(vec![Label::primary(name, err.error.span.to_range())
                            .with_message(format!("Unrecognized token {} found", tok))])
                    }
                }
            }

            Error::Layout(LayoutError::LayoutError { offside, token }) => {
                // A layout token carries `Position`s rather than `BytePos`, so the
                // range is built from their byte offsets. The layout pass injects
                // `OpenBlock`/`CloseBlock` tokens whose start and end are equal and
                // an indentation error can land on one of those, hence `non_empty`.
                let start = token.span.start.absolute.0 as usize;
                let end = token.span.end.absolute.0 as usize;

                Diagnostic::error()
                    .with_message("this line is not indented far enough")
                    .with_labels(vec![Label::primary(name, non_empty(start..end))
                        .with_message(format!(
                        "this token starts at column {}, but its block requires column {} or more",
                        token.span.start.column,
                        offside.min_indent()
                    ))])
                    .with_notes(vec![format!(
                        "the block it belongs to ({}) starts on line {}",
                        offside.context().description(),
                        offside.line()
                    )])
            }

            Error::InvalidToken(position) => Diagnostic::error()
                .with_message("the parser could not read this token")
                .with_labels(vec![Label::primary(name, one_byte_at(*position))
                    .with_message("the parser stopped here")]),

            Error::UnexpectedEOF { position, expected } => Diagnostic::error()
                .with_message("the file ended before the declaration did")
                .with_labels(vec![
                    Label::primary(name, one_byte_at(*position)).with_message("the file ends here")
                ])
                .with_notes(vec![format!(
                    "we were expecting one of the following tokens here: {}",
                    expected.join(", ")
                )]),

            Error::ExtraToken { token } => Diagnostic::error()
                .with_message("the module continues past its end")
                .with_labels(vec![Label::primary(name, non_empty(token.span.to_range()))
                    .with_message("this token comes after the module was complete")]),
        }
    }
}

/// Widen a single position into a one byte range.
///
/// `BytePos::to_range` is zero width by construction (`u..u`), which
/// codespan-reporting renders as a caret with no character above it. The errors
/// which only know one position (`InvalidToken`, `UnexpectedEOF`) therefore
/// underline the single byte starting there: that is the character the parser
/// stopped on, and the range stays valid even at the end of the file.
fn one_byte_at(position: BytePos) -> Range<usize> {
    let start = position.0 as usize;

    start..start + 1
}

/// Keep a span-derived range visible, for the same reason as `one_byte_at`: a
/// range whose start equals its end underlines nothing.
fn non_empty(range: Range<usize>) -> Range<usize> {
    if range.start < range.end {
        range
    } else {
        range.start..range.start + 1
    }
}

/// lalrpop expected tokens in error are wrapped in double quote, which we don't really want
fn unquote_tokens(mut tokens: Vec<String>) -> Vec<String> {
    for token in &mut tokens {
        if token.starts_with('"') {
            token.remove(0);
        }

        if token.ends_with('"') {
            token.pop();
        }
    }

    tokens.to_vec()
}

impl From<ParseError<BytePos, Token, Error>> for Error {
    fn from(e: ParseError<BytePos, Token, Error>) -> Self {
        match e {
            ParseError::InvalidToken { location } => Error::InvalidToken(location),
            ParseError::UnrecognizedEof { location, expected } => Error::UnexpectedEOF {
                position: location,
                expected: unquote_tokens(expected),
            },
            ParseError::UnrecognizedToken { token, expected } => Error::UnexpectedToken {
                token: token.into(),
                expected: unquote_tokens(expected),
            },
            ParseError::ExtraToken { token } => Error::ExtraToken {
                token: token.into(),
            },
            ParseError::User { error } => error,
        }
    }
}

impl From<TokenizerError> for Error {
    fn from(e: TokenizerError) -> Self {
        Error::Tokenizer(e)
    }
}
impl From<LayoutError> for Error {
    fn from(e: LayoutError) -> Self {
        Error::Layout(e)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::position::spanned;
    use indoc::indoc;

    /// A rendered diagnostic has to point somewhere visible: a zero-width range
    /// renders as a caret with no character under it.
    fn assert_points_at_source(diagnostic: &Diagnostic<()>) {
        assert!(
            !diagnostic.labels.is_empty(),
            "diagnostic has no label: {:?}",
            diagnostic
        );

        let range = &diagnostic.labels[0].range;
        assert!(
            range.start < range.end,
            "the primary label is zero-width ({:?}) and would render as an invisible caret",
            range
        );
    }

    /// The headline is what the user reads first, so it has to be a sentence
    /// about their source rather than the name of a Rust type.
    fn assert_prose_message(diagnostic: &Diagnostic<()>, expected: &str) {
        assert_eq!(diagnostic.message, expected);
        assert!(
            diagnostic.message.split(' ').count() >= 3,
            "the message is not prose: {:?}",
            diagnostic.message
        );
    }

    /// Run a real source through the tokenizer and the layout pass — the two
    /// phases which raise these errors — and return the error it produced.
    ///
    /// This stops short of the grammar on purpose: the parser reads the token
    /// stream lazily and reports its own `UnexpectedToken` before the offending
    /// indentation is ever reached.
    fn layout_error(source: &str) -> Error {
        let tokens = crate::compiler::parser::tokenizer::make_tokenizer(source)
            .map(|r| r.map_err(|e| e.into()));

        let mut errors: Vec<Error> = crate::compiler::parser::layout::layout(tokens)
            .filter_map(|item| item.err())
            .collect();

        match errors.pop() {
            Some(e) => e,
            None => panic!("expected the source to fail the layout pass"),
        }
    }

    /// A mis-indented source reaches the user through `diagnostic`, so it must
    /// not panic on the way. Verified to fail by restoring the `e => todo!()`
    /// catch-all: this test then panics inside `diagnostic` instead of
    /// asserting.
    ///
    /// The source opens its top level declaration at column 3, which sets that
    /// context's minimum indentation, and then puts `|` at column 1.
    #[test]
    fn layout_error_of_a_mis_indented_top_level_declaration() {
        let error = layout_error(indoc! {"
              type Maybe
            | Nothing
        "});

        assert!(
            matches!(error, Error::Layout(_)),
            "expected a layout error, got {:?}",
            error
        );

        let diagnostic = error.diagnostic(());

        assert_prose_message(&diagnostic, "this line is not indented far enough");
        assert_points_at_source(&diagnostic);
        assert_eq!(
            diagnostic.labels[0].message,
            "this token starts at column 1, but its block requires column 3 or more"
        );
        assert_eq!(
            diagnostic.notes,
            vec!["the block it belongs to (a top level declaration) starts on line 1".to_string()]
        );
    }

    /// The column a `case … of` block requires is the one of its first branch,
    /// not the block context's own indentation, so the message has to report
    /// `Offside::min_indent` rather than `Offside::indent` (2 here, which the
    /// offending token does satisfy). Verified to fail by making
    /// `Offside::min_indent` return `self.indent` unconditionally — the layout
    /// pass enforces the rule through that same method, so the source below then
    /// stops being an error at all and the helper panics.
    #[test]
    fn layout_error_reports_the_case_block_minimum_indentation() {
        let error = layout_error(indoc! {"
            module Main exposing (..)

            f x =
              case x of
                Just y ->
              y
        "});

        let diagnostic = error.diagnostic(());

        assert_prose_message(&diagnostic, "this line is not indented far enough");
        assert_points_at_source(&diagnostic);
        assert_eq!(
            diagnostic.labels[0].message,
            "this token starts at column 3, but its block requires column 5 or more"
        );
        assert_eq!(
            diagnostic.notes,
            vec![
                "the block it belongs to (the branches of a `case … of`) starts on line 4"
                    .to_string()
            ]
        );
    }

    /// `UnexpectedEOF` only knows a single byte position, and
    /// `BytePos::to_range` is zero-width. Verified to fail by restoring the
    /// `e => todo!()` catch-all (panic), and again — once handled — by having
    /// the arm use `position.to_range()`, which turns the label into an
    /// invisible caret and reddens `assert_points_at_source`.
    #[test]
    fn unexpected_eof_points_at_the_end_of_the_file() {
        let error = Error::UnexpectedEOF {
            position: BytePos(12),
            expected: vec!["of".to_string(), "->".to_string()],
        };

        let diagnostic = error.diagnostic(());

        assert_prose_message(&diagnostic, "the file ended before the declaration did");
        assert_points_at_source(&diagnostic);
        assert_eq!(diagnostic.labels[0].range, 12..13);
        assert_eq!(
            diagnostic.notes,
            vec!["we were expecting one of the following tokens here: of, ->".to_string()]
        );
    }

    /// Same zero-width problem as `UnexpectedEOF`. Verified to fail by
    /// restoring the `e => todo!()` catch-all, and by swapping the widened
    /// range back to `position.to_range()`.
    #[test]
    fn invalid_token_points_at_a_character() {
        let error = Error::InvalidToken(BytePos(4));

        let diagnostic = error.diagnostic(());

        assert_prose_message(&diagnostic, "the parser could not read this token");
        assert_points_at_source(&diagnostic);
        assert_eq!(diagnostic.labels[0].range, 4..5);
    }

    /// `ExtraToken` carries a real span, so the label uses it directly.
    /// Verified to fail by restoring the `e => todo!()` catch-all.
    #[test]
    fn extra_token_points_at_the_leftover_token() {
        let error = Error::ExtraToken {
            token: spanned(BytePos(7), BytePos(11), Token::Type),
        };

        let diagnostic = error.diagnostic(());

        assert_prose_message(&diagnostic, "the module continues past its end");
        assert_points_at_source(&diagnostic);
        assert_eq!(diagnostic.labels[0].range, 7..11);
    }

    /// String literals are not implemented, so this variant is unconstructed
    /// today — it still has to render rather than panic. Verified to fail by
    /// restoring `TokenizerErrorType::StringError => todo!()`.
    #[test]
    fn tokenizer_string_error_renders() {
        let error = Error::Tokenizer(TokenizerError {
            error: spanned(BytePos(3), BytePos(9), TokenizerErrorType::StringError),
        });

        let diagnostic = error.diagnostic(());

        assert_prose_message(&diagnostic, "this string literal could not be read");
        assert_points_at_source(&diagnostic);
        assert_eq!(diagnostic.labels[0].range, 3..9);
    }

    /// Same as `tokenizer_string_error_renders`, for the sibling variant.
    /// Verified to fail by restoring
    /// `TokenizerErrorType::UnicodeError => todo!()`.
    #[test]
    fn tokenizer_unicode_error_renders() {
        let error = Error::Tokenizer(TokenizerError {
            error: spanned(BytePos(2), BytePos(8), TokenizerErrorType::UnicodeError),
        });

        let diagnostic = error.diagnostic(());

        assert_prose_message(
            &diagnostic,
            "this unicode escape sequence could not be read",
        );
        assert_points_at_source(&diagnostic);
        assert_eq!(diagnostic.labels[0].range, 2..8);
    }
}
