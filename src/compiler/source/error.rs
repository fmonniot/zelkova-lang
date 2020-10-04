use super::layout::LayoutError;
use super::tokenizer::{Token, TokenizerError};
use crate::compiler::position::{BytePos, Spanned};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use lalrpop_util::ParseError;

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
    pub fn diagnostic<Id>(&self, name: Id) -> Diagnostic<Id> {
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

            e => todo!("{:?}", e),
        }
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
            ParseError::UnrecognizedEOF { location, expected } => Error::UnexpectedEOF {
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
