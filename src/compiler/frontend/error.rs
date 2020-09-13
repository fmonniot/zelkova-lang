use super::layout::LayoutError;
use super::tokenizer::{LexicalError, Spanned, Token};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use lalrpop_util::ParseError;

use crate::compiler::position::Position;

//pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    Tokenizer(LexicalError),
    Layout(LayoutError),
    // Errors coming from the parser
    InvalidToken(Position),
    UnexpectedEOF {
        position: Position,
        expected: Vec<String>, // The kind of token the parser was expecting
    },
    UnexpectedToken {
        token: Spanned,
        expected: Vec<String>, // The kind of token the parser was expecting
    },
    ExtraToken {
        token: Spanned,
    },
}

impl Error {
    pub fn diagnostic(&self, name: usize) -> Diagnostic<usize> {
        match self {
            Error::UnexpectedToken { token, expected } => {
                let (start, token, end) = token;
                let range = start.absolute..end.absolute;

                Diagnostic::error()
                    .with_message(format!("unexpected token: `{:?}`", token)) // TODO display instead of debug
                    .with_labels(vec![
                        Label::primary(name, range).with_message("unexpected token")
                    ])
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

impl From<ParseError<Position, Token, Error>> for Error {
    fn from(e: ParseError<Position, Token, Error>) -> Self {
        match e {
            ParseError::InvalidToken { location } => Error::InvalidToken(location),
            ParseError::UnrecognizedEOF { location, expected } => Error::UnexpectedEOF {
                position: location,
                expected: unquote_tokens(expected),
            },
            ParseError::UnrecognizedToken { token, expected } => Error::UnexpectedToken {
                token,
                expected: unquote_tokens(expected),
            },
            ParseError::ExtraToken { token } => Error::ExtraToken { token },
            ParseError::User { error } => error,
        }
    }
}

impl From<LexicalError> for Error {
    fn from(e: LexicalError) -> Self {
        Error::Tokenizer(e)
    }
}
impl From<LayoutError> for Error {
    fn from(e: LayoutError) -> Self {
        Error::Layout(e)
    }
}
