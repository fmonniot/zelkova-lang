use super::indentation::IndentationError;
use super::tokenizer::{LexicalError, Spanned, Token};
use lalrpop_util::ParseError;

use crate::compiler::position::Position;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    Tokenizer(LexicalError),
    Indentation(IndentationError),
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
    pub fn position_start(&self) -> &Position {
        match self {
            Error::Tokenizer(e) => &e.position,
            Error::Indentation(IndentationError::IndentationError { spanned, .. }) => &spanned.0,
            Error::Indentation(IndentationError::NotInitialized) => {
                panic!("Error shouldn't be reachable")
            }
            Error::InvalidToken(position) => &position,
            Error::UnexpectedEOF { position, .. } => &position,
            Error::UnexpectedToken { token, .. } => &token.0,
            Error::ExtraToken { token } => &token.0,
        }
    }
}

impl From<ParseError<Position, Token, Error>> for Error {
    fn from(e: ParseError<Position, Token, Error>) -> Self {
        match e {
            ParseError::InvalidToken { location } => Error::InvalidToken(location),
            ParseError::UnrecognizedEOF { location, expected } => Error::UnexpectedEOF {
                position: location,
                expected,
            },
            ParseError::UnrecognizedToken { token, expected } => {
                Error::UnexpectedToken { token, expected }
            }
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

impl From<IndentationError> for Error {
    fn from(e: IndentationError) -> Self {
        Error::Indentation(e)
    }
}
