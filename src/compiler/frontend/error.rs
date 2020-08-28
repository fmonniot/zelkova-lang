use super::indentation::IndentationError;
use super::tokenizer::{LexicalError, Token};
use lalrpop_util::ParseError;

use crate::compiler::position::Position;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    Tokenizer(LexicalError),
    Indentation(IndentationError),
    //Parser(ParseError<Position, Token, Error>),
}

impl From<ParseError<Position, Token, Error>> for Error {
    fn from(e: ParseError<Position, Token, Error>) -> Self {
        match e {
            ParseError::InvalidToken { location } => todo!(),
            ParseError::UnrecognizedEOF { location, expected } => todo!(),
            ParseError::UnrecognizedToken { token, expected } => todo!(),
            ParseError::ExtraToken { token } => todo!(),
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
