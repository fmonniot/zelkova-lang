//! Simplify the indentation manager for the parser
//! by doing it in before the token iterator is passed to the parser.

use super::tokenizer::{Spanned, Token, LexicalError};
use crate::compiler::position::Position;

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    LexicalError(LexicalError),
    NotInitialized,
}

pub fn layout<I: Iterator<Item = Result<Spanned, LexicalError>>>(
    iter: I,
) -> impl Iterator<Item = Result<Spanned, Error>> {
    let mut l = Layout { 
        tokens: iter,
        indentation: vec![],
        processed_tokens: vec![],
        lookahead: (Err(Error::NotInitialized), Err(Error::NotInitialized)),
    };

    // Fill out the lookahead structure
    l.next_token();
    l.next_token();

    l
}

/// Context represent the kind of expression we are looking at.
/// 
/// It let us associate context-aware indentation rules
enum Context {
    /// This is a type context with its indentation level
    Type(u8),
}

/// The Layout struct is an iterator over a serie of Spanned tokens
/// which is managing some indentation rules.
///
/// It does so by having a context for the current token. A context
/// represent what kind of terms we are looking at and what indentation
/// rules we should apply.
///
/// TODO: I really need to use references with this one. There are too many
/// `clone()` call on something which is part of the core loop. Let's do so
/// once I have a somewhat working algorithm.
struct Layout<I> {
    /// The source iterator
    tokens: I,
    /// The current level of indentation
    indentation: Vec<Context>,
    /// Buffer of processed tokens ready to be emitted.
    /// 
    /// We need this buffer as some tokens needs to lookahead in the source
    /// tokens. And as we don't want to lose any tokens, we need to put those
    /// somewhere until the consumer ask for them.
    processed_tokens: Vec<Result<Spanned, Error>>,
    /// The current token (and the next one)
    lookahead: (Result<Spanned, Error>, Result<Spanned, Error>),
}

impl<I> Layout<I> 
where
    I: Iterator<Item = Result<Spanned, LexicalError>>,
{

    /// A simple function which manage the internal lookahead structure
    /// in tandem with the source iterator.
    /// 
    /// It also convert the end of the source iterator into `Token::EndOfFile`.
    fn next_token(&mut self) -> Result<Spanned, Error> {
        let current = self.lookahead.0.clone(); // should probably use reference here

        self.lookahead.0 = self.lookahead.1.clone();
        self.lookahead.1 =  self.tokens
            .next()
            .unwrap_or_else(|| {
                // This position is discarded, so can be rubish
                let position = Position::new(0, 0);

                Ok((position, Token::EndOfFile, position))
            })
            .map_err(Error::LexicalError);

        current
    }

    /// This is the main function of this iterator. It's in charge of detecting
    /// and applying the indentation logic.
    fn consume(&mut self) {
        match self.lookahead.0 {
            // end early if we have reached the end of the stream and deindented everything.
            Ok((_, Token::EndOfFile, _)) => {
                // TODO Check indentation_level
                let token = self.next_token();
                self.emit(token);
                return;
            }

            Ok((_, Token::Type, _)) => {
                // We enter a type definition section
                // Let's emit the current line
                let token = self.next_token();
                self.emit(token);
                self.emit_to_end_of_line();

                // Here self.lookahead.0 should point to the first token on the next line

                match self.lookahead.0 {
                    // If a new line immediately, the type definition was a one-liner
                    // let's emit the token and stop the current layout
                    Ok((_,Token::Newline,_)) => {
                        let next = self.next_token();
                        self.emit(next);
                        return
                    },

                    Ok((_,Token::Indent,_)) => {
                        // Ok, we probably have a multi line definition here
                        let indent = self.consume_indent();

                        match self.lookahead.0 {
                            Ok((_, Token::Equal, _))
                            | Ok((_, Token::Pipe, _)) => {
                                // We have a pipe declaration
                            },
                            _ => {
                                // Not sure what we have here
                            }
                        }
                        self.indentation.push(Context::Type(indent));

                        // Now we are looking at the end of the indent section
                        // TODO
                        
                    },
                    // Probably a syntax error, let the parser deal with this case
                    _ => return,
                }

                // Alg:
                // 1. Skip to end of the line
                // 2. At next line,
                //       if there is nothing exit the context
                //       if there is indent+ followed by a =, count indent and store in context
                //           skip line, look at next line: nothing => exit, otherwise require number of indent
            }

            // Another type of token, let's just return it
            _ => {
                let token = self.next_token();
                self.emit(token)
            },
        }
    }

    /// consume the tokens until a non-`Token::Indent` token is found and
    /// return the number of such tokens consumed.
    /// 
    /// `lookahead.0` will be left pointing at the first non-indent token found
    fn consume_indent(&mut self) -> u8 {
        let mut indent = 0;

        loop {            
            if let Ok((_, Token::Indent, _)) = self.lookahead.0 {
                indent += 1;
                self.next_token(); // skip over the indent token
            } else {
                // We found a non-indent character, let's stop here
                break;
            }
        }

        indent
    }


    //
    // Utility functions
    //

    fn emit(&mut self, tok: Result<Spanned, Error>) {
        self.processed_tokens.push(tok);
    }

    fn emit_to_end_of_line(&mut self) {
        loop {
            let next = self.next_token();
            self.emit(next.clone()); // TODO Find a way to not clone here

            // Breaking here means we have already emitted the nl/eof token
            match next {
                Ok((_, Token::Newline, _)) => break,
                Ok((_, Token::EndOfFile, _)) => break,
                _ => (),
            }
        }
    }
}

impl<I> Iterator for Layout<I>
where
    I: Iterator<Item = Result<Spanned, LexicalError>>,
{
    type Item = Result<Spanned, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        // Let's fill the processed_tokens queue if it's empty
        while self.processed_tokens.is_empty() {
            self.consume();
        }        

        // We have something to emit, let's consume the queue
        match self.processed_tokens.remove(0) {
            Ok((_, Token::EndOfFile, _)) => None,
            tok => Some(tok)
        }
    }
}
