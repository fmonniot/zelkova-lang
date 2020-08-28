//! Simplify the indentation manager for the parser
//! by doing it in before the token iterator is passed to the parser.

use super::error::Error;
use super::tokenizer::{Spanned, Token};
use crate::compiler::position::Position;

#[derive(Debug, PartialEq, Clone)]
pub enum IndentationError {
    IndentationError { context: Context, spanned: Spanned },
    NotInitialized,
}

pub fn layout<I: Iterator<Item = Result<Spanned, Error>>>(
    iter: I,
) -> impl Iterator<Item = Result<Spanned, Error>> {
    let mut l = Layout {
        tokens: iter,
        contexts: vec![],
        processed_tokens: vec![],
        lookahead: (
            Err(Error::Indentation(IndentationError::NotInitialized)),
            Err(Error::Indentation(IndentationError::NotInitialized)),
        ),
    };

    // Fill out the lookahead structure
    l.next_token();
    l.next_token();

    l
}

/// Context represent the kind of expression we are looking at.
///
/// It let us associate context-aware indentation rules
#[derive(Debug, PartialEq, Clone)]
pub enum Context {
    /// This is a type context with its indentation level
    Type(Option<u8>),
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
    contexts: Vec<Context>,
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
    I: Iterator<Item = Result<Spanned, Error>>,
{
    /// A simple function which manage the internal lookahead structure
    /// in tandem with the source iterator.
    ///
    /// It also convert the end of the source iterator into `Token::EndOfFile`.
    fn next_token(&mut self) -> Result<Spanned, Error> {
        let current = self.lookahead.0.clone(); // should probably use reference here

        self.lookahead.0 = self.lookahead.1.clone();
        self.lookahead.1 = self.tokens.next().unwrap_or_else(|| {
            // This position is discarded, so can be rubish
            let position = Position::new(0, 0);

            Ok((position, Token::EndOfFile, position))
        });

        current
    }

    /// This is the main function of this iterator. It's in charge of detecting
    /// and applying the indentation logic.
    ///
    /// This function always assume it is called on the beginning of a new line
    /// and will consume all tokens of the line before returning.
    fn consume(&mut self) {
        let current_indent = self.consume_indent();
        let current_context = self.contexts.last();

        match (self.lookahead.0.as_ref(), current_context) {
            // end early if we have reached the end of the stream and deindented everything.
            (Ok((_, Token::EndOfFile, _)), _) => {
                // TODO Check indentation_level
                let token = self.next_token();
                self.emit(token);
                return;
            }

            // We enter a type definition section. They can only happens without indentation,
            // hence there must be no context available.
            (Ok((_, Token::Type, _)), None) => {
                // We enter a type definition section (but we don't know the indent yet)
                self.contexts.push(Context::Type(None));

                // Let's emit the current line
                self.emit_to_end_of_line();

                // Alg:
                // 1. Skip to end of the line
                // 2. At next line,
                //       if there is nothing exit the context
                //       if there is indent+ followed by a =, count indent and store in context
                //           skip line, look at next line: nothing => exit, otherwise require number of indent
            }

            // If we have an empty line with a type context, it means we can end the context
            (Ok((_, Token::Newline, _)), Some(Context::Type(_))) => {
                self.contexts.pop();
                let token = self.next_token();
                self.emit(token);
            }

            // Here we have a type definition context open and we found a delimiter token (= or |)
            (Ok(spanned @ (_, Token::Equal, _)), Some(Context::Type(type_indent)))
            | (Ok(spanned @ (_, Token::Pipe, _)), Some(Context::Type(type_indent))) => {
                match type_indent {
                    Some(i) => {
                        // We have an indent to match
                        if i != &current_indent {
                            // Indent mismatch, let's emit an error
                            // TODO
                            self.processed_tokens
                                .push(Err(IndentationError::IndentationError {
                                    context: Context::Type(Some(*i)),
                                    spanned: spanned.clone(),
                                }
                                .into()))
                        }
                    }
                    None => {
                        // We don't have any indentation recorded yet, let's set it up
                        self.contexts.pop();
                        self.contexts.push(Context::Type(Some(current_indent)));
                    }
                }

                // Then consume the rest of the line
                self.emit_to_end_of_line();
            }

            // Another type of token, let's just emit it as is
            _ => {
                let token = self.next_token();
                self.emit(token)
            }
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
    I: Iterator<Item = Result<Spanned, Error>>,
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
            tok => Some(tok),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::*;
    use crate::compiler::position::Position;
    use tokenizer::Token;

    // Create an approximation for the token position in the stream.
    // We don't count the spaces between tokens, but it gives us enough
    // to understand where a failure happened.
    fn tokens_to_spanned(tokens: &Vec<Token>) -> Vec<Result<Spanned, Error>> {
        let mut pos = Position::new(0, 0);

        tokens
            .into_iter()
            .cloned()
            .map(|token| {
                let start = pos.clone();
                let inc = match &token {
                    Token::Module => 6,
                    Token::Identifier { name } => name.len() as u32,
                    Token::Exposing => 8,
                    Token::LPar | Token::RPar => 1,
                    Token::Comma => 1,
                    Token::Indent => 2,
                    Token::Pipe => 1,
                    _ => 0,
                };

                pos.go_right_by(inc);

                if token == Token::Newline {
                    pos.newline();
                }

                let end = pos.clone();

                Ok((start, token, end))
            })
            .collect()
    }

    fn ident_token(s: &str) -> Token {
        Token::Identifier {
            name: s.to_string(),
        }
    }

    #[test]
    fn emit_module_declaration() {
        let module_tokens = vec![
            Token::Module,
            ident_token("Main"),
            Token::Exposing,
            Token::LPar,
            ident_token("main"),
            Token::RPar,
            Token::Newline,
        ];

        let v: Vec<_> = layout(tokens_to_spanned(&module_tokens).into_iter())
            .map(|x| x.expect("no error in layout").1)
            .collect();

        assert_eq!(v, module_tokens);
    }

    #[test]
    fn emit_type_declaration_ok() {
        let v: Vec<_> = layout(
            tokens_to_spanned(&vec![
                Token::Type,
                ident_token("Maybe"),
                ident_token("a"),
                Token::Newline,
                Token::Indent,
                Token::Equal,
                ident_token("Just"),
                ident_token("a"),
                Token::Newline,
                Token::Indent,
                Token::Pipe,
                ident_token("Nothing"),
                Token::Newline,
            ])
            .into_iter(),
        )
        .map(|x| x.expect("no error in layout").1)
        .collect();

        assert_eq!(
            v,
            vec![
                Token::Type,
                ident_token("Maybe"),
                ident_token("a"),
                Token::Newline,
                Token::Equal,
                ident_token("Just"),
                ident_token("a"),
                Token::Newline,
                Token::Pipe,
                ident_token("Nothing"),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn emit_type_declaration_error() {
        let v: Vec<_> = layout(
            tokens_to_spanned(&vec![
                Token::Type,
                ident_token("Maybe"),
                ident_token("a"),
                Token::Newline,
                Token::Indent,
                Token::Equal,
                ident_token("Just"),
                ident_token("a"),
                Token::Newline,
                Token::Indent,
                Token::Indent, // Here we have one indent too many
                Token::Pipe,
                ident_token("Nothing"),
                Token::Newline,
            ])
            .into_iter(),
        )
        .map(|x| x.map(|s| s.1))
        .collect();

        assert_eq!(
            v,
            vec![
                Ok(Token::Type),
                Ok(ident_token("Maybe")),
                Ok(ident_token("a")),
                Ok(Token::Newline),
                Ok(Token::Equal),
                Ok(ident_token("Just")),
                Ok(ident_token("a")),
                Ok(Token::Newline),
                Err(IndentationError::IndentationError {
                    context: Context::Type(Some(1)),
                    spanned: (Position::new(2, 4), Token::Pipe, Position::new(2, 5)),
                }
                .into()),
                Ok(Token::Pipe),
                Ok(ident_token("Nothing")),
                Ok(Token::Newline),
            ]
        );
    }
}
