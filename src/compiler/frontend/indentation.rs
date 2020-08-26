//! Simplify the indentation manager for the parser
//! by doing it in before the token iterator is passed to the parser.

use super::tokenizer::{Result, Spanned, Token};
use crate::compiler::position::Position;

pub fn layout<I: Iterator<Item = Result<Spanned>>>(
    iter: I,
) -> impl Iterator<Item = Result<Spanned>> {
    Layout { 
        tokens: iter,
        indentation: vec![],
        processed_tokens: vec![],
    }
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
struct Layout<I> {
    tokens: I,
    indentation: Vec<Context>,
    /// Buffer of processed tokens ready to be emitted.
    /// 
    /// We need this buffer as some tokens needs to lookahead in the source
    /// tokens. And as we don't want to lose any tokens, we need to put those
    /// somewhere until the consumer ask for them.
    processed_tokens: Vec<Result<Spanned>>,
}

impl<I> Layout<I> 
where
    I: Iterator<Item = Result<Spanned>>,
{
    /// utility to transform the source None into an EndOfFile
    /// Not entirely sure if I need this function actually.
    fn next_token(&mut self) -> Result<Spanned> {
        self.tokens
            .next()
            .unwrap_or_else(|| {
                // This position is discarded, so can be rubish
                let position = Position::new(0, 0);

                Ok((position, Token::EndOfFile, position))
            })
    }

    /// This is the main function of this iterator. It's in charge of detecting
    /// and applying the indentation logic.
    fn layout(&mut self) -> Result<Spanned> {
        let token = self.next_token()?;

        // end early if we have reached the end of the stream and
        // deindented everything.
        if token.1 == Token::EndOfFile && self.indentation.is_empty() {
            return Ok(token);
        }

        // Match the current token
        match token.1 {
            Token::Type => {
                // We enter a type definition section
                self.emit(Ok(token));
                self.emit_until_end_of_line();
                // Here we are at the beginning of the next line

                loop {
                    let next = self.next_token()?;

                    match next.1 {
                        // If a new line immediately, the type definition was a one-liner
                        Token::Newline => break,

                        Token::Indent => {
                            // Ok, we probably have a multi line definition here
                            let (indent, next_tok) = self.consume_indent();
                            self.indentation.push(Context::Type(indent));

                            // Now we are looking at the end of the indent section
                            // TODO
                            
                        },
                        // Probably a syntax error, let the parser deal with this case
                        _ => break,
                    }
                }

                // Alg:
                // 1. Skip to end of the line
                // 2. At next line,
                //       if there is nothing exit the context
                //       if there is indent+ followed by a =, count indent and store in context
                //           skip line, look at next line: nothing => exit, otherwise require number of indent
            },
            _ => (),
        }

        todo!()
    }


    fn consume_indent(&mut self) -> (u8, Result<Spanned>) {
        let mut indent = 0;
        let mut next = self.next_token();

        loop {            
            if let Ok((_, Token::Indent, _)) = next {
                indent += 1;
            } else {
                // We found a non-indent character, let's stop here
                break;
            }
            
            next = self.next_token();
        }

        (indent, next)
    }


    //
    // Utility functions
    //

    fn emit(&mut self, tok: Result<Spanned>) {
        self.processed_tokens.push(tok);
    }

    fn emit_until_end_of_line(&mut self) {
        loop {
            let next = self.next_token();
            self.emit(next.clone()); // TODO Find a way to not clone here

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
    I: Iterator<Item = Result<Spanned>>,
{
    type Item = Result<Spanned>;

    fn next(&mut self) -> Option<Self::Item> {
        // First let's emit the already processed tokens
        if !self.processed_tokens.is_empty() {
            return Some(self.processed_tokens.remove(0));
        }

        // We have catch-up with the already processed tokens, let's
        // continue reading the source.
        match self.layout() {
            Ok((_, Token::EndOfFile, _)) => None,
            tok => Some(tok)
        }
    }
}
