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
    Layout::new(iter)
}

/// Context represent the kind of expression we are looking at.
///
/// It let us associate context-aware indentation rules
#[derive(Debug, PartialEq, Clone)]
pub enum Context {
    /// This is a type context with its indentation level
    Type(Option<u8>),

    /// We are declaring a module.
    Module,
}

/// The Layout struct is an iterator over a serie of Spanned tokens
/// which is managing some indentation rules.
///
/// It does so by having a context for the current token. A context
/// represent what kind of terms we are looking at and what indentation
/// rules we should apply.
///
/// > **TODO**: I really need to use references with this structure. There are too many
/// > `clone()` call on something which is part of the core loop. Let's do so
/// > once I have a somewhat working algorithm.
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
    /// Create and initialize a new `Layout` iterator
    pub fn new(iter: I) -> Layout<I> {
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
        let _ = l.next_token();
        let _ = l.next_token();

        l
    }

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

            /* We are declaring a module. There are two cases here:

                1. This is a single line declaration, in which case no context needs
                   to be created and pushed on the stack. We know it is a single line declaration
                   when the last token is a closing parenthesis.
                2. This a multi line declaration (eg. the closing parenthesis is somewhere
                   in later lines). We require an indentation level of at least one space.
            */
            (Ok((_, Token::Module, _)), None) => {
                // TODO Check we are emitting the module token
                let last = self.emit_to_end_of_line(false);

                if let Some(Ok((_, Token::RPar, end))) = last {
                    // the module declaration is closed on the same line, we are done

                    // still emit the new line we skipped over previously
                    // (this is only necessary until we have a block concept in place. At which
                    // point we will close the block instead)
                    let mut real_end = end.clone();
                    real_end.newline();
                    self.emit(Ok((end, Token::Newline, real_end)));
                } else {
                    self.contexts.push(Context::Module);
                }
            }

            (Ok((_, _, _)), Some(Context::Module)) => {
                // Current indentation level needs to be at least 1
                if current_indent < 1 {
                    // TODO not good, emit an error
                }

                let last = self.emit_to_end_of_line(false);
                if let Some(Ok((_, Token::RPar, end))) = last {
                    // the module declaration is closed, let's remove the context
                    self.contexts.pop();

                    // and emit the new line we skipped over previously
                    // (this is only necessary until we have a block concept in place. At which
                    // point we will close the block instead)
                    let mut real_end = end.clone();
                    real_end.newline();
                    self.emit(Ok((end, Token::Newline, real_end)));
                }
            }

            // We enter a type definition section. They can only happens without indentation,
            // hence there must be no context available.
            (Ok((_, Token::Type, _)), None) => {
                // We enter a type definition section (but we don't know the indent yet)
                self.contexts.push(Context::Type(None));

                // Let's emit the current line
                self.emit_to_end_of_line(true);

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
                self.emit_to_end_of_line(true);
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
                let _ = self.next_token(); // skip over the indent token
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

    /// emit the given token to the `processed_tokens` buffer
    fn emit(&mut self, tok: Result<Spanned, Error>) {
        self.processed_tokens.push(tok);
    }

    /// Emit all tokens until the end of the line, leaving the pointer after the newline/eof
    /// token and returning the token just before the newline/eof (when there is one)
    ///
    /// TODO Remove emit_new_line once we have a block concept to use in our grammar.
    /// e.g. the grammar won't need to know about new lines at all.
    fn emit_to_end_of_line(&mut self, emit_new_line: bool) -> Option<Result<Spanned, Error>> {
        // First let's look at the current token.
        // If we are already looking at a newline/eof, there is nothing to return.
        // Let's consume and emit the token and be done
        match self.lookahead.0 {
            Ok((_, Token::Newline, _)) => {
                let current = self.next_token();
                if emit_new_line {
                    self.emit(current);
                }
                return None;
            }
            Ok((_, Token::EndOfFile, _)) => {
                let current = self.next_token();
                self.emit(current);
                return None;
            }
            _ => (),
        };

        // Ok here we know we have at least two tokens in the buffer.
        // So let's peak at current + 1 token, and if it's a newline/eof
        // we can return the current token, advance our iterator by 2 and then return
        loop {
            let prev = self.next_token();
            self.emit(prev.clone()); // TODO Find a way to not clone here

            // Looking at the new current
            match self.lookahead.0 {
                Ok((_, Token::Newline, _)) => {
                    let newline = self.next_token();
                    if emit_new_line {
                        self.emit(newline);
                    }
                    return Some(prev);
                }
                Ok((_, Token::EndOfFile, _)) => {
                    let eof = self.next_token();
                    self.emit(eof);
                    return Some(prev);
                }
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
    fn emit_end_of_line() {
        let tokens = tokens_to_spanned(&vec![
            ident_token("Main"),
            Token::Newline,
            ident_token("main"),
            Token::Equal,
            ident_token("const"),
            Token::Newline,
            Token::Newline,
        ]);

        let mut layout = Layout::new(tokens.iter().cloned());

        // Test with one token in a line
        let ret = layout.emit_to_end_of_line(true);
        assert_eq!(ret, Some((&tokens[0]).clone()));

        // Test with multiple token in a line
        let ret = layout.emit_to_end_of_line(true);
        assert_eq!(ret, Some((&tokens[4]).clone()));

        // Test with an empty line
        let ret = layout.emit_to_end_of_line(true);
        assert_eq!(ret, None);

        // Test at the end of the file
        let ret = layout.emit_to_end_of_line(true);
        assert_eq!(ret, None);
    }

    #[test]
    fn emit_module_declaration_single_line() {
        let module_tokens = tokens_to_spanned(&vec![
            Token::Module,
            ident_token("Main"),
            Token::Exposing,
            Token::LPar,
            ident_token("main"),
            Token::Comma,
            ident_token("const"),
            Token::RPar,
            Token::Newline,
        ]);
        let spanned = module_tokens
            .iter()
            .cloned()
            .map(|r| r.unwrap())
            .collect::<Vec<_>>();

        let v: Vec<_> = layout(module_tokens.iter().cloned())
            .map(|x| x.expect("no error in layout"))
            .collect();

        assert_eq!(v, spanned);
    }

    #[test]
    fn emit_module_declaration_multi_line() {
        let module_tokens = tokens_to_spanned(&vec![
            Token::Module,
            ident_token("Main"),
            Token::Exposing,
            Token::Newline,
            Token::Indent,
            Token::LPar,
            ident_token("main"),
            Token::Newline,
            Token::Indent,
            Token::Comma,
            ident_token("const"),
            Token::Newline,
            Token::Indent,
            Token::RPar,
            Token::Newline,
        ]);
        let max_index = module_tokens.len() - 1;
        let spanned = module_tokens
            .iter()
            .cloned()
            .map(|r| r.unwrap())
            .enumerate()
            .filter_map(|(idx, s)| {
                // Filter out indent and newlines, except for the last new line
                if s.1 == Token::Indent || (s.1 == Token::Newline && idx != max_index) {
                    None
                } else {
                    Some(s)
                }
            })
            .collect::<Vec<_>>();

        let v: Vec<_> = layout(module_tokens.iter().cloned())
            .map(|x| x.expect("no error in layout"))
            .collect();

        assert_eq!(v, spanned);
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
