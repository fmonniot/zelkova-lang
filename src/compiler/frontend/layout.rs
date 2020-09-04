//! Simplify the indentation manager for the parser
//! by doing it in before the token iterator is passed to the parser.

use super::error::Error;
use super::tokenizer::{Spanned, Token};
use crate::compiler::position::Position;
use log::trace;
use std::cmp::Ordering;

// TODO Don't use context, maybe offside ?
#[derive(Debug, PartialEq, Clone)]
pub enum LayoutError {
    LayoutError { offside: Offside, token: Spanned },
}

pub fn layout<I: Iterator<Item = Result<Spanned, Error>>>(
    iter: I,
) -> impl Iterator<Item = Result<Spanned, Error>> {
    Layout::new(iter)
}

/// Context represent the kind of expression we are looking at.
///
/// It let us associate context-aware indentation rules
// TODO Remove context parameters
#[derive(Debug, PartialEq, Clone)]
pub enum Context {
    /// Context for a case expression with the number of indents required
    /// for each branch expression.
    Case,

    /// Context for a let expression
    Let,

    /// Context for a top level declaration.
    /// Those can be module, custom type, type alias or functions (type annotation/value).
    TopLevelDeclaration,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Offside {
    context: Context,
    indent: usize, // TODO rename to min_indent
    line: usize,
}

struct Contexts {
    stack: Vec<Offside>,
}

impl Contexts {
    fn new() -> Contexts {
        Contexts { stack: vec![] }
    }

    fn last(&self) -> Option<&Offside> {
        self.stack.last()
    }

    fn push(&mut self, offside: Offside) {
        self.stack.push(offside)
    }

    fn pop(&mut self) -> Option<Offside> {
        self.stack.pop()
    }
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
    contexts: Contexts,
    /// Buffer of tokens already read, but that couldn't have been emitted.
    ///
    /// For example, when opening a block we return the OpenBlock token and thus
    /// we have to reprocess the original token.
    reprocess_tokens: Vec<Spanned>,
}

impl<I> Layout<I>
where
    I: Iterator<Item = Result<Spanned, Error>>,
{
    /// Create and initialize a new `Layout` iterator
    pub fn new(iter: I) -> Layout<I> {
        Layout {
            tokens: iter,
            contexts: Contexts::new(),
            reprocess_tokens: vec![],
        }
    }

    /// A simple function which manage the internal lookahead structure
    /// in tandem with the source iterator.
    ///
    /// It also convert the end of the source iterator into `Token::EndOfFile`.
    fn next_token(&mut self) -> Result<Spanned, Error> {
        self.reprocess_tokens.pop().map(Ok).unwrap_or_else(|| {
            let next = self.tokens.next().unwrap_or_else(|| {
                // The absolute part is unused (hence 0) but the column value is important
                // (we want a value of 1 to match the first token of a line)
                let position = Position::new(0, 1, 1);

                Ok((position, Token::EndOfFile, position))
            });

            if let Ok((_, t, _)) = &next {
                if t == &Token::Newline || t == &Token::Indent || t == &Token::Dedent {
                    // Skip over newline or indentation tokens, which should be done in the tokenizer.
                    // We will do it once I'm convinced the approach in this module is worth it.
                    return self.next_token();
                }
            }

            next
        })
    }

    /// This is the entry point for our layout processor.
    ///
    /// It is called by the iterator's next on each token.
    ///
    fn handle_next_token(&mut self) -> Result<Spanned, Error> {
        let token = self.next_token()?;

        // Short circuit handling of EOF, unless we have some indentation
        // level to pop.
        if let (start, Token::EndOfFile, end) = &token {
            // We need to clean up all the accumulated contexts
            while let Some(offside) = self.contexts.pop() {
                if offside.context == Context::TopLevelDeclaration {
                    self.reprocess_tokens.push(token.clone());
                    return Ok((*start, Token::CloseBlock, *end));
                }
            }

            return Ok(token);
        }

        // Retrieve the current offside and, if none exists, create one,
        // put the current token on the back burner and emit the new block.
        // In theory this should only happens when we are looking at a top
        // level declaration (or it's a bug)
        let offside = match self.contexts.last() {
            Some(offside) => offside,
            None => {
                let off = Offside {
                    context: Context::TopLevelDeclaration,
                    indent: token.0.column,
                    line: token.0.line,
                };
                self.contexts.push(off);

                self.reprocess_tokens.push(token.clone());
                return Ok((token.0, Token::OpenBlock, token.0));
            }
        };

        /*  Elm has surprisingly few indentation rules:
            - case <> of must be followed by branches on indent + 1 level, and the content of each branch must be indent + 1 if on a next line
            - let <> in <>: the first block must be indent + 1 compared to the let keyword, and the in expression must be on indent + 1 of the _parent_ block
                            Note that I'll probably change the in rule to be at the same level.
            - top level ~function~ declaration body must either be one liner or be in an opened block at indent + 1 (this apply to function, custom types or type alias)
            - function application have no rules on where they should be. Meaning the let/in and case/of rules apply.

            We will start with those rules, but will probably implement a "strict mode" along the road to enforce some convention on
            indentation. Probably something loosely based on what elm-format recommend. Let's be draconian and enforce uniformity :pirate:.
        */

        trace!("token: {:?}, offside: {:?}", token, offside);

        // First, we check if we have a closing token with an associated context.
        // If we do, let's remove the context and return the token
        match (&token.1, &offside.context) {
            (Token::Of, Context::Case) => {
                // TODO Strange thing is, this might probably be wrong in retrospect.
                // Needs to investigate the parser, but we will probably need a case
                // block after Of, to group the different branches.
                self.contexts.pop();
                return Ok(token);
            }
            (Token::In, Context::Let) => {
                // TODO akin to of/case above, we might have to create a let/in block
                // to let the parser know when the let part ended. Not sure yet.
                self.contexts.pop();
                return Ok(token);
            }
            (Token::CloseBlock, Context::TopLevelDeclaration) => {
                self.contexts.pop();
                return Ok(token);
            }
            _ => (),
        }

        // Second, we enforce the indentation rules we have on record
        match token.0.column.cmp(&offside.indent) {
            Ordering::Less => {
                let offside = offside.clone();

                self.reprocess_tokens.push(token.clone());

                return Err(LayoutError::LayoutError { offside, token }.into());
            }
            ord => (), // ok
        };

        // Third, we create new tokens and emit block tokens as required

        match token.1 {
            Token::Case => {
                self.contexts.push(Offside {
                    context: Context::Case,
                    indent: token.0.column + 1,
                    line: token.0.line,
                });
            }
            Token::Let => self.contexts.push(Offside {
                context: Context::Let,
                indent: token.0.column + 1,
                line: token.0.line,
            }),
            Token::OpenBlock => (),
            _ => {
                if token.0.column == 1 && token.0.line > offside.line {
                    // Here we have a token which isn't OpenBlock (special case above)
                    // but which is at the beginning of a new line. This most probably
                    // mean we have reached the end of the previous block and are
                    // starting a new one.

                    self.reprocess_tokens.push(token.clone());

                    // Furthermore in case of implicitely terminated block,
                    // pop the context from the stack and let the parser complain
                    // about the invalid syntax. We do this to break an infinite
                    // loop where we would always be checking the current token
                    // against the current context.
                    if offside.context == Context::TopLevelDeclaration {
                        self.contexts.pop();
                    }

                    return Ok((token.0, Token::CloseBlock, token.2));
                }
            }
        }

        Ok(token)
    }
}

impl<I> Iterator for Layout<I>
where
    I: Iterator<Item = Result<Spanned, Error>>,
{
    type Item = Result<Spanned, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.handle_next_token() {
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
        let mut pos = Position::new(0, 1, 1);

        tokens
            .into_iter()
            .cloned()
            .map(|token| {
                let start = pos.clone();
                let inc = match &token {
                    Token::Module => 6,
                    Token::Identifier { name } => name.len(),
                    Token::Exposing => 8,
                    Token::LPar | Token::RPar => 1,
                    Token::Comma => 1,
                    Token::Indent => 2,
                    Token::Pipe => 1,
                    Token::Equal => 1,
                    Token::Type => 4,
                    _ => 0,
                };

                if &token == &Token::Newline {
                    pos.new_line();
                } else {
                    pos.increment_by(inc);
                }

                let end = pos.clone();

                Ok((start, token, end))
            })
            .collect()
    }

    fn test_layout_without_error(source: Vec<Token>, expectation: Vec<Token>) {
        let v: Vec<_> = layout(tokens_to_spanned(&source).into_iter())
            .map(|x| x.expect("no error in layout").1)
            .collect();

        assert_eq!(v, expectation);
    }

    fn ident_token(s: &str) -> Token {
        Token::Identifier {
            name: s.to_string(),
        }
    }

    #[test]
    fn module_declaration_single_line() {
        test_layout_without_error(
            vec![
                Token::Module,
                ident_token("Main"),
                Token::Exposing,
                Token::LPar,
                ident_token("main"),
                Token::Comma,
                ident_token("const"),
                Token::RPar,
                Token::Newline,
            ],
            vec![
                Token::OpenBlock,
                Token::Module,
                ident_token("Main"),
                Token::Exposing,
                Token::LPar,
                ident_token("main"),
                Token::Comma,
                ident_token("const"),
                Token::RPar,
                Token::CloseBlock,
            ],
        )
    }

    #[test]
    fn module_declaration_multi_line() {
        test_layout_without_error(
            vec![
                Token::Module,
                ident_token("Maybe"),
                Token::Exposing,
                Token::Newline,
                Token::Indent,
                Token::LPar,
                ident_token("Maybe"),
                Token::LPar,
                Token::DotDot,
                Token::RPar,
                Token::Newline,
                Token::Indent,
                Token::Comma,
                ident_token("andThen"),
                Token::Newline,
                Token::Indent,
                Token::Comma,
                ident_token("map"),
                Token::Newline,
                Token::Indent,
                Token::RPar,
                Token::Newline,
            ],
            vec![
                Token::OpenBlock,
                Token::Module,
                ident_token("Maybe"),
                Token::Exposing,
                Token::LPar,
                ident_token("Maybe"),
                Token::LPar,
                Token::DotDot,
                Token::RPar,
                Token::Comma,
                ident_token("andThen"),
                Token::Comma,
                ident_token("map"),
                Token::RPar,
                Token::CloseBlock,
            ],
        )
    }

    #[test]
    fn type_declaration_multi_line() {
        test_layout_without_error(
            vec![
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
            ],
            vec![
                Token::OpenBlock,
                Token::Type,
                ident_token("Maybe"),
                ident_token("a"),
                Token::Equal,
                ident_token("Just"),
                ident_token("a"),
                Token::Pipe,
                ident_token("Nothing"),
                Token::CloseBlock,
            ],
        )
    }

    #[test]
    fn top_level_implicit_code_block() {
        test_layout_without_error(
            vec![
                Token::Type,
                ident_token("Maybe"),
                ident_token("a"),
                Token::Newline,
                Token::Indent,
                Token::Equal,
                ident_token("Just"),
                ident_token("a"),
                Token::Newline, // Here we are missing an indent
                Token::Pipe,
                ident_token("Nothing"),
                Token::Newline,
            ],
            vec![
                Token::OpenBlock,
                Token::Type,
                ident_token("Maybe"),
                ident_token("a"),
                Token::Equal,
                ident_token("Just"),
                ident_token("a"),
                Token::CloseBlock,
                // Because we missed the indent, we went back to the beginning
                // of the line and triggered a new block.
                Token::OpenBlock,
                Token::Pipe,
                ident_token("Nothing"),
                Token::CloseBlock,
            ],
        )
    }
}
