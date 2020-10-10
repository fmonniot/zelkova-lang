//! Simplify the indentation manager for the parser
//! by doing it in before the token iterator is passed to the parser.

use super::error::Error;
use super::tokenizer::Token;
use crate::compiler::position::{spanned, BytePos, Position, Span, Spanned};
use log::trace;
use std::cmp::Ordering;

#[derive(Debug, PartialEq, Clone)]
pub enum LayoutError {
    LayoutError {
        offside: Offside,
        token: Spanned<Position, Token>,
    },
}

pub fn layout<I: Iterator<Item = Result<Spanned<Position, Token>, Error>>>(
    iter: I,
) -> impl Iterator<Item = Result<(BytePos, Token, BytePos), Error>> {
    Layout::new(iter)
}

/// Context represent the kind of expression we are looking at.
///
/// It let us associate context-aware indentation rules
///
/// ## Elm Rules
///
/// Elm has surprisingly few indentation rules:
/// - `case <> of` must be followed by branches on indent + 1 level, and the content of each branch must be indent + 1 if on a next line
/// - `let <> in`: the first block must be indent + 1 compared to the let keyword, and the in expression must be on indent + 1 of the _parent_ block
///                 Note that I'll probably change the in rule to be at the same level.
/// - top level declaration body must either be one liner or be in an opened block at indent + 1 (this apply to function, custom types or type alias)
/// - function application have no rules on where they should be. Meaning the let/in and case/of rules apply.
///
/// We will start with those rules, but will probably implement a "strict mode" along the road to enforce some convention on
/// indentation. Probably something loosely based on what elm-format recommend. Let's be draconian and enforce uniformity :pirate:.
///
/// ## Examples
/// Here is an example of context for a pattern matching expression
///
/// ```text
///    case maybe of
///         |---| is a CaseExpression
/// |-   Just value ->       -|
/// |      Just (f value)    -|- is a CaseBranch
/// |    Nothing ->        -|
/// |-     Nothing         -|- is a second CaseBranch
/// |
/// |-- is a `CaseBlock`
/// ```
#[derive(Debug, PartialEq, Clone)]
pub enum Context {
    /// Context for the expression a pattern matching will match on
    CaseExpression,

    /// Context for the block containing the different matches of a catch/of
    /// A case block minimum indentation is set by the first token after the block is opened
    CaseBlock(Option<usize>),

    /// Context for a branch in a case/of expression.
    CaseBranch,

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
    reprocess_tokens: Vec<Spanned<Position, Token>>,
}

impl<I> Layout<I>
where
    I: Iterator<Item = Result<Spanned<Position, Token>, Error>>,
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
    fn next_token(&mut self) -> Result<Spanned<Position, Token>, Error> {
        self.reprocess_tokens.pop().map(Ok).unwrap_or_else(|| {
            self.tokens.next().unwrap_or_else(|| {
                // The absolute part is unused (hence 0) but the column value is important
                // (we want a value of 1 to match the first token of a line)
                let position = Position::new(0, 1, 1);

                Ok(spanned(position, position, Token::EndOfFile))
            })
        })
    }

    /// This is the entry point for our layout processor.
    ///
    /// It is called by the iterator's next on each token.
    ///
    fn handle_next_token(&mut self) -> Result<Spanned<Position, Token>, Error> {
        let token = self.next_token()?;

        // Short circuit handling of EOF, and verify we don't have any
        // remaining contexts to clean.
        if let Spanned {
            value: Token::EndOfFile,
            span: Span { start, end },
        } = &token
        {
            return match self.contexts.pop() {
                Some(_) => {
                    self.reprocess_tokens.push(token.clone());
                    Ok(spanned(*start, *end, Token::CloseBlock))
                }
                None => Ok(token),
            };
        }

        // Retrieve the current offside and, if none exists, create one,
        // put the current token on the back burner and emit the new block.
        // In theory this should only happens when we are looking at a top
        // level declaration (or it's a bug)
        let offside = match self.contexts.stack.last_mut() {
            Some(offside) => offside,
            None => {
                let start = token.start();
                let off = Offside {
                    context: Context::TopLevelDeclaration,
                    indent: start.column,
                    line: start.line,
                };
                self.contexts.push(off);

                self.reprocess_tokens.push(token.clone());
                return Ok(spanned(*start, *start, Token::OpenBlock));
            }
        };

        trace!("step 1: {:?}, offside: {:?}", token.value, offside);

        // First, we check if we have a closing token with an associated context.
        // If we do, let's remove the context and return the token
        match (&token.value, &mut offside.context) {
            (Token::Of, Context::CaseExpression) => {
                self.contexts.pop();
                self.reprocess_tokens.push(token.clone());
                return Ok(token.map(|_| Token::CloseBlock));
            }
            (Token::OpenBlock, Context::CaseBlock(None)) => (),
            (_, Context::CaseBlock(c @ None)) => {
                // Here we are seeing the first token after opening the block, and
                // this token set the minimum indentation for the block.
                c.replace(token.start().column);
            }
            (Token::In, Context::Let) => {
                // TODO akin to of/case above, we might have to create a let/in block
                // to let the parser know when the let part ended. Not sure yet.
                // TODO We might need to check for the `in` indentation here, needs to be
                // same as `let`.
                self.contexts.pop();
                return Ok(token);
            }
            (Token::CloseBlock, Context::TopLevelDeclaration) => {
                self.contexts.pop();
                return Ok(token);
            }
            _ => (),
        }

        drop(offside);

        // Now that we have checked explicit context poping, let's check the implicit one.
        // These apply to contexts which are terminated by simply having a token on a column
        // less than the one required by the context.
        let offside = loop {
            // We repeat the contexts checking here, because we are going to remove contexts
            // and
            let offside = match self.contexts.last() {
                Some(offside) => offside,
                None => {
                    let start = *token.start();
                    let off = Offside {
                        context: Context::TopLevelDeclaration,
                        indent: start.column,
                        line: start.line,
                    };
                    self.contexts.push(off);

                    self.reprocess_tokens.push(token.clone());
                    return Ok(spanned(start, start, Token::OpenBlock));
                }
            };

            let token_column = token.start().column;
            let context_column = offside.indent;

            trace!(
                "step 2: {:?}, token:{:?}, context:{:?}",
                offside.context,
                token_column,
                context_column
            );

            match &offside.context {
                // case branch terminates when we have a token at a level
                Context::CaseBranch | Context::CaseBlock(_) => {
                    if token_column <= context_column {
                        //   value // token
                        // Nothing // context
                        // i i
                        // Here we have a token on an indentation level lower than the case
                        // context, so we close that context.
                        self.contexts.pop();
                        self.reprocess_tokens.push(token.clone());
                        return Ok(token.map(|_| Token::CloseBlock));
                    }
                }

                // let and top level declaration aren't managed here
                // although tld could be.
                _ => (),
            };

            // we release the reference on self.contexts because we need to
            // mutate it down the line.
            break offside.clone();
        };

        // Second, we enforce the indentation rule we have on record
        let min_indent_required = match &offside.context {
            Context::CaseBlock(Some(min)) => min,
            _ => &offside.indent,
        };

        match token.start().column.cmp(min_indent_required) {
            Ordering::Less => {
                let offside = offside.clone();

                self.reprocess_tokens.push(token.clone());

                return Err(LayoutError::LayoutError { offside, token }.into());
            }
            _ => (), // ok
        };

        // Third, we create new tokens, new contexts and emit block tokens as required

        trace!(
            "step 3: {:?} ({}:{}), context: {:?}",
            token.value,
            token.start().column,
            token.end().column,
            offside.context
        );
        match (&token.value, &offside.context) {
            (Token::Case, _) => {
                self.contexts.push(Offside {
                    context: Context::CaseExpression,
                    indent: token.start().column + 1,
                    line: token.start().line,
                });
                self.reprocess_tokens
                    .push(spanned(*token.end(), *token.end(), Token::OpenBlock));
            }
            (Token::Of, _) => {
                self.contexts.push(Offside {
                    context: Context::CaseBlock(None),
                    indent: offside.indent + 1,
                    line: token.start().line,
                });
                self.reprocess_tokens
                    .push(spanned(*token.end(), *token.end(), Token::OpenBlock));
            }
            (Token::Let, _) => self.contexts.push(Offside {
                context: Context::Let,
                indent: token.start().column + 1,
                line: token.start().line,
            }),
            (Token::Arrow, Context::CaseBlock(Some(min_indent))) => {
                self.contexts.push(Offside {
                    context: Context::CaseBranch,
                    indent: min_indent + 1,
                    line: token.start().line,
                });
                self.reprocess_tokens
                    .push(spanned(*token.end(), *token.end(), Token::OpenBlock));
            }
            (Token::OpenBlock, _) => (),
            _ => {
                if token.start().column == 1 && token.start().line > offside.line {
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

                    return Ok(spanned(*token.start(), *token.start(), Token::CloseBlock));
                }
            }
        }

        Ok(token)
    }
}

impl<I> Iterator for Layout<I>
where
    I: Iterator<Item = Result<Spanned<Position, Token>, Error>>,
{
    type Item = Result<(BytePos, Token, BytePos), Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let res = self.handle_next_token();
        trace!("step 4: {:?}", res);

        match res {
            Ok(Spanned {
                value: Token::EndOfFile,
                ..
            }) => None,
            Ok(Spanned { value, span }) => {
                Some(Ok((span.start.absolute, value, span.end.absolute)))
            }
            Err(err) => Some(Err(err)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::parser::*;
    use crate::compiler::position::Position;
    use tokenizer::Token;

    // Create an approximation for the token position in the stream.
    // We don't count the spaces between tokens, but it gives us enough
    // to understand where a failure happened.
    fn tokens_to_spanned(tokens: &Vec<Token>) -> Vec<Result<Spanned<Position, Token>, Error>> {
        let mut pos = Position::new(0, 1, 1);

        tokens
            .into_iter()
            .cloned()
            .filter_map(|token| {
                let start = pos.clone();
                let inc = match &token {
                    Token::Module => 6,
                    Token::UpperIdentifier(name) => name.len(),
                    Token::Exposing => 8,
                    Token::LPar | Token::RPar => 1,
                    Token::Comma => 1,
                    Token::Pipe => 1,
                    Token::Equal => 1,
                    Token::Type | Token::Case => 4,
                    Token::Of | Token::Arrow => 2,
                    _ => 0,
                };

                // Hack to simulate new lines and indentation
                let emit = if let Token::LowerIdentifier(name) = &token {
                    match name.as_str() {
                        "\n" => {
                            pos.new_line();
                            false
                        }
                        "  " => {
                            pos.increment_by(2);
                            false
                        }
                        _ => {
                            pos.increment_by(name.len());
                            true
                        }
                    }
                } else {
                    pos.increment_by(inc);
                    true
                };

                let end = pos.clone();

                if emit {
                    Some(Ok(spanned(start, end, token)))
                } else {
                    None
                }
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
        let first = s.chars().next().unwrap();
        if first.is_uppercase() {
            Token::UpperIdentifier(s.to_string())
        } else {
            Token::LowerIdentifier(s.to_string())
        }
    }

    // hack to control tokens_to_spanned behavior regarding source code position
    fn newline() -> Token {
        Token::LowerIdentifier("\n".to_string())
    }
    fn indent() -> Token {
        Token::LowerIdentifier("  ".to_string())
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
                newline(),
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
                newline(),
                indent(),
                Token::LPar,
                ident_token("Maybe"),
                Token::LPar,
                Token::DotDot,
                Token::RPar,
                newline(),
                indent(),
                Token::Comma,
                ident_token("andThen"),
                newline(),
                indent(),
                Token::Comma,
                ident_token("map"),
                newline(),
                indent(),
                Token::RPar,
                newline(),
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
                newline(),
                indent(),
                Token::Equal,
                ident_token("Just"),
                ident_token("a"),
                newline(),
                indent(),
                Token::Pipe,
                ident_token("Nothing"),
                newline(),
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
                newline(),
                indent(),
                Token::Equal,
                ident_token("Just"),
                ident_token("a"),
                newline(), // Here we are missing an indent
                Token::Pipe,
                ident_token("Nothing"),
                newline(),
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

    #[test]
    fn top_level_case_expression() {
        test_layout_without_error(
            vec![
                ident_token("map"),
                ident_token("f"),
                ident_token("maybe"),
                Token::Equal,
                newline(),
                indent(),
                Token::Case,
                ident_token("maybe"),
                Token::Of,
                newline(),
                indent(),
                indent(),
                ident_token("Just"),
                ident_token("value"),
                Token::Arrow,
                newline(),
                indent(),
                indent(),
                indent(),
                ident_token("Just"),
                Token::LPar,
                ident_token("f"),
                ident_token("value"),
                Token::RPar,
                newline(),
                newline(),
                indent(),
                indent(),
                ident_token("Nothing"),
                Token::Arrow,
                newline(),
                indent(),
                indent(),
                indent(),
                ident_token("Nothing"),
                newline(),
            ],
            vec![
                Token::OpenBlock,
                ident_token("map"),
                ident_token("f"),
                ident_token("maybe"),
                Token::Equal,
                Token::Case,
                Token::OpenBlock,
                ident_token("maybe"),
                Token::CloseBlock,
                Token::Of,
                Token::OpenBlock,
                ident_token("Just"),
                ident_token("value"),
                Token::Arrow,
                Token::OpenBlock,
                ident_token("Just"),
                Token::LPar,
                ident_token("f"),
                ident_token("value"),
                Token::RPar,
                Token::CloseBlock,
                ident_token("Nothing"),
                Token::Arrow,
                Token::OpenBlock,
                ident_token("Nothing"),
                Token::CloseBlock,
                Token::CloseBlock,
                Token::CloseBlock,
            ],
        )
    }
}
