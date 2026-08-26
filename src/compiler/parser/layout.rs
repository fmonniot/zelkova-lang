//! Simplify the indentation manager for the parser
//! by doing it in before the token iterator is passed to the parser.

use super::error::Error;
use super::tokenizer::Token;
use crate::compiler::position::{spanned, BytePos, Position, Span, Spanned};
use log::trace;
use std::cmp::Ordering;
use std::iter::FusedIterator;

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
///   Note that I'll probably change the in rule to be at the same level.
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
///
/// `Context` and `Offside` are `Copy`: `handle_next_token` has to read the
/// current context and then mutate the context stack, so it takes a copy of
/// the top of the stack to release the borrow. Keep any future variant small
/// enough that this stays true.
#[derive(Debug, PartialEq, Clone, Copy)]
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

#[derive(Debug, PartialEq, Clone, Copy)]
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
/// The core loop does not clone tokens. A token which has to be emitted as
/// something else (a block token) *and* reprocessed afterwards is moved into
/// `reprocess_tokens`, and the token actually emitted is rebuilt from the
/// original's span, which is `Copy`.
///
/// The iterator is fused: once `next` has returned `None` — because it hit an
/// `Err`, or because the source ran out — every subsequent call returns `None`.
/// Both cases go through the same `finished` latch, so `Layout` implements
/// `FusedIterator`. See `Iterator::next` below for why an `Err` is terminal.
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
    /// Set once `next` has returned `None`, whether because of an `Err` or
    /// because the source is exhausted; from then on it keeps returning `None`.
    /// This is what makes the `FusedIterator` impl below sound.
    /// See `Iterator::next`.
    finished: bool,
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
            finished: false,
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
        if let Token::EndOfFile = token.value {
            let Span { start, end } = token.span;

            return match self.contexts.pop() {
                Some(_) => {
                    self.reprocess_tokens.push(token);
                    Ok(spanned(start, end, Token::CloseBlock))
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
                let start = token.span.start;
                let off = Offside {
                    context: Context::TopLevelDeclaration,
                    indent: start.column,
                    line: start.line,
                };
                self.contexts.push(off);

                self.reprocess_tokens.push(token);
                return Ok(spanned(start, start, Token::OpenBlock));
            }
        };

        trace!("step 1: {:?}, offside: {:?}", token.value, offside);

        // First, we check if we have a closing token with an associated context.
        // If we do, let's remove the context and return the token
        match (&token.value, &mut offside.context) {
            (Token::Of, Context::CaseExpression) => {
                let Span { start, end } = token.span;

                self.contexts.pop();
                self.reprocess_tokens.push(token);
                return Ok(spanned(start, end, Token::CloseBlock));
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

        // Now that we have checked explicit context poping, let's check the implicit one.
        // These apply to contexts which are terminated by simply having a token on a column
        // less than the one required by the context.
        let offside: Offside = {
            // We repeat the contexts checking here, because we are going to remove contexts
            // and
            let offside = match self.contexts.last() {
                Some(offside) => offside,
                None => {
                    let start = token.span.start;
                    let off = Offside {
                        context: Context::TopLevelDeclaration,
                        indent: start.column,
                        line: start.line,
                    };
                    self.contexts.push(off);

                    self.reprocess_tokens.push(token);
                    return Ok(spanned(start, start, Token::OpenBlock));
                }
            };

            let token_column = token.span.start.column;
            let context_column = offside.indent;

            trace!(
                "step 2: {:?}, token:{:?}, context:{:?}",
                offside.context,
                token_column,
                context_column
            );

            match &offside.context {
                // case branch terminates when we have a token at a level
                Context::CaseBranch | Context::CaseBlock(_) if token_column <= context_column => {
                    //   value // token
                    // Nothing // context
                    // i i
                    // Here we have a token on an indentation level lower than the case
                    // context, so we close that context.
                    let Span { start, end } = token.span;

                    self.contexts.pop();
                    self.reprocess_tokens.push(token);
                    return Ok(spanned(start, end, Token::CloseBlock));
                }

                // let and top level declaration aren't managed here
                // although tld could be.
                _ => (),
            };

            // we release the reference on self.contexts because we need to
            // mutate it down the line. `Offside` is `Copy`, so this is a few
            // words on the stack and not an allocation.
            *offside
        };

        // Second, we enforce the indentation rule we have on record
        let min_indent_required = match offside.context {
            Context::CaseBlock(Some(min)) => min,
            _ => offside.indent,
        };

        if token.span.start.column.cmp(&min_indent_required) == Ordering::Less {
            // The token is moved into the error and is *not* pushed onto
            // `reprocess_tokens`. Nothing in this branch mutates `self.contexts`,
            // so replaying the token would re-run this exact comparison against
            // this exact context and produce the same error forever. `next`
            // fuses the iterator on `Err` anyway, so the token has no reader
            // left; keeping it buffered would only make the loop reachable
            // again for anyone who removes that fuse.
            return Err(LayoutError::LayoutError { offside, token }.into());
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
                if token.span.start.column == 1 && token.span.start.line > offside.line {
                    // Here we have a token which isn't OpenBlock (special case above)
                    // but which is at the beginning of a new line. This most probably
                    // mean we have reached the end of the previous block and are
                    // starting a new one.

                    let start = token.span.start;
                    self.reprocess_tokens.push(token);

                    // Furthermore in case of implicitely terminated block,
                    // pop the context from the stack and let the parser complain
                    // about the invalid syntax. We do this to break an infinite
                    // loop where we would always be checking the current token
                    // against the current context.
                    if offside.context == Context::TopLevelDeclaration {
                        self.contexts.pop();
                    }

                    return Ok(spanned(start, start, Token::CloseBlock));
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

    /// Yields one layout-processed token per call, and stops — returns `None`
    /// forever — at the first of two events: `Token::EndOfFile`, or an `Err`.
    ///
    /// Both events set the same `finished` latch, which is what makes "forever"
    /// unconditional. It is worth being precise about this, because the two
    /// events do not arrive the same way. The `Err` is terminal by decision.
    /// `EndOfFile` is *re-derived* on each call — `next_token` finds
    /// `reprocess_tokens` empty, asks the source for another token, gets `None`
    /// and synthesises a fresh `EndOfFile` — and `Iterator`'s contract permits a
    /// source to yield `Some` again after a `None`. Every source used here is in
    /// fact fused, so latching changes nothing in practice; it just means the
    /// guarantee is a property of this type rather than of its callers, which is
    /// what the `FusedIterator` impl below asserts.
    ///
    /// The error fuse matters because layout errors are not, in general,
    /// recoverable *by this iterator*: an indentation violation is diagnosed
    /// without changing `self.contexts`, so there is no state transition that
    /// would let the same input be read differently on a second attempt. Errors
    /// from the tokenizer, propagated through `handle_next_token`, are fused
    /// the same way. A consumer which drains this iterator fully therefore sees
    /// at most one error and then terminates, rather than the same error
    /// repeated without bound.
    ///
    /// This deliberately forecloses accumulating layout diagnostics: should the
    /// layout phase ever need to report every error rather than stop at the
    /// first (`ERR-2`), this fuse is what has to change, and the branch above
    /// would need a state transition that guarantees forward progress.
    fn next(&mut self) -> Option<Self::Item> {
        if self.finished {
            return None;
        }

        let res = self.handle_next_token();
        trace!("step 4: {:?}", res);

        match res {
            Ok(Spanned {
                value: Token::EndOfFile,
                ..
            }) => {
                self.finished = true;
                None
            }
            Ok(Spanned { value, span }) => {
                Some(Ok((span.start.absolute, value, span.end.absolute)))
            }
            Err(err) => {
                self.finished = true;
                Some(Err(err))
            }
        }
    }
}

/// `next` latches `finished` on both of its terminating events, so once it has
/// returned `None` it cannot return `Some` again regardless of what the source
/// iterator does. That is exactly `FusedIterator`'s contract, and stating it
/// makes `.fuse()` a no-op for callers.
impl<I> FusedIterator for Layout<I> where I: Iterator<Item = Result<Spanned<Position, Token>, Error>>
{}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::parser::*;
    use crate::compiler::position::Position;
    use tokenizer::Token;

    // Create an approximation for the token position in the stream.
    // We don't count the spaces between tokens, but it gives us enough
    // to understand where a failure happened.
    fn tokens_to_spanned(tokens: &[Token]) -> Vec<Result<Spanned<Position, Token>, Error>> {
        let mut pos = Position::new(0, 1, 1);

        tokens
            .iter()
            .cloned()
            .filter_map(|token| {
                let start = pos;
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

                let end = pos;

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

    /// Poll `iter` up to `cap` times, stopping early if it terminates.
    ///
    /// The cap is what makes a non-terminating iterator show up as a failed
    /// assertion rather than as a hung test process.
    fn drain_bounded<I: Iterator>(iter: &mut I, cap: usize) -> Vec<I::Item> {
        let mut items = Vec::with_capacity(cap);

        for _ in 0..cap {
            match iter.next() {
                Some(item) => items.push(item),
                None => break,
            }
        }

        items
    }

    /// A consumer which keeps polling past a `LayoutError` must not see that
    /// same error again, and iteration has to terminate (`BUG-4`).
    ///
    /// The source starts its first top level declaration at column 3, which
    /// sets the top level context's minimum indentation to 3. The `|` on the
    /// following line sits at column 1 and so violates it. Note that column 1
    /// is *also* what the implicit-block-closing rule keys on, but that rule
    /// runs after the indentation check, so what comes out here is the
    /// indentation error and not a `CloseBlock`.
    ///
    /// Verified to fail by neutralising both halves of the fix: restoring the
    /// `self.reprocess_tokens.push(token.clone())` on `handle_next_token`'s
    /// error branch *and* removing the `finished` guard from `Iterator::next`.
    /// Either one alone stops the loop, so both have to be reverted to observe
    /// the original bug — with both reverted this collects `CAP` items, the
    /// last three of which are the identical error at the identical position.
    #[test]
    fn layout_error_is_never_reported_twice() {
        const CAP: usize = 6;

        let source = vec![
            indent(), // the first declaration starts at column 3, not column 1
            Token::Type,
            ident_token("Maybe"),
            newline(),
            Token::Pipe, // column 1: below the minimum indentation of its context
            ident_token("Nothing"),
            newline(),
        ];

        let mut iter = layout(tokens_to_spanned(&source).into_iter());
        let items = drain_bounded(&mut iter, CAP);

        assert!(
            items.len() < CAP,
            "the iterator did not terminate within {} items: {:?}",
            CAP,
            items
        );
        assert!(iter.next().is_none(), "the iterator restarted after ending");

        let errors: Vec<_> = items.iter().filter(|item| item.is_err()).collect();
        assert_eq!(
            errors.len(),
            1,
            "expected exactly one error, got {:?}",
            items
        );

        match items.last() {
            Some(Err(Error::Layout(LayoutError::LayoutError { token, offside }))) => {
                assert_eq!(token.value, Token::Pipe);
                assert_eq!(token.start().column, 1);
                assert_eq!(offside.context, Context::TopLevelDeclaration);
                assert_eq!(offside.indent, 3);
            }
            other => panic!("expected a trailing layout error, got {:?}", other),
        }
    }

    /// The same fuse has to cover errors which `Layout` did not raise itself.
    /// `handle_next_token` propagates an upstream (tokenizer) error with `?`
    /// without consuming any token from the source, so a caller polling past it
    /// would otherwise keep seeing whatever the source iterator hands out next.
    ///
    /// Verified to fail by removing the `finished` guard from `Iterator::next`:
    /// the source below then yields its second `Err` too, and the assertion on
    /// the item count goes red.
    #[test]
    fn upstream_error_also_stops_iteration() {
        // Any `Error` does here: `Layout` is generic over the source iterator
        // and only ever propagates what it is given.
        let upstream_error = || Err(Error::InvalidToken(BytePos(0)));

        let mut iter = layout(vec![upstream_error(), upstream_error()].into_iter());
        let items = drain_bounded(&mut iter, 4);

        assert_eq!(
            items.len(),
            1,
            "iteration continued past an upstream error: {:?}",
            items
        );
        assert!(matches!(items[0], Err(Error::InvalidToken(_))));
        assert!(iter.next().is_none(), "the iterator restarted after ending");
    }

    /// The `FusedIterator` impl claims `Layout` cannot yield `Some` after a
    /// `None`. The `Err` path is terminal by decision, but the `EndOfFile` path
    /// is re-derived from the source on every call, so on its own it inherits
    /// whatever the source does — and `Iterator`'s contract lets a source hand
    /// out `Some` again after `None`. The source below does exactly that.
    ///
    /// Verified to fail by removing `self.finished = true;` from `next`'s
    /// `EndOfFile` arm: `Layout` then asks the resumed source for more tokens
    /// and yields the second identifier, so the length assertion goes red.
    #[test]
    fn iteration_does_not_resume_after_a_non_fused_source_ends() {
        // Yields one token, then `None`, then another token — legal for a
        // plain `Iterator`, and precisely what `FusedIterator` forbids.
        let mut steps = vec![
            Some(ident_token("a")),
            None,
            Some(ident_token("b")),
            Some(ident_token("c")),
        ]
        .into_iter();
        let mut pos = Position::new(0, 1, 1);
        let source = std::iter::from_fn(move || {
            let token = steps.next()??;
            let start = pos;
            pos.increment_by(1);
            Some(Ok(spanned(start, pos, token)))
        });

        let mut iter = layout(source);
        let items = drain_bounded(&mut iter, 8);

        // `OpenBlock`, the single identifier, and the `CloseBlock` emitted for
        // the top level context when the source first reports exhaustion.
        assert_eq!(
            items.len(),
            3,
            "iteration resumed after the source reported exhaustion: {:?}",
            items
        );
        assert!(iter.next().is_none(), "the iterator restarted after ending");
    }
}
