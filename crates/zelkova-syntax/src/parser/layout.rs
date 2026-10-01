//! Simplify the indentation manager for the parser
//! by doing it in before the token iterator is passed to the parser.

use super::chunk::can_start_declaration;
use super::error::Error;
use super::tokenizer::Token;
use crate::position::{spanned, BytePos, Position, Span, Spanned};
use log::trace;
use std::cmp::Ordering;
use std::iter::FusedIterator;

#[derive(Debug, PartialEq, Clone)]
pub enum LayoutError {
    LayoutError {
        offside: Offside,
        token: Spanned<Position, Token>,
    },
    /// A line indented past column 1 at the top level, which the grammar read
    /// as a continuation of the declaration above it although that declaration
    /// was already complete, and which starts the way a declaration does.
    ///
    /// A top-level declaration begins in column 1, and a line indented past it
    /// continues the declaration above (`docs/spec/layout.md`, *Top-level
    /// declarations*). The layout pass cannot tell such a line from an ordinary
    /// continuation on its own — `module M exposing` followed by an indented
    /// `(f)` is one — so it only records the first token of each top-level
    /// continuation line, and `Layout::explain` raises this error once the
    /// grammar has rejected that line. `token` is the line's first token, and
    /// `declaration_line` is the line the declaration above began on.
    IndentedDeclaration {
        token: Spanned<Position, Token>,
        declaration_line: usize,
    },
}

/// Apply the offside rule to a token stream, injecting `OpenBlock`/`CloseBlock`
/// so the parser does not have to track indentation itself.
///
/// **The returned iterator stops at the first `Err`.** A consumer which drains
/// it fully therefore observes at most one error, ever, and then terminates —
/// a layout error is not recoverable by the iterator, so replaying past it
/// would repeat that error without bound (`BUG-4`). It is a `FusedIterator`,
/// so `.fuse()` is a no-op. See `Layout` and its `Iterator::next` for the
/// reasoning.
///
/// `end` is where the input ends: the blocks still open when `iter` runs out are
/// closed there.
pub fn layout<I: Iterator<Item = Result<Spanned<Position, Token>, Error>>>(
    iter: I,
    end: Position,
) -> impl FusedIterator<Item = Result<(BytePos, Token, BytePos), Error>> {
    Layout::new(iter, end)
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
    /// Context for the expression a pattern matching will match on. Carries
    /// the column of the `case` keyword itself. When this context is popped
    /// on seeing `of`, that column is stashed in `Layout::case_of_column` so
    /// the branch block can later check its own column against it: the
    /// branch block's minimum column is required to sit strictly right of
    /// `case`'s own (`BUG-10`).
    CaseExpression(usize),

    /// Context for the block containing the different matches of a catch/of
    /// A case block minimum indentation is set by the first token after the block is opened.
    ///
    /// The `case` keyword's own column lives in `Layout::case_of_column`
    /// rather than on this variant: widening `CaseBlock` by a `usize` pushes
    /// `Context` from 16 to 24 bytes, and `CompilationError` — which embeds it
    /// several layers down — from 120 to 128, where clippy's `result_large_err`
    /// default starts complaining.
    CaseBlock(Option<usize>),

    /// Context for a branch in a case/of expression.
    CaseBranch,

    /// Context for a let expression
    Let,

    /// Context for a top level declaration.
    /// Those can be module, custom type, type alias or functions (type annotation/value).
    TopLevelDeclaration,
}

impl Context {
    /// How to name this block when talking to the person who wrote the source.
    /// Used by `Error::diagnostic` to say which block an indentation error
    /// belongs to.
    pub fn description(&self) -> &'static str {
        match self {
            Context::CaseExpression(_) => "the expression of a `case … of`",
            Context::CaseBlock(_) => "the branches of a `case … of`",
            Context::CaseBranch => "the body of a `case … of` branch",
            Context::Let => "a `let` block",
            Context::TopLevelDeclaration => "a top level declaration",
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Offside {
    context: Context,
    indent: usize, // TODO rename to min_indent
    line: usize,
}

impl Offside {
    /// The smallest column a token may start on without breaking this context's
    /// indentation rule.
    ///
    /// A `case … of` block takes its minimum from the first token which followed
    /// the block opening, recorded in `CaseBlock`; every other context uses its
    /// own indentation. `handle_next_token` enforces this, and
    /// `Error::diagnostic` reports it, so the rule is written here once.
    pub fn min_indent(&self) -> usize {
        match self.context {
            Context::CaseBlock(Some(min)) => min,
            _ => self.indent,
        }
    }

    /// The 1-indexed line the context was opened on.
    pub fn line(&self) -> usize {
        self.line
    }

    /// The kind of block this context describes.
    pub fn context(&self) -> Context {
        self.context
    }
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
pub(crate) struct Layout<I> {
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
    /// The column of the `case` keyword whose `CaseExpression` context was
    /// just popped by seeing `of`, set aside until the first token of the
    /// matching `CaseBlock` needs to check its own column against it
    /// (`BUG-10`; see the `(_, Context::CaseBlock(c @ None))` arm). It is not
    /// carried on `Context::CaseBlock` itself to avoid growing that type —
    /// see the variant's doc comment.
    ///
    /// A single slot, rather than a stack, is enough even for a `case`
    /// nested in another's scrutinee: the token stream between an `of` and
    /// its block's first real token can only be that block's own content
    /// (the synthetic `OpenBlock`), so a second `case … of` cannot have its
    /// `of` popped before the first one's floor has been read and cleared.
    case_of_column: Option<usize>,
    /// The position of the last token read from `tokens`, and of the first
    /// token read on that token's line. A real token is first on its line
    /// exactly when its start is `line_start` — comments are not tokens, so
    /// the `f` of `{- a note -} f = 1` is the first token on its line.
    ///
    /// A token taken back out of `reprocess_tokens` is always the one most
    /// recently read from `tokens`, or a synthetic block token, so comparing
    /// against the latest line is enough. For the same reason, a real token
    /// the grammar rejects is the one at `last_read`: the grammar stops at the
    /// first token it cannot shift, and this pass never reads past the token
    /// it is about to emit.
    last_read: Option<Position>,
    line_start: Option<Position>,
    /// The first token of the most recent top-level continuation line — see
    /// `LayoutError::IndentedDeclaration` — and the line the declaration it
    /// continues began on. Read only by `explain`.
    top_level_continuation: Option<(Spanned<Position, Token>, usize)>,
    /// Where the input ends. The blocks still open when `tokens` runs out are
    /// closed here, so a declaration left unfinished is reported where its text
    /// stops rather than at the start of the file.
    end: Position,
}

impl<I> Layout<I>
where
    I: Iterator<Item = Result<Spanned<Position, Token>, Error>>,
{
    /// Create and initialize a new `Layout` iterator over `iter`, whose input
    /// ends at `end`.
    pub(crate) fn new(iter: I, end: Position) -> Layout<I> {
        Layout {
            tokens: iter,
            contexts: Contexts::new(),
            reprocess_tokens: vec![],
            finished: false,
            case_of_column: None,
            last_read: None,
            line_start: None,
            top_level_continuation: None,
            end,
        }
    }

    /// Turn the grammar's rejection of a top-level continuation line into
    /// `LayoutError::IndentedDeclaration`, and hand every other error back as it
    /// came. `parser::parse_recovering` calls this on a chunk's `Layout` once the
    /// grammar has stopped pulling that chunk's tokens.
    ///
    /// Three things have to hold, and each rules out a misreading the others
    /// leave open:
    ///
    /// - The line's first token is one a declaration can start with: a
    ///   lowercase name, soft keywords included, `type`, `import` or `infix`
    ///   (`can_start_declaration`). A line opening
    ///   on `(`, `=`, `then` or an operator is a typo inside the declaration
    ///   above, and the grammar's own list of expected tokens says more about
    ///   it than an indentation error would.
    /// - The grammar rejected either that first token, or a later `=` or `:`
    ///   on the same line — the sign of a definition or an annotation, which
    ///   no expression or type in the grammar contains. The second shape is what an indented
    ///   declaration looks like after a declaration whose last expression or
    ///   type takes arguments: `f = 1` then `  g = 2` reads as `f = 1 g`, and
    ///   the grammar only stops at `=`.
    /// - The declaration above was complete when the line began. When the
    ///   first token is the one rejected, the grammar says so itself: it
    ///   listed `close block` among the tokens it would have accepted there.
    ///   When the grammar stopped later, it had already read the first token
    ///   as part of the declaration, so `complete_before` is asked instead: it
    ///   is given where the line's first token starts and answers whether the
    ///   chunk's tokens before it parse on their own, with the chunk's grammar
    ///   entry point. `f =` then `  g x = 1` fails that
    ///   test and keeps the grammar's error. `close block` has to be expected
    ///   in this shape too, so that the part of the line before the `=` or `:`
    ///   reads as a whole declaration head.
    ///
    /// The check that the rejected `=` or `:` sits on the recorded line is
    /// belt-and-braces: every later top-level continuation line replaces the
    /// record, so a stale one would take a line inside a nested block, and no
    /// test reaches that.
    pub(crate) fn explain(
        &self,
        error: Error,
        complete_before: impl FnOnce(Position) -> bool,
    ) -> Error {
        let Error::UnexpectedToken { token, expected } = error else {
            return error;
        };
        let explained = match &self.top_level_continuation {
            Some((first, declaration_line))
                if can_start_declaration(&first.value)
                    && expected.iter().any(|e| e == "close block") =>
            {
                let rejects_first =
                    token.span.start == first.span.start.absolute && token.value == first.value;
                let rejects_sign_on_the_same_line =
                    matches!(token.value, Token::Equal | Token::Colon)
                        && self.last_read.is_some_and(|last| {
                            last.absolute == token.span.start && last.line == first.span.start.line
                        })
                        && complete_before(first.span.start);

                (rejects_first || rejects_sign_on_the_same_line).then(|| {
                    LayoutError::IndentedDeclaration {
                        token: first.clone(),
                        declaration_line: *declaration_line,
                    }
                })
            }
            _ => None,
        };

        match explained {
            Some(error) => error.into(),
            None => Error::UnexpectedToken { token, expected },
        }
    }

    /// A simple function which manage the internal lookahead structure
    /// in tandem with the source iterator.
    ///
    /// It also convert the end of the source iterator into `Token::EndOfFile`.
    fn next_token(&mut self) -> Result<Spanned<Position, Token>, Error> {
        if let Some(token) = self.reprocess_tokens.pop() {
            return Ok(token);
        }

        match self.tokens.next() {
            Some(Ok(token)) => {
                let start = token.span.start;
                if self.last_read.map(|last| last.line) != Some(start.line) {
                    self.line_start = Some(start);
                }
                self.last_read = Some(start);
                Ok(token)
            }
            Some(Err(error)) => Err(error),
            None => {
                // The blocks still open are closed where the input ends, which is
                // where `handle_next_token` puts the `CloseBlock` it emits for each.
                let position = self.end;

                Ok(spanned(position, position, Token::EndOfFile))
            }
        }
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
            (Token::Of, Context::CaseExpression(case_col)) => {
                let Span { start, end } = token.span;

                self.case_of_column = Some(*case_col);
                self.contexts.pop();
                self.reprocess_tokens.push(token);
                return Ok(spanned(start, end, Token::CloseBlock));
            }
            (Token::OpenBlock, Context::CaseBlock(None)) => (),
            (_, Context::CaseBlock(c @ None)) => {
                // Here we are seeing the first token after opening the block, and
                // this token sets the minimum indentation for the block — unless
                // it does not clear the `case` keyword's own column, in which case
                // the block is invalid (BUG-10): a branch level with, or left of,
                // `case` reads as belonging to something enclosing it.
                //
                // `case_of_column` was set when the matching `CaseExpression` was
                // popped by `of`, several tokens ago (the synthetic `OpenBlock`
                // sits in between); it is `take`n rather than peeked because
                // this is the one and only time this block needs it. It is only
                // ever absent for a malformed `of` with no preceding `case`, in
                // which case the check below degrades to "column 0 or more" —
                // i.e. it never fires — rather than rejecting every column.
                let case_col = self.case_of_column.take().unwrap_or(0);
                let column = token.start().column;

                // A token at or left of `offside.indent` closes this block in
                // step 2 below; letting it through here is what allows that
                // close to happen, so the grammar reports the missing branches
                // against the `CloseBlock` instead of this arm blaming the
                // closing token for a misindented branch it never wrote.
                //
                // The price is reach: since `offside.indent` is the enclosing
                // block's indent plus one, the check below only ever fires for a
                // `case` whose own column is strictly greater than that — and
                // for anything shallower, a branch really written level with its
                // `case` is left to the grammar too, whether or not it was meant
                // as a branch. A `case` nested directly under an enclosing
                // branch is the common shape that falls out this way.
                if column > offside.indent && column <= case_col {
                    // `indent` reports `case_col + 1` — the floor this error is
                    // actually about — rather than `offside.indent`, which
                    // stays enclosing-derived (see the `Token::Of` push arm
                    // below) and would report the block's unrelated
                    // implicit-close threshold instead.
                    let error_offside = Offside {
                        context: Context::CaseBlock(None),
                        indent: case_col + 1,
                        line: offside.line,
                    };
                    return Err(LayoutError::LayoutError {
                        offside: error_offside,
                        token,
                    }
                    .into());
                }

                c.replace(column);
            }
            (Token::Else, Context::CaseBlock(Some(min))) if token.start().column < *min => {
                // `else` closes a `CaseBlock` when it sits strictly left of the
                // column every branch pattern is required to start at (`min`) —
                // i.e. it cannot be read as the start of another branch, which
                // would land exactly on `min`, not left of it (`BUG-23`).
                //
                // This has to be a column check, not a bare-token one. An
                // earlier version of this fix popped on `Token::Else`
                // unconditionally, on the reasoning that `else` "cannot appear
                // inside an unclosed `case` branch for any other reason" — but
                // it can: a branch's body can itself be a multi-line `if …
                // then … else`, and `if`/`then`/`else` push no context of
                // their own (see this file's module doc comment), so the
                // *first* `Else` seen while a `CaseBranch`/`CaseBlock` is open
                // may belong to that nested `if` rather than to whatever
                // encloses the `case`. Such an `else` is indented to at least
                // the branch body's own column, i.e. at or right of `min`, so
                // the `< *min` guard leaves it alone and the ordinary
                // indentation rule (`min_indent_required` below) accepts it as
                // part of the branch body instead of popping anything.
                //
                // `Context::CaseBranch` is deliberately not matched by this
                // arm at all, unlike the version it replaces. Its `.indent`
                // already equals its `min_indent()` (`CaseBlock` is the one
                // context where the two differ — see its doc comment for why),
                // so step 2's ordinary implicit close just below already
                // closes a `CaseBranch` exactly when a token — `else` included
                // — dedents out of its body, with no shallow/deep gap for a
                // nested `if`'s `else` to be misread through. This arm exists
                // only for the gap that's specific to `CaseBlock`.
                //
                // `CaseBlock(None)` — a `case` with no branches yet — is left
                // out on purpose: that shape is already a grammar error
                // (`CaseBranch+` requires at least one), and leaving it alone
                // keeps this fix scoped to the block `else` can legally follow.
                let Span { start, end } = token.span;

                self.contexts.pop();
                self.reprocess_tokens.push(token);
                return Ok(spanned(start, end, Token::CloseBlock));
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
        let min_indent_required = offside.min_indent();

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

        // A token opening a line indented past column 1, with a top-level
        // declaration open, continues that declaration. Record it, so that
        // `explain` can name the indentation if the grammar finds the
        // declaration was already complete. A `TopLevelDeclaration` is only
        // ever pushed onto an empty stack, so being the innermost context
        // makes it the only one. Only a declaration which itself began in
        // column 1 counts: one that did not is a file whose top level is
        // indented as a whole, and its first line is where that goes wrong.
        let start = token.span.start;
        if offside.context == Context::TopLevelDeclaration
            && offside.indent == 1
            && start.line > offside.line
            && start.column > 1
            && self.line_start.map(|first| first.absolute) == Some(start.absolute)
            && !matches!(token.value, Token::OpenBlock | Token::CloseBlock)
        {
            self.top_level_continuation = Some((token.clone(), offside.line));
        }

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
                    context: Context::CaseExpression(token.start().column),
                    indent: token.start().column + 1,
                    line: token.start().line,
                });
                self.reprocess_tokens
                    .push(spanned(*token.end(), *token.end(), Token::OpenBlock));
            }
            (Token::Of, _) => {
                // `case_of_column`, set by the matching `Context::CaseExpression`
                // pop above, is left set here on purpose: the BUG-10 check in the
                // `(_, Context::CaseBlock(c @ None))` arm reads and clears it when
                // this block's first real token arrives.
                //
                // `indent` is derived from the enclosing block rather than from
                // `case`'s column, because it is the threshold step 2's implicit
                // close keys on: a dedent back to the enclosing block's level has
                // to *close* this block, not merely violate it.
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
    /// This deliberately forecloses accumulating layout diagnostics. The phases
    /// after parsing do accumulate — canonicalization, type checking and
    /// exhaustiveness all return `Result<_, Vec<Error>>` so one bad declaration
    /// cannot hide the next — and should layout ever want the same, this fuse is
    /// what has to change: the branch above would need a state transition that
    /// guarantees forward progress before the error could be resumed from.
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
    use crate::parser::*;
    use crate::position::Position;
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
        let v: Vec<_> = layout(tokens_to_spanned(&source).into_iter(), Position::default())
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

    /// `BUG-23`: a `case` in the `then` arm of an `if` is closed by its `else`,
    /// however shallow the `CaseBlock`'s own `.indent` field turns out to be —
    /// the mirror of the `top_level_case_expression` test above, but with the
    /// case's *last* branch followed by `else` rather than by an implicit
    /// dedent to end of input.
    ///
    /// This is a layout-only rendering of the ticket's example:
    ///
    /// ```zel
    /// f c v =
    ///   if c then
    ///     case v of
    ///       On ->
    ///         Off
    ///
    ///       Off ->
    ///         On
    ///   else
    ///     On
    /// ```
    ///
    /// Both `CloseBlock`s right before `Else` matter: one for the second
    /// branch's body (`CaseBranch`), one for the branch list itself
    /// (`CaseBlock`) — `else` has to unwind both, not just the innermost.
    ///
    /// Verified to fail by changing the `(Token::Else, Context::CaseBlock(Some(min)))`
    /// arm's guard from `token.start().column < *min` to `false`: layout then falls
    /// through to the ordinary indentation check, which reports a `LayoutError` on
    /// `Else` instead of yielding it.
    #[test]
    fn else_closes_case_block_opened_in_a_then_arm() {
        test_layout_without_error(
            vec![
                ident_token("f"),
                ident_token("c"),
                ident_token("v"),
                Token::Equal,
                newline(),
                indent(),
                Token::If,
                ident_token("c"),
                Token::Then,
                newline(),
                indent(),
                indent(),
                Token::Case,
                ident_token("v"),
                Token::Of,
                newline(),
                indent(),
                indent(),
                indent(),
                ident_token("On"),
                Token::Arrow,
                newline(),
                indent(),
                indent(),
                indent(),
                indent(),
                ident_token("Off"),
                newline(),
                newline(),
                indent(),
                indent(),
                indent(),
                ident_token("Off"),
                Token::Arrow,
                newline(),
                indent(),
                indent(),
                indent(),
                indent(),
                ident_token("On"),
                newline(),
                indent(),
                Token::Else,
                newline(),
                indent(),
                indent(),
                ident_token("On"),
                newline(),
            ],
            vec![
                Token::OpenBlock,
                ident_token("f"),
                ident_token("c"),
                ident_token("v"),
                Token::Equal,
                Token::If,
                ident_token("c"),
                Token::Then,
                Token::Case,
                Token::OpenBlock,
                ident_token("v"),
                Token::CloseBlock,
                Token::Of,
                Token::OpenBlock,
                ident_token("On"),
                Token::Arrow,
                Token::OpenBlock,
                ident_token("Off"),
                Token::CloseBlock,
                ident_token("Off"),
                Token::Arrow,
                Token::OpenBlock,
                ident_token("On"),
                Token::CloseBlock, // closes the second branch's body (CaseBranch)
                Token::CloseBlock, // closes the branch list (CaseBlock)
                Token::Else,
                ident_token("On"),
                Token::CloseBlock,
            ],
        )
    }

    /// Regression for the review finding on `BUG-23`'s first fix: a `case`
    /// branch whose body is itself a multi-line `if … then … else` must stay
    /// open across that nested `if`'s `else` — the inverse shape of
    /// `else_closes_case_block_opened_in_a_then_arm` above, where the `case`
    /// is inside the `then` arm rather than the `if` being inside a branch.
    ///
    /// ```zel
    /// f v w =
    ///   case v of
    ///     On ->
    ///       if w then
    ///         Off
    ///       else
    ///         On
    ///
    ///     Off ->
    ///       On
    /// ```
    ///
    /// The unconditional first fix popped `CaseBranch`/`CaseBlock` on seeing
    /// any `Else`, so this `else` — which belongs to the nested `if`, not to
    /// anything enclosing the `case` — closed the branch prematurely and the
    /// rest of the branch's body (`On`) came out as a dangling token instead
    /// of staying nested. The column-based version leaves this `else` alone:
    /// it sits at the same column as the branch's own body content, deeper
    /// than the column a real next branch pattern would need to dedent to.
    ///
    /// Verified to fail by reverting the `if token.start().column < *min`
    /// guard back to an unconditional match on `(Token::Else,
    /// Context::CaseBlock(Some(_)) | Context::CaseBranch)`: with that
    /// version, the `Else` here pops `CaseBranch` immediately and the
    /// expected `CloseBlock` before it never appears; `On` (the branch's
    /// second constructor pattern) and the nested `if`'s own `else`/`On`
    /// then diverge from what's asserted below.
    #[test]
    fn nested_if_else_inside_case_branch_does_not_close_case_block() {
        test_layout_without_error(
            vec![
                ident_token("f"),
                ident_token("v"),
                ident_token("w"),
                Token::Equal,
                newline(),
                indent(),
                Token::Case,
                ident_token("v"),
                Token::Of,
                newline(),
                indent(),
                indent(),
                ident_token("On"),
                Token::Arrow,
                newline(),
                indent(),
                indent(),
                indent(),
                Token::If,
                ident_token("w"),
                Token::Then,
                newline(),
                indent(),
                indent(),
                indent(),
                indent(),
                ident_token("Off"),
                newline(),
                indent(),
                indent(),
                indent(),
                Token::Else,
                newline(),
                indent(),
                indent(),
                indent(),
                indent(),
                ident_token("On"),
                newline(),
                newline(),
                indent(),
                indent(),
                ident_token("Off"),
                Token::Arrow,
                newline(),
                indent(),
                indent(),
                indent(),
                ident_token("On"),
                newline(),
            ],
            vec![
                Token::OpenBlock,
                ident_token("f"),
                ident_token("v"),
                ident_token("w"),
                Token::Equal,
                Token::Case,
                Token::OpenBlock,
                ident_token("v"),
                Token::CloseBlock,
                Token::Of,
                Token::OpenBlock,
                ident_token("On"),
                Token::Arrow,
                Token::OpenBlock,
                Token::If,
                ident_token("w"),
                Token::Then,
                ident_token("Off"),
                Token::Else,
                ident_token("On"),
                Token::CloseBlock, // closes the first branch's body (CaseBranch) — triggered by the next pattern, not by `else`
                ident_token("Off"),
                Token::Arrow,
                Token::OpenBlock,
                ident_token("On"),
                Token::CloseBlock, // closes the second branch's body (CaseBranch)
                Token::CloseBlock, // closes the branch list (CaseBlock)
                Token::CloseBlock, // closes the top level declaration
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

        let mut iter = layout(tokens_to_spanned(&source).into_iter(), Position::default());
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

        let mut iter = layout(
            vec![upstream_error(), upstream_error()].into_iter(),
            Position::default(),
        );
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

        let mut iter = layout(source, Position::default());
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
