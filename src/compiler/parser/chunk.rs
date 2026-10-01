//! Cut a module's raw token stream into one chunk per top-level declaration, so that a
//! syntax error in one declaration does not end the parse of the ones after it.
//!
//! A top-level declaration begins in column 1, and a line indented past column 1
//! continues the declaration above it (`docs/spec/layout.md`, *Top-level
//! declarations*). [`Chunks`] reads that rule literally, on the tokenizer's output and
//! before the layout pass: a token starts a new chunk when it sits in column 1, is not
//! the first token of the file, and [`can_start_declaration`] accepts it. The first
//! chunk is the module header. Because the cut is made before layout, nothing a
//! declaration contains — an unclosed `case`, a missing `of` — can move where the next
//! one begins.
//!
//! What the cut deliberately does not do:
//!
//! - A column-1 token no declaration starts with (`1`, `|`, `=`, `)`) stays in its
//!   chunk. The layout pass closes the declaration at it as it would anywhere, and the
//!   mistake costs that chunk one error.
//! - A tokenizer error never starts a chunk. It belongs to the chunk it falls in, and
//!   the tokens after it are still read, so the declarations after it are still cut.
//!
//! Each chunk is then given its own [`Layout`](super::layout::Layout) and its own
//! grammar entry point by `parser::parse_recovering`.

use super::error::Error;
use super::tokenizer::Token;
use crate::compiler::position::{BytePos, Position, Span, Spanned};
use std::iter::FusedIterator;

/// One item of the tokenizer's output, its error already converted to the parser's.
pub(crate) type RawToken = Result<Spanned<Position, Token>, Error>;

/// Whether a declaration can begin with `token`: the tokens the grammar's `Decl`
/// alternatives open on, soft keywords included, since each of those is also a name a
/// function can have.
///
/// [`Chunks`] cuts before a column-1 token this accepts, and `Layout::explain` asks it
/// whether an indented line could have been meant as a declaration of its own.
pub(crate) fn can_start_declaration(token: &Token) -> bool {
    matches!(
        token,
        Token::LowerIdentifier(_)
            | Token::Left
            | Token::Right
            | Token::Non
            | Token::Foreign
            | Token::Unsafe
            | Token::Type
            | Token::Import
            | Token::Infix
    )
}

/// The tokens of one top-level declaration, or of the module header, as the tokenizer
/// produced them, errors included.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Chunk {
    /// The tokenizer's items for this chunk, in source order.
    pub(crate) tokens: Vec<RawToken>,
    /// Where this chunk's text begins: byte 0 for the first chunk, and the first token's
    /// start for every other. The chunks of a file therefore tile it.
    pub(crate) start: BytePos,
    /// Where this chunk's input ends: the start of the next chunk's first token, or the
    /// end of the source for the last chunk. The layout pass closes the chunk's open
    /// blocks here.
    pub(crate) end: Position,
}

impl Chunk {
    /// A chunk with no tokens, ending at `end`. What the header chunk of a file with no
    /// tokens at all is.
    pub(crate) fn empty(end: Position) -> Chunk {
        Chunk {
            tokens: vec![],
            start: end.absolute,
            end,
        }
    }

    /// The source text this chunk covers.
    pub(crate) fn span(&self) -> Span<BytePos> {
        Span {
            start: self.start,
            end: self.end.absolute,
        }
    }
}

/// The position just past the last character of `source`, counted the way the tokenizer
/// counts: lines and columns are 1-indexed, and a column advances by a character's byte
/// length.
pub(crate) fn end_of(source: &str) -> Position {
    let line = source.matches('\n').count() + 1;
    let column = match source.rfind('\n') {
        Some(newline) => source.len() - newline,
        None => source.len() + 1,
    };

    Position::new(source.len(), column, line)
}

/// An iterator adaptor that takes the tokenizer's output and yields one [`Chunk`] per
/// top-level declaration, the module header first. See the module documentation for
/// where it cuts.
///
/// # Advance or stop
///
/// This is the first consumer that reads the tokenizer past an error, which is the
/// operation `BUG-4` and `BUG-5` were about: a tokenizer path that reports an error
/// without consuming input reports it again on every poll. `Chunks` therefore ends the
/// stream at the second of two consecutive equal errors, keeping the first in its chunk.
/// A tokenizer that repeats one error truncates the parse there, where it would
/// otherwise never end (`CLAUDE.md` — *A `Result`-yielding iterator must advance or
/// stop*). Two consecutive errors that differ are both kept: each is evidence that the
/// tokenizer moved.
///
/// Once `next` has returned `None` it keeps returning `None`, whatever the source does,
/// so `Chunks` is a `FusedIterator`.
pub(crate) struct Chunks<I> {
    tokens: I,
    /// Where the source ends, which is where the last chunk's input ends.
    end_of_source: Position,
    /// The token that started the next chunk, read while the previous one was still
    /// being collected.
    pending: Option<Spanned<Position, Token>>,
    /// Whether any item has been read yet. The first token of the file never cuts, so
    /// the header chunk always opens on it.
    read_any: bool,
    /// The previous item read, when it was an error: the error a second, equal one is
    /// compared against.
    last_error: Option<Error>,
    /// Set once the source is exhausted, or repeated an error.
    finished: bool,
}

impl<I> Chunks<I>
where
    I: Iterator<Item = RawToken>,
{
    /// Cut `tokens`, a source's tokenizer output, into chunks. `end_of_source` is where
    /// the last chunk's input ends — see [`end_of`].
    pub(crate) fn new(tokens: I, end_of_source: Position) -> Chunks<I> {
        Chunks {
            tokens,
            end_of_source,
            pending: None,
            read_any: false,
            last_error: None,
            finished: false,
        }
    }

    /// Whether `token` opens a new chunk rather than continuing the current one.
    fn cuts(&self, token: &Spanned<Position, Token>) -> bool {
        self.read_any && token.span.start.column == 1 && can_start_declaration(&token.value)
    }
}

impl<I> Iterator for Chunks<I>
where
    I: Iterator<Item = RawToken>,
{
    type Item = Chunk;

    fn next(&mut self) -> Option<Chunk> {
        let mut chunk = match self.pending.take() {
            Some(first) => Chunk {
                start: first.span.start.absolute,
                tokens: vec![Ok(first)],
                end: self.end_of_source,
            },
            None if self.finished => return None,
            None => Chunk {
                tokens: vec![],
                start: BytePos(0),
                end: self.end_of_source,
            },
        };

        while !self.finished {
            match self.tokens.next() {
                None => self.finished = true,
                Some(Ok(token)) => {
                    self.last_error = None;
                    if self.cuts(&token) {
                        chunk.end = token.span.start;
                        self.pending = Some(token);
                        return Some(chunk);
                    }
                    self.read_any = true;
                    chunk.tokens.push(Ok(token));
                }
                Some(Err(error)) => {
                    if self.last_error.as_ref() == Some(&error) {
                        self.finished = true;
                    } else {
                        self.read_any = true;
                        self.last_error = Some(error.clone());
                        chunk.tokens.push(Err(error));
                    }
                }
            }
        }

        // The source ran out with nothing read since the last cut: there is no chunk to
        // hand back. That only happens to a source with no items at all, since a cut
        // always leaves its token pending.
        if chunk.tokens.is_empty() {
            return None;
        }

        Some(chunk)
    }
}

/// `next` returns `None` only once `finished` is set and nothing is pending, and nothing
/// clears `finished` or fills `pending` afterwards.
impl<I> FusedIterator for Chunks<I> where I: Iterator<Item = RawToken> {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::parser::tokenizer::{make_tokenizer, TokenizerError};
    use crate::compiler::position::spanned;
    use indoc::indoc;
    use std::cell::Cell;
    use std::rc::Rc;

    fn chunks_of(source: &str) -> Vec<Chunk> {
        let tokens = make_tokenizer(source).map(|r| r.map_err(Error::from));
        Chunks::new(tokens, end_of(source)).collect()
    }

    /// The first token of every chunk, or `None` for one that opens on an error.
    fn first_tokens(chunks: &[Chunk]) -> Vec<Option<Token>> {
        chunks
            .iter()
            .map(|chunk| match chunk.tokens.first() {
                Some(Ok(token)) => Some(token.value.clone()),
                _ => None,
            })
            .collect()
    }

    fn lower(name: &str) -> Option<Token> {
        Some(Token::LowerIdentifier(name.to_string()))
    }

    /// A column-1 token a declaration can start with cuts; an indented one and a
    /// column-1 one no declaration starts with do not. The chunks tile the source, and
    /// the last one ends at its end.
    ///
    /// Verified to fail by making `cuts` return `false`: the source is then one chunk.
    #[test]
    fn a_column_1_declaration_start_cuts_and_nothing_else_does() {
        let source = indoc! {"
            module M exposing (..)

            f x =
              g x
            | oops
            type T = A
        "};

        let chunks = chunks_of(source);

        assert_eq!(
            first_tokens(&chunks),
            vec![Some(Token::Module), lower("f"), Some(Token::Type)]
        );

        let f_start = source.find("f x").unwrap();
        let type_start = source.find("type").unwrap();
        assert_eq!(chunks[0].span().to_range(), 0..f_start);
        assert_eq!(chunks[1].span().to_range(), f_start..type_start);
        assert_eq!(chunks[2].span().to_range(), type_start..source.len());
        assert_eq!(chunks[1].end.column, 1);
    }

    /// A tokenizer error never starts a chunk, and the tokens after it are still cut.
    ///
    /// Verified to fail by making `cuts` return `false`: `g` is then in `f`'s chunk.
    #[test]
    fn a_tokenizer_error_stays_in_its_chunk() {
        let chunks = chunks_of("module M exposing (..)\nf = \t1\ng = 2\n");

        assert_eq!(
            first_tokens(&chunks),
            vec![Some(Token::Module), lower("f"), lower("g")]
        );
        assert!(chunks[1].tokens.iter().any(|item| item.is_err()));
        assert!(chunks[2].tokens.iter().all(|item| item.is_ok()));
    }

    /// The first token of the file never cuts, even when it is one a declaration
    /// starts with, so the header chunk is never empty for a file with tokens in it.
    ///
    /// Verified to fail by dropping `self.read_any` from `cuts`: an empty chunk then
    /// comes first.
    #[test]
    fn the_first_token_of_the_file_does_not_cut() {
        let chunks = chunks_of("f = 1\ng = 2\n");

        assert_eq!(first_tokens(&chunks), vec![lower("f"), lower("g")]);
    }

    /// A source that yields one token and then the same error without end is cut short
    /// at the second error, so polling the adaptor ends.
    ///
    /// The source counts its polls and panics past a cap, which is how the test turns
    /// a loop that would never return into a failure. Verified to fail by removing the
    /// `self.last_error.as_ref() == Some(&error)` check from `next`: the first poll then
    /// reads the source past its cap.
    #[test]
    fn a_repeated_tokenizer_error_ends_the_stream() {
        const SOURCE_CAP: usize = 64;
        const POLLS: usize = 4;

        let polls = Rc::new(Cell::new(0));
        let counter = Rc::clone(&polls);
        let position = Position::new(0, 1, 1);
        let error = Error::Tokenizer(TokenizerError {
            error: spanned(
                BytePos(1),
                BytePos(2),
                crate::compiler::parser::tokenizer::TokenizerErrorType::TabError,
            ),
        });
        let mut first = Some(Ok(spanned(position, position, Token::Module)));
        let source = std::iter::from_fn(move || {
            counter.set(counter.get() + 1);
            assert!(
                counter.get() <= SOURCE_CAP,
                "the adaptor kept reading a source that repeats one error"
            );
            Some(first.take().unwrap_or_else(|| Err(error.clone())))
        });

        let mut chunks = Chunks::new(source, position);
        let items: Vec<Chunk> = (0..POLLS).map_while(|_| chunks.next()).collect();

        assert_eq!(items.len(), 1, "expected one chunk, got {:?}", items);
        assert_eq!(items[0].tokens.len(), 2, "the token and the first error");
        assert!(
            chunks.next().is_none(),
            "the adaptor restarted after ending"
        );
        assert!(
            polls.get() <= 3,
            "read {} items from the source",
            polls.get()
        );
    }
}
