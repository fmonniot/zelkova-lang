#[derive(Debug, PartialEq, Clone, Copy, Default)]
pub struct Position {
    /// The byte index in the original source code
    pub absolute: BytePos,
    /// Column is a 1-indexed value telling us where the character is on a line
    pub column: usize,
    /// Line is a 1-indexed value telling us on which line the character is
    pub line: usize,
}

impl Position {
    pub fn new(byte_index: usize, column: usize, line: usize) -> Position {
        Position {
            absolute: BytePos(byte_index as u32),
            column,
            line,
        }
    }

    pub fn increment(&mut self) {
        self.increment_by(1);
    }

    pub fn increment_by(&mut self, inc: usize) {
        self.absolute += inc as u32;
        self.column += inc;
    }

    pub fn new_line(&mut self) {
        self.absolute += 1;
        self.column = 1;
        self.line += 1;
    }

    /// reset the position to the beginning of a document
    pub fn reset(&mut self) {
        self.absolute = BytePos(0);
        self.column = 1;
        self.line = 1;
    }
}

/// A value `T` enriched with its starting and ending position in the source code
#[derive(Clone, Debug, PartialEq)]
pub struct Spanned<I, T> {
    pub span: Span<I>,
    pub value: T,
}

impl<I, T> Spanned<I, T> {
    pub fn start(&self) -> &I {
        &self.span.start
    }

    pub fn end(&self) -> &I {
        &self.span.end
    }

    pub fn map<U, F>(self, mut f: F) -> Spanned<I, U>
    where
        F: FnMut(T) -> U,
    {
        Spanned {
            span: self.span,
            value: f(self.value),
        }
    }
}

impl<I, T> From<(I, T, I)> for Spanned<I, T> {
    fn from(tuple: (I, T, I)) -> Self {
        let (start, value, end) = tuple;

        Spanned {
            span: Span { start, end },
            value,
        }
    }
}

pub fn spanned<I, T>(start: I, end: I, value: T) -> Spanned<I, T> {
    Spanned {
        span: Span { start, end },
        value,
    }
}

/// A `Span` represents a section of the original source code
///
/// TODO `Span` is curently generic. Once we have something working
/// end to end, we should see if we are using `BytePos` everywhere
/// or not.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Span<I> {
    pub start: I,
    pub end: I,
}

/// The raw byte index.
///
/// We use a 32-bit integer here for space efficiency, assuming we won't
/// be working with sources larger than 4GB.
#[derive(Clone, Copy, Debug, PartialEq, Default)]
pub struct BytePos(pub u32);

impl BytePos {
    pub fn to_range(self) -> std::ops::Range<usize> {
        let u = self.0 as usize;

        u..u
    }
}

impl Span<BytePos> {
    /// This method will panic when used on 16 bits platform or less.
    /// We assume zelkova won't support such platform.
    pub fn to_range(self) -> std::ops::Range<usize> {
        let start = self.start.0 as usize;
        let end = self.end.0 as usize;

        start..end
    }
}

/// The position of an AST node in the source it was parsed from, if it had one.
///
/// # Why `PartialEq` is blind
///
/// `PartialEq` here always returns `true`, so two nodes that differ only in where
/// they were written compare equal. That is deliberate, not an oversight: the
/// parser tests compare a whole `Module` value against a literal built by hand,
/// and a hand-written literal cannot know the byte offsets the tokenizer computed.
/// The alternative — a hand-written traversal that strips spans before comparing —
/// silently stops covering whatever node the next person forgets to add to it.
///
/// The cost is real and worth naming: a whole-value `assert_eq!` can no longer pin
/// *where* something parsed, because every span in it compares equal to every other.
/// A test that cares about a position therefore has to assert on `.span` directly.
///
/// Keeping the blindness inside this one newtype is what leaves [`Span`] and
/// [`Spanned`] with their real, derived equality, so the tokenizer, layout and
/// parser-error tests are unaffected.
///
/// # What `None` means
///
/// `None` means "not built from source": a node a test constructed by hand, or one
/// a future desugaring pass synthesised with nothing in the user's text to point at.
/// Such a node contributes no label to a diagnostic — it is skipped, not rendered as
/// a zero-width caret at the top of the file.
#[derive(Clone, Copy, Debug, Default)]
pub struct NodeSpan(Option<Span<BytePos>>);

impl NodeSpan {
    /// The span of a node the parser built, from the `@L`/`@R` its production captured.
    pub fn new(start: BytePos, end: BytePos) -> NodeSpan {
        NodeSpan(Some(Span { start, end }))
    }

    /// A node with no position: built by a test, or synthesised by the compiler.
    pub const fn none() -> NodeSpan {
        NodeSpan(None)
    }

    pub fn span(self) -> Option<Span<BytePos>> {
        self.0
    }

    /// The smallest span covering both, tolerating a missing one on either side.
    ///
    /// A declaration's span is its annotation merged with each of its bindings, and
    /// either half may be absent — an annotation with no body, a body with no
    /// annotation — so a missing operand yields the other rather than nothing.
    pub fn merge(self, other: NodeSpan) -> NodeSpan {
        match (self.0, other.0) {
            (Some(a), Some(b)) => NodeSpan(Some(Span {
                start: if a.start.0 <= b.start.0 {
                    a.start
                } else {
                    b.start
                },
                end: if a.end.0 >= b.end.0 { a.end } else { b.end },
            })),
            (Some(a), None) => NodeSpan(Some(a)),
            (None, Some(b)) => NodeSpan(Some(b)),
            (None, None) => NodeSpan(None),
        }
    }

    /// A span from `start` to wherever `inner` ends.
    ///
    /// The end of a production is not always the `@R` after its last symbol. In
    /// `grammar.lalrpop`, `Expr`'s `case` alternative finishes by consuming a layout
    /// `CloseBlock`, and the layout pass positions an implicitly-closed block at the
    /// token that closed it — the first token of the *next* declaration, or, at end
    /// of file, `EndOfFile`, whose `BytePos` is 0. An `@R` taken past such a
    /// nonterminal therefore runs into the following declaration or inverts the span
    /// outright. Reading the end off the node the production just built sidesteps
    /// that: a node's own span already stops at the user's last character.
    ///
    /// A span-less `inner` — a hand-built node, never one the grammar produced —
    /// degrades to the empty span at `start` rather than to a wrong one.
    pub fn to_end_of(start: BytePos, inner: NodeSpan) -> NodeSpan {
        match inner.0 {
            Some(s) => NodeSpan::new(start, s.end),
            None => NodeSpan::new(start, start),
        }
    }

    /// The byte range a `codespan_reporting::Label` wants, when there is one.
    pub fn to_range(self) -> Option<std::ops::Range<usize>> {
        self.0.map(|s| s.to_range())
    }
}

impl PartialEq for NodeSpan {
    /// Always `true` — see the type's documentation for why.
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl std::ops::Add<u32> for BytePos {
    type Output = BytePos;

    fn add(self, rhs: u32) -> BytePos {
        BytePos(self.0 + rhs)
    }
}

impl std::ops::AddAssign<u32> for BytePos {
    fn add_assign(&mut self, rhs: u32) {
        self.0 += rhs;
    }
}

#[cfg(test)]
mod tests {
    use super::Position;

    #[test]
    fn position_increment() {
        let mut position = Position::new(0, 1, 1);

        position.increment();
        assert_eq!(position, Position::new(1, 2, 1));

        position.increment_by(3);
        assert_eq!(position, Position::new(4, 5, 1));
    }

    #[test]
    fn position_reset() {
        let mut position = Position::new(42, 2, 2);
        position.reset();

        assert_eq!(position, Position::new(0, 1, 1));
    }

    #[test]
    fn position_fmt_debug() {
        assert_eq!(
            format!("{:?}", Position::new(54, 1, 1)),
            "Position { absolute: BytePos(54), column: 1, line: 1 }"
        );
    }
}
