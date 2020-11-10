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
