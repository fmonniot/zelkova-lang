#[derive(Debug, PartialEq, Clone, Copy, Default)]
pub struct Position {
    /// The byte index in the original source code
    pub absolute: usize,
    /// Column is a 1-indexed value telling us where the character is on a line
    pub column: usize,
    /// Line is a 1-indexed value telling us on which line the character is
    pub line: usize,
}

impl Position {
    pub fn new(byte_index: usize, column: usize, line: usize) -> Position {
        Position {
            absolute: byte_index,
            column,
            line,
        }
    }

    pub fn increment(&mut self) {
        self.increment_by(1);
    }

    pub fn increment_by(&mut self, inc: usize) {
        self.absolute += inc;
        self.column += inc;
    }

    pub fn new_line(&mut self) {
        self.absolute += 1;
        self.column = 1;
        self.line += 1;
    }

    /// reset the position to the beginning of a document
    pub fn reset(&mut self) {
        self.absolute = 0;
        self.column = 1;
        self.line = 1;
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
            "Position { absolute: 54, column: 1, line: 1 }"
        );
    }
}
