use codespan::{ColumnIndex, LineIndex};

#[derive(Debug, PartialEq, Clone, Copy, Default)]
pub struct Position {
    pub line: LineIndex,
    pub column: ColumnIndex,
    pub absolute: usize,
}

impl Position {
    pub fn new(line: u32, column: u32, byte_index: usize) -> Position {
        Position {
            line: LineIndex(line),
            column: ColumnIndex(column),
            absolute: byte_index,
        }
    }

    pub fn newline(&mut self) {
        self.column.0 = 0;
        self.line.0 += 1;
        self.absolute += 1;
    }

    pub fn go_right(&mut self) {
        self.column.0 += 1;
        self.absolute += 1;
    }

    // Used by some tests
    #[allow(dead_code)]
    pub fn go_right_by(&mut self, inc: usize) {
        self.column.0 += inc as u32;
        self.absolute += inc;
    }

    /// reset the position to the beginning of a document
    pub fn reset(&mut self) {
        self.line.0 = 0;
        self.column.0 = 0;
        self.absolute = 0;
    }
}

impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "line: {}, column: {}",
            self.line.number(),
            self.column.number()
        )
    }
}

#[cfg(test)]
mod tests {
    use super::Position;

    #[test]
    fn position_newline() {
        let mut position = Position::new(10, 1, 0);
        position.newline();

        let expected = Position::new(11, 0, 1);

        assert_eq!(position, expected);
    }

    #[test]
    fn position_go_right() {
        let mut position = Position::new(10, 1, 0);
        position.go_right();

        let expected = Position::new(10, 2, 1);

        assert_eq!(position, expected);
    }

    #[test]
    fn position_reset() {
        let mut position = Position::new(10, 1, 1);
        position.reset();

        assert_eq!(position, Position::new(0, 0, 0));
    }

    #[test]
    fn position_fmt_display() {
        assert_eq!(
            format!("{}", Position::new(2, 43, 54)),
            "line: 3, column: 44"
        );
    }

    #[test]
    fn position_fmt_debug() {
        assert_eq!(
            format!("{:?}", Position::new(2, 43, 54)),
            "Position { line: LineIndex(2), column: ColumnIndex(43), absolute: 54 }"
        );
    }
}
