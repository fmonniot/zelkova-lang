use codespan::{ColumnIndex, LineIndex};

#[derive(Debug, PartialEq, Clone, Copy, Default)]
pub struct Position {
    pub line: LineIndex,
    pub column: ColumnIndex,
}

impl Position {
    pub fn new(line: u32, column: u32) -> Position {
        Position {
            line: LineIndex(line),
            column: ColumnIndex(column),
        }
    }

    pub fn newline(&mut self) {
        self.column.0 = 0;
        self.line.0 = self.line.0 + 1;
    }

    pub fn go_right(&mut self) {
        self.column.0 = self.column.0 + 1;
    }

    pub fn go_right_by(&mut self, inc: u32) {
        self.column.0 = self.column.0 + inc;
    }

    /// reset the position to the beginning of a document
    pub fn reset(&mut self) {
        self.line.0 = 0;
        self.column.0 = 0;
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
        let mut position = Position::new(10, 1);
        position.newline();

        let expected = Position::new(11, 0);

        assert_eq!(position, expected);
    }

    #[test]
    fn position_go_right() {
        let mut position = Position::new(10, 1);
        position.go_right();

        let expected = Position::new(10, 2);

        assert_eq!(position, expected);
    }

    #[test]
    fn position_reset() {
        let mut position = Position::new(10, 1);
        position.reset();

        assert_eq!(position, Position::new(0, 0));
    }
}
