#[derive(Debug, PartialEq, Clone, Copy, Default)]
pub struct Position {
    /// The byte index in the original source code
    pub absolute: usize,
}

impl Position {
    pub fn new(byte_index: usize) -> Position {
        Position {
            absolute: byte_index,
        }
    }

    pub fn increment(&mut self) {
        self.increment_by(1);
    }

    pub fn increment_by(&mut self, inc: usize) {
        self.absolute += inc;
    }

    /// reset the position to the beginning of a document
    pub fn reset(&mut self) {
        self.absolute = 0;
    }
}

#[cfg(test)]
mod tests {
    use super::Position;

    #[test]
    fn position_increment() {
        let mut position = Position::new(0);

        position.increment();
        assert_eq!(position, Position::new(1));

        position.increment_by(3);
        assert_eq!(position, Position::new(4));
    }

    #[test]
    fn position_reset() {
        let mut position = Position::new(42);
        position.reset();

        assert_eq!(position, Position::new(0));
    }

    #[test]
    fn position_fmt_debug() {
        assert_eq!(
            format!("{:?}", Position::new(54)),
            "Position { absolute: 54 }"
        );
    }
}
