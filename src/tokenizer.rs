// Directly inspired by the great work on the RustPython team
// https://github.com/RustPython/RustPython/blob/master/parser/src/lexer.rs

use crate::position::Position;
use log::{trace, warn}; // Location in RustPython
use std::cmp::Ordering;

/// Represents the different part which constitute our source code
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Identifier { name: String },
    Integer { value: i32 }, // web assembly support i/f 32/64
    Float { value: f32 },

    // Control characters
    Newline,
    Indent,
    Dedent,
    StartProgram,
    EndOfFile,

    // Symbols
    LPar,
    RPar,
    Colon,
    Equal,

    // Keywords
    Module,
    Exposing,
    If,
    Else,
    Then,
}

// TODO Rename to TokenizerError
/// Represents an error during tokenization.
#[derive(Debug, PartialEq)]
pub struct LexicalError {
    pub error: LexicalErrorType,
    pub position: Position,
}

// TODO Remove errors that aren't needed
/// The type of error refered in `LexicalError`
#[derive(Debug, PartialEq)]
pub enum LexicalErrorType {
    StringError,
    UnicodeError,
    IndentationError,
    TabError,
    DefaultArgumentError,
    CommentError, // A comment call was issued on a non-comment character
    DuplicateKeywordArgumentError,
    UnrecognizedToken { tok: char },
    LineContinuationError,
    EOF,
    OtherError(String),
}

/// A `Token` enriched with its starting and ending position in the source code
pub type Spanned = (Position, Token, Position); // (start, tok, end) Location in RustPython

/// Represent a standard `Result` scoped to a `LexicalError`
pub type Result<T> = std::result::Result<T, LexicalError>;

/// Take a source code and return an iterator of [`Spanned`](type.Spanned.html)
///
/// This is the access point of this module, and the only way to create the underlying
/// `Tokenizer` (although it isn't exposed to public consumption)
pub fn make_tokenizer<'a>(source: &'a str) -> impl Iterator<Item = Result<Spanned>> + 'a {
    let c = NewlineCollapser::new(source.chars());
    Tokenizer::new(c)
}

/// A structure taking an char iterator and collapsing newlines
/// control characters into `\n`.
struct NewlineCollapser<T: Iterator<Item = char>> {
    source: T,
    char_curr: Option<char>,
    char_next: Option<char>,
}

impl<T> NewlineCollapser<T>
where
    T: Iterator<Item = char>,
{
    fn new(source: T) -> Self {
        let mut c = NewlineCollapser {
            source,
            char_curr: None,
            char_next: None,
        };

        // We need to pre-populate the current and next
        // characters, otherwise the first call to `next`
        // won't return anything.

        c.shift();
        c.shift();

        c
    }

    /// Shift the current iterator by one, saving current character
    /// and next one in the structure.
    /// Returns the current char before the shift
    fn shift(&mut self) -> Option<char> {
        let prev = self.char_curr;
        self.char_curr = self.char_next;
        self.char_next = self.source.next();
        prev
    }
}

impl<T> Iterator for NewlineCollapser<T>
where
    T: Iterator<Item = char>,
{
    type Item = char;

    // Windows use \r\n and the rest of the world \n
    // (except some unsupported system which use \r only)
    // The iterator will collapse \r\n into \n
    fn next(&mut self) -> Option<Self::Item> {
        if self.char_curr == Some('\r') {
            if self.char_next == Some('\n') {
                // We have a Windows-like new line, let's shift the iterator by one
                self.shift();
            }
        }

        // We aren't looking at a new line, let's advance the iterator
        self.shift()
    }
}

/// Tokenizer is an iterator which consume a (UNIX) source and
/// produces `Result<Token>`
///  
/// We use an intermediate Vec between the processing
/// and the iterator's next because some loop could
/// create more than one token (eg. end of an increment
/// block or end of file).
struct Tokenizer<I: Iterator<Item = char>> {
    chars: I,
    at_line_start: bool,
    processed_tokens: Vec<Spanned>, // Tokens we have parsed but not yet emitted
    position: Position,
    lookahead: (Option<char>, Option<char>, Option<char>), // current char, next and +1
    indentation: usize,
}

impl<I> Tokenizer<I>
where
    I: Iterator<Item = char>,
{
    fn new(collapser: I) -> Tokenizer<I> {
        let mut tok = Tokenizer {
            chars: collapser,
            at_line_start: true, // Nothing have been read yet, so…
            processed_tokens: vec![],
            position: Position::new(0, 0),
            lookahead: (None, None, None),
            indentation: 0,
        };

        // Fill out the lookahead structure
        tok.next_char();
        tok.next_char();
        tok.next_char();

        // Because next_char advanced the position, we need
        // to reset it to the beginning of the document
        tok.position.reset();

        tok
    }

    //
    // iterator helper
    //

    // A simple utility function which will advance the iterator
    // by one character. It also manage the current position in
    // the document.
    fn next_char(&mut self) -> Option<char> {
        let current = self.lookahead.0;

        self.lookahead.0 = self.lookahead.1;
        self.lookahead.1 = self.lookahead.2;
        self.lookahead.2 = self.chars.next();

        if current == Some('\n') {
            self.position.newline();
            self.at_line_start = true;
        } else {
            self.position.go_right();
        };

        trace!(
            "next_char: lookahead={:?}, position={:?}",
            self.lookahead,
            self.position
        );

        current
    }

    /// Utility to skip character until the current char is a `\n`
    /// (or we reached the end of the iterator).
    fn skip_end_of_line(&mut self) {
        loop {
            match self.lookahead.0 {
                Some('\n') => break,
                Some(_) => (),
                None => break,
            }
            self.next_char();
        }
    }

    //
    // Token processing
    //

    fn process_next_tokens(&mut self) -> Result<Spanned> {
        // Here we have to process characters until we can form a complete
        // token. We will also use this time to handle indentations.

        let mut cnt = 0;

        // We have nothing to emit, continue processing chars
        while self.processed_tokens.is_empty() {
            // Start of a new line, let's get indentations out of the way
            if self.at_line_start {
                self.handle_indentation()?;
            }

            trace!(
                "after handle_indentation: self.processed_tokens={:?}",
                self.processed_tokens
            );
            self.consume_char()?;
            cnt += 1;

            // TODO Remove once bug is fixed
            if cnt > 20 {
                break;
            }
        }

        Ok(self.processed_tokens.remove(0))
    }

    //
    // Character consumption helpers
    //

    fn handle_indentation(&mut self) -> Result<()> {
        let indentation = self.consume_indentation()?;

        trace!(
            "handle_indentation: indentation={}, self.indentation={}",
            indentation,
            self.indentation
        );

        match indentation.cmp(&self.indentation) {
            Ordering::Equal => {
                // Same level as previous line, nothing to do
            }
            Ordering::Greater => {
                // Current level is greater than previous line
                let diff = indentation - self.indentation;

                // Emit one Indent token per indentation level
                for _ in 1..(diff / 2) {
                    self.processed_tokens
                        .push((self.position, Token::Indent, self.position));
                }
            }
            Ordering::Less => {
                // We have less indentation than previous line
                let diff = self.indentation - indentation;

                // TODO Does it actually make sense to emit multiple dedent per 2 spaces ?
                for _ in 1..(diff / 2) {
                    self.processed_tokens
                        .push((self.position, Token::Dedent, self.position));
                }
            }
        }

        Ok(())
    }

    /// Consume the characters until we reach a non-indentation
    /// and/or non-comment character, leaving the iterator to
    /// point at it.
    fn consume_indentation(&mut self) -> Result<usize> {
        let mut spaces = 0;

        trace!("consume_indentation()");

        loop {
            match self.lookahead.0 {
                Some(' ') => {
                    spaces += 1;
                    self.next_char();
                }
                Some('\t') => {
                    // Zelkova forbid the use of tabs for indentation
                    return Err(LexicalError {
                        error: LexicalErrorType::TabError,
                        position: self.position,
                    });
                }
                Some('-') => {
                    // Possible comment
                    if let Some('-') = self.lookahead.1 {
                        // This is a comment, let's skip it (and start counting again)
                        self.consume_comment()?;
                        spaces = 0;
                    }
                }
                Some('{') => {
                    if let Some('-') = self.lookahead.1 {
                        // This is a comment, let's skip it (and start counting again)
                        self.consume_comment()?;
                        spaces = 0;
                    }
                }
                Some('\n') => {
                    // We have an empty line, reset and start again
                    spaces = 0;
                    self.next_char();
                }
                None => {
                    // EOF
                    spaces = 0;
                    break;
                }
                _ => {
                    // We arrived at the first character of the line
                    self.at_line_start = false;
                    break;
                }
            }
        }

        // Indentation must be by 2 spaces, anything else is an error
        if spaces % 2 != 0 {
            return Err(LexicalError {
                error: LexicalErrorType::IndentationError,
                position: self.position,
            });
        }

        Ok(spaces)
    }

    /// Consume the iterator until the end of the comment.
    /// Calling this method when the current character is not one
    /// starting a comment will result in an error
    fn consume_comment(&mut self) -> Result<()> {
        if self.lookahead.0 == Some('-') && self.lookahead.1 == Some('-') {
            // Single line comment end at the start of the next line
            trace!("Start skipping single line comment {:?}", self.position);
            self.skip_end_of_line();
        } else if self.lookahead.0 == Some('{') && self.lookahead.1 == Some('-') {
            // A multi line comment end at the newline after the -} symbol
            trace!("Start skipping multi line comment {:?}", self.position);

            loop {
                match self.lookahead.0 {
                    Some('-') => {
                        if let Some('}') = self.lookahead.1 {
                            // We have reached the end symbol, let's skip the rest of the line
                            self.skip_end_of_line();
                            break;
                        }
                    }
                    Some(_) => (),
                    None => break,
                }
                self.next_char();
            }
        } else {
            // We aren't looking at the symbols -- or {-, this isn't a comment
            warn!(
                "Called Tokenizer.consume_comment on non-comment symbols ({})",
                self.position
            );
            return Err(LexicalError {
                error: LexicalErrorType::CommentError,
                position: self.position,
            });
        }

        trace!(
            "Comment skipped: lookahead={:?}, position={:?}",
            self.lookahead,
            self.position
        );
        Ok(())
    }

    /// The meat of the Tokenizer structure. This method is in charge
    /// of producing the symbols, keywords, literals and other
    /// identifier tokens (all the pesky details like comments should
    /// have been handled by the [`process_next_tokens`](#method.process_next_tokens)
    /// method).
    fn consume_char(&mut self) -> Result<()> {
        if let Some(_) = self.lookahead.0 {
            Ok(()) // TODO
        } else {
            // Nothing else to pull, let's wrap it up

            // Insert a trailing Newline if none, this is to simplify the
            // parser step (making it assume there is always a newline at
            // the end).
            if !self.at_line_start {
                self.at_line_start = true;
                self.processed_tokens
                    .push((self.position, Token::Newline, self.position));
            }

            // Next emit the remaining deindent tokens (if any)
            while self.indentation > 0 {
                self.indentation -= 2;
                self.processed_tokens
                    .push((self.position, Token::Dedent, self.position));
            }

            // And finally emit the EOF token
            self.processed_tokens
                .push((self.position, Token::EndOfFile, self.position));

            Ok(())
        }
    }
}

impl<T> Iterator for Tokenizer<T>
where
    T: Iterator<Item = char>,
{
    type Item = Result<Spanned>;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.process_next_tokens();

        trace!("Tokenizer.next. token={:?}", token);

        match token {
            Ok((_, Token::EndOfFile, _)) => None,
            r => Some(r),
        }
    }
}

#[cfg(test)]
mod tests {

    use super::{
        make_tokenizer, LexicalError, LexicalErrorType, NewlineCollapser, Position, Token,
    };
    use indoc::indoc;

    /// This function is useful when debugging a test failure.
    /// When not used, the logs aren't properly redirected to
    /// stdout and thus we don't see them.
    ///
    /// See https://github.com/env-logger-rs/env_logger/issues/107
    /// for context.
    #[allow(dead_code)]
    fn enable_logs() {
        env_logger::builder().is_test(true).init();
    }

    pub fn tokenize(source: &str) -> Vec<Token> {
        make_tokenizer(source)
            .map(|x| x.expect("no error in tokenize").1)
            .collect()
    }

    #[test]
    fn test_newline_collapser() {
        let src = "ab\ncd\r\ne";
        let result: Vec<_> = NewlineCollapser::new(src.chars()).collect();

        assert_eq!(result, vec!['a', 'b', '\n', 'c', 'd', '\n', 'e']);
    }

    #[test]
    fn test_empty_programs() {
        assert_eq!(make_tokenizer("").collect::<Vec<_>>(), vec![]);
        assert_eq!(make_tokenizer("    ").collect::<Vec<_>>(), vec![]);
        assert_eq!(make_tokenizer("  \n  ").collect::<Vec<_>>(), vec![]);
    }

    #[test]
    fn test_invalid_indentation() {
        assert_eq!(
            make_tokenizer(" a").collect::<Result<Vec<_>, _>>(),
            Err(LexicalError {
                error: LexicalErrorType::IndentationError,
                position: Position::new(1, 0)
            })
        );

        assert_eq!(
            make_tokenizer("  \ta").collect::<Result<Vec<_>, _>>(),
            Err(LexicalError {
                error: LexicalErrorType::TabError,
                position: Position::new(2, 0)
            })
        );
    }

    #[test]
    fn test_comments() {
        let tokens = tokenize(indoc! {
            "-- this is a comment
             {- and this is a
                multiline comment
             -}
            "
        });

        assert_eq!(tokens, vec![]);
    }

    #[test]
    fn test_simple_constant() {
        let tokens = tokenize(indoc! {"
            module Main exposing(main)

            main : Int
            main = 42
        "});
        let expected: Vec<Token> = vec![
            Token::Identifier {
                name: "module".to_string(),
            },
            Token::Identifier {
                name: "exposing".to_string(),
            }, // Special token ?
            Token::LPar,
            Token::Identifier {
                name: "main".to_string(),
            },
            Token::RPar,
            Token::Newline,
            Token::Newline,
            Token::Identifier {
                name: "main".to_string(),
            },
            Token::Colon,
            Token::Identifier {
                name: "Int".to_string(),
            },
            Token::Newline,
            Token::Identifier {
                name: "main".to_string(),
            },
            Token::Equal,
            Token::Integer { value: 42 },
            Token::EndOfFile,
        ];

        assert_eq!(tokens, expected)
    }
}
