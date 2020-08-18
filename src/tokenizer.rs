// Directly inspired by the great work on the RustPython team
// https://github.com/RustPython/RustPython/blob/master/parser/src/lexer.rs

use crate::position::Position;
use log::{trace, warn}; // Location in RustPython
use std::cmp::Ordering;
use std::collections::HashMap;
use std::str::FromStr;
use unic_ucd_category::GeneralCategory;

/// Represents the different part which constitute our source code
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Identifier { name: String },
    Integer { value: i32 }, // web assembly support i/f 32/64
    Float { value: f32 },
    Char { value: char },

    // Control characters
    Newline,
    Indent,
    Dedent,
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
    Then,
    Else,
}

fn get_keywords() -> HashMap<String, Token> {
    let mut m = HashMap::new();

    m.insert("module".to_string(), Token::Module);
    m.insert("exposing".to_string(), Token::Exposing);
    m.insert("if".to_string(), Token::If);
    m.insert("then".to_string(), Token::Then);
    m.insert("else".to_string(), Token::Else);

    m
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
    CharError,
    StringError,  // TODO String literal
    UnicodeError, // TODO String literal
    IndentationError,
    TabError,
    CommentError, // A comment call was issued on a non-comment character
    UnrecognizedToken { tok: char },
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
    keywords: HashMap<String, Token>,
}

impl<I> Tokenizer<I>
where
    I: Iterator<Item = char>,
{
    /// Build a new Tokenizer based on a char iterator
    fn new(collapser: I) -> Tokenizer<I> {
        let mut tok = Tokenizer {
            chars: collapser,
            at_line_start: true, // Nothing have been read yet, so…
            processed_tokens: vec![],
            position: Position::new(0, 0),
            lookahead: (None, None, None),
            indentation: 0,
            keywords: get_keywords(),
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

    /// Skip over the next character and return a `Spanned` with the
    /// skipped character position.
    fn skip_char_as(&mut self, token: Token) -> Spanned {
        let start = self.position;
        self.next_char().unwrap(); // skip over the char
        let end = self.position;

        (start, token, end)
    }

    /// Indicates whether the given `char` is fit to start an identifier
    ///
    /// An Elm identifier must beging with a character from the unicode categories:
    /// - Uppercase letter (Lu) (modules, types)
    /// - Lowercase letter (Ll) (functions, variables)
    /// - Titlecase letter (Lt) (modules, types)
    fn is_identifier_start(&self, c: char) -> bool {
        // Fast check on ASCII characters, assuming that's the common case
        match c {
            'A'..='Z' => true,
            'a'..='z' => true,
            _ => GeneralCategory::of(c).is_cased_letter(),
        }
    }

    /// Indicates wether the given `char` is fit to continue an identifier
    ///
    /// An Elm identifier can contains a character from the following unicode categories
    /// - Uppercase letter (Lu)
    /// - Lowercase letter (Ll)
    /// - Titlecase letter (Lt)
    /// - Modifier letter (Lm)
    /// - Other letter (Lo)
    /// - Decimal digit number (Nd)
    /// - Letter number (Nl)
    /// - Or be _ (except for in module names).
    fn is_identifier_continuation(&self, c: char) -> bool {
        // Fast check on ASCII characters, assuming that's the common case
        match c {
            'A'..='Z' => true,
            'a'..='z' => true,
            '0'..='9' => true,
            '_' => true,
            _ => matches!(
                GeneralCategory::of(c),
                GeneralCategory::UppercaseLetter |     // Lu
                    GeneralCategory::LowercaseLetter | // Ll
                    GeneralCategory::TitlecaseLetter | // Lt
                    GeneralCategory::ModifierLetter |  // Lm
                    GeneralCategory::OtherLetter |     // Lo
                    GeneralCategory::DecimalNumber |   // Nd
                    GeneralCategory::LetterNumber // Nl
            ),
        }
    }

    //
    // Token processing
    //

    fn process_next_tokens(&mut self) -> Result<Spanned> {
        // Here we have to process characters until we can form a complete
        // token. We will also use this time to handle indentations.

        // We have nothing to emit, continue processing chars
        while self.processed_tokens.is_empty() {
            // Start of a new line, let's get indentations out of the way
            if self.at_line_start {
                self.handle_indentation()?;
            }

            self.consume_char()?;
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
        trace!(
            "consume_char: lookahead={:?}, position={:?}",
            self.lookahead,
            self.position
        );

        if let Some(c) = self.lookahead.0 {
            if self.is_identifier_start(c) {
                let identifier = self.consume_identifier()?;
                self.processed_tokens.push(identifier);
            } else {
                // Something else
                match c {
                    '0'..='9' => {
                        let number = self.consume_number();
                        self.processed_tokens.push(number);
                    }
                    '(' => {
                        let spanned = self.skip_char_as(Token::LPar);
                        self.processed_tokens.push(spanned);
                    }
                    ')' => {
                        let spanned = self.skip_char_as(Token::RPar);
                        self.processed_tokens.push(spanned);
                    }
                    ':' => {
                        let spanned = self.skip_char_as(Token::Colon);
                        self.processed_tokens.push(spanned);
                    }
                    '=' => {
                        // TODO Here we have to disambiguate on ==
                        let spanned = self.skip_char_as(Token::Equal);
                        self.processed_tokens.push(spanned);
                    }
                    '\'' => {
                        if let Some(value) = self.lookahead.1 {
                            if let Some('\'') = self.lookahead.2 {
                                let start_pos = self.position;
                                // skip over the opening quote, char and closing quote
                                self.next_char().unwrap();
                                self.next_char().unwrap();
                                self.next_char().unwrap();
                                let end_pos = self.position;

                                self.processed_tokens.push((
                                    start_pos,
                                    Token::Char { value },
                                    end_pos,
                                ));
                            } else {
                                // error: opened quote with char but no closing quote
                                // We haven't moved the cursor yet, but we know the error
                                // is on the next character, so we build the position manually
                                let mut position = self.position.clone();
                                position.go_right();
                                return Err(LexicalError {
                                    error: LexicalErrorType::CharError,
                                    position: position,
                                })
                            }
                        } else {
                            // error: opened single quote without character following
                            return Err(LexicalError {
                                error: LexicalErrorType::CharError,
                                position: self.position,
                            })
                        }
                    }
                    ' ' => {
                        self.next_char().unwrap(); // let's skip over whitespace
                    }
                    '\t' => {
                        return Err(LexicalError {
                            error: LexicalErrorType::TabError,
                            position: self.position,
                        })
                    }
                    '\n' => {
                        let spanned = self.skip_char_as(Token::Newline);
                        self.processed_tokens.push(spanned);
                    }
                    _ => {
                        let c = self.next_char().expect("lookahead.0 should be present");
                        return Err(LexicalError {
                            error: LexicalErrorType::UnrecognizedToken { tok: c },
                            position: self.position,
                        });
                    }
                }
            }

            Ok(())
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

    /// Consume the iterator until we reach a char which isn't suited for an identifier.
    ///
    /// This can return two types of tokens:
    /// - a keyword token (e.g. `Token::Module`) if the consumed identifier is part of
    ///   the reserved list of keywords. See [`get_keywords()`](#function.get_keywords)
    ///   for a list of keywords.
    /// - a `Token::Identifier` if the identifier isn't a keyword, this encompass basically
    ///   everything which isn't a symbol, literal or keyword in the language.
    fn consume_identifier(&mut self) -> Result<Spanned> {
        trace!(
            "consume_identifier: lookahead={:?}, position={:?}",
            self.lookahead,
            self.position
        );
        let mut name = String::new();

        let start_pos = self.position;

        let first = self.next_char().unwrap(); // if we ended up here, the first char should be present
        name.push(first);

        loop {
            if let Some(c) = self.lookahead.0 {
                if self.is_identifier_continuation(c) {
                    name.push(self.next_char().unwrap());
                } else {
                    // Not fit for an ident, let's stop
                    break;
                }
            } else {
                // Nothing remaining in the iterator, so we're done
                break;
            }
        }

        let end_pos = self.position;

        // Check if the identifier is a reserved keyword
        let token = if let Some(keyword) = self.keywords.get(&name) {
            keyword.clone()
        } else {
            Token::Identifier { name }
        };

        Ok((start_pos, token, end_pos))
    }

    fn consume_number(&mut self) -> Spanned {
        trace!(
            "consume_number: lookahead={:?}, position={:?}",
            self.lookahead,
            self.position
        );
        let start_pos = self.position;

        let mut buf = String::new();
        let mut is_float = false;

        loop {
            // looping over the iterator until we find a char which isn't
            // a number or a dot.
            if let Some(c) = self.lookahead.0 {
                if c.is_numeric() {
                    buf.push(c);
                } else if c == '.' {
                    is_float = true;
                    buf.push(c);
                } else {
                    break; // Not a number, we are done
                }

                // We have looked at the current char, let's move to the next
                self.next_char().unwrap();
            } else {
                break; // Stream finished, let's break the loop
            }
        }

        let end_pos = self.position;

        let token = if is_float {
            Token::Float {
                value: f32::from_str(&buf).unwrap(),
            } //
        } else {
            Token::Integer {
                value: i32::from_str(&buf).unwrap(),
            }
        };

        (start_pos, token, end_pos)
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

    fn tokenize(source: &str) -> Vec<Token> {
        make_tokenizer(source)
            .map(|x| x.expect("no error in tokenize").1)
            .collect()
    }

    fn ident_token(s: &str) -> Token {
        Token::Identifier {
            name: s.to_string(),
        }
    }

    fn int_token(value: i32) -> Token {
        Token::Integer { value }
    }

    fn float_token(value: f32) -> Token {
        Token::Float { value }
    }

    fn char_token(value: char) -> Token {
        Token::Char { value }
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
    fn test_literal_number() {
        // Integer
        assert_eq!(tokenize("42"), vec![int_token(42), Token::Newline]);
        assert_eq!(tokenize("2"), vec![int_token(2), Token::Newline]);

        // Float
        assert_eq!(tokenize("42.99"), vec![float_token(42.99), Token::Newline]);
        assert_eq!(tokenize("2.0"), vec![float_token(2.0), Token::Newline]);
    }

    #[test]
    fn test_literal_char() {
        assert_eq!(
            make_tokenizer("'").collect::<Result<Vec<_>, _>>(),
            Err(LexicalError {
                error: LexicalErrorType::CharError,
                position: Position::new(0, 0)
            })
        );

        assert_eq!(
            make_tokenizer("'a").collect::<Result<Vec<_>, _>>(),
            Err(LexicalError {
                error: LexicalErrorType::CharError,
                position: Position::new(0, 1)
            })
        );

        assert_eq!(tokenize("'a'"), vec![char_token('a'), Token::Newline]);
    }

    #[test]
    fn test_invalid_indentation() {
        assert_eq!(
            make_tokenizer(" a").collect::<Result<Vec<_>, _>>(),
            Err(LexicalError {
                error: LexicalErrorType::IndentationError,
                position: Position::new(0, 1)
            })
        );

        assert_eq!(
            make_tokenizer("  \ta").collect::<Result<Vec<_>, _>>(),
            Err(LexicalError {
                error: LexicalErrorType::TabError,
                position: Position::new(0, 2)
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
    fn test_consume_identifier() {
        assert_eq!(
            tokenize("ident"),
            vec![ident_token("ident"), Token::Newline]
        );
    }

    #[test]
    fn test_skip_whitespaces_between_ident() {
        assert_eq!(
            tokenize("map f"),
            vec![ident_token("map"), ident_token("f"), Token::Newline]
        );
        assert_eq!(
            tokenize("map  f"),
            vec![ident_token("map"), ident_token("f"), Token::Newline]
        );
    }

    #[test]
    fn test_refuse_tab_in_expression() {
        assert_eq!(
            make_tokenizer("map \ta").collect::<Result<Vec<_>, _>>(),
            Err(LexicalError {
                error: LexicalErrorType::TabError,
                position: Position::new(0, 4)
            })
        );
    }

    #[test]
    fn test_simple_program() {
        enable_logs();
        let tokens = tokenize(indoc! {"
            module Main exposing(main)

            main : Int
            main = 42
        "});
        let expected: Vec<Token> = vec![
            Token::Module,
            ident_token("Main"),
            Token::Exposing,
            Token::LPar,
            ident_token("main"),
            Token::RPar,
            Token::Newline,
            ident_token("main"),
            Token::Colon,
            ident_token("Int"),
            Token::Newline,
            ident_token("main"),
            Token::Equal,
            Token::Integer { value: 42 },
            Token::Newline,
        ];

        assert_eq!(tokens, expected)
    }
}
