//! Module providing a transformation from a textual source code to a serie of tokens.
//!
//! Directly inspired by the great work on the RustPython team
//! https://github.com/RustPython/RustPython/blob/master/parser/src/lexer.rs

use crate::compiler::position::{spanned, BytePos, Position, Spanned};
use log::trace; // Location in RustPython
use std::collections::HashMap;
use std::str::FromStr;
use unic_ucd_category::GeneralCategory;

/// Represents the different part which constitute our source code
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    UpperIdentifier(String),
    LowerIdentifier(String),
    Integer { value: i64 }, // web assembly support i/f 32/64
    Float { value: f64 },
    Char { value: char },
    True,
    False,
    Operator(String),
    // TODO String literal

    // Control character
    EndOfFile,

    // Symbols
    LPar,
    RPar,
    LBracket,
    RBracket,
    Comma,
    Arrow,
    Dot,
    DotDot,
    Underscore,
    Colon,
    Pipe,
    Equal,
    Minus,

    // Keywords
    Module,
    Exposing,
    Import,
    As,
    Infix,
    Type,
    Case,
    Of,
    If,
    Then,
    Else,
    Let,
    In,

    // Soft keywords
    Left,
    Right,
    Non,
    Javascript,

    // Layout
    OpenBlock,
    CloseBlock,
}

/// An utility function to build a map of reserved
/// keywords with their associated tokens.
fn get_keywords() -> HashMap<String, Token> {
    let mut m = HashMap::new();

    m.insert("module".to_string(), Token::Module);
    m.insert("exposing".to_string(), Token::Exposing);
    m.insert("import".to_string(), Token::Import);
    m.insert("as".to_string(), Token::As);
    m.insert("infix".to_string(), Token::Infix);
    m.insert("type".to_string(), Token::Type);
    m.insert("case".to_string(), Token::Case);
    m.insert("of".to_string(), Token::Of);
    m.insert("if".to_string(), Token::If);
    m.insert("then".to_string(), Token::Then);
    m.insert("else".to_string(), Token::Else);
    m.insert("let".to_string(), Token::Let);
    m.insert("in".to_string(), Token::In);
    m.insert("true".to_string(), Token::True);
    m.insert("false".to_string(), Token::False);

    // soft keywords
    m.insert("left".to_string(), Token::Left);
    m.insert("right".to_string(), Token::Right);
    m.insert("non".to_string(), Token::Non);
    m.insert("javascript".to_string(), Token::Javascript);

    m
}

fn is_operator_char(c: char) -> bool {
    match c {
        '!' | '#' | '$' | '%' | '&' | '*' | '+' | '-' | '.' | '/' | '<' | '=' | '>' | '?' | '@'
        | '\\' | '^' | '|' | '~' | ':' => true,
        _ => false,
    }
}

/// Represents an error during tokenization.
#[derive(Debug, PartialEq, Clone)]
pub struct TokenizerError {
    pub error: Spanned<BytePos, TokenizerErrorType>,
}

impl TokenizerError {
    fn new(start: BytePos, end: BytePos, tpe: TokenizerErrorType) -> TokenizerError {
        TokenizerError {
            error: spanned(start, end, tpe),
        }
    }
}

/// The type of error refered in `TokenizerError`
#[derive(Debug, PartialEq, Clone)]
pub enum TokenizerErrorType {
    CharNotClosedError(Option<char>),
    // TODO If can be implemented, lookahead and try to find a closing single quote
    // This will require to implement backtracking in the tokenizer though
    //CharTooBigError,
    StringError,  // TODO String literal
    UnicodeError, // TODO String literal
    IndentationError,
    TabError,
    UnrecognizedToken { tok: char },
}

/// Represent a standard `Result` scoped to a `TokenizerError`
pub type Result<T> = std::result::Result<T, TokenizerError>;

/// Take a source code and return an iterator of [`Spanned`](type.Spanned.html)
///
/// This is the access point of this module, and the only way to create the underlying
/// `Tokenizer` (although it isn't exposed to public consumption)
// TODO return super::error::Result instead of the module one
pub fn make_tokenizer<'a>(
    source: &'a str,
) -> impl Iterator<Item = Result<Spanned<Position, Token>>> + 'a {
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
    /// The source iterator
    chars: I,
    /// Indicates whether the iterator is pointing to the first character of a line or not
    at_line_start: bool,
    /// Tokens we have parsed but not yet emitted
    processed_tokens: Vec<Spanned<Position, Token>>,
    /// The current position in the source code
    position: Position,
    /// A preview of the current character (and the two following).
    ///
    /// This is especially helpful to let us find symbols containing
    /// more than one character.
    lookahead: (Option<char>, Option<char>, Option<char>),
    /// A dictionary of all keywords with their tokens representation.
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
            position: Position::new(0, 1, 1),
            lookahead: (None, None, None),
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
            self.at_line_start = true;
            self.position.new_line();
        } else if let Some(c) = current {
            self.position.increment_by(c.len_utf8());
        } else {
            // EOF, we increment by one (even though I'm not sure we use
            // this position)
            self.position.increment();
        }

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

    /// Skip over the next character and return a `Spanned<Position, Token>` with the
    /// skipped character position.
    fn skip_char_as(&mut self, token: Token) -> Spanned<Position, Token> {
        let start = self.position;
        self.next_char().unwrap(); // skip over the char
        let end = self.position;

        spanned(start, end, token)
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

    fn process_next_tokens(&mut self) -> Result<Spanned<Position, Token>> {
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

    /// Consume the characters until we reach a non-indentation
    /// and/or non-comment character, leaving the iterator to
    /// point at it.
    fn handle_indentation(&mut self) -> Result<usize> {
        let mut spaces = 0;
        let start_position = self.position;

        trace!("consume_indentation()");

        loop {
            match self.lookahead.0 {
                Some(' ') => {
                    spaces += 1;
                    self.next_char();
                }
                Some('\t') => {
                    // Zelkova forbid the use of tabs for indentation
                    return Err(TokenizerError::new(
                        self.position.absolute,
                        self.position.absolute + '\t'.len_utf8() as u32,
                        TokenizerErrorType::TabError,
                    ));
                }
                Some('-') => {
                    // Possible comment
                    if let Some('-') = self.lookahead.1 {
                        // This is a comment, let's skip it (and start counting again)
                        self.consume_comment()?;
                        spaces = 0;
                    } else {
                        break;
                    }
                }
                Some('{') => {
                    if let Some('-') = self.lookahead.1 {
                        // This is a comment, let's skip it (and start counting again)
                        self.consume_comment()?;
                        spaces = 0;
                    } else {
                        break;
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
            return Err(TokenizerError::new(
                start_position.absolute,
                self.position.absolute,
                TokenizerErrorType::IndentationError,
            ));
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
            panic!(
                "Called Tokenizer.consume_comment on non-comment symbol ({:?})",
                self.position
            );
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
                '[' => {
                    let spanned = self.skip_char_as(Token::LBracket);
                    self.processed_tokens.push(spanned);
                }
                ']' => {
                    let spanned = self.skip_char_as(Token::RBracket);
                    self.processed_tokens.push(spanned);
                }
                ',' => {
                    let spanned = self.skip_char_as(Token::Comma);
                    self.processed_tokens.push(spanned);
                }
                '_' => {
                    let spanned = self.skip_char_as(Token::Underscore);
                    self.processed_tokens.push(spanned);
                }
                '-' => {
                    // TODO Add support for negative number
                    let spanned = match self.lookahead.1 {
                        Some('-') => {
                            self.consume_comment()?;
                            None
                        }
                        _ => Some(self.consume_operator()),
                    };

                    if let Some(spanned) = spanned {
                        self.processed_tokens.push(spanned);
                    }
                }
                '\'' => {
                    match (self.lookahead.1, self.lookahead.2) {
                        (Some(value), Some('\'')) => {
                            let start_pos = self.position;
                            // skip over the opening quote, char and closing quote
                            self.next_char().unwrap();
                            self.next_char().unwrap();
                            self.next_char().unwrap();
                            let end_pos = self.position;

                            self.processed_tokens.push(spanned(
                                start_pos,
                                end_pos,
                                Token::Char { value },
                            ));
                        }

                        (Some(v), Some(closing)) => {
                            // error: opened quote with char but no closing quote
                            // We haven't moved the cursor yet, but we know where
                            // the error is, so we build the position manually
                            let end = self.position.absolute
                                + (v.len_utf8() as u32)
                                + (closing.len_utf8() as u32);
                            return Err(TokenizerError::new(
                                self.position.absolute,
                                end,
                                TokenizerErrorType::CharNotClosedError(Some(closing)),
                            ));
                        }

                        (v, _) => {
                            // error: opened single quote without character following

                            let char_width = v.map_or_else(|| 0, |v| v.len_utf8()) as u32;
                            return Err(TokenizerError::new(
                                self.position.absolute,
                                self.position.absolute + char_width + 1, // +1 for the opening quote
                                TokenizerErrorType::CharNotClosedError(None),
                            ));
                        }
                    }
                }
                ' ' | '\n' => {
                    self.next_char().unwrap(); // let's skip over whitespace and new lines
                }
                '\t' => {
                    return Err(TokenizerError::new(
                        self.position.absolute,
                        self.position.absolute + '\t'.len_utf8() as u32,
                        TokenizerErrorType::TabError,
                    ))
                }
                c if self.is_identifier_start(c) => {
                    let identifier = self.consume_identifier()?;
                    self.processed_tokens.push(identifier);
                }
                c if is_operator_char(c) => {
                    let operator = self.consume_operator();
                    self.processed_tokens.push(operator);
                }
                _ => {
                    let start_position = self.position.absolute;
                    let tok = self.next_char().expect("lookahead.0 should be present");
                    return Err(TokenizerError::new(
                        start_position,
                        self.position.absolute,
                        TokenizerErrorType::UnrecognizedToken { tok },
                    ));
                }
            }

            Ok(())
        } else {
            // Nothing else to pull, let's wrap it up
            self.processed_tokens
                .push(spanned(self.position, self.position, Token::EndOfFile));

            Ok(())
        }
    }

    /// Consume the iterator until we reach a char which isn't suited for an identifier.
    ///
    /// This can return two types of tokens:
    /// - a keyword token (e.g. `Token::Module`) if the consumed identifier is part of
    ///   the reserved list of keywords. See [`get_keywords()`](#function.get_keywords)
    ///   for a list of keywords.
    /// - a `Token::*Identifier` if the identifier isn't a keyword, this encompass basically
    ///   everything which isn't a symbol, literal or keyword in the language.
    fn consume_identifier(&mut self) -> Result<Spanned<Position, Token>> {
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
            let first = name.chars().next().unwrap();
            if first.is_uppercase() {
                Token::UpperIdentifier(name)
            } else {
                Token::LowerIdentifier(name)
            }
        };

        Ok(spanned(start_pos, end_pos, token))
    }

    fn consume_operator(&mut self) -> Spanned<Position, Token> {
        let mut buf = String::new();
        let start_pos = self.position;

        loop {
            if let Some(c) = self.lookahead.0 {
                if is_operator_char(c) {
                    buf.push(c);

                    self.next_char().unwrap();
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        let end_pos = self.position;

        let tok = match buf.as_ref() {
            "." => Token::Dot,
            ".." => Token::DotDot,
            "|" => Token::Pipe,
            "=" => Token::Equal,
            ":" => Token::Colon,
            "->" => Token::Arrow,
            "-" => Token::Minus,
            _ => Token::Operator(buf),
        };

        spanned(start_pos, end_pos, tok)
    }

    fn consume_number(&mut self) -> Spanned<Position, Token> {
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
                value: f64::from_str(&buf).unwrap(),
            } //
        } else {
            Token::Integer {
                value: i64::from_str(&buf).unwrap(),
            }
        };

        spanned(start_pos, end_pos, token)
    }
}

impl<T> Iterator for Tokenizer<T>
where
    T: Iterator<Item = char>,
{
    type Item = Result<Spanned<Position, Token>>;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.process_next_tokens();

        trace!("Tokenizer.next. token={:?}", token);

        match token {
            Ok(Spanned {
                value: Token::EndOfFile,
                ..
            }) => None,
            r => Some(r),
        }
    }
}

#[cfg(test)]
mod tests {

    use super::{
        make_tokenizer, spanned, NewlineCollapser, Position, Token, TokenizerError,
        TokenizerErrorType,
    };
    use crate::compiler::position::BytePos;
    use indoc::indoc;

    // utilities

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
            .map(|x| x.expect("no error in tokenize").value)
            .collect()
    }

    fn ident_token(s: &str) -> Token {
        let first = s.chars().next().unwrap();
        if first.is_uppercase() {
            Token::UpperIdentifier(s.to_string())
        } else {
            Token::LowerIdentifier(s.to_string())
        }
    }

    fn int_token(value: i64) -> Token {
        Token::Integer { value }
    }

    fn float_token(value: f64) -> Token {
        Token::Float { value }
    }

    fn char_token(value: char) -> Token {
        Token::Char { value }
    }

    // actual tests

    #[test]
    fn newline_collapser() {
        let src = "ab\ncd\r\ne";
        let result: Vec<_> = NewlineCollapser::new(src.chars()).collect();

        assert_eq!(result, vec!['a', 'b', '\n', 'c', 'd', '\n', 'e']);
    }

    #[test]
    fn empty_programs() {
        assert_eq!(make_tokenizer("").collect::<Vec<_>>(), vec![]);
        assert_eq!(make_tokenizer("    ").collect::<Vec<_>>(), vec![]);
        assert_eq!(make_tokenizer("  \n  ").collect::<Vec<_>>(), vec![]);
    }

    #[test]
    fn literal_number() {
        // Integer
        assert_eq!(tokenize("42"), vec![int_token(42)]);
        assert_eq!(tokenize("2"), vec![int_token(2)]);

        // Float
        assert_eq!(tokenize("42.99"), vec![float_token(42.99)]);
        assert_eq!(tokenize("2.0"), vec![float_token(2.0)]);
    }

    #[test]
    fn literal_boolean() {
        assert_eq!(tokenize("true"), vec![Token::True]);
        assert_eq!(tokenize("false"), vec![Token::False]);
    }

    #[test]
    fn literal_char() {
        enable_logs();

        assert_eq!(
            make_tokenizer("'").collect::<Result<Vec<_>, _>>(),
            Err(TokenizerError::new(
                BytePos(0),
                BytePos(1),
                TokenizerErrorType::CharNotClosedError(None)
            ))
        );

        assert_eq!(
            make_tokenizer("'a").collect::<Result<Vec<_>, _>>(),
            Err(TokenizerError::new(
                BytePos(0),
                BytePos(2),
                TokenizerErrorType::CharNotClosedError(None)
            ))
        );

        assert_eq!(
            make_tokenizer("'aa").collect::<Result<Vec<_>, _>>(),
            Err(TokenizerError::new(
                BytePos(0),
                BytePos(2),
                TokenizerErrorType::CharNotClosedError(Some('a'))
            ))
        );

        assert_eq!(tokenize("'a'"), vec![char_token('a')]);
        assert_eq!(tokenize("'🙂'"), vec![char_token('🙂')]);
    }

    #[test]
    fn symbols() {
        let op = |s: &str| Token::Operator(s.to_owned());
        assert_eq!(
            tokenize("(),[]._ .. -> = + - / * == < <= >= > && || |> <| |"),
            vec![
                Token::LPar,
                Token::RPar,
                Token::Comma,
                Token::LBracket,
                Token::RBracket,
                Token::Dot,
                Token::Underscore,
                Token::DotDot,
                Token::Arrow,
                Token::Equal,
                op("+"),
                Token::Minus,
                op("/"),
                op("*"),
                op("=="),
                op("<"),
                op("<="),
                op(">="),
                op(">"),
                op("&&"),
                op("||"),
                op("|>"),
                op("<|"),
                Token::Pipe
            ]
        );
    }

    #[test]
    fn invalid_indentation() {
        assert_eq!(
            make_tokenizer(" a").collect::<Result<Vec<_>, _>>(),
            Err(TokenizerError::new(
                BytePos(0),
                BytePos(1),
                TokenizerErrorType::IndentationError
            ))
        );

        assert_eq!(
            make_tokenizer("  \ta").collect::<Result<Vec<_>, _>>(),
            Err(TokenizerError::new(
                BytePos(2),
                BytePos(3),
                TokenizerErrorType::TabError
            ))
        );
    }

    #[test]
    fn comments() {
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
    fn consume_identifier() {
        assert_eq!(tokenize("ident"), vec![ident_token("ident")]);
    }

    #[test]
    fn large_utf8_glyphs() {
        let spans: Vec<_> = make_tokenizer(indoc! {"
            -- 1.602e−19
            ident
        "})
        .map(|x| x.expect("must be Ok"))
        .collect();

        assert_eq!(
            spans,
            vec![spanned(
                Position::new(15, 1, 2),
                Position::new(20, 6, 2),
                ident_token("ident"),
            )]
        );
    }

    #[test]
    fn skip_whitespaces_between_ident() {
        assert_eq!(
            tokenize("map f"),
            vec![ident_token("map"), ident_token("f")]
        );
        assert_eq!(
            tokenize("map  f"),
            vec![ident_token("map"), ident_token("f")]
        );
    }

    #[test]
    fn refuse_tab_in_expression() {
        assert_eq!(
            make_tokenizer("map \ta").collect::<Result<Vec<_>, _>>(),
            Err(TokenizerError::new(
                BytePos(4),
                BytePos(5),
                TokenizerErrorType::TabError
            ))
        );
    }

    #[test]
    fn simple_program() {
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
            ident_token("main"),
            Token::Colon,
            ident_token("Int"),
            ident_token("main"),
            Token::Equal,
            Token::Integer { value: 42 },
        ];

        assert_eq!(tokens, expected)
    }
}
