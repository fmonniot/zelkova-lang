//! Module providing a transformation from a textual source code to a serie of tokens.
//!
//! Directly inspired by the great work on the RustPython team
//! <https://github.com/RustPython/RustPython/blob/master/parser/src/lexer.rs>

use crate::position::{spanned, BytePos, Position, Spanned};
use log::trace; // Location in RustPython
use std::str::FromStr;
use unic_ucd_category::GeneralCategory;

/// Represents the different part which constitute our source code
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    UpperIdentifier(String),
    LowerIdentifier(String),
    Integer {
        value: i64,
    }, // web assembly support i/f 32/64
    Float {
        value: f64,
    },
    Char {
        value: char,
    },
    /// A single-line string literal, `"hello"`, holding its value with every escape
    /// sequence already replaced by the character it names.
    String {
        value: String,
    },
    Operator(String),

    // Control character
    EndOfFile,

    // Symbols
    LPar,
    RPar,
    LBracket,
    RBracket,
    /// `{`, which opens a record: a record type, a record expression, an update and a
    /// record pattern. A `{` written against a `-` after it is not this token but the
    /// opening of a block comment.
    LBrace,
    /// `}`, which closes a record. An expression can end with one, so a `.` written against
    /// it is an access of the record.
    RBrace,
    Comma,
    Arrow,
    /// `=>`, which separates a signature's constraint context from its type
    /// (`Comparable a => a -> a -> a`). A symbol of the language rather than an
    /// operator name, so it cannot be declared with `infix` or exposed as `(=>)`.
    FatArrow,
    /// A `.` written against an operand on its left and against the character after it:
    /// the `.` of `Dict.get` and of the field access `r.name`.
    Dot,
    /// A `.` that is not a [`Dot`](Token::Dot) and is written against the start of a
    /// lowercase name after it: the `.` of an accessor, as in `f .name`, `(.name)` and
    /// `Dict .get`. Where the grammar has no accessor to read — in a type, a pattern, an
    /// `import` — the parser rejects it as it rejects a [`SpacedDot`](Token::SpacedDot).
    AccessorDot,
    /// Any other `.`: one with whitespace, a comment, or the end of the source after it
    /// (`Dict . get`, `Dict. get`, `. name`), or one with neither a `Dot`'s operand on
    /// its left nor a lowercase name against its right (`Dict .Get`, `(.1)`). No
    /// production consumes it, so the parser rejects it where it stands, and
    /// `Error::SpacedDot` says why. The doc comment on the tokenizer's `consume_operator`
    /// has the reason this is a token of its own.
    SpacedDot,
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
    //
    // Each is a keyword in exactly one position and an ordinary lowercase
    // identifier everywhere else. The lexer does not know the position, so it
    // always produces the distinct token and the grammar re-admits it as a name
    // wherever the keyword reading does not apply — see `VarIdent` in
    // `grammar.lalrpop`, which is the other half of every word listed here.
    Left,
    Right,
    Non,
    Foreign,
    Unsafe,

    // Layout
    OpenBlock,
    CloseBlock,
}

impl Token {
    /// Whether an expression can end with this token, which is to say whether a `.`
    /// written straight after it can be the `.` of a field access or a qualified name.
    ///
    /// Identifiers (the soft keywords are identifiers where an expression reads them,
    /// see `VarIdent` in `grammar.lalrpop`), literals of every kind, `)`, `]` and `}`:
    /// the `}` of a record ends it, and `{ a = 1 }.a` reads a field of that record. Every
    /// other token — an operator, `(`, `[`, `{`, `,`, a keyword, layout — leaves the next
    /// expression still to begin, so a `.` after it opens one: `{.name}` has a `SpacedDot`
    /// after its `{`.
    ///
    /// The match has no wildcard arm on purpose: a new token has to be placed here.
    fn ends_an_operand(&self) -> bool {
        match self {
            Token::UpperIdentifier(_)
            | Token::LowerIdentifier(_)
            | Token::Left
            | Token::Right
            | Token::Non
            | Token::Foreign
            | Token::Unsafe
            | Token::Integer { .. }
            | Token::Float { .. }
            | Token::Char { .. }
            | Token::String { .. }
            | Token::RPar
            | Token::RBracket
            | Token::RBrace => true,

            Token::Operator(_)
            | Token::EndOfFile
            | Token::LPar
            | Token::LBracket
            | Token::LBrace
            | Token::Comma
            | Token::Arrow
            | Token::FatArrow
            | Token::Dot
            | Token::AccessorDot
            | Token::SpacedDot
            | Token::DotDot
            | Token::Underscore
            | Token::Colon
            | Token::Pipe
            | Token::Equal
            | Token::Minus
            | Token::Module
            | Token::Exposing
            | Token::Import
            | Token::As
            | Token::Infix
            | Token::Type
            | Token::Case
            | Token::Of
            | Token::If
            | Token::Then
            | Token::Else
            | Token::Let
            | Token::In
            | Token::OpenBlock
            | Token::CloseBlock => false,
        }
    }
}

/// Look up a reserved keyword by its textual representation.
///
/// Returns `None` when `s` isn't one of the fixed set of reserved words, in
/// which case the caller should treat it as an ordinary identifier.
fn keyword(s: &str) -> Option<Token> {
    match s {
        "module" => Some(Token::Module),
        "exposing" => Some(Token::Exposing),
        "import" => Some(Token::Import),
        "as" => Some(Token::As),
        "infix" => Some(Token::Infix),
        "type" => Some(Token::Type),
        "case" => Some(Token::Case),
        "of" => Some(Token::Of),
        "if" => Some(Token::If),
        "then" => Some(Token::Then),
        "else" => Some(Token::Else),
        "let" => Some(Token::Let),
        "in" => Some(Token::In),

        // soft keywords
        "left" => Some(Token::Left),
        "right" => Some(Token::Right),
        "non" => Some(Token::Non),
        "foreign" => Some(Token::Foreign),
        "unsafe" => Some(Token::Unsafe),

        _ => None,
    }
}

fn is_operator_char(c: char) -> bool {
    matches!(
        c,
        '!' | '#'
            | '$'
            | '%'
            | '&'
            | '*'
            | '+'
            | '-'
            | '.'
            | '/'
            | '<'
            | '='
            | '>'
            | '?'
            | '@'
            | '\\'
            | '^'
            | '|'
            | '~'
            | ':'
    )
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
    /// A string literal (`consume_string`) reaches a line ending or the end of the file
    /// before its closing `"`. A single-line string may not contain an unescaped line
    /// ending, so the literal is unclosed whichever of the two it met. Spanned from the
    /// opening quote to where the line ended.
    StringNotClosedError,
    /// A `\` inside a string literal (`consume_escape`) followed by a character that
    /// begins none of the escape sequences. Spanned over the backslash and that
    /// character.
    InvalidEscape,
    /// A `\u` escape (`consume_escape`) that is not `{`, one to six hexadecimal digits
    /// and `}`, or whose digits name no Unicode scalar value — a surrogate, or a number
    /// past `10FFFF`. Spanned from the backslash to where reading the escape stopped.
    UnicodeError,
    IndentationError,
    TabError,
    UnrecognizedToken {
        tok: char,
    },
    /// A run of ASCII digits parsed as an integer literal (`consume_number`) does not
    /// fit in the `i64` `Token::Integer` carries it in (`BUG-12`, which made this an
    /// error rather than a panic).
    ///
    /// `i64` is not a bound `docs/spec/` states. *Integers* guarantees `-2^31 .. 2^31
    /// - 1` on every target and leaves the rest to the compilation target, and
    /// *Numbers* in `evaluation-semantics.md` says `Int` is 32-bit everywhere; the two
    /// disagree, and `SPEC-28` is where that is settled and this bound then revisited.
    IntegerOverflow,
    /// A numeric literal (`consume_number`) contains a second `.` — `1.2.3` — which no
    /// float grammar accepts. Raised as soon as the second point is seen, rather than
    /// handed to `f64::from_str` on a buffer no error message could describe precisely
    /// (`BUG-12`).
    MultipleDecimalPoints,
    /// A numeric literal (`consume_number`) continues with a Unicode numeric character
    /// that is not an ASCII digit — an Arabic-Indic digit such as `١`, say. `char::is_numeric`
    /// is true for such characters, so without this check they get folded into the
    /// literal's buffer and `i64`/`f64::from_str` rejects it as invalid rather than out
    /// of range (`BUG-12`).
    NonAsciiDigit {
        digit: char,
    },
    /// A numeric literal (`consume_number`) that neither `i64::from_str` nor
    /// `f64::from_str` can read, for a reason the two variants above do not already
    /// name. No input reaches it today: the accumulation loop puts only ASCII digits
    /// and at most one `.` into the buffer, and every such buffer parses. It exists
    /// so that a loop which stops upholding that has a diagnostic to raise, rather
    /// than a panic or a fabricated value (`BUG-12`).
    MalformedNumber,
    /// End of file reached while a block comment (`consume_comment`'s `{-` branch) is
    /// still open — a stray `{-` with no matching `-}` would otherwise comment out the
    /// rest of the file with no diagnostic at all (`BUG-13`). Spanned from the `{-` that
    /// opened the outermost unclosed comment, since that is the one the author has to go
    /// fix; a comment nested inside it that never closed is not separately reported.
    UnclosedBlockComment,
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
        if self.char_curr == Some('\r') && self.char_next == Some('\n') {
            // We have a Windows-like new line, let's shift the iterator by one
            self.shift();
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
    /// Where the last token handed out by `process_next_tokens` ended, when that token
    /// [can end an operand](Token::ends_an_operand); `None` before the first token and
    /// after any other. A `.` whose start is this byte is written against an operand to
    /// its left; whitespace or a comment in between puts the two apart.
    previous_operand_end: Option<BytePos>,
    /// A preview of the current character (and the two following).
    ///
    /// This is especially helpful to let us find symbols containing
    /// more than one character.
    lookahead: (Option<char>, Option<char>, Option<char>),
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
            previous_operand_end: None,
            lookahead: (None, None, None),
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

        let token = self.processed_tokens.remove(0);
        self.previous_operand_end = token
            .value
            .ends_an_operand()
            .then_some(token.span.end.absolute);

        Ok(token)
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
                    // Zelkova forbid the use of tabs for indentation. The span is read
                    // from `self.position` before advancing, matching every other arm
                    // in this loop (and `IndentationError` below), because `next_char`
                    // moves `self.position` as a side effect.
                    let start = self.position.absolute;
                    self.next_char();
                    let end = self.position.absolute;

                    // We have consumed the tab, so the loop would otherwise resume
                    // looking for more indentation; clear the flag ourselves since
                    // this arm returns instead of reaching the "first real character"
                    // arm that normally clears it (BUG-5: without this, the tab is
                    // never consumed, `at_line_start` stays true, and the tokenizer
                    // re-enters this same arm on every subsequent poll, forever).
                    self.at_line_start = false;

                    return Err(TokenizerError::new(
                        start,
                        end,
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
                        // A lone `-` is not a comment opener, so it is the line's first
                        // real character — the same conclusion the `_` arm below reaches
                        // for every other character. Clear the flag here too, or the next
                        // poll re-enters this function believing it is still scanning
                        // leading whitespace and measures the run of spaces *after* this
                        // token (e.g. between `->` and what follows it) against the
                        // two-space rule instead (BUG-19).
                        self.at_line_start = false;
                        break;
                    }
                }
                Some('{') => {
                    if let Some('-') = self.lookahead.1 {
                        // A block comment may appear anywhere a space may (SPEC-2), but
                        // it is not itself whitespace of a known width. If it carries us
                        // onto a later line, or ends the line it was on, whatever spaces
                        // come next really are a fresh line's leading indentation, so we
                        // restart the count exactly as for a line comment. But if more
                        // content follows the closing `-}` on the *same* line, that
                        // content is this line's first real token, and `spaces` already
                        // holds its indentation from before the comment — resetting it
                        // here would measure only the accidental run of spaces after the
                        // comment instead, misfiring the even-indentation check below on
                        // input the language accepts (BUG-13).
                        let line_before = self.position.line;
                        self.consume_comment()?;

                        if self.position.line != line_before
                            || matches!(self.lookahead.0, Some('\n') | None)
                        {
                            spaces = 0;
                        } else {
                            self.at_line_start = false;
                            break;
                        }
                    } else {
                        // A lone `{` not opening a `{-` comment is not a token the
                        // tokenizer knows yet (BUG-13), so this arm has no observable
                        // effect today. But it reaches the same "first real character"
                        // conclusion as the `-` arm above, so it clears the flag on the
                        // same reasoning (BUG-19) rather than leaving it to surface later.
                        self.at_line_start = false;
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
            // A block comment ends exactly at its matching `-}`; whatever follows on
            // that line is ordinary source, so unlike the line-comment branch above we
            // must not call `skip_end_of_line` (BUG-13). Block comments also nest: `depth`
            // counts how many `{-` are currently open, incremented on each `{-` and
            // decremented on each `-}`, and the loop only stops once it reaches zero. We
            // are entered with the outermost `{-` about to be read, so `depth` starts at
            // one rather than zero.
            trace!("Start skipping multi line comment {:?}", self.position);

            let start = self.position;
            self.next_char(); // the opening `{`
            self.next_char(); // the opening `-`
            let mut depth: usize = 1;

            loop {
                match (self.lookahead.0, self.lookahead.1) {
                    (Some('{'), Some('-')) => {
                        depth += 1;
                        self.next_char();
                        self.next_char();
                    }
                    (Some('-'), Some('}')) => {
                        depth -= 1;
                        self.next_char();
                        self.next_char();
                        if depth == 0 {
                            break;
                        }
                    }
                    (Some(_), _) => {
                        self.next_char();
                    }
                    (None, _) => {
                        // End of file reached with `depth` comments still open. The
                        // outermost one — spanned from `start`, captured before we
                        // consumed anything — is the one the author has to go fix, so
                        // that is where the error points rather than at EOF or at
                        // whichever nested `{-` happened to be innermost.
                        return Err(TokenizerError::new(
                            start.absolute,
                            self.position.absolute,
                            TokenizerErrorType::UnclosedBlockComment,
                        ));
                    }
                }
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
                    let number = self.consume_number()?;
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
                // A block comment may appear anywhere a space may (SPEC-2), including
                // mid-expression, not only in a line's leading whitespace, so `f = {- a
                // note -} 1` is accepted (BUG-13). This arm has to stay ahead of the
                // `{` arm below: a `{` followed by `-` opens a comment, and read the
                // other way round it would be a brace and an operator.
                '{' if self.lookahead.1 == Some('-') => {
                    self.consume_comment()?;
                }
                '{' => {
                    let spanned = self.skip_char_as(Token::LBrace);
                    self.processed_tokens.push(spanned);
                }
                '}' => {
                    let spanned = self.skip_char_as(Token::RBrace);
                    self.processed_tokens.push(spanned);
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

                        // Both error arms below build their span from character widths
                        // while the cursor is still on the opening quote, then consume
                        // that quote — and only that quote — before returning, so the
                        // next poll resumes on the character after it instead of
                        // re-entering this arm on the same quote forever (BUG-28).
                        //
                        // Only the quote is consumed because it is the one character
                        // known to be wrong: what follows is tokenized as ordinary
                        // source (`'ab` yields this error, then the identifier `ab`), and
                        // no character this arm only peeked at — a `\n`, a quote that
                        // opens a real literal — is swallowed along with it.
                        // A quote that closes a malformed literal is not consumed either, so
                        // it is re-read as an opener: `''` yields a second error. The parser
                        // stops at the first, so a user does not see it.
                        //
                        // The span is not the consumed range. `error.rs`'s
                        // `CharNotClosedError(Some(_))` arm reads `span.start` as the
                        // opening quote and `span.end` as falling inside the character
                        // where the closing quote should have been.
                        (Some(v), Some(closing)) => {
                            // error: opened quote with char but no closing quote
                            let start = self.position.absolute;
                            let end = start + (v.len_utf8() as u32) + (closing.len_utf8() as u32);
                            self.next_char(); // the opening quote
                            return Err(TokenizerError::new(
                                start,
                                end,
                                TokenizerErrorType::CharNotClosedError(Some(closing)),
                            ));
                        }

                        (v, _) => {
                            // error: opened single quote without character following
                            let start = self.position.absolute;
                            let char_width = v.map_or_else(|| 0, |v| v.len_utf8()) as u32;
                            let end = start + char_width + 1; // +1 for the opening quote
                            self.next_char(); // the opening quote
                            return Err(TokenizerError::new(
                                start,
                                end,
                                TokenizerErrorType::CharNotClosedError(None),
                            ));
                        }
                    }
                }
                '"' => {
                    let string = self.consume_string()?;
                    self.processed_tokens.push(string);
                }
                ' ' | '\n' => {
                    self.next_char().unwrap(); // let's skip over whitespace and new lines
                }
                '\t' => {
                    // Mirrors the `Some('\t')` arm of `handle_indentation` (BUG-5): the span
                    // is read from `self.position` before advancing, and `next_char` consumes
                    // the tab so the next poll finds a different character instead of
                    // re-entering this same arm forever (BUG-11). Unlike that sibling arm,
                    // this one leaves `at_line_start` alone, because it is only ever reached
                    // with the flag already false. `handle_indentation` does not clear the
                    // flag on every exit — its `None` (EOF) case returns with it still set
                    // (every other `break`, including `Some('-')` and `Some('{')`'s since
                    // BUG-19, clears it first) — but that leaves the iterator on `None`
                    // rather than on a tab, so it cannot reach this arm. A tab reaches this
                    // arm only once the "first real character" arm has cleared the flag.
                    let start = self.position.absolute;
                    self.next_char();
                    let end = self.position.absolute;

                    return Err(TokenizerError::new(
                        start,
                        end,
                        TokenizerErrorType::TabError,
                    ));
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

    /// Consume a single-line string literal, from its opening `"` to its closing one.
    ///
    /// Every character between the quotes is taken as written, except a `\`, which
    /// begins an escape sequence ([`consume_escape`](#method.consume_escape)). A string
    /// may not contain an unescaped line ending, so meeting one — or the end of the file
    /// — before the closing quote is a `StringNotClosedError`; the line ending itself is
    /// left unconsumed, and the next poll resumes from it. A horizontal tab and a
    /// carriage return are rejected inside a string exactly as they are anywhere else
    /// outside a comment, each consumed so that the next poll moves past it.
    ///
    /// Only the single-line form is read. A `"""` opens the empty string `""` followed
    /// by a second literal, which the line ending then leaves unclosed.
    fn consume_string(&mut self) -> Result<Spanned<Position, Token>> {
        let start = self.position;
        self.next_char(); // the opening quote

        let mut value = String::new();

        loop {
            match self.lookahead.0 {
                Some('"') => {
                    self.next_char();
                    break;
                }
                Some('\\') => value.push(self.consume_escape(start)?),
                Some('\n') | None => {
                    return Err(TokenizerError::new(
                        start.absolute,
                        self.position.absolute,
                        TokenizerErrorType::StringNotClosedError,
                    ));
                }
                Some('\t') => {
                    let tab = self.position.absolute;
                    self.next_char();
                    return Err(TokenizerError::new(
                        tab,
                        self.position.absolute,
                        TokenizerErrorType::TabError,
                    ));
                }
                Some('\r') => {
                    let at = self.position.absolute;
                    self.next_char();
                    return Err(TokenizerError::new(
                        at,
                        self.position.absolute,
                        TokenizerErrorType::UnrecognizedToken { tok: '\r' },
                    ));
                }
                Some(c) => {
                    self.next_char();
                    value.push(c);
                }
            }
        }

        Ok(spanned(start, self.position, Token::String { value }))
    }

    /// Consume one escape sequence inside a string literal, the `\` included, and return
    /// the character it names.
    ///
    /// The sequences are `\n`, `\r`, `\t`, `\\`, `\'`, `\"` and `\u{H…}`, whose one to six
    /// hexadecimal digits name a Unicode scalar value. A backslash at the end of the line
    /// or of the file leaves the string unclosed, so that is reported against `string`,
    /// the opening quote, as `consume_string` would report it. Every other error has
    /// consumed at least the backslash, so the next poll never meets the same one.
    fn consume_escape(&mut self, string: Position) -> Result<char> {
        let start = self.position.absolute;
        self.next_char(); // the backslash

        let simple = match self.lookahead.0 {
            Some('n') => Some('\n'),
            Some('r') => Some('\r'),
            Some('t') => Some('\t'),
            Some('\\') => Some('\\'),
            Some('\'') => Some('\''),
            Some('"') => Some('"'),
            _ => None,
        };

        match (simple, self.lookahead.0) {
            (Some(c), _) => {
                self.next_char();
                Ok(c)
            }
            (None, Some('\n') | None) => Err(TokenizerError::new(
                string.absolute,
                self.position.absolute,
                TokenizerErrorType::StringNotClosedError,
            )),
            (None, Some('u')) => {
                self.next_char();
                self.consume_unicode_escape(start)
            }
            (None, Some(_)) => {
                self.next_char();
                Err(TokenizerError::new(
                    start,
                    self.position.absolute,
                    TokenizerErrorType::InvalidEscape,
                ))
            }
        }
    }

    /// Consume the `{H…}` of a `\u{H…}` escape whose `\u` began at `start`, and return
    /// the character its digits name.
    ///
    /// Reading stops at the first character that cannot continue the escape, which is
    /// left unconsumed; a `UnicodeError` then spans from `start` to that character.
    fn consume_unicode_escape(&mut self, start: BytePos) -> Result<char> {
        let error = |end: BytePos| {
            Err(TokenizerError::new(
                start,
                end,
                TokenizerErrorType::UnicodeError,
            ))
        };

        if self.lookahead.0 != Some('{') {
            return error(self.position.absolute);
        }
        self.next_char();

        let mut code: u32 = 0;
        let mut digits = 0;

        loop {
            match self.lookahead.0 {
                Some('}') => {
                    self.next_char();
                    break;
                }
                Some(c) if digits < 6 => match c.to_digit(16) {
                    Some(digit) => {
                        self.next_char();
                        code = code * 16 + digit;
                        digits += 1;
                    }
                    None => return error(self.position.absolute),
                },
                _ => return error(self.position.absolute),
            }
        }

        match char::from_u32(code) {
            Some(c) if digits > 0 => Ok(c),
            _ => error(self.position.absolute),
        }
    }

    /// Consume the iterator until we reach a char which isn't suited for an identifier.
    ///
    /// This can return two types of tokens:
    /// - a keyword token (e.g. `Token::Module`) if the consumed identifier is part of
    ///   the reserved list of keywords. See [`keyword()`](#function.keyword)
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

        while let Some(c) = self.lookahead.0 {
            if self.is_identifier_continuation(c) {
                name.push(self.next_char().unwrap());
            } else {
                // Not fit for an ident, let's stop
                break;
            }
        }

        let end_pos = self.position;

        // Check if the identifier is a reserved keyword
        let token = if let Some(tok) = keyword(&name) {
            tok
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

    /// Consume a run of operator characters and name the token it is.
    ///
    /// A lone `.` is where the whitespace rule of `docs/spec/records.md` (*Whitespace
    /// before a `.` decides which form it is*) is decided, and it is one of three tokens.
    ///
    /// - [`Token::Dot`] when it is written against an operand on its left — a token for
    ///   which [`Token::ends_an_operand`] holds, with nothing between — and against the
    ///   character after it: `Dict.get` qualifying `get`, and `r.name` reading a field.
    /// - [`Token::AccessorDot`] when it is not a `Dot` and the character after it starts
    ///   a lowercase name: the `.` of an accessor, in `f .name`, `Dict .get`, and in
    ///   `(.name)`, `[.name]` and `a,.name`, where it follows a token that cannot end an
    ///   operand and opens an expression however close it sits.
    /// - [`Token::SpacedDot`] otherwise: `Dict . get`, `Dict. get`, `. name`, `Dict
    ///   .Get`. The grammar has no production for it.
    ///
    /// This is decided here and not in the `QualVarIdent`/`QualTypeIdent` actions of
    /// `grammar.lalrpop`, for two reasons. Whitespace is only visible here: the grammar
    /// sees the same `up_ident "." up_ident` stream whichever way the source was spaced,
    /// so deciding it there means comparing the `@R` of one token with the `@L` of the
    /// next in an action that can only accept or fail, and leaves the grammar unable to
    /// tell the two spellings apart where it needs to. Records need exactly that: `f
    /// .name` is `f` applied to an accessor and `f.name` is an access, and a grammar
    /// that gets the same token for both can only be made to distinguish them by
    /// resolving a conflict in an action. With the spellings split into tokens, `Dot` is
    /// an access or a qualification, `AccessorDot` begins an accessor and is never
    /// either, and `SpacedDot` is none of the three.
    ///
    /// The accessor's own `.` is written against its label, and that half is decided
    /// here too, by the character after the `.`, for the same reason: `.name` and `.
    /// name` would otherwise reach the grammar as one token followed by a name, and
    /// rejecting the second would take a comparison of spans in the accessor's action.
    /// Such an action can only fail where it stands, after the parser has already taken
    /// the `.` as an accessor's, so `Widget . size` would be reported without knowing
    /// that a qualified name was what it interrupted, and `1 .` at the end of a line as
    /// the layout token after it. A label is a lowercase name, so a `.` against an
    /// uppercase one, a digit or a symbol is no accessor's and is `SpacedDot`: `Widget
    /// .Size` is rejected at its `.` as `Widget . Size` is.
    ///
    /// Adjacency is compared against where the previous token ended, not against the
    /// previous character, so a block comment between a name and its dot — `Dict{- -}.get`
    /// — separates them as whitespace does, and makes the dot an accessor's. The character
    /// after the dot is whitespace, the end of the source, or the opening of a block
    /// comment when it is not written against the dot.
    fn consume_operator(&mut self) -> Spanned<Position, Token> {
        let mut buf = String::new();
        let start_pos = self.position;

        while let Some(c) = self.lookahead.0 {
            if is_operator_char(c) {
                buf.push(c);

                self.next_char().unwrap();
            } else {
                break;
            }
        }

        let end_pos = self.position;

        let tok = match buf.as_ref() {
            "." => {
                let after_is_apart = match (self.lookahead.0, self.lookahead.1) {
                    (None, _) => true,
                    (Some('{'), Some('-')) => true,
                    (Some(c), _) => c.is_whitespace(),
                };

                // A name is lowercase exactly when `consume_identifier` would make it a
                // `LowerIdentifier` or a keyword: its first character starts an
                // identifier and is not uppercase.
                let before_a_lowercase_name = self
                    .lookahead
                    .0
                    .is_some_and(|c| self.is_identifier_start(c) && !c.is_uppercase());

                if self.previous_operand_end == Some(start_pos.absolute) && !after_is_apart {
                    Token::Dot
                } else if before_a_lowercase_name {
                    Token::AccessorDot
                } else {
                    Token::SpacedDot
                }
            }
            ".." => Token::DotDot,
            "|" => Token::Pipe,
            "=" => Token::Equal,
            ":" => Token::Colon,
            "->" => Token::Arrow,
            "=>" => Token::FatArrow,
            "-" => Token::Minus,
            _ => Token::Operator(buf),
        };

        spanned(start_pos, end_pos, tok)
    }

    /// Consume a numeric literal — a run of ASCII digits, optionally containing one
    /// `.` — and turn it into an `Integer` or `Float` token.
    ///
    /// The accumulation loop only ever puts ASCII digits and at most one `.` into
    /// `buf`. Rust's float grammar accepts every such string, trailing `.` included
    /// (see the "Known gap" on `1.` in `docs/spec/lexical-structure.md`), so
    /// `f64::from_str` cannot fail on it; `i64::from_str` still can, on sheer
    /// magnitude. Both `from_str` calls at the end nevertheless report their failure
    /// as an error: before this tightened the loop to `is_ascii_digit`,
    /// `char::is_numeric` let a second `.` or a non-ASCII numeric character (an
    /// Arabic-Indic digit, say) into `buf` and both calls panicked on it (`BUG-12`),
    /// and a buffer that reaches them malformed again is a defect in this loop —
    /// something to say out loud, not to substitute a value for.
    fn consume_number(&mut self) -> Result<Spanned<Position, Token>> {
        trace!(
            "consume_number: lookahead={:?}, position={:?}",
            self.lookahead,
            self.position
        );
        let start_pos = self.position;

        let mut buf = String::new();
        let mut is_float = false;

        while let Some(c) = self.lookahead.0 {
            // looping over the iterator until we find a char which isn't
            // an ASCII digit or a dot.
            if c.is_ascii_digit() {
                buf.push(c);
            } else if c == '.' {
                if is_float {
                    // A second decimal point: `1.2.3` is not a number under any
                    // reading, so we stop right here instead of accumulating
                    // further digits into a buffer that would only fail later
                    // with no way to explain why.
                    //
                    // The point is consumed before returning for two reasons. The
                    // span then covers `1.2.`, the whole literal read so far; and
                    // the character this error already blames is not handed back to
                    // `consume_char`, which would tokenize it as a stray `Dot` and
                    // hand the grammar a token the author never wrote. Leaving it
                    // does not hang — `.` is an operator character, so the main loop
                    // advances past it on its own — so this is not the never-advance
                    // shape of `BUG-4`, `BUG-5` and `BUG-11`.
                    self.next_char();
                    return Err(TokenizerError::new(
                        start_pos.absolute,
                        self.position.absolute,
                        TokenizerErrorType::MultipleDecimalPoints,
                    ));
                }
                is_float = true;
                buf.push(c);
            } else if c.is_numeric() {
                // A Unicode numeric character that isn't an ASCII digit: it reads,
                // to a human, like a continuation of the literal, but neither
                // `i64::from_str` nor `f64::from_str` will accept it.
                //
                // Consumed before returning, for the same two reasons as the second
                // `.` above: the span covers both characters of `1١`, and the
                // character this error blames is not then reported a second time by
                // `consume_char`'s fallback arm as an unrelated `UnrecognizedToken`.
                // That arm advances, so leaving it here would not hang either.
                let digit = c;
                self.next_char();
                return Err(TokenizerError::new(
                    start_pos.absolute,
                    self.position.absolute,
                    TokenizerErrorType::NonAsciiDigit { digit },
                ));
            } else {
                break; // Not a number, we are done
            }

            // We have looked at the current char, let's move to the next
            self.next_char().unwrap();
        }

        let end_pos = self.position;

        let token = if is_float {
            match f64::from_str(&buf) {
                Ok(value) => Token::Float { value },
                Err(_) => {
                    // Unreachable while the loop above holds: `buf` is ASCII digits
                    // and at most one `.`, which Rust's float grammar always
                    // accepts, and a magnitude no `f64` can hold parses as
                    // `Ok(f64::INFINITY)` rather than failing. So reaching here
                    // means the loop let something else through, and the only honest
                    // thing to report is that this is not a number — substituting a
                    // value would accept `1.2.3` as infinity.
                    return Err(TokenizerError::new(
                        start_pos.absolute,
                        end_pos.absolute,
                        TokenizerErrorType::MalformedNumber,
                    ));
                }
            }
        } else {
            match i64::from_str(&buf) {
                Ok(value) => Token::Integer { value },
                Err(_) => {
                    // The only way `i64::from_str` can still fail on a buffer of
                    // pure ASCII digits is magnitude: more digits than an `i64` can
                    // hold. That is the carrier's bound rather than the language's —
                    // see `TokenizerErrorType::IntegerOverflow` and `SPEC-28`.
                    return Err(TokenizerError::new(
                        start_pos.absolute,
                        end_pos.absolute,
                        TokenizerErrorType::IntegerOverflow,
                    ));
                }
            }
        };

        Ok(spanned(start_pos, end_pos, token))
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
        make_tokenizer, spanned, NewlineCollapser, Position, Result as TokenizerResult, Spanned,
        Token, TokenizerError, TokenizerErrorType,
    };
    use crate::position::BytePos;
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

    /// Poll `source`'s tokenizer at most `cap` times — stopping early if the iterator
    /// terminates — and return everything it yielded, having first checked the two things
    /// a non-termination regression is about: that the iterator *stopped* inside the cap,
    /// and that it never handed back the same error twice in a row.
    ///
    /// This is what the `*_does_not_hang` tests below use instead of
    /// `collect::<Result<Vec<_>, _>>()`, which short-circuits on the first `Err` and so can
    /// never observe a repeat — exactly why `invalid_indentation` and
    /// `refuse_tab_in_expression`, which do collect, missed `BUG-5` and `BUG-11`.
    ///
    /// Both assertions live here rather than in the callers because a caller that forgot
    /// either one would still pass while the defect was present.
    fn drain_capped(source: &str, cap: usize) -> Vec<TokenizerResult<Spanned<Position, Token>>> {
        let mut iter = make_tokenizer(source);
        let mut items = Vec::with_capacity(cap);

        for _ in 0..cap {
            match iter.next() {
                Some(item) => items.push(item),
                None => break,
            }
        }

        assert!(
            items.len() < cap,
            "the iterator did not terminate within {} items: {:?}",
            cap,
            items
        );

        for pair in items.windows(2) {
            if let (Err(e1), Err(e2)) = (&pair[0], &pair[1]) {
                assert_ne!(
                    e1, e2,
                    "the identical error was observed twice in a row: {:?}",
                    e1
                );
            }
        }

        items
    }

    /// The errors among a [`drain_capped`] result, in order.
    fn errors_of(items: &[TokenizerResult<Spanned<Position, Token>>]) -> Vec<&TokenizerError> {
        items
            .iter()
            .filter_map(|item| item.as_ref().err())
            .collect()
    }

    /// The tokens among a [`drain_capped`] result, in order.
    fn tokens_of(items: &[TokenizerResult<Spanned<Position, Token>>]) -> Vec<&Token> {
        items
            .iter()
            .filter_map(|item| item.as_ref().ok())
            .map(|s| &s.value)
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

    /// An integer literal too large for an `i64` is a `TokenizerErrorType::IntegerOverflow`
    /// rather than a `ParseIntError { kind: PosOverflow }` panic (`BUG-12`). The span covers
    /// the whole 20-digit literal.
    ///
    /// Verified to fail (panic, not a red assertion) by reverting `consume_number` to
    /// `i64::from_str(&buf).unwrap()`.
    #[test]
    fn integer_literal_too_large_is_an_error() {
        assert_eq!(
            make_tokenizer("99999999999999999999").collect::<Result<Vec<_>, _>>(),
            Err(TokenizerError::new(
                BytePos(0),
                BytePos(20),
                TokenizerErrorType::IntegerOverflow
            ))
        );
    }

    /// A numeric literal with a second `.` is a `TokenizerErrorType::MultipleDecimalPoints`
    /// rather than a `ParseFloatError { kind: Invalid }` panic (`BUG-12`). The span covers
    /// `1.2.`, up to and including the offending second point — the tokenizer stops
    /// accumulating right there rather than reading on into `3`.
    ///
    /// Verified to fail by reverting the accumulation loop's `'.'` arm to push every `.`
    /// into `buf` unconditionally, as before: `f64::from_str` then rejects the buffer and
    /// the error is a `MalformedNumber` over `0..5`, so this goes red on the variant and
    /// the span rather than panicking.
    #[test]
    fn second_decimal_point_is_an_error() {
        assert_eq!(
            make_tokenizer("1.2.3").collect::<Result<Vec<_>, _>>(),
            Err(TokenizerError::new(
                BytePos(0),
                BytePos(4),
                TokenizerErrorType::MultipleDecimalPoints
            ))
        );
    }

    /// A numeric literal continuing into a Unicode digit that is not ASCII — here `١`,
    /// `U+0661 ARABIC-INDIC DIGIT ONE`, two bytes in UTF-8 — is a
    /// `TokenizerErrorType::NonAsciiDigit` rather than a `ParseIntError { kind: InvalidDigit
    /// }` panic (`BUG-12`), because `char::is_numeric` is true for it. The span covers
    /// both characters of `1١`.
    ///
    /// Verified to fail by reverting the accumulation loop's digit check from
    /// `is_ascii_digit` back to `is_numeric`: `١` then lands in `buf`, `i64::from_str`
    /// rejects it, and the error is an `IntegerOverflow` over `0..3`, so this goes red on
    /// the variant rather than panicking. A panic needs that `i64::from_str` error path
    /// reverted too.
    #[test]
    fn non_ascii_digit_is_an_error() {
        assert_eq!(
            make_tokenizer("1\u{0661}").collect::<Result<Vec<_>, _>>(),
            Err(TokenizerError::new(
                BytePos(0),
                BytePos(3),
                TokenizerErrorType::NonAsciiDigit { digit: '\u{0661}' }
            ))
        );
    }

    /// Each of the three malformed literals raises its error **once** and leaves the rest
    /// of the source tokenizable. The three tests above use
    /// `collect::<Result<Vec<_>, _>>()`, which short-circuits on the first `Err` and so
    /// says nothing about what the iterator does afterwards; this one goes through
    /// [`drain_capped`], which asserts that the iterator terminated and that no error
    /// repeats — the property `BUG-4`, `BUG-5` and `BUG-11` were each a violation of.
    ///
    /// What holds the single-error half up is the `self.next_char()` in each of
    /// `consume_number`'s two early returns: without them `1.2.3` yields the error and
    /// then a stray `Dot`, and `1١` yields the error and then a second, unrelated
    /// `UnrecognizedToken` for the same `١`. Neither hangs — the comments at those two
    /// lines say so — so this test pins the duplicate, not a hang.
    ///
    /// Verified to fail by deleting the `self.next_char()` from the `NonAsciiDigit` arm:
    /// the `1١` case then yields two errors and the error-count assertion goes red.
    #[test]
    fn numeric_errors_do_not_hang() {
        // A second `.`: one error, then `3` and the rest of the line.
        let items = drain_capped("1.2.3 + 4", 20);
        assert_eq!(
            errors_of(&items),
            vec![&TokenizerError::new(
                BytePos(0),
                BytePos(4),
                TokenizerErrorType::MultipleDecimalPoints
            )],
            "expected exactly one error, got {:?}",
            items
        );
        assert_eq!(
            tokens_of(&items),
            vec![
                &int_token(3),
                &Token::Operator("+".to_string()),
                &int_token(4),
            ]
        );

        // A non-ASCII digit: one error, and the character it blames is not reported
        // again as an unrecognized token.
        let items = drain_capped("1\u{0661} + 4", 20);
        assert_eq!(
            errors_of(&items),
            vec![&TokenizerError::new(
                BytePos(0),
                BytePos(3),
                TokenizerErrorType::NonAsciiDigit { digit: '\u{0661}' }
            )],
            "expected exactly one error, got {:?}",
            items
        );
        assert_eq!(
            tokens_of(&items),
            vec![&Token::Operator("+".to_string()), &int_token(4)]
        );

        // An overflowing integer: one error, and the operand after it still tokenizes.
        let items = drain_capped("99999999999999999999 + 4", 20);
        assert_eq!(
            errors_of(&items),
            vec![&TokenizerError::new(
                BytePos(0),
                BytePos(20),
                TokenizerErrorType::IntegerOverflow
            )],
            "expected exactly one error, got {:?}",
            items
        );
        assert_eq!(
            tokens_of(&items),
            vec![&Token::Operator("+".to_string()), &int_token(4)]
        );
    }

    #[test]
    fn true_and_false_are_identifiers() {
        assert_eq!(
            tokenize("true"),
            vec![Token::LowerIdentifier("true".to_string())]
        );
        assert_eq!(
            tokenize("false"),
            vec![Token::LowerIdentifier("false".to_string())]
        );
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

    fn string_token(value: &str) -> Token {
        Token::String {
            value: value.to_owned(),
        }
    }

    /// The first error `source` fails to tokenize with.
    fn first_error(source: &str) -> TokenizerError {
        make_tokenizer(source)
            .collect::<Result<Vec<_>, _>>()
            .expect_err("expected the source to fail the tokenizer")
    }

    #[test]
    fn literal_string() {
        assert_eq!(tokenize(r#""hello""#), vec![string_token("hello")]);
        assert_eq!(tokenize(r#""""#), vec![string_token("")]);
        assert_eq!(tokenize(r#""a'b 🙂""#), vec![string_token("a'b 🙂")]);
        assert_eq!(
            tokenize(r#"f "a" "b""#),
            vec![ident_token("f"), string_token("a"), string_token("b")]
        );

        let spans: Vec<_> = make_tokenizer(r#"x = "é""#)
            .map(|t| t.unwrap())
            .map(|t| (t.span.start.absolute, t.span.end.absolute))
            .collect();
        assert_eq!(spans[2], (BytePos(4), BytePos(8)));
    }

    #[test]
    fn literal_string_escapes() {
        assert_eq!(
            tokenize(r#""\n\r\t\\\'\"""#),
            vec![string_token("\n\r\t\\'\"")]
        );
        assert_eq!(
            tokenize(r#""\u{41}\u{1F600}\u{0}""#),
            vec![string_token("A\u{1F600}\u{0}")]
        );
        // An escaped quote does not close the string.
        assert_eq!(tokenize(r#""a\"b""#), vec![string_token("a\"b")]);
    }

    /// Mirrors `literal_char`'s unclosed cases: the error spans from the opening quote
    /// to where the string was cut off.
    ///
    /// Verified by removing `consume_char`'s `'"'` arm, which reddens every `literal_string*`
    /// test (here a `"` is then an `UnrecognizedToken`), and by letting `consume_escape`
    /// treat only the end of the file as unclosing, which reddens the backslash case.
    #[test]
    fn literal_string_not_closed() {
        assert_eq!(
            first_error(r#""hello"#),
            TokenizerError::new(
                BytePos(0),
                BytePos(6),
                TokenizerErrorType::StringNotClosedError
            )
        );
        assert_eq!(
            first_error("\"hel\nlo\""),
            TokenizerError::new(
                BytePos(0),
                BytePos(4),
                TokenizerErrorType::StringNotClosedError
            )
        );
        assert_eq!(
            first_error("\"a\r\nb\""),
            TokenizerError::new(
                BytePos(0),
                BytePos(2),
                TokenizerErrorType::StringNotClosedError
            )
        );
        // A backslash cannot escape the line ending.
        assert_eq!(
            first_error("\"a\\\nb\""),
            TokenizerError::new(
                BytePos(0),
                BytePos(3),
                TokenizerErrorType::StringNotClosedError
            )
        );
        // `"""` is the empty string, then a second string the line ending cuts off.
        assert_eq!(
            first_error("\"\"\"\nx\n\"\"\""),
            TokenizerError::new(
                BytePos(2),
                BytePos(3),
                TokenizerErrorType::StringNotClosedError
            )
        );
    }

    #[test]
    fn literal_string_bad_escapes() {
        assert_eq!(
            first_error(r#""a\qb""#),
            TokenizerError::new(BytePos(2), BytePos(4), TokenizerErrorType::InvalidEscape)
        );
        let unicode = |source: &str, end: u32| {
            assert_eq!(
                first_error(source),
                TokenizerError::new(BytePos(1), BytePos(end), TokenizerErrorType::UnicodeError),
                "{}",
                source
            )
        };
        unicode(r#""\u41""#, 3);
        unicode(r#""\u{}""#, 5);
        unicode(r#""\u{zz}""#, 4);
        unicode(r#""\u{1234567}""#, 10);
        unicode(r#""\u{D800}""#, 9);
        unicode(r#""\u{110000}""#, 11);
    }

    /// A tab and a lone carriage return are rejected inside a string as they are
    /// everywhere else outside a comment. Verified by removing either arm of
    /// `consume_string`: the character then lands in the value and the test goes red.
    #[test]
    fn literal_string_rejects_tab_and_carriage_return() {
        assert_eq!(
            first_error("\"a\tb\""),
            TokenizerError::new(BytePos(2), BytePos(3), TokenizerErrorType::TabError)
        );
        assert_eq!(
            first_error("\"a\rb\""),
            TokenizerError::new(
                BytePos(2),
                BytePos(3),
                TokenizerErrorType::UnrecognizedToken { tok: '\r' }
            )
        );
    }

    /// Every string error consumes input, so draining past it terminates and never
    /// repeats one error (`BUG-4`).
    #[test]
    fn string_errors_do_not_hang() {
        for source in [
            "\"ab\n\"cd\n",
            "\"\\q\\q\"",
            "\"\\u{zz}\"",
            "\"a\tb\"",
            "\"\\",
        ] {
            drain_capped(source, 64);
        }
    }

    #[test]
    fn symbols() {
        let op = |s: &str| Token::Operator(s.to_owned());
        assert_eq!(
            tokenize("(),[]{}._ .. -> = + - / * == < <= >= > && || |> <| |"),
            vec![
                Token::LPar,
                Token::RPar,
                Token::Comma,
                Token::LBracket,
                Token::RBracket,
                Token::LBrace,
                Token::RBrace,
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

    /// A `.` is `Dot` only when it is written against the token before it and against the
    /// character after it. Whitespace, a block comment, or an end of the source after it
    /// makes it `SpacedDot`; the same before it, with a lowercase name against its right,
    /// makes it the `AccessorDot` of `Widget .size`.
    ///
    /// Verified to fail by making `consume_operator` yield `Token::Dot` for every lone
    /// `.`: every `SpacedDot` and `AccessorDot` expectation below then fails.
    #[test]
    fn a_dot_is_spaced_unless_written_against_both_sides() {
        let up = |s: &str| Token::UpperIdentifier(s.to_owned());
        let low = |s: &str| Token::LowerIdentifier(s.to_owned());

        assert_eq!(
            tokenize("Widget.size"),
            vec![up("Widget"), Token::Dot, low("size")]
        );
        assert_eq!(
            tokenize("Widget . size"),
            vec![up("Widget"), Token::SpacedDot, low("size")]
        );
        assert_eq!(
            tokenize("Widget .size"),
            vec![up("Widget"), Token::AccessorDot, low("size")]
        );
        assert_eq!(
            tokenize("Widget. size"),
            vec![up("Widget"), Token::SpacedDot, low("size")]
        );
        assert_eq!(
            tokenize("Widget\n.size"),
            vec![up("Widget"), Token::AccessorDot, low("size")]
        );
        assert_eq!(
            tokenize("Widget{- -}.size"),
            vec![up("Widget"), Token::AccessorDot, low("size")]
        );
        assert_eq!(
            tokenize("Widget.{- -}size"),
            vec![up("Widget"), Token::SpacedDot, low("size")]
        );
        assert_eq!(tokenize(".size"), vec![Token::AccessorDot, low("size")]);
        assert_eq!(tokenize("Widget."), vec![up("Widget"), Token::SpacedDot]);
    }

    /// A `.` that is not a `Dot` is an accessor's exactly when a lowercase name is written
    /// against its right: a label is a lowercase name, so a `.` before an uppercase name, a
    /// digit, a symbol or a space is a `SpacedDot` wherever it stands. A soft keyword and a
    /// reserved word are lowercase names too; the grammar is what rejects `.if`.
    ///
    /// Verified to fail by making `before_a_lowercase_name` in `consume_operator` answer
    /// `true` for every character: the `SpacedDot` expectations then read `AccessorDot`;
    /// and by making it answer `false`: the `AccessorDot` ones then read `SpacedDot`.
    #[test]
    fn a_dot_is_an_accessors_only_against_a_lowercase_name() {
        let up = |s: &str| Token::UpperIdentifier(s.to_owned());
        let low = |s: &str| Token::LowerIdentifier(s.to_owned());

        assert_eq!(tokenize(".name"), vec![Token::AccessorDot, low("name")]);
        assert_eq!(tokenize(".left"), vec![Token::AccessorDot, Token::Left]);
        assert_eq!(tokenize(".if"), vec![Token::AccessorDot, Token::If]);
        assert_eq!(tokenize(".été"), vec![Token::AccessorDot, low("été")]);

        assert_eq!(tokenize(". name"), vec![Token::SpacedDot, low("name")]);
        assert_eq!(tokenize(".Name"), vec![Token::SpacedDot, up("Name")]);
        assert_eq!(tokenize(".Été"), vec![Token::SpacedDot, up("Été")]);
        assert_eq!(tokenize(".1"), vec![Token::SpacedDot, int_token(1)]);
        assert_eq!(
            tokenize(".(a)"),
            vec![Token::SpacedDot, Token::LPar, low("a"), Token::RPar]
        );
        assert_eq!(
            tokenize("Widget .Size"),
            vec![up("Widget"), Token::SpacedDot, up("Size")]
        );
    }

    /// A `.` is written against the token before it only when that token can end an
    /// operand. Any other token before it leaves the `.` opening an expression, so it is
    /// never `Dot` however close it sits, and before a name it is an accessor's:
    /// `(.name)`, `[.name]`, `a,.name`, `then.name`.
    ///
    /// Verified to fail by making `ends_an_operand` answer `true` for every token: the
    /// first group then fails; and by making it answer `false` for `RPar`, `RBracket` and
    /// the string literal: the second group then fails.
    #[test]
    fn a_dot_after_a_token_that_cannot_end_an_operand_is_not_a_dot() {
        let up = |s: &str| Token::UpperIdentifier(s.to_owned());
        let low = |s: &str| Token::LowerIdentifier(s.to_owned());

        assert_eq!(
            tokenize("(.name)"),
            vec![Token::LPar, Token::AccessorDot, low("name"), Token::RPar]
        );
        assert_eq!(
            tokenize("[.name]"),
            vec![
                Token::LBracket,
                Token::AccessorDot,
                low("name"),
                Token::RBracket
            ]
        );
        assert_eq!(
            tokenize("a,.name"),
            vec![low("a"), Token::Comma, Token::AccessorDot, low("name")]
        );
        assert_eq!(
            tokenize("f (.name) r"),
            vec![
                low("f"),
                Token::LPar,
                Token::AccessorDot,
                low("name"),
                Token::RPar,
                low("r")
            ]
        );
        assert_eq!(
            tokenize("then.name"),
            vec![Token::Then, Token::AccessorDot, low("name")]
        );
        assert_eq!(
            tokenize("else.name"),
            vec![Token::Else, Token::AccessorDot, low("name")]
        );
        assert_eq!(
            tokenize("of.name"),
            vec![Token::Of, Token::AccessorDot, low("name")]
        );

        // The tokens that can end an operand keep the `.` attached.
        assert_eq!(
            tokenize("(f x).y"),
            vec![
                Token::LPar,
                low("f"),
                low("x"),
                Token::RPar,
                Token::Dot,
                low("y")
            ]
        );
        assert_eq!(
            tokenize("[a].y"),
            vec![
                Token::LBracket,
                low("a"),
                Token::RBracket,
                Token::Dot,
                low("y")
            ]
        );
        assert_eq!(
            tokenize("\"s\".y"),
            vec![
                Token::String {
                    value: "s".to_owned()
                },
                Token::Dot,
                low("y")
            ]
        );
        assert_eq!(
            tokenize("'c'.y"),
            vec![Token::Char { value: 'c' }, Token::Dot, low("y")]
        );
        assert_eq!(
            tokenize("r.centre.x"),
            vec![low("r"), Token::Dot, low("centre"), Token::Dot, low("x")]
        );
        assert_eq!(tokenize("left.x"), vec![Token::Left, Token::Dot, low("x")]);
        assert_eq!(
            tokenize("Widget.size"),
            vec![up("Widget"), Token::Dot, low("size")]
        );
    }

    /// `{` and `}` are tokens of their own, and a `{` is a brace unless a `-` follows it
    /// directly, which opens a block comment.
    ///
    /// Verified to fail by moving the `'{'` brace arm of `consume_char` ahead of the
    /// `'{' if self.lookahead.1 == Some('-')` arm: the second assertion, `f = {- a note
    /// -} { a = 1 }`, then reads the `{- a note -}` as a brace, a minus and so on, and
    /// fails along with the block comment tests. The first assertion has no comment in
    /// it and passes either way. The swap also leaves the comment arm unreachable, so
    /// rustc warns `unreachable pattern`, which the `clippy -D warnings` gate rejects on
    /// its own.
    #[test]
    fn braces_are_tokens_and_a_brace_dash_is_a_comment() {
        let low = |s: &str| Token::LowerIdentifier(s.to_owned());

        assert_eq!(
            tokenize("f = { a = 1 }"),
            vec![
                low("f"),
                Token::Equal,
                Token::LBrace,
                low("a"),
                Token::Equal,
                int_token(1),
                Token::RBrace
            ]
        );
        assert_eq!(
            tokenize("f = {- a note -} { a = 1 }"),
            vec![
                low("f"),
                Token::Equal,
                Token::LBrace,
                low("a"),
                Token::Equal,
                int_token(1),
                Token::RBrace
            ]
        );
        // A space between the `{` and the `-` ends the comment's opening: the `-` is the
        // minus sign again.
        assert_eq!(
            tokenize("{ -a }"),
            vec![Token::LBrace, Token::Minus, low("a"), Token::RBrace]
        );
        assert_eq!(
            tokenize("{a}{b}"),
            vec![
                Token::LBrace,
                low("a"),
                Token::RBrace,
                Token::LBrace,
                low("b"),
                Token::RBrace
            ]
        );
    }

    /// A `}` can end an operand and a `{` cannot: a `.` written against the `}` of a
    /// record is an access of it, and a `.` after a `{` opens an expression, as it does
    /// after `(` and `[`.
    ///
    /// Verified to fail by making `ends_an_operand` answer `false` for `RBrace`: the first
    /// group then reads `Dot` as `AccessorDot`. And by making it answer `true` for `LBrace`:
    /// the second group then reads `{.name}`'s `AccessorDot` as `Dot`.
    #[test]
    fn a_dot_after_a_brace() {
        let low = |s: &str| Token::LowerIdentifier(s.to_owned());

        assert_eq!(
            tokenize("{ r }.a"),
            vec![Token::LBrace, low("r"), Token::RBrace, Token::Dot, low("a")]
        );
        assert_eq!(
            tokenize("{ a = 1 }.a.b"),
            vec![
                Token::LBrace,
                low("a"),
                Token::Equal,
                int_token(1),
                Token::RBrace,
                Token::Dot,
                low("a"),
                Token::Dot,
                low("b")
            ]
        );
        // Whitespace after the dot makes it spaced, and whitespace or a comment before it
        // makes it an accessor's.
        assert_eq!(
            tokenize("{ r } .a"),
            vec![
                Token::LBrace,
                low("r"),
                Token::RBrace,
                Token::AccessorDot,
                low("a")
            ]
        );
        assert_eq!(
            tokenize("{ r }. a"),
            vec![
                Token::LBrace,
                low("r"),
                Token::RBrace,
                Token::SpacedDot,
                low("a")
            ]
        );
        assert_eq!(
            tokenize("{ r }{- -}.a"),
            vec![
                Token::LBrace,
                low("r"),
                Token::RBrace,
                Token::AccessorDot,
                low("a")
            ]
        );

        assert_eq!(
            tokenize("{.name}"),
            vec![
                Token::LBrace,
                Token::AccessorDot,
                low("name"),
                Token::RBrace
            ]
        );
        assert_eq!(
            tokenize("{ .name }"),
            vec![
                Token::LBrace,
                Token::AccessorDot,
                low("name"),
                Token::RBrace
            ]
        );
        assert_eq!(
            tokenize("f {.name} r"),
            vec![
                low("f"),
                Token::LBrace,
                Token::AccessorDot,
                low("name"),
                Token::RBrace,
                low("r")
            ]
        );
    }

    /// `=>` is its own token, the separator of a constraint context, and a longer
    /// run of operator characters that merely starts with it is still one ordinary
    /// operator: the table matches the whole run, not a prefix of it.
    ///
    /// Verified to fail by deleting the `"=>" => Token::FatArrow` arm, which makes
    /// the first token `Operator("=>")`.
    #[test]
    fn fat_arrow() {
        assert_eq!(
            tokenize("=> =>> ="),
            vec![
                Token::FatArrow,
                Token::Operator("=>>".to_owned()),
                Token::Equal
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

    /// A continuation line starting with `->` must tokenize regardless of how many spaces
    /// follow the arrow (`BUG-19`). Before the fix, `handle_indentation`'s `Some('-')` arm
    /// broke out of the indentation loop without clearing `at_line_start`, so the next poll
    /// re-entered `handle_indentation` believing it was still scanning the line's leading
    /// whitespace and measured the run of spaces *after* `->` against the two-space rule
    /// instead — rejecting the line whenever that run was odd, and accepting it whenever it
    /// was even, which is the parity this test rules out by trying one, two and three spaces.
    ///
    /// Verified to fail by reverting the fix (dropping `self.at_line_start = false;` from the
    /// `Some('-')` arm's `else` branch): the one- and three-space cases then raise
    /// `IndentationError` on the space between `->` and `Int`, while the two-space case still
    /// passes — reproducing the parity table in `BUG-19`.
    #[test]
    fn continuation_line_starting_with_arrow_tokenizes() {
        for spaces in 1..=3 {
            let source = format!("f : Int\n  ->{}Int\nf a = a\n", " ".repeat(spaces));

            assert_eq!(
                tokenize(&source),
                vec![
                    ident_token("f"),
                    Token::Colon,
                    ident_token("Int"),
                    Token::Arrow,
                    ident_token("Int"),
                    ident_token("f"),
                    ident_token("a"),
                    Token::Equal,
                    ident_token("a"),
                ],
                "failed with {} space(s) after the arrow",
                spaces
            );
        }
    }

    /// A consumer which keeps polling past a `TabError` raised while scanning
    /// leading indentation must not see that same error again, and iteration
    /// has to terminate (`BUG-5`). Before the fix, the tab arm of
    /// `handle_indentation` returned without consuming the tab or clearing
    /// `at_line_start`, so every subsequent poll re-entered the same arm on
    /// the same character and produced a byte-identical `TabError` forever.
    ///
    /// [`drain_capped`] carries the termination checks and says why polling
    /// explicitly is the only way to see this class of defect at all.
    ///
    /// Verified to fail by reverting the fix (returning immediately from the
    /// `Some('\t')` arm without advancing, as before): `drain_capped` then
    /// hits its cap without the iterator ever terminating, and all of the
    /// trailing items are the identical `TabError` at `BytePos(2)..BytePos(3)`.
    #[test]
    fn tab_indentation_does_not_hang() {
        let items = drain_capped("a\n\tb\n", 10);

        // Exactly one TabError should be raised, for the one tab-indented line;
        // tokenization should then move on and produce `b`.
        let errors = errors_of(&items);
        assert_eq!(
            errors.len(),
            1,
            "expected exactly one error, got {:?}",
            items
        );
        assert_eq!(
            errors[0],
            &TokenizerError::new(BytePos(2), BytePos(3), TokenizerErrorType::TabError)
        );

        assert_eq!(
            tokens_of(&items),
            vec![&ident_token("a"), &ident_token("b")]
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

    /// A block comment is recognised mid-expression, not only in a line's leading
    /// whitespace (defect 1 of `BUG-13`). Before the fix, `consume_char`'s `match` had no
    /// arm for `{`, so this fell through to the catch-all and was rejected as an
    /// `UnrecognizedToken` naming `{` — a character that was never the problem.
    ///
    /// Verified to fail by removing the `'{' if self.lookahead.1 == Some('-')` arm from
    /// `consume_char`: the `{` is then read as `LBrace` and the `-` as `Minus`, so the
    /// token sequence no longer matches.
    #[test]
    fn block_comment_recognised_mid_expression() {
        assert_eq!(
            tokenize("f = {- a note -} 1"),
            vec![ident_token("f"), Token::Equal, int_token(1),]
        );
    }

    /// Whatever follows a block comment's closing `-}` on the same line is ordinary
    /// source, not discarded (defect 2 of `BUG-13`). This source has the comment at the
    /// very start of the line — the one case `consume_char`'s new arm above does not
    /// exercise, since `handle_indentation` reads leading `{-` on its own — so it also
    /// pins the `handle_indentation` interaction the ticket calls out: `spaces` must keep
    /// counting the indentation from before the comment rather than being reset by it,
    /// or the spurious content after `-}` would misfire the even-indentation check.
    ///
    /// Verified to fail by restoring the unconditional `self.skip_end_of_line()` call in
    /// `consume_comment`'s block branch: `f`, `=` and `1` are then swallowed along with
    /// the comment and `tokenize` returns `vec![]`.
    #[test]
    fn code_after_block_comment_close_still_parses() {
        assert_eq!(
            tokenize("{- a note -} f = 1"),
            vec![ident_token("f"), Token::Equal, int_token(1),]
        );
    }

    /// Block comments nest: an inner `{-` must be closed by its own `-}` before the outer
    /// comment ends (defect 3 of `BUG-13`). Without nesting the first `-}` — the inner
    /// comment's — would end the whole thing, leaving `still outer -}` to be read as
    /// source.
    ///
    /// Verified to fail by reverting the block branch to the old first-`-}`-wins loop:
    /// the sequence then holds `still` and `outer` as identifiers, and the trailing `-}`
    /// as a `Minus` immediately followed by an `RBrace`.
    #[test]
    fn nested_block_comments() {
        assert_eq!(
            tokenize("{- outer {- inner -} still outer -} f = 1"),
            vec![ident_token("f"), Token::Equal, int_token(1),]
        );
    }

    /// Reaching end of file inside an open block comment is an error, not silent
    /// acceptance of the rest of the file (defect 4 of `BUG-13`). The primary label sits
    /// on the opening `{-` — the outermost one, when several are nested — since that is
    /// the one the author has to go fix.
    ///
    /// Verified to fail by reverting the loop's `(None, _)` arm to `break` (as the old
    /// `None => break` did): the tokenizer then reaches end of file with no error at all,
    /// and this assertion fails as `Ok([])` rather than the expected `Err`.
    #[test]
    fn unclosed_block_comment_is_an_error() {
        assert_eq!(
            make_tokenizer("f = 1\n{- oops, never closed").collect::<Result<Vec<_>, _>>(),
            Err(TokenizerError::new(
                BytePos(6),
                BytePos(27),
                TokenizerErrorType::UnclosedBlockComment
            ))
        );

        // Nesting: the error blames the outermost `{-`, not the inner one.
        assert_eq!(
            make_tokenizer("{- outer {- inner -} still open").collect::<Result<Vec<_>, _>>(),
            Err(TokenizerError::new(
                BytePos(0),
                BytePos(31),
                TokenizerErrorType::UnclosedBlockComment
            ))
        );
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

    /// A consumer which keeps polling past a `TabError` raised by a tab *after* the first
    /// non-whitespace character of a line — the main-loop `'\t'` arm of `consume_char`, not
    /// the indentation one — must not see that same error again, and iteration has to
    /// terminate (`BUG-11`). Before the fix, that arm returned without consuming the tab or
    /// advancing `self.position`, so every subsequent poll re-entered the same arm on the
    /// same character and produced a byte-identical `TabError` forever — the same shape
    /// `BUG-5` fixed for `handle_indentation`'s sibling arm, but that fix did not touch this
    /// one.
    ///
    /// Like `tab_indentation_does_not_hang` above, this goes through [`drain_capped`] rather
    /// than `collect::<Result<Vec<_>, _>>()` — `refuse_tab_in_expression` below predates this
    /// fix and did not catch it for exactly the reason that helper's doc comment gives.
    ///
    /// Verified to fail by reverting the fix (returning immediately from the main-loop
    /// `'\t'` arm without advancing, as before): `drain_capped` then hits its cap without the
    /// iterator ever terminating, and all of the trailing items are the identical `TabError`
    /// at `BytePos(9)..BytePos(10)`.
    #[test]
    fn tab_after_first_character_does_not_hang() {
        let items = drain_capped("f x =\n  1\t+ 2\n", 20);

        // Exactly one TabError should be raised, for the one tab in the source; tokenization
        // should then move on and produce the rest of the line.
        let errors = errors_of(&items);
        assert_eq!(
            errors.len(),
            1,
            "expected exactly one error, got {:?}",
            items
        );
        assert_eq!(
            errors[0],
            &TokenizerError::new(BytePos(9), BytePos(10), TokenizerErrorType::TabError)
        );

        assert_eq!(
            tokens_of(&items),
            vec![
                &ident_token("f"),
                &ident_token("x"),
                &Token::Equal,
                &int_token(1),
                &Token::Operator("+".to_string()),
                &int_token(2),
            ]
        );
    }

    /// A consumer which keeps polling past a `CharNotClosedError` must not see that same
    /// error again, and iteration has to terminate (`BUG-28`). Both of `consume_char`'s
    /// unclosed-literal arms used to return without consuming the opening quote, so every
    /// poll re-entered the `'\''` arm on the same character. `literal_char` covers the
    /// same three sources and missed it, because it collects and so stops at the first
    /// `Err`; [`drain_capped`] says why.
    ///
    /// After the error, the characters following the quote are tokenized as ordinary
    /// source: only the quote itself is consumed.
    ///
    /// Verified to fail by removing the `next_char()` call from each of the two arms in
    /// turn: `drain_capped` then hits its cap with the identical error repeated.
    #[test]
    fn unclosed_char_does_not_hang() {
        let cases = [
            (
                "'",
                TokenizerError::new(
                    BytePos(0),
                    BytePos(1),
                    TokenizerErrorType::CharNotClosedError(None),
                ),
                vec![],
            ),
            (
                "'a",
                TokenizerError::new(
                    BytePos(0),
                    BytePos(2),
                    TokenizerErrorType::CharNotClosedError(None),
                ),
                vec![ident_token("a")],
            ),
            (
                "'ab",
                TokenizerError::new(
                    BytePos(0),
                    BytePos(2),
                    TokenizerErrorType::CharNotClosedError(Some('b')),
                ),
                vec![ident_token("ab")],
            ),
        ];

        for (source, error, tokens) in cases {
            let items = drain_capped(source, 10);

            let errors = errors_of(&items);
            assert_eq!(
                errors,
                vec![&error],
                "expected exactly one error for {:?}, got {:?}",
                source,
                items
            );
            assert_eq!(
                tokens_of(&items),
                tokens.iter().collect::<Vec<_>>(),
                "unexpected tokens for {:?}",
                source
            );
        }
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
