//! The lexer.

use std::{
    fmt,
    num::{ParseFloatError, ParseIntError},
    str::Chars,
};

use thiserror::Error;
use unicode_ident;

use crate::utils::Location;

use super::token::{
    LiteralKind::*,
    Token,
    TokenKind::{self, *},
};

/// Peekable iterator over a char sequence.
///
/// Next characters can be peeked via `first` method,
/// and position can be shifted forward via `bump` method.
struct Cursor<'a> {
    initial_len: usize,
    /// Iterator over chars. Slightly faster than a &str.
    chars: Chars<'a>,
    lineno: u32,
    column: u32,
    #[cfg(debug_assertions)]
    prev: char,
}

const EOF_CHAR: char = '\0';

impl<'a> Cursor<'a> {
    fn new(input: &'a str) -> Cursor<'a> {
        Cursor {
            initial_len: input.len(),
            chars: input.chars(),
            lineno: 1,
            column: 1,
            #[cfg(debug_assertions)]
            prev: EOF_CHAR,
        }
    }

    /// Returns the last eaten symbol (or `'\0'` in release builds).
    /// (For debug assertions only.)
    fn prev(&self) -> char {
        #[cfg(debug_assertions)]
        {
            self.prev
        }

        #[cfg(not(debug_assertions))]
        {
            EOF_CHAR
        }
    }

    /// Peeks the next symbol from the input stream without consuming it.
    /// If requested position doesn't exist, `EOF_CHAR` is returned.
    /// However, getting `EOF_CHAR` doesn't always mean actual end of file,
    /// it should be checked with `is_eof` method.
    fn first(&self) -> char {
        // `.next()` optimizes better than `.nth(0)`
        self.chars.clone().next().unwrap_or(EOF_CHAR)
    }

    /// Checks if there is nothing more to consume.
    fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    /// Moves to the next character.
    fn bump(&mut self) -> Option<char> {
        let c = self.chars.next()?;

        if c == '\n' {
            self.lineno += 1;
            self.column = 0;
        }
        self.column += 1;

        #[cfg(debug_assertions)]
        {
            self.prev = c;
        }

        Some(c)
    }

    /// Gets current location
    fn location(&self) -> Location {
        Location {
            lineno: self.lineno,
            column: self.column,
            offset: (self.initial_len - self.chars.as_str().len()) as u32,
        }
    }

    /// Eats symbols while predicate returns true or until the end of file is reached.
    fn eat_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        while predicate(self.first()) && !self.is_eof() {
            self.bump();
        }
    }
}

/// Base of numeric literal encoding according to its prefix.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Base {
    /// Literal starts with "0b".
    Binary,
    /// Literal starts with "0o".
    Octal,
    /// Literal starts with "0x".
    Hexadecimal,
    /// Literal doesn't contain a prefix.
    Decimal,
}

/// Creates an iterator that produces tokens from the input string.
pub fn tokenize(input: &str) -> impl Iterator<Item = Token> + '_ {
    let mut cursor = Cursor::new(input);
    std::iter::from_fn(move || loop {
        if cursor.is_eof() {
            break Some(Token::new(
                TokenKind::EOF,
                cursor.location(),
                cursor.location(),
            ));
        } else {
            let t = cursor.advance_token();
            if t.kind != Whitespace {
                break Some(t);
            }
        }
    })
}

/// True if `c` is considered a whitespace according to Rust language definition.
pub fn is_whitespace(c: char) -> bool {
    // This is Pattern_White_Space.
    //
    // Note that this set is stable (ie, it doesn't change with different
    // Unicode versions), so it's ok to just hard-code the values.

    matches!(
        c,
        // Usual ASCII suspects
        '\u{0009}'   // \t
        | '\u{000B}' // vertical tab
        | '\u{000C}' // form feed
        | '\u{000D}' // \r
        | '\u{0020}' // space

        // NEXT LINE from latin1
        | '\u{0085}'

        // Bidi markers
        | '\u{200E}' // LEFT-TO-RIGHT MARK
        | '\u{200F}' // RIGHT-TO-LEFT MARK

        // Dedicated whitespace characters from Unicode
        | '\u{2028}' // LINE SEPARATOR
        | '\u{2029}' // PARAGRAPH SEPARATOR
    )
}

/// True if `c` is valid as a first character of an identifier.
pub fn is_id_start(c: char) -> bool {
    // This is XID_Start OR '_' (which formally is not a XID_Start).
    c == '_' || unicode_ident::is_xid_start(c)
}

/// True if `c` is valid as a non-first character of an identifier.
pub fn is_id_continue(c: char) -> bool {
    unicode_ident::is_xid_continue(c)
}

/// The passed string is lexically an identifier.
pub fn is_ident(string: &str) -> bool {
    let mut chars = string.chars();
    if let Some(start) = chars.next() {
        is_id_start(start) && chars.all(is_id_continue)
    } else {
        false
    }
}

impl Cursor<'_> {
    pub fn advance_token(&mut self) -> Token {
        let start = self.location();
        let first_char = self.bump().unwrap_or(EOF_CHAR);
        let token_kind = match first_char {
            // Slash, comment or block comment.
            '/' => match self.first() {
                '/' => self.line_comment(),
                '*' => self.block_comment(),
                '=' => {
                    self.bump();
                    DivAssign
                }
                _ => Div,
            },

            '-' => match self.first() {
                '>' => {
                    self.bump();
                    Arrow
                }
                '=' => {
                    self.bump();
                    SubAssign
                }
                _ => Sub,
            },

            // Whitespace sequence.
            c if is_whitespace(c) => {
                self.eat_while(is_whitespace);
                Whitespace
            }

            // Raw identifier, raw string literal or identifier.
            'r' => match self.first() {
                c @ ('"' | '\'') => self.string(c, true),
                _ => self.ident_or_reserved_word('r'),
            },

            // Identifier (this should be checked after other variant that can
            // start as identifier).
            c if is_id_start(c) => self.ident_or_reserved_word(c),

            // Numeric literal.
            c @ '0'..='9' => self.number(c),

            // String literal.
            c @ ('"' | '\'') => self.string(c, false),

            // Two-char tokens.
            ':' if self.first() == ':' => {
                self.bump();
                DoubleColon
            }
            '=' if self.first() == '=' => {
                self.bump();
                Eq
            }
            '!' if self.first() == '=' => {
                self.bump();
                NotEq
            }
            '<' if self.first() == '=' => {
                self.bump();
                LtEq
            }
            '>' if self.first() == '=' => {
                self.bump();
                GtEq
            }
            '+' if self.first() == '=' => {
                self.bump();
                AddAssign
            }
            '*' if self.first() == '=' => {
                self.bump();
                MulAssign
            }
            '%' if self.first() == '=' => {
                self.bump();
                RemAssign
            }

            // One-symbol tokens.
            '\n' => self.eol(),
            '\\' if self.first() == '\n' => {
                self.bump();
                Whitespace
            }
            ',' => Comma,
            '.' => Dot,
            '(' => OpenParen,
            ')' => CloseParen,
            '{' => OpenBrace,
            '}' => CloseBrace,
            '[' => OpenBracket,
            ']' => CloseBracket,
            '#' => Pound,
            '?' => Question,
            '!' => Exclamation,
            ':' => Colon,
            '=' => Assign,
            '<' => Lt,
            '>' => Gt,
            '|' => VBar,
            '+' => Add,
            '*' => Mul,
            '%' => Rem,
            c => Unknown(c),
        };
        Token::new(token_kind, start, self.location())
    }

    fn eol(&mut self) -> TokenKind {
        debug_assert!(self.prev() == '\n');
        self.eat_while(|c| c == '\n');
        EOL
    }

    fn line_comment(&mut self) -> TokenKind {
        debug_assert!(self.prev() == '/' && self.first() == '/');
        self.bump();
        let mut v = String::new();
        loop {
            let c = self.first();
            if c == '\n' || self.is_eof() {
                break;
            }
            v.push(c);
            self.bump();
        }
        LineComment(v.into())
    }

    fn block_comment(&mut self) -> TokenKind {
        debug_assert!(self.prev() == '/' && self.first() == '*');
        self.bump();
        let mut v = String::new();
        let mut depth = 1usize;
        while let Some(c) = self.bump() {
            v.push(c);
            match c {
                '/' if self.first() == '*' => {
                    self.bump();
                    depth += 1;
                }
                '*' if self.first() == '/' => {
                    self.bump();
                    depth -= 1;
                    if depth == 0 {
                        // This block comment is closed, so for a construction like "/* */ */"
                        // there will be a successfully parsed block comment "/* */"
                        // and " */" will be processed separately.
                        break;
                    }
                }
                _ => (),
            }
        }
        v.truncate(v.len() - 1);
        BlockComment(v.into())
    }

    fn ident_or_reserved_word(&mut self, first_char: char) -> TokenKind {
        debug_assert!(is_id_start(self.prev()));
        let mut value = String::from(first_char);
        loop {
            let c = self.first();
            if is_id_continue(c) {
                value.push(c);
            } else {
                break;
            }
            self.bump();
        }

        match value.as_str() {
            "if" => If,
            "else" => Else,
            "loop" => Loop,
            "while" => While,
            "for" => For,
            "in" => In,
            "break" => Break,
            "continue" => Continue,
            "throw" => Throw,
            "return" => Return,
            "global" => Global,
            "import" => Import,
            "as" => As,
            "is" => Is,
            "not" => Not,
            "and" => And,
            "or" => Or,
            "try" => Try,
            "fn" => Fn,
            "do" => Do,
            "null" => Null,
            "true" => True,
            "false" => False,
            _ => Ident(value.into()),
        }
    }

    fn number(&mut self, first_digit: char) -> TokenKind {
        debug_assert!('0' <= self.prev() && self.prev() <= '9');
        let mut base = Base::Decimal;
        let mut value = String::new();
        let mut has_point = false;
        let mut has_exponent = false;
        if first_digit == '0' {
            // Attempt to parse encoding base.
            match self.first() {
                'b' => {
                    base = Base::Binary;
                    self.bump();
                }
                'o' => {
                    base = Base::Octal;
                    self.bump();
                }
                'x' => {
                    base = Base::Hexadecimal;
                    self.bump();
                }
                // Not a base prefix.
                '0'..='9' | '_' | '.' | 'e' | 'E' => {
                    base = Base::Decimal;
                    value.push('0');
                }
                // Just a 0.
                _ => return Literal(Ok(Int(0))),
            };
        } else {
            value.push(first_digit);
        }
        loop {
            let t = self.first();
            match t {
                '_' => {
                    self.bump();
                    continue;
                }
                '.' if base == Base::Decimal => {
                    if has_point {
                        return Literal(Err(LexerError::NumberFormatError));
                    }
                    has_point = true;
                }
                'e' | 'E' if base == Base::Decimal => {
                    if has_exponent {
                        return Literal(Err(LexerError::NumberFormatError));
                    }
                    has_exponent = true;

                    value.push(t);
                    self.bump();

                    let first_char_after_exponent = self.first();
                    if first_char_after_exponent == '+' || first_char_after_exponent == '-' {
                        value.push(first_char_after_exponent);
                        self.bump();
                    }

                    continue;
                }
                '0'..='1' if base == Base::Binary => {}
                '0'..='7' if base == Base::Octal => {}
                '0'..='9' if base == Base::Decimal => {}
                '0'..='9' | 'a'..='f' | 'A'..='F' if base == Base::Hexadecimal => {}
                _ => break,
            }
            value.push(t);
            self.bump();
        }

        if has_point || has_exponent {
            // only support decimal float literal
            if base != Base::Decimal {
                Literal(Err(LexerError::NumberFormatError))
            } else {
                match value.parse::<f64>() {
                    Ok(v) => Literal(Ok(Float(v))),
                    Err(e) => Literal(Err(LexerError::ParseFloatError(e))),
                }
            }
        } else {
            match i64::from_str_radix(
                &value,
                match base {
                    Base::Binary => 2,
                    Base::Octal => 8,
                    Base::Hexadecimal => 16,
                    Base::Decimal => 10,
                },
            ) {
                Ok(v) => Literal(Ok(Int(v))),
                Err(e) => Literal(Err(LexerError::ParseIntError(e))),
            }
        }
    }

    fn string(&mut self, quoted: char, is_raw: bool) -> TokenKind {
        if is_raw {
            debug_assert!(self.prev() == 'r');
            self.bump();
        }
        debug_assert!(self.prev() == '"' || self.prev() == '\'');
        let mut value = String::new();
        loop {
            if let Some(c) = self.bump() {
                let t = match c {
                    _ if c == quoted => break,
                    '\\' if !is_raw => match self.first() {
                        '\n' => {
                            self.bump();
                            continue;
                        }
                        _ => self.scan_escape(),
                    },
                    '\r' => Err(EscapeError::BareCarriageReturn),
                    _ => Ok(c),
                };
                match t {
                    Ok(c) => value.push(c),
                    Err(e) => return Literal(Err(LexerError::EscapeError(e))),
                }
            } else {
                return Literal(Err(LexerError::UnterminatedStringError));
            }
        }
        Literal(Ok(Str(value.into())))
    }

    fn scan_escape(&mut self) -> std::result::Result<char, EscapeError> {
        debug_assert!(self.prev() == '\\');
        // Previous character was '\\', unescape what follows.
        let res = match self.bump().unwrap_or(EOF_CHAR) {
            '"' => '"',
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            '\\' => '\\',
            '\'' => '\'',
            '0' => '\0',

            'x' => {
                // Parse hexadecimal character code.

                let hi = self.bump().ok_or(EscapeError::TooShortHexEscape)?;
                let hi = hi.to_digit(16).ok_or(EscapeError::InvalidCharInHexEscape)?;

                let lo = self.bump().ok_or(EscapeError::TooShortHexEscape)?;
                let lo = lo.to_digit(16).ok_or(EscapeError::InvalidCharInHexEscape)?;

                let value = hi * 16 + lo;

                // Verify that it is within ASCII range.
                if value > 0x7F {
                    return Err(EscapeError::OutOfRangeHexEscape);
                }
                let value = value as u8;

                value as char
            }

            'u' => {
                // We've parsed '\u', now we have to parse '{..}'.

                if self.bump() != Some('{') {
                    return Err(EscapeError::NoBraceInUnicodeEscape);
                }

                // First character must be a hexadecimal digit.
                let mut n_digits = 1;
                let mut value: u32 = match self.bump().ok_or(EscapeError::UnclosedUnicodeEscape)? {
                    '_' => return Err(EscapeError::LeadingUnderscoreUnicodeEscape),
                    '}' => return Err(EscapeError::EmptyUnicodeEscape),
                    c => c
                        .to_digit(16)
                        .ok_or(EscapeError::InvalidCharInUnicodeEscape)?,
                };

                // First character is valid, now parse the rest of the number
                // and closing brace.
                loop {
                    match self.bump() {
                        None => return Err(EscapeError::UnclosedUnicodeEscape),
                        Some('_') => continue,
                        Some('}') => {
                            if n_digits > 6 {
                                return Err(EscapeError::OverlongUnicodeEscape);
                            }

                            break std::char::from_u32(value).ok_or({
                                if value > 0x10FFFF {
                                    EscapeError::OutOfRangeUnicodeEscape
                                } else {
                                    EscapeError::LoneSurrogateUnicodeEscape
                                }
                            })?;
                        }
                        Some(c) => {
                            let digit: u32 = c
                                .to_digit(16)
                                .ok_or(EscapeError::InvalidCharInUnicodeEscape)?;
                            n_digits += 1;
                            if n_digits > 6 {
                                // Stop updating value since we're sure that it's incorrect already.
                                continue;
                            }
                            value = value * 16 + digit;
                        }
                    };
                }
            }
            _ => return Err(EscapeError::InvalidEscape),
        };
        Ok(res)
    }
}

/// Kind of LexerError.
#[derive(Error, Debug, Clone, PartialEq)]
pub enum LexerError {
    #[error("parse int error ({0})")]
    ParseIntError(#[from] ParseIntError),
    #[error("parse float error ({0})")]
    ParseFloatError(#[from] ParseFloatError),
    #[error("number format error")]
    NumberFormatError,
    #[error("unterminated string error")]
    UnterminatedStringError,
    #[error("escape error ({0})")]
    EscapeError(#[from] EscapeError),
}

/// Errors and warnings that can occur during string unescaping.
#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum EscapeError {
    /// Invalid escape character (e.g. '\z').
    InvalidEscape,
    /// Raw '\r' encountered.
    BareCarriageReturn,

    /// Numeric character escape is too short (e.g. '\x1').
    TooShortHexEscape,
    /// Invalid character in numeric escape (e.g. '\xz')
    InvalidCharInHexEscape,
    /// Character code in numeric escape is non-ascii (e.g. '\xFF').
    OutOfRangeHexEscape,

    /// '\u' not followed by '{'.
    NoBraceInUnicodeEscape,
    /// Non-hexadecimal value in '\u{..}'.
    InvalidCharInUnicodeEscape,
    /// '\u{}'
    EmptyUnicodeEscape,
    /// No closing brace in '\u{..}', e.g. '\u{12'.
    UnclosedUnicodeEscape,
    /// '\u{_12}'
    LeadingUnderscoreUnicodeEscape,
    /// More than 6 characters in '\u{..}', e.g. '\u{10FFFF_FF}'
    OverlongUnicodeEscape,
    /// Invalid in-bound unicode character code, e.g. '\u{DFFF}'.
    LoneSurrogateUnicodeEscape,
    /// Out of bounds unicode character code, e.g. '\u{FFFFFF}'.
    OutOfRangeUnicodeEscape,
}

impl fmt::Display for EscapeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! check_first_literal_token {
        ($input:expr, $value:expr $(,)?) => {
            if let TokenKind::Literal(Ok(literal_kind)) = tokenize($input).next().unwrap().kind {
                assert_eq!(literal_kind, $value.into())
            } else {
                panic!("first token not a literal")
            }
        };
    }

    #[test]
    fn test_string_escape() {
        check_first_literal_token!(r#" "\"" "#, "\"");
        check_first_literal_token!(r#" "\n" "#, "\n");
        check_first_literal_token!(r#" "\r" "#, "\r");
        check_first_literal_token!(r#" "\t" "#, "\t");
        check_first_literal_token!(r#" "\\" "#, "\\");
        check_first_literal_token!(r#" "\'" "#, "\'");
        check_first_literal_token!(r#" "\0" "#, "\0");
        check_first_literal_token!(r#" "\x21" "#, "!"); // ASCII 0x21 is "!"
        check_first_literal_token!(r#" "\u{4F60}\u{597D}\u{2764}" "#, "你好❤");
    }

    #[test]
    fn test_number_int() {
        check_first_literal_token!("0", 0);
        check_first_literal_token!("1", 1);
        check_first_literal_token!("100_000_000", 100_000_000);
        check_first_literal_token!("0b1010", 10);
        check_first_literal_token!("0o1010", 520);
        check_first_literal_token!("0xABCD", 43981);
    }

    #[test]
    fn test_number_float() {
        check_first_literal_token!("0.0", 0.);
        check_first_literal_token!("1.0", 1.);
        check_first_literal_token!("1.0001", 1.0001);
        check_first_literal_token!("1.2e2", 120.);
        check_first_literal_token!("1.2E2", 120.);
        check_first_literal_token!("12e1", 120.);
    }
}
