use std::str::Chars;

use unicode_xid;

use crate::errors::{EscapeError, SyntaxError};
use crate::token::{
    LiteralKind::*,
    Location, Token,
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
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
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
            break None;
        } else {
            let t = cursor.advance_token();
            match t.kind {
                LineComment | BlockComment | Whitespace => (),
                _ => break Some(t),
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
    c == '_' || unicode_xid::UnicodeXID::is_xid_start(c)
}

/// True if `c` is valid as a non-first character of an identifier.
pub fn is_id_continue(c: char) -> bool {
    unicode_xid::UnicodeXID::is_xid_continue(c)
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
                '=' => DivAssign,
                _ => Div,
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
            '-' if self.first() == '=' => {
                self.bump();
                SubAssign
            }
            '*' if self.first() == '=' => {
                self.bump();
                MulAssign
            }
            '%' if self.first() == '=' => {
                self.bump();
                ModAssign
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
            ':' => Colon,
            '=' => Assign,
            '<' => Lt,
            '>' => Gt,
            '|' => VBar,
            '+' => Add,
            '-' => Sub,
            '*' => Mul,
            '%' => Mod,
            _ => Unknown,
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
        self.eat_while(|c| c != '\n');
        LineComment
    }

    fn block_comment(&mut self) -> TokenKind {
        debug_assert!(self.prev() == '/' && self.first() == '*');
        self.bump();
        let mut depth = 1usize;
        while let Some(c) = self.bump() {
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
        BlockComment
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
            "fn" => Fn,
            "do" => Do,
            "null" => Null,
            "true" => True,
            "false" => False,
            _ => Ident(value),
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
                _ => return Literal(Int(Ok(0))),
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
                        return Literal(Float(Err(SyntaxError::NumberFormatError.into())));
                    }
                    has_point = true;
                }
                'e' | 'E' if base == Base::Decimal => {
                    if has_exponent {
                        return Literal(Float(Err(SyntaxError::NumberFormatError.into())));
                    }
                    has_exponent = true;
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
                Literal(Float(Err(SyntaxError::NumberFormatError.into())))
            } else {
                match value.parse::<f64>() {
                    Ok(v) => Literal(Float(Ok(v))),
                    Err(e) => Literal(Float(Err(SyntaxError::ParseFloatError(e).into()))),
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
                Ok(v) => Literal(Int(Ok(v))),
                Err(e) => Literal(Int(Err(SyntaxError::ParseIntError(e).into()))),
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
                    Err(e) => return Literal(Str(Err(SyntaxError::EscapeError(e).into()))),
                }
            } else {
                return Literal(Str(Err(SyntaxError::UnterminatedStringError.into())));
            }
        }
        Literal(Str(Ok(value)))
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
