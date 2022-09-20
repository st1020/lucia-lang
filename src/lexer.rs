use std::str::Chars;

use unicode_xid;

use crate::errors::{LResult, LucyError, SyntaxErrorKind};

use self::LiteralKind::*;
use self::TokenKind::*;

/// Peekable iterator over a char sequence.
///
/// Next characters can be peeked via `first` method,
/// and position can be shifted forward via `bump` method.
pub(crate) struct Cursor<'a> {
    initial_len: usize,
    /// Iterator over chars. Slightly faster than a &str.
    chars: Chars<'a>,
    lineno: u32,
    column: u32,
    #[cfg(debug_assertions)]
    prev: char,
}

pub(crate) const EOF_CHAR: char = '\0';

impl<'a> Cursor<'a> {
    pub(crate) fn new(input: &'a str) -> Cursor<'a> {
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
    pub(crate) fn prev(&self) -> char {
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
    pub(crate) fn first(&self) -> char {
        // `.next()` optimizes better than `.nth(0)`
        self.chars.clone().next().unwrap_or(EOF_CHAR)
    }

    /// Checks if there is nothing more to consume.
    pub(crate) fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    /// Moves to the next character.
    pub(crate) fn bump(&mut self) -> Option<char> {
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
    pub(crate) fn location(&self) -> Location {
        Location {
            lineno: self.lineno,
            column: self.column,
            offset: (self.initial_len - self.chars.as_str().len()) as u32,
        }
    }

    /// Eats symbols while predicate returns true or until the end of file is reached.
    pub(crate) fn eat_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        while predicate(self.first()) && !self.is_eof() {
            self.bump();
        }
    }
}

/// Location of token in the code.
#[derive(Clone, Debug, Copy, PartialEq)]
pub struct Location {
    pub lineno: u32,
    pub column: u32,
    pub offset: u32,
}

/// Parsed token.
#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub start: Location,
    pub end: Location,
}

impl Token {
    pub fn new(kind: TokenKind, start: Location, end: Location) -> Self {
        Token { kind, start, end }
    }

    pub fn dummy() -> Self {
        Token {
            kind: Unknown,
            start: Location {
                lineno: 1,
                column: 1,
                offset: 0,
            },
            end: Location {
                lineno: 1,
                column: 1,
                offset: 0,
            },
        }
    }
}

/// Enum representing common lexeme types.
#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    // Multi-char tokens:
    /// "if"
    If,
    /// "else"
    Else,
    /// "lopp"
    Loop,
    /// "while"
    While,
    /// "for"
    For,
    /// "in"
    In,
    /// "break"
    Break,
    /// "continue"
    Continue,
    /// "return"
    Return,
    /// "global"
    Global,
    /// "import"
    Import,
    /// "as"
    As,
    /// "is"
    Is,
    /// "not"
    Not,
    /// "and"
    And,
    /// "or"
    Or,
    /// "func"
    Func,
    /// "null"
    Null,
    /// "true"
    True,
    /// "false"
    False,

    // Two-char tokens:
    /// "::"
    DoubleColon,
    /// "=="
    Eq,
    /// "!="
    NotEq,
    /// "<="
    LtEq,
    /// ">="
    GtEq,
    /// "+="
    AddAssign,
    /// "-="
    SubAssign,
    /// "*="
    MulAssign,
    /// "/="
    DivAssign,
    /// "%="
    ModAssign,

    // One-char tokens:
    /// ";"
    Semi,
    /// ","
    Comma,
    /// "."
    Dot,
    /// "("
    OpenParen,
    /// ")"
    CloseParen,
    /// "{"
    OpenBrace,
    /// "}"
    CloseBrace,
    /// "["
    OpenBracket,
    /// "]"
    CloseBracket,
    /// "@"
    At,
    /// "#"
    Pound,
    /// "~"
    Tilde,
    /// "?"
    Question,
    /// ":"
    Colon,
    /// "$"
    Dollar,
    /// "="
    Assign,
    /// "!"
    Bang,
    /// "<"
    Lt,
    /// ">"
    Gt,
    /// "&"
    Ampersand,
    /// "|"
    VBar,
    /// "+"
    Add,
    /// "-"
    Sub,
    /// "*"
    Mul,
    /// "/"
    Div,
    /// "%"
    Mod,
    /// "^"
    Caret,

    // other
    /// "// comment"
    LineComment,
    /// `/* block comment */`
    ///
    /// Block comments can be recursive, so the sequence like `/* /* */`
    /// will not be considered terminated and will result in a parsing error.
    BlockComment,
    /// Any whitespace characters sequence.
    Whitespace,
    /// Ident
    Ident(String),
    /// "12", "1.0e-40", ""123"". See `LiteralKind` for more details.
    Literal(LiteralKind),
    /// Unknown token, not expected by the lexer, e.g. "№"
    Unknown,
}

/// Enum representing literal types, included wrong literal like unterminated string.
#[derive(Clone, Debug, PartialEq)]
pub enum LiteralKind {
    /// "12", "0o100", "0b110"
    Int(LResult<i64>),
    /// "12.34", "0b100.100"
    Float(LResult<f64>),
    /// ""abc"", ""abc"
    Str(LResult<String>),
}

/// Base of numeric literal encoding according to its prefix.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Base {
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
        | '\u{000A}' // \n
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
            c if is_whitespace(c) => self.whitespace(),

            // Identifier (this should be checked after other variant that can
            // start as identifier).
            c if is_id_start(c) => self.ident_or_reserved_word(c),

            // Numeric literal.
            c @ '0'..='9' => self.number(c),

            // String literal.
            c @ ('"' | '\'') => self.string(c),

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
            ';' => Semi,
            ',' => Comma,
            '.' => Dot,
            '(' => OpenParen,
            ')' => CloseParen,
            '{' => OpenBrace,
            '}' => CloseBrace,
            '[' => OpenBracket,
            ']' => CloseBracket,
            '@' => At,
            '#' => Pound,
            '~' => Tilde,
            '?' => Question,
            ':' => Colon,
            '$' => Dollar,
            '=' => Assign,
            '!' => Bang,
            '<' => Lt,
            '>' => Gt,
            '&' => Ampersand,
            '|' => VBar,
            '+' => Add,
            '-' => Sub,
            '*' => Mul,
            '%' => Mod,
            '^' => Caret,
            _ => Unknown,
        };
        Token::new(token_kind, start, self.location())
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

    fn whitespace(&mut self) -> TokenKind {
        debug_assert!(is_whitespace(self.prev()));
        self.eat_while(is_whitespace);
        Whitespace
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
            "return" => Return,
            "global" => Global,
            "import" => Import,
            "as" => As,
            "is" => Is,
            "not" => Not,
            "and" => And,
            "or" => Or,
            "func" => Func,
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
                        return Literal(Float(Err(LucyError::SyntaxError(
                            SyntaxErrorKind::NumberFormatError,
                        ))));
                    }
                    has_point = true;
                }
                'e' | 'E' if base == Base::Decimal => {
                    if has_exponent {
                        return Literal(Float(Err(LucyError::SyntaxError(
                            SyntaxErrorKind::NumberFormatError,
                        ))));
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
                return Literal(Float(Err(LucyError::SyntaxError(
                    SyntaxErrorKind::NumberFormatError,
                ))));
            } else {
                match value.parse::<f64>() {
                    Ok(v) => Literal(Float(Ok(v))),
                    Err(e) => Literal(Float(Err(LucyError::SyntaxError(
                        SyntaxErrorKind::ParseFloatError(e),
                    )))),
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
                Err(e) => Literal(Int(Err(LucyError::SyntaxError(
                    SyntaxErrorKind::ParseIntError(e),
                )))),
            }
        }
    }

    fn string(&mut self, quoted: char) -> TokenKind {
        debug_assert!(self.prev() == '"' || self.prev() == '\'');
        let mut value = String::new();
        loop {
            if let Some(c) = self.bump() {
                if c == quoted {
                    break;
                }
                match c {
                    '\\' => {
                        let c = match self.first() {
                            '"' => '"',
                            'n' => '\n',
                            'r' => '\r',
                            't' => '\t',
                            '\\' => '\\',
                            '\'' => '\'',
                            '0' => '\0',
                            c @ _ => {
                                value.push('\\');
                                c
                            }
                        };
                        value.push(c);
                        self.bump();
                    }
                    _ => value.push(c),
                }
            } else {
                return Literal(Str(Err(LucyError::SyntaxError(
                    SyntaxErrorKind::UnterminatedStringError,
                ))));
            }
        }
        Literal(Str(Ok(value)))
    }
}
