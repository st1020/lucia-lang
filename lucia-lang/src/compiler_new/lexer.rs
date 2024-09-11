//! The lexer.

use std::str::Chars;

use rowan::{TextRange, TextSize};

use super::token::{
    Token,
    TokenKind::{self, *},
};

/// Peekable iterator over a char sequence.
///
/// Next characters can be peeked via `first` method,
/// and position can be shifted forward via `bump` method.
struct Cursor<'a> {
    /// The input string.
    input: &'a str,
    /// Iterator over chars. Slightly faster than a &str.
    chars: Chars<'a>,
    #[cfg(debug_assertions)]
    prev: char,
}

const EOF_CHAR: char = '\0';

impl<'a> Cursor<'a> {
    fn new(input: &'a str) -> Cursor<'a> {
        Cursor {
            input,
            chars: input.chars(),
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

    /// Peeks the second symbol from the input stream without consuming it.
    fn second(&self) -> char {
        // `.next()` optimizes better than `.nth(1)`
        let mut iter = self.chars.clone();
        iter.next();
        iter.next().unwrap_or(EOF_CHAR)
    }

    /// Checks if there is nothing more to consume.
    fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    /// Returns position of cursor.
    pub(crate) fn pos(&self) -> TextSize {
        TextSize::try_from(self.input.len() - self.chars.as_str().len()).unwrap()
    }

    /// Moves to the next character.
    fn bump(&mut self) -> Option<char> {
        let c = self.chars.next()?;

        #[cfg(debug_assertions)]
        {
            self.prev = c;
        }

        Some(c)
    }

    /// Eats symbols while predicate returns true or until the end of file is reached.
    fn eat_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        while predicate(self.first()) && !self.is_eof() {
            self.bump();
        }
    }
}

/// Creates an iterator that produces tokens from the input string.
pub fn tokenize(input: &str) -> impl Iterator<Item = Token<'_>> + '_ {
    let mut cursor = Cursor::new(input);
    std::iter::from_fn(move || {
        let token = cursor.advance_token();
        if token.kind != TokenKind::Eof {
            Some(token)
        } else {
            None
        }
    })
}

/// True if `c` is considered a whitespace according to Lucia language definition.
pub fn is_whitespace(c: char) -> bool {
    // This is Pattern_White_Space except '\n'.
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

impl<'a> Cursor<'a> {
    /// Parses a token from the input string.
    pub fn advance_token(&mut self) -> Token<'a> {
        let start = self.pos();
        let first_char = match self.bump() {
            Some(c) => c,
            None => {
                return Token::new(
                    TokenKind::Eof,
                    "",
                    TextRange::empty(TextSize::try_from(self.input.len()).unwrap()),
                )
            }
        };
        let token_kind = match first_char {
            // Div, DivAssign, comment or block comment.
            '/' => match self.first() {
                '/' => self.line_comment(),
                '*' => self.block_comment(),
                '=' => {
                    self.bump();
                    DivAssign
                }
                _ => Div,
            },

            // Sub, SubAssign or Arrow.
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

            // Question, SubAssign or Arrow.
            '?' => match self.first() {
                '.' => {
                    self.bump();
                    SafeDot
                }
                '[' => {
                    self.bump();
                    SafeOpenBracket
                }
                _ => Sub,
            },

            // Whitespace sequence.
            c if is_whitespace(c) => self.whitespace(),

            // Raw string.
            'r' => match self.first() {
                c @ ('"' | '\'') => self.string(c, true),
                _ => self.ident_or_keyword('r'),
            },

            // Identifier (this should be checked after other variant that can
            // start as identifier).
            c if is_id_start(c) => self.ident_or_keyword(c),

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
            '!' => Exclamation,
            ':' => Colon,
            '=' => Assign,
            '<' => Lt,
            '>' => Gt,
            '|' => VBar,
            '+' => Add,
            '*' => Mul,
            '%' => Rem,

            _ => Unknown,
        };
        let end = self.pos();
        let text = &self.input[start.into()..end.into()];
        let res = Token::new(token_kind, text, TextRange::new(start, end));
        res
    }

    fn eol(&mut self) -> TokenKind {
        debug_assert!(self.prev() == '\n');
        self.eat_while(|c| c == '\n');
        Eol
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

        if depth == 0 {
            BlockComment
        } else {
            UnterminatedBlockComment
        }
    }

    fn whitespace(&mut self) -> TokenKind {
        debug_assert!(is_whitespace(self.prev()));
        self.eat_while(is_whitespace);
        Whitespace
    }

    fn ident_or_keyword(&mut self, first_char: char) -> TokenKind {
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
            _ => Ident,
        }
    }

    fn number(&mut self, first_digit: char) -> TokenKind {
        debug_assert!('0' <= self.prev() && self.prev() <= '9');
        if first_digit == '0' {
            // Attempt to parse encoding base.
            match self.first() {
                'b' => {
                    self.bump();
                    if !self.eat_decimal_digits() {
                        return EmptyInt;
                    }
                }
                'o' => {
                    self.bump();
                    if !self.eat_decimal_digits() {
                        return EmptyInt;
                    }
                }
                'x' => {
                    self.bump();
                    if !self.eat_hexadecimal_digits() {
                        return EmptyInt;
                    }
                }
                // Not a base prefix; consume additional digits.
                '0'..='9' | '_' => {
                    self.eat_decimal_digits();
                }

                // Also not a base prefix; nothing more to do here.
                '.' | 'e' | 'E' => {}

                // Just a 0.
                _ => {
                    return Int;
                }
            }
        } else {
            // No base prefix, parse number in the usual way.
            self.eat_decimal_digits();
        };

        match self.first() {
            // Don't be greedy if this is actually an
            // integer literal followed by field/method access or a range pattern
            // (`0..2` and `12.foo()`)
            '.' if self.second() != '.' && !is_id_start(self.second()) => {
                // might have stuff after the ., and if it does, it needs to start
                // with a number
                self.bump();
                let mut empty_exponent = false;
                if self.first().is_ascii_digit() {
                    self.eat_decimal_digits();
                    match self.first() {
                        'e' | 'E' => {
                            self.bump();
                            empty_exponent = !self.eat_float_exponent();
                        }
                        _ => (),
                    }
                }
                if empty_exponent {
                    EmptyExponentFloat
                } else {
                    Float
                }
            }
            'e' | 'E' => {
                self.bump();
                let empty_exponent = !self.eat_float_exponent();
                if empty_exponent {
                    EmptyExponentFloat
                } else {
                    Float
                }
            }
            _ => Int,
        }
    }

    fn string(&mut self, quoted: char, is_raw: bool) -> TokenKind {
        if is_raw {
            debug_assert!(self.prev() == 'r');
            self.bump();
        }
        debug_assert!(self.prev() == '"' || self.prev() == '\'');
        while let Some(c) = self.bump() {
            match c {
                _ if c == quoted => {
                    return if is_raw { RawStr } else { Str };
                }
                '\\' if self.first() == '\\' || self.first() == '"' || self.first() == '\'' => {
                    // Bump again to skip escaped character.
                    self.bump();
                }
                _ => (),
            }
        }
        // End of file reached.
        UnterminatedStr
    }

    fn eat_decimal_digits(&mut self) -> bool {
        let mut has_digits = false;
        loop {
            match self.first() {
                '_' => {
                    self.bump();
                }
                '0'..='9' => {
                    has_digits = true;
                    self.bump();
                }
                _ => break,
            }
        }
        has_digits
    }

    fn eat_hexadecimal_digits(&mut self) -> bool {
        let mut has_digits = false;
        loop {
            match self.first() {
                '_' => {
                    self.bump();
                }
                '0'..='9' | 'a'..='f' | 'A'..='F' => {
                    has_digits = true;
                    self.bump();
                }
                _ => break,
            }
        }
        has_digits
    }

    /// Eats the float exponent. Returns true if at least one digit was met,
    /// and returns false otherwise.
    fn eat_float_exponent(&mut self) -> bool {
        debug_assert!(self.prev() == 'e' || self.prev() == 'E');
        if self.first() == '-' || self.first() == '+' {
            self.bump();
        }
        self.eat_decimal_digits()
    }
}
