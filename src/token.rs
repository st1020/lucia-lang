use crate::errors::Result;

use self::TokenKind::*;

/// Location of token in the code.
#[derive(Clone, Debug, Copy, PartialEq, Eq)]
pub struct Location {
    pub lineno: u32,
    pub column: u32,
    pub offset: u32,
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
    /// "throw"
    Throw,
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
    /// "fn"
    Fn,
    /// "do"
    Do,
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
    /// End of line (`\n`)
    EOL,
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
    Int(Result<i64>),
    /// "12.34", "0b100.100"
    Float(Result<f64>),
    /// ""abc"", ""abc"
    Str(Result<String>),
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
