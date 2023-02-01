use std::fmt::{Debug, Display};

use crate::errors::Result;

/// Location of token in the code.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Location {
    pub lineno: u32,
    pub column: u32,
    pub offset: u32,
}

impl Debug for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}({})", self.lineno, self.column, self.offset)
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

/// Enum representing common lexeme types.
#[derive(Clone, PartialEq)]
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
    /// "?"
    Question,
    /// ":"
    Colon,
    /// "="
    Assign,
    /// "<"
    Lt,
    /// ">"
    Gt,
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

impl Debug for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::If => write!(f, "If(if)"),
            Self::Else => write!(f, "Else(else)"),
            Self::Loop => write!(f, "Loop(loop)"),
            Self::While => write!(f, "While(while)"),
            Self::For => write!(f, "For(for)"),
            Self::In => write!(f, "In(in)"),
            Self::Break => write!(f, "Break(break)"),
            Self::Continue => write!(f, "Continue(continue)"),
            Self::Return => write!(f, "Return(return)"),
            Self::Throw => write!(f, "Throw(throw)"),
            Self::Global => write!(f, "Global(global)"),
            Self::Import => write!(f, "Import(import)"),
            Self::As => write!(f, "As(as)"),
            Self::Is => write!(f, "Is(is)"),
            Self::Not => write!(f, "Not(not)"),
            Self::And => write!(f, "And(and)"),
            Self::Or => write!(f, "Or(or)"),
            Self::Fn => write!(f, "Fn(fn)"),
            Self::Do => write!(f, "Do(do)"),
            Self::Null => write!(f, "Null(null)"),
            Self::True => write!(f, "True(true)"),
            Self::False => write!(f, "False(false)"),
            Self::DoubleColon => write!(f, "DoubleColon(::)"),
            Self::Eq => write!(f, "Eq(==)"),
            Self::NotEq => write!(f, "NotEq(!=)"),
            Self::LtEq => write!(f, "LtEq(<=)"),
            Self::GtEq => write!(f, "GtEq(>=)"),
            Self::AddAssign => write!(f, "AddAssign(+=)"),
            Self::SubAssign => write!(f, "SubAssign(-=)"),
            Self::MulAssign => write!(f, "MulAssign(*=)"),
            Self::DivAssign => write!(f, "DivAssign(/=)"),
            Self::ModAssign => write!(f, "ModAssign(%=)"),
            Self::Comma => write!(f, "Comma(,)"),
            Self::Dot => write!(f, "Dot(.)"),
            Self::OpenParen => write!(f, "OpenParen(())"),
            Self::CloseParen => write!(f, "CloseParen())"),
            Self::OpenBrace => write!(f, "OpenBrace({{)"),
            Self::CloseBrace => write!(f, "CloseBrace(}})"),
            Self::OpenBracket => write!(f, "OpenBracket([)"),
            Self::CloseBracket => write!(f, "CloseBracket(])"),
            Self::Question => write!(f, "Question(?)"),
            Self::Colon => write!(f, "Colon(:)"),
            Self::Assign => write!(f, "Assign(=)"),
            Self::Lt => write!(f, "Lt(<)"),
            Self::Gt => write!(f, "Gt(>)"),
            Self::VBar => write!(f, "VBar(|)"),
            Self::Add => write!(f, "Add(+)"),
            Self::Sub => write!(f, "Sub(-)"),
            Self::Mul => write!(f, "Mul(*)"),
            Self::Div => write!(f, "Div(/)"),
            Self::Mod => write!(f, "Mod(%)"),
            Self::EOL => write!(f, "EOL(\\n)"),
            Self::LineComment => write!(f, "LineComment(// ...)"),
            Self::BlockComment => write!(f, "BlockComment(/* ... */)"),
            Self::Whitespace => write!(f, "Whitespace( )"),
            Self::Ident(arg0) => f.debug_tuple("Ident").field(arg0).finish(),
            Self::Literal(arg0) => f.debug_tuple("Literal").field(arg0).finish(),
            Self::Unknown => write!(f, "Unknown"),
        }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

/// Enum representing literal types, included wrong literal like unterminated string.
#[derive(Debug, Clone, PartialEq)]
pub enum LiteralKind {
    /// "12", "0o100", "0b110"
    Int(Result<i64>),
    /// "12.34", "0b100.100"
    Float(Result<f64>),
    /// ""abc"", ""abc"
    Str(Result<String>),
}

/// Parsed token.
#[derive(Debug, Clone, PartialEq)]
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
            kind: TokenKind::Unknown,
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

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

/// Type of Token. Cocommon Token, Idnet or Literal.
#[derive(Clone, PartialEq)]
pub enum TokenType {
    Token(TokenKind),
    Ident,
    Literal,
}

impl Debug for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Token(arg0) => Debug::fmt(arg0, f),
            Self::Ident => write!(f, "Ident"),
            Self::Literal => write!(f, "Literal"),
        }
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}