use std::num::{ParseFloatError, ParseIntError};

use thiserror::Error;

use crate::codegen::OPCode;
use crate::lexer::{Token, TokenKind};
use crate::object::{Closuer, LucyValueType};

pub type LResult<T> = Result<T, LucyError>;

#[derive(Error, Debug, Clone, PartialEq)]
pub enum LexerError {
    #[error("parse int error ({0})")]
    ParseIntError(#[source] ParseIntError),
    #[error("parse float error  ({0})")]
    ParseFloatError(#[source] ParseFloatError),
    #[error("number format error")]
    NumberFormatError,
    #[error("unterminated string error")]
    UnterminatedStringError,
}

#[derive(Error, Debug, Clone, PartialEq)]
pub enum LucyError {
    #[error("syntax error: {0}")]
    SyntaxError(#[source] SyntaxErrorKind),
    #[error("runtime error: {0}")]
    RuntimeError(#[source] RuntimeErrorKind),
    #[error("import error")]
    ImportError,
    #[error("build table error")]
    BuildTableError,
    #[error("type error: {0}")]
    TypeError(#[source] TypeErrorKind),
}

#[derive(Error, Debug, Clone, PartialEq)]
pub enum SyntaxErrorKind {
    /// lexer error
    #[error("parse int error ({0})")]
    ParseIntError(#[source] ParseIntError),
    #[error("parse float error  ({0})")]
    ParseFloatError(#[source] ParseFloatError),
    #[error("number format error")]
    NumberFormatError,
    #[error("unterminated string error")]
    UnterminatedStringError,

    /// parser error
    #[error("unexpect token (expected {expected:?}, found {token:?})")]
    UnexpectToken {
        token: Box<Token>,
        expected: ExpectedToken,
    },
    #[error("unexpect EOF")]
    UnexpectEOF,
    #[error("parse assign statement error")]
    ParseAssignStmtError,

    /// codegen error
    #[error("illegal ast")]
    IllegalAst,
    #[error("break out of loop")]
    BreakOutsideLoop,
    #[error("continue out of loop")]
    ContinueOutsideLoop,
}

impl From<LexerError> for SyntaxErrorKind {
    fn from(value: LexerError) -> Self {
        match value {
            LexerError::ParseIntError(v) => SyntaxErrorKind::ParseIntError(v),
            LexerError::ParseFloatError(v) => SyntaxErrorKind::ParseFloatError(v),
            LexerError::NumberFormatError => SyntaxErrorKind::NumberFormatError,
            LexerError::UnterminatedStringError => SyntaxErrorKind::UnterminatedStringError,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpectedToken {
    TokenKind(Box<TokenKind>),
    AtomExpr,
    FuncExpr,
    Ident,
}

#[derive(Error, Debug, Clone, PartialEq)]
pub enum RuntimeErrorKind {
    #[error("stack error")]
    StackError,
    #[error("upvalue error")]
    UpvalueError,
    #[error("program error")]
    ProgramError,
}

#[derive(Error, Debug, Clone, PartialEq)]
pub enum TypeErrorKind {
    #[error("convert error (from {from:?} to {to:?})")]
    ConvertError {
        from: LucyValueType,
        to: LucyValueType,
    },
    #[error("operator error (unsupported operand type(s) for {operator:?}: {operand:?})")]
    OperatorError {
        operator: OPCode,
        operand: (Option<LucyValueType>, Option<LucyValueType>),
    },
    #[error("not callable error ({0} value is not callable)")]
    NotCallableError(LucyValueType),
    #[error("call arguments error (required {required} arguments, but {given} was given)")]
    CallArgumentsError {
        value: Option<Box<Closuer>>,
        required: usize,
        given: usize,
    },
}
