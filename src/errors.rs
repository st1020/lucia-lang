use std::error::Error;
use std::fmt::Display;
use std::num::{ParseFloatError, ParseIntError};

use crate::codegen::OPCode;
use crate::lexer::{Token, TokenKind};
use crate::object::{Closuer, LucyValueType};

pub type LResult<T> = Result<T, LucyError>;

#[derive(Clone, Debug, PartialEq)]
pub enum LucyError {
    SyntaxError(SyntaxErrorKind),
    RuntimeError(RuntimeErrorKind),
    ImportError,
    BuildTableError,
    TypeError(TypeErrorKind),
    ConvertError {
        from: LucyValueType,
        to: LucyValueType,
    },
}

impl Display for LucyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "lucy error")
    }
}

impl Error for LucyError {}

#[derive(Clone, Debug, PartialEq)]
pub enum SyntaxErrorKind {
    LexerError(LexerErrorKind),
    ParserError(ParserErrorKind),
    CodegenError(CodegenErrorKind),
}

#[derive(Clone, Debug, PartialEq)]
pub enum LexerErrorKind {
    ParseIntError(ParseIntError),
    ParseFloatError(ParseFloatError),
    NumberFormatError,
    UnterminatedStringError,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ParserErrorKind {
    UnexpectToken {
        token: Box<Token>,
        expect_token_kind: Option<Box<TokenKind>>,
    },
    UnexpectEOF,
    ParserUnOpError,
    ParserBinOpError,
    ParserMemberExprError,
    ParserAssignStmtError,
    ParserImportStmtError,
}

#[derive(Clone, Debug, PartialEq)]
pub enum CodegenErrorKind {
    IllegalAst,
    BreakOutOfLoop,
    ContinueOutOfLoop,
    LoadValueBefore,
}

#[derive(Clone, Debug, PartialEq)]
pub enum RuntimeErrorKind {
    StackError,
    UpvalueError,
    ProgramError,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeErrorKind {
    OperatorError {
        operator: OPCode,
        operand: (Option<LucyValueType>, Option<LucyValueType>),
    },
    NotCallableError(LucyValueType),
    CallArgumentsError {
        value: Box<Closuer>,
        require: usize,
        give: usize,
    },
}
