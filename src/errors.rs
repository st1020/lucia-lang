use std::error::Error;
use std::fmt::Display;
use std::num::{ParseFloatError, ParseIntError};

use crate::lexer::{Token, TokenKind};
use crate::object::LucyValueType;

pub type LResult<T> = Result<T, LucyError>;

#[derive(Clone, Debug, PartialEq)]
pub enum LucyError {
    ParserError(ParserErrorKind),
    CodegenError(CodegenErrorKind),
    LvmError(LvmErrorKind),
}

impl Display for LucyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "lucy error")
    }
}

impl Error for LucyError {}

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
pub enum LvmErrorKind {
    ProgramError,
    ImportError,
    BuildTableError,
    TypeError(String),
    RuntimeError(String),
    ConvertError {
        from: LucyValueType,
        to: LucyValueType,
    },
}
