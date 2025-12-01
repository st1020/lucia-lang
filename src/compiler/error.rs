//! The compiler error type.

use std::num::{ParseFloatError, ParseIntError};

use itertools::Itertools;
use rustc_literal_escaper::EscapeError;
use text_size::TextRange;
use thiserror::Error;

use crate::utils::Locatable;

use super::token::TokenKind;

/// The compiler error type.
#[derive(Debug, PartialEq, Eq, Error)]
pub enum CompilerError {
    #[error(
        "unexpected token (expected {}, found {}) {:?}",
        .expected.iter().join(", "),
        .found,
        .range,
    )]
    UnexpectedToken {
        expected: Vec<TokenKind>,
        found: TokenKind,
        range: TextRange,
    },
    #[error("parse int error ({}) {:?}", .error, .range)]
    ParseIntError {
        error: ParseIntError,
        range: TextRange,
    },
    #[error("parse float error ({}) {:?}", .error, .range)]
    ParseFloatError {
        error: ParseFloatError,
        range: TextRange,
    },
    #[error("escape error ({:?}) {:?}", .error, .range)]
    EscapeError {
        error: EscapeError,
        range: TextRange,
    },
    #[error("break outside loop {:?}", .range)]
    BreakOutsideLoop { range: TextRange },
    #[error("continue outside loop {:?}", .range)]
    ContinueOutsideLoop { range: TextRange },
    #[error("return outside loop {:?}", .range)]
    ReturnOutsideFunction { range: TextRange },
    #[error("throw outside loop {:?}", .range)]
    ThrowOutsideFunction { range: TextRange },
}

impl Locatable for CompilerError {
    fn range(&self) -> TextRange {
        match self {
            CompilerError::UnexpectedToken { range, .. }
            | CompilerError::ParseIntError { range, .. }
            | CompilerError::ParseFloatError { range, .. }
            | CompilerError::EscapeError { range, .. }
            | CompilerError::BreakOutsideLoop { range, .. }
            | CompilerError::ContinueOutsideLoop { range, .. }
            | CompilerError::ReturnOutsideFunction { range, .. }
            | CompilerError::ThrowOutsideFunction { range, .. } => *range,
        }
    }
}
