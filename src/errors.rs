//! Errors of this crate.

use std::fmt;

use itertools::Itertools;
use thiserror::Error;

use crate::{
    compiler::{opcode::OpCode, value::MetaName},
    frame::Frame,
    objects::{ArgumentRange, Value, ValueType},
    thread::ThreadMode,
    utils::Indent,
};

/// Lucia error.
#[derive(Debug, Clone, Error)]
#[expect(clippy::error_impl_error)]
pub struct Error {
    pub kind: ErrorKind,
    pub traceback: Option<Vec<Frame>>,
}

impl PartialEq for Error {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl Eq for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Error: {}", self.kind)?;
        if let Some(traceback) = &self.traceback {
            writeln!(f, "Traceback:")?;
            for (i, frame) in traceback.iter().rev().enumerate() {
                match frame {
                    Frame::Lucia(frame) => {
                        writeln!(f, "[{i}] lucia frame")?;
                        writeln!(f, "{}", frame.indent(4))?;
                    }
                    Frame::Callback { callback, args } => {
                        writeln!(f, "[{i}] callback frame")?;
                        writeln!(f, "    callback: {callback}")?;
                        writeln!(f, "    args: {}", args.iter().join(", "))?;
                    }
                }
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

impl Error {
    pub fn new<T: Into<ErrorKind>>(kind: T) -> Self {
        Error {
            kind: kind.into(),
            traceback: None,
        }
    }

    pub fn with_traceback<T: Into<ErrorKind>>(kind: T, traceback: Vec<Frame>) -> Self {
        Error {
            kind: kind.into(),
            traceback: Some(traceback),
        }
    }
}

/// Kind of all errors.
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum ErrorKind {
    #[error("{0}")]
    LuciaError(Value),
    #[error("panic: {0}")]
    LuciaPanic(Value),
    #[error("assert: {0}")]
    LuciaAssert(Value),
    #[error("bad frame mode (expected {expected}, found {found})")]
    BadThreadMode {
        expected: ThreadMode,
        found: ThreadMode,
    },
    #[error("unexpected type error (expected {expected}, found {found})")]
    UnexpectedType {
        expected: ValueType,
        found: ValueType,
    },
    #[error("call arguments error (required {required} arguments, but {given} was given)")]
    CallArguments {
        required: ArgumentRange,
        given: usize,
    },
    #[error("operator error (unsupported operand type for {operator}: {operand})")]
    UnOperator {
        operator: OpCode,
        operand: ValueType,
    },
    #[error("operator error (unsupported operand types for {operator}: {} and {})", .operand.0, .operand.1)]
    BinOperator {
        operator: OpCode,
        operand: (ValueType, ValueType),
    },
    #[error("operator error (unsupported operand type for {operator}: {operand})")]
    MetaUnOperator {
        operator: MetaName,
        operand: ValueType,
    },
    #[error("operator error (unsupported operand types for {operator}: {} and {})", .operand.0, .operand.1)]
    MetaBinOperator {
        operator: MetaName,
        operand: (ValueType, ValueType),
    },
    #[error("divide by zero: can not divide {value} by zero")]
    DivideByZero { value: i64 },
    #[error("parse error: {reason}")]
    ParseError { reason: String },
}

impl ErrorKind {
    pub fn recoverable(&self) -> bool {
        !matches!(
            self,
            ErrorKind::LuciaPanic(_) | ErrorKind::BadThreadMode { .. }
        )
    }
}
