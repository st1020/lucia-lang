//! Errors of this crate.

use std::fmt;

use gc_arena::Collect;
use thiserror::Error;

use crate::{
    compiler::opcode::OpCode,
    frame::Frame,
    meta_ops::MetaMethod,
    objects::{ArgumentRange, ExternValue, Value, ValueType},
    thread::ThreadMode,
    utils::{Indent, Join},
};

/// Lucia error.
#[derive(Debug, Clone, Collect, Error)]
#[collect(no_drop)]
pub struct Error<'gc> {
    pub kind: ErrorKind<'gc>,
    pub traceback: Option<Vec<Frame<'gc>>>,
}

impl<'gc> PartialEq for Error<'gc> {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl<'gc> Eq for Error<'gc> {}

impl<'gc> fmt::Display for Error<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Error: {}", self.kind)?;
        if let Some(traceback) = &self.traceback {
            writeln!(f, "Traceback:")?;
            for (i, frame) in traceback.iter().rev().enumerate() {
                match frame {
                    Frame::Lucia(frame) => {
                        writeln!(f, "[{}] lucia frame", i)?;
                        writeln!(f, "{}", frame.indent(4))?;
                    }
                    Frame::Callback(callback, args) => {
                        writeln!(f, "[{}] callback frame", i)?;
                        writeln!(f, "    callback: {:?}", callback)?;
                        writeln!(f, "    args: {}", args.iter().join(", "))?;
                    }
                }
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

impl<'gc> Error<'gc> {
    pub fn new<T: Into<ErrorKind<'gc>>>(kind: T) -> Self {
        Error {
            kind: kind.into(),
            traceback: None,
        }
    }

    pub fn with_traceback<T: Into<ErrorKind<'gc>>>(kind: T, traceback: Vec<Frame<'gc>>) -> Self {
        Error {
            kind: kind.into(),
            traceback: Some(traceback),
        }
    }

    pub fn into_extern(self) -> ExternError {
        ExternError::from(self.kind)
    }
}

/// Kind of all errors.
#[derive(Debug, Clone, PartialEq, Eq, Collect, Error)]
#[collect(no_drop)]
pub enum ErrorKind<'gc> {
    #[error("{0}")]
    LuciaError(LuciaError<'gc>),
    #[error("{0}")]
    RuntimeError(#[from] RuntimeError),
}

impl ErrorKind<'_> {
    pub fn recoverable(&self) -> bool {
        !matches!(
            self,
            ErrorKind::LuciaError(LuciaError::Panic(_))
                | ErrorKind::RuntimeError(RuntimeError::BadThreadMode { .. })
        )
    }
}

impl<'gc> From<LuciaError<'gc>> for ErrorKind<'gc> {
    fn from(value: LuciaError<'gc>) -> Self {
        ErrorKind::LuciaError(value)
    }
}

/// Kind of Lucia Error.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Collect, Error)]
#[collect(no_drop)]
pub enum LuciaError<'gc> {
    #[error("{0}")]
    Error(Value<'gc>),
    #[error("panic: {0}")]
    Panic(Value<'gc>),
    #[error("assert: {0}")]
    Assert(Value<'gc>),
}

/// Kind of Runtime Error.
#[derive(Debug, Clone, PartialEq, Eq, Collect, Error)]
#[collect(no_drop)]
pub enum RuntimeError {
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
    #[error("operator error (unsupported operand type(s) for {operator}: {operand})")]
    UnOperator {
        operator: OpCode,
        operand: ValueType,
    },
    #[error("operator error (unsupported operand type(s) for {operator}: {} and {})", .operand.0, .operand.1)]
    BinOperator {
        operator: OpCode,
        operand: (ValueType, ValueType),
    },
    #[error("operator error (unsupported operand type(s) for {operator}: {operand})")]
    MetaUnOperator {
        operator: MetaMethod,
        operand: ValueType,
    },
    #[error("operator error (unsupported operand type(s) for {operator}: {} and {})", .operand.0, .operand.1)]
    MetaBinOperator {
        operator: MetaMethod,
        operand: (ValueType, ValueType),
    },
    #[error("divide by zero")]
    DivideByZero,
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum ExternError {
    #[error("{0}")]
    LuciaError(#[from] ExternLuciaError),
    #[error("{0}")]
    RuntimeError(#[from] RuntimeError),
}

impl From<ErrorKind<'_>> for ExternError {
    fn from(value: ErrorKind<'_>) -> Self {
        match value {
            ErrorKind::LuciaError(v) => ExternError::LuciaError(v.into()),
            ErrorKind::RuntimeError(v) => ExternError::RuntimeError(v),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Error)]
pub enum ExternLuciaError {
    #[error("{0}")]
    Error(ExternValue),
    #[error("panic: {0}")]
    Panic(ExternValue),
    #[error("assert: {0}")]
    Assert(ExternValue),
}

impl From<LuciaError<'_>> for ExternLuciaError {
    fn from(value: LuciaError<'_>) -> Self {
        match value {
            LuciaError::Error(v) => ExternLuciaError::Error(v.into()),
            LuciaError::Panic(v) => ExternLuciaError::Panic(v.into()),
            LuciaError::Assert(v) => ExternLuciaError::Assert(v.into()),
        }
    }
}
