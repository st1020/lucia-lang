//! Errors of this crate.

use std::{
    fmt,
    hash::{Hash, Hasher},
    ops::{Bound, RangeBounds},
};

use gc_arena::Collect;
use thiserror::Error;

use crate::{
    compiler::opcode::OpCode,
    frame::{Frame, FrameMode},
    meta_ops::MetaMethod,
    objects::{Closure, Value, ValueType},
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

impl<'gc> Hash for Error<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
    }
}

impl<'gc> fmt::Display for Error<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Error: {}", self.kind)?;
        if let Some(traceback) = &self.traceback {
            writeln!(f, "Traceback:")?;
            for (i, frame) in traceback.iter().rev().enumerate() {
                match frame {
                    Frame::Lua(frame) => {
                        writeln!(f, "[{}] lucia frame", i)?;
                        writeln!(f, "{}", frame.indent(4))?;
                    }
                    Frame::Callback(callback, args) => {
                        writeln!(f, "[{}] callback frame", i)?;
                        writeln!(f, "    callback: {:?}", callback)?;
                        writeln!(f, "    args: {}", args.iter().join(", "))?;
                    }
                    Frame::Calling => (),
                }
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

impl<'gc> Error<'gc> {
    pub fn new(kind: ErrorKind<'gc>) -> Self {
        Error {
            kind,
            traceback: None,
        }
    }

    pub fn with_traceback(kind: ErrorKind<'gc>, traceback: Vec<Frame<'gc>>) -> Self {
        Error {
            kind,
            traceback: Some(traceback),
        }
    }
}

impl<'gc> From<Value<'gc>> for Error<'gc> {
    fn from(value: Value<'gc>) -> Self {
        Error::new(ErrorKind::LuciaError(value))
    }
}

/// Kind of Lucia Error.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Collect, Error)]
#[collect(no_drop)]
pub enum ErrorKind<'gc> {
    #[error("bad frame mode (expected {expected}, found {found})")]
    BadFrameMode {
        expected: FrameMode,
        found: FrameMode,
    },
    #[error("{0}")]
    LuciaError(Value<'gc>),
    #[error("{0}")]
    UserPanic(Value<'gc>),
    #[error("assert error: {0}")]
    AssertError(Value<'gc>),
    #[error("unexpected type error (expected {expected}, found {found})")]
    UnexpectedType {
        expected: ValueType,
        found: ValueType,
    },
    #[error("call arguments error (required {required} arguments, but {given} was given)")]
    CallArguments {
        value: Option<Closure<'gc>>,
        required: CallArgumentsErrorKind,
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
}

impl<'gc> ErrorKind<'gc> {
    pub const fn recoverable(&self) -> bool {
        !matches!(
            self,
            ErrorKind::BadFrameMode { .. } | ErrorKind::UserPanic(_)
        )
    }
}

/// Kind of CallArgumentsError.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Collect)]
#[collect(require_static)]
pub struct CallArgumentsErrorKind {
    pub start: usize,
    pub end: Option<usize>,
}

impl CallArgumentsErrorKind {
    pub fn new(start: usize, end: Option<usize>) -> Self {
        Self { start, end }
    }

    pub fn more_then(start: usize) -> Self {
        Self { start, end: None }
    }
}

impl From<usize> for CallArgumentsErrorKind {
    fn from(value: usize) -> Self {
        CallArgumentsErrorKind {
            start: value,
            end: Some(value),
        }
    }
}

impl From<(usize, usize)> for CallArgumentsErrorKind {
    fn from(value: (usize, usize)) -> Self {
        CallArgumentsErrorKind {
            start: value.0,
            end: Some(value.1),
        }
    }
}

impl From<(usize, Option<usize>)> for CallArgumentsErrorKind {
    fn from(value: (usize, Option<usize>)) -> Self {
        CallArgumentsErrorKind {
            start: value.0,
            end: value.1,
        }
    }
}

impl RangeBounds<usize> for CallArgumentsErrorKind {
    fn start_bound(&self) -> Bound<&usize> {
        Bound::Included(&self.start)
    }

    fn end_bound(&self) -> Bound<&usize> {
        if let Some(end) = &self.end {
            Bound::Included(end)
        } else {
            Bound::Unbounded
        }
    }
}

impl CallArgumentsErrorKind {
    pub fn contains(&self, item: &usize) -> bool {
        <Self as RangeBounds<usize>>::contains(self, item)
    }
}

impl fmt::Display for CallArgumentsErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(end) = self.end {
            if self.start == end {
                write!(f, "{}", end)
            } else {
                write!(f, "[{}, {}]", self.start, end)
            }
        } else {
            write!(f, "at least {}", self.start)
        }
    }
}
