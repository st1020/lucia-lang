//! Errors of this crate.

use std::io;

use compact_str::CompactString;
use thiserror::Error;

use crate::{
    compiler::{opcode::OpCode, value::MetaName},
    objects::{ArgumentRange, Effect, Value, ValueType},
};

/// Kind of all errors.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Error)]
#[expect(clippy::error_impl_error)]
pub enum Error {
    #[error("{0}")]
    LuciaError(Value),
    #[error("panic: {0}")]
    LuciaPanic(Value),
    #[error("assert: {0}")]
    LuciaAssert(Value),
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
    ParseError { reason: CompactString },
    #[error("I/O error: {reason}")]
    IOError { reason: io::ErrorKind },
    #[error("unhandled effect: {effect} with arguments {args:?}")]
    UnhandledEffect { effect: Effect, args: Vec<Value> },
}
