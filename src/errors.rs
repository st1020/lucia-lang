use std::fmt::Display;
use std::num::{ParseFloatError, ParseIntError};
use std::result;

use thiserror::Error;

use crate::codegen::OPCode;
use crate::lexer::{Token, TokenKind};
use crate::lvm::Frame;
use crate::objects::{Closure, Value, ValueType};

pub type Result<T> = result::Result<T, Error>;

/// Enum representing any lucia error.
#[derive(Error, Debug, Clone, PartialEq)]
pub enum Error {
    #[error("syntax error: {0}")]
    SyntaxError(#[source] SyntaxError),
    #[error("runtime error: {0}")]
    RuntimeError(#[source] RuntimeError),
}

/// Kind of SyntaxError.
#[derive(Error, Debug, Clone, PartialEq)]
pub enum SyntaxError {
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
    #[error("break outside loop")]
    BreakOutsideLoop,
    #[error("continue outside loop")]
    ContinueOutsideLoop,
    #[error("global outside function")]
    GlobalOutsideFunction,
    #[error("return outside function")]
    ReturnOutsideFunction,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpectedToken {
    TokenKind(Box<TokenKind>),
    AtomExpr,
    FuncExpr,
    Ident,
}

/// RuntimeError.
#[derive(Error, Debug, Clone)]
#[error("{kind}")]
pub struct RuntimeError {
    pub kind: RuntimeErrorKind,
    pub traceback: Vec<Frame>,
}

impl PartialEq for RuntimeError {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

/// Kind of RuntimeError.
#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum RuntimeErrorKind {
    #[error("stack error")]
    StackError,
    #[error("program error: {0}")]
    ProgramError(#[source] ProgramError),
    #[error("throw error: throw illegal value ({0})")]
    ThrowError(Value),
    #[error("user panic: {0}")]
    UserPanic(Value),
}

/// Kind of ProgramError.
#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum ProgramError {
    #[error("module error: {0}")]
    ModuleError(usize),
    #[error("code index error: {0}")]
    CodeIndexError(usize),
    #[error("unexpect code: {0:?}")]
    UnexpectCodeError(OPCode),
    #[error("local name error: {0}")]
    LocalNameError(usize),
    #[error("global name error: {0}")]
    GlobalNameError(usize),
    #[error("const error: {0}")]
    ConstError(usize),
    #[error("upvalue error: {0}")]
    UpvalueError(usize),
    #[error("function list error: {0}")]
    FuncListError(usize),
}

/// Enum representing any lucia lang builtin error.
/// BuiltinError will be converted to the LuciaTable and handled by lucia lang runtime.
#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum BuiltinError {
    TypeError(TypeErrorKind),
    ImportError(String),
}

impl BuiltinError {
    pub fn error_type(&self) -> &'static str {
        match self {
            BuiltinError::TypeError(_) => "type_error",
            BuiltinError::ImportError(_) => "import_error",
        }
    }

    pub fn msg(&self) -> String {
        match self {
            BuiltinError::TypeError(v) => v.to_string(),
            BuiltinError::ImportError(v) => v.clone(),
        }
    }
}

impl Display for BuiltinError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.error_type(), self.msg())
    }
}

/// Kind of TypeError.
#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum TypeErrorKind {
    #[error("convert error (from {from:?} to {to:?})")]
    ConvertError { from: ValueType, to: ValueType },
    #[error("operator error (unsupported operand type(s) for {operator:?}: {operand})")]
    UnOperatorError {
        operator: OPCode,
        operand: ValueType,
    },
    #[error("operator error (unsupported operand type(s) for {operator:?}: {} and {})", .operand.0, .operand.1)]
    BinOperatorError {
        operator: OPCode,
        operand: (ValueType, ValueType),
    },
    #[error("not callable error ({0} value is not callable)")]
    NotCallableError(ValueType),
    #[error("call arguments error (required {required} arguments, but {given} was given)")]
    CallArgumentsError {
        value: Option<Box<Closure>>,
        required: usize,
        given: usize,
    },
    #[error("build table error ({0} value can't be table key)")]
    BuildTableError(ValueType),
}

#[macro_export]
macro_rules! unsupported_operand_type {
    ($operator:expr, $arg1:expr) => {
        $crate::errors::BuiltinError::TypeError($crate::errors::TypeErrorKind::UnOperatorError {
            operator: $operator,
            operand: $arg1.value_type(),
        })
    };
    ($operator:expr, $arg1:expr, $arg2:expr) => {
        $crate::errors::BuiltinError::TypeError($crate::errors::TypeErrorKind::BinOperatorError {
            operator: $operator,
            operand: ($arg1.value_type(), $arg2.value_type()),
        })
    };
}

#[macro_export]
macro_rules! type_convert_error {
    ($from:expr, $to:expr) => {
        $crate::errors::BuiltinError::TypeError($crate::errors::TypeErrorKind::ConvertError {
            from: $from,
            to: $to,
        })
    };
}

#[macro_export]
macro_rules! not_callable_error {
    ($value:expr) => {
        $crate::errors::BuiltinError::TypeError($crate::errors::TypeErrorKind::NotCallableError(
            $value.value_type(),
        ))
    };
}

#[macro_export]
macro_rules! call_arguments_error {
    ($value:expr, $require:expr, $give:expr) => {
        $crate::errors::BuiltinError::TypeError($crate::errors::TypeErrorKind::CallArgumentsError {
            value: $value,
            required: $require,
            given: $give,
        })
    };
}

#[macro_export]
macro_rules! build_table_error {
    ($value:expr) => {
        $crate::errors::BuiltinError::TypeError($crate::errors::TypeErrorKind::BuildTableError(
            $value.value_type(),
        ))
    };
}

#[macro_export]
macro_rules! type_error {
    ($value:expr) => {
        $crate::errors::BuiltinError::TypeError($crate::errors::TypeErrorKind::TypeError($value))
    };
}
