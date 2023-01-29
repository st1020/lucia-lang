use std::fmt::Display;
use std::num::{ParseFloatError, ParseIntError};
use std::ops::{Bound, Range, RangeBounds, RangeFrom, RangeInclusive};
use std::result;

use thiserror::Error;

use crate::codegen::OPCode;
use crate::lexer::{Token, TokenKind};
use crate::lvm::Lvm;
use crate::objects::{Closure, Table, Value, ValueType};

pub type Result<T> = result::Result<T, Error>;

/// Enum representing any lucia error.
#[derive(Error, Debug, Clone, PartialEq)]
pub enum Error {
    #[error("syntax error: {0}")]
    SyntaxError(#[from] SyntaxError),
    #[error("runtime error: {0}")]
    RuntimeError(#[from] RuntimeError),
}

/// Kind of SyntaxError.
#[derive(Error, Debug, Clone, PartialEq)]
pub enum SyntaxError {
    // lexer error
    #[error("parse int error ({0})")]
    ParseIntError(#[from] ParseIntError),
    #[error("parse float error ({0})")]
    ParseFloatError(#[from] ParseFloatError),
    #[error("number format error")]
    NumberFormatError,
    #[error("unterminated string error")]
    UnterminatedStringError,
    #[error("escape error ({0})")]
    EscapeError(#[from] EscapeError),

    // parser error
    #[error("unexpect token (expected {expected:?}, found {token:?})")]
    UnexpectToken {
        token: Box<Token>,
        expected: ExpectedToken,
    },
    #[error("unexpect EOF")]
    UnexpectEOF,
    #[error("parse assign statement error")]
    ParseAssignStmtError,

    // codegen error
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
    #[error("throw outside function")]
    ThrowOutsideFunction,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpectedToken {
    TokenKind(Box<TokenKind>),
    MemberExpr,
    AtomExpr,
    FuncExpr,
    Ident,
}

/// Errors and warnings that can occur during string unescaping.
#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum EscapeError {
    /// Invalid escape character (e.g. '\z').
    InvalidEscape,
    /// Raw '\r' encountered.
    BareCarriageReturn,

    /// Numeric character escape is too short (e.g. '\x1').
    TooShortHexEscape,
    /// Invalid character in numeric escape (e.g. '\xz')
    InvalidCharInHexEscape,
    /// Character code in numeric escape is non-ascii (e.g. '\xFF').
    OutOfRangeHexEscape,

    /// '\u' not followed by '{'.
    NoBraceInUnicodeEscape,
    /// Non-hexadecimal value in '\u{..}'.
    InvalidCharInUnicodeEscape,
    /// '\u{}'
    EmptyUnicodeEscape,
    /// No closing brace in '\u{..}', e.g. '\u{12'.
    UnclosedUnicodeEscape,
    /// '\u{_12}'
    LeadingUnderscoreUnicodeEscape,
    /// More than 6 characters in '\u{..}', e.g. '\u{10FFFF_FF}'
    OverlongUnicodeEscape,
    /// Invalid in-bound unicode character code, e.g. '\u{DFFF}'.
    LoneSurrogateUnicodeEscape,
    /// Out of bounds unicode character code, e.g. '\u{FFFFFF}'.
    OutOfRangeUnicodeEscape,
}

impl Display for EscapeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

/// RuntimeError.
#[derive(Error, Debug, Clone)]
#[error("{kind}")]
pub struct RuntimeError {
    pub kind: RuntimeErrorKind,
    pub traceback: Vec<TracebackFrame>,
}

impl PartialEq for RuntimeError {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

#[derive(Debug, Clone)]
pub struct TracebackFrame {
    pub pc: usize,
    pub operate_stack: Vec<Value>,
    pub closure: Closure,
}

/// Kind of RuntimeError.
#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum RuntimeErrorKind {
    #[error("stack error")]
    StackError,
    #[error("program error: {0}")]
    ProgramError(#[from] ProgramError),
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
    #[error("unexpect code: {0}")]
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
    TypeError(#[from] TypeError),
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

    #[inline]
    pub fn into_table(&self, lvm: &mut Lvm) -> Table {
        let mut error_table = Table::new();
        error_table.set(
            &lvm.get_builtin_str("type"),
            lvm.new_str_value(self.error_type().to_string()),
        );
        error_table.set(&lvm.get_builtin_str("msg"), lvm.new_str_value(self.msg()));
        error_table
    }

    #[inline]
    pub fn into_table_value(&self, lvm: &mut Lvm) -> Value {
        let t = self.into_table(lvm);
        lvm.new_table_value(t)
    }
}

impl Display for BuiltinError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.error_type(), self.msg())
    }
}

/// Kind of TypeError.
#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum TypeError {
    #[error("convert error (from {from} to {to})")]
    ConvertError { from: ValueType, to: ValueType },
    #[error("operator error (unsupported operand type(s) for {operator}: {operand})")]
    UnOperatorError {
        operator: OPCode,
        operand: ValueType,
    },
    #[error("operator error (unsupported operand type(s) for {operator}: {} and {})", .operand.0, .operand.1)]
    BinOperatorError {
        operator: OPCode,
        operand: (ValueType, ValueType),
    },
    #[error("not callable error ({0} value is not callable)")]
    NotCallableError(ValueType),
    #[error("call arguments error (required {required} arguments, but {given} was given)")]
    CallArgumentsError {
        value: Option<Box<Closure>>,
        required: CallArgumentsErrorKind,
        given: usize,
    },
    #[error("build table error ({0} value can't be table key)")]
    BuildTableError(ValueType),
}

/// Kind of CallArgumentsError.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CallArgumentsErrorKind {
    Eq(usize),
    Range(Range<usize>),
    RangeFrom(RangeFrom<usize>),
    RangeInclusive(RangeInclusive<usize>),
}

impl RangeBounds<usize> for CallArgumentsErrorKind {
    fn start_bound(&self) -> Bound<&usize> {
        match self {
            CallArgumentsErrorKind::Eq(v) => Bound::Included(v),
            CallArgumentsErrorKind::Range(v) => v.start_bound(),
            CallArgumentsErrorKind::RangeFrom(v) => v.start_bound(),
            CallArgumentsErrorKind::RangeInclusive(v) => v.start_bound(),
        }
    }

    fn end_bound(&self) -> Bound<&usize> {
        match self {
            CallArgumentsErrorKind::Eq(v) => Bound::Included(v),
            CallArgumentsErrorKind::Range(v) => v.end_bound(),
            CallArgumentsErrorKind::RangeFrom(v) => v.end_bound(),
            CallArgumentsErrorKind::RangeInclusive(v) => v.end_bound(),
        }
    }
}

impl CallArgumentsErrorKind {
    pub fn contains(&self, item: &usize) -> bool {
        <Self as RangeBounds<usize>>::contains(self, item)
    }
}

impl Display for CallArgumentsErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CallArgumentsErrorKind::Eq(v) => write!(f, "{}", v),
            CallArgumentsErrorKind::Range(v) => write!(f, "[{}, {})", v.start, v.end),
            CallArgumentsErrorKind::RangeFrom(v) => write!(f, "at least {}", v.start),
            CallArgumentsErrorKind::RangeInclusive(v) => write!(f, "[{}, {}]", v.start(), v.end()),
        }
    }
}

#[macro_export]
macro_rules! operator_error {
    ($operator:expr, $arg1:expr) => {
        $crate::errors::BuiltinError::TypeError($crate::errors::TypeError::UnOperatorError {
            operator: $operator,
            operand: $arg1.value_type(),
        })
    };

    ($operator:expr, $arg1:expr, $arg2:expr) => {
        $crate::errors::BuiltinError::TypeError($crate::errors::TypeError::BinOperatorError {
            operator: $operator,
            operand: ($arg1.value_type(), $arg2.value_type()),
        })
    };
}

#[macro_export]
macro_rules! type_convert_error {
    ($from:expr, $to:expr) => {
        $crate::errors::BuiltinError::TypeError($crate::errors::TypeError::ConvertError {
            from: $from,
            to: $to,
        })
    };
}

#[macro_export]
macro_rules! not_callable_error {
    ($value:expr) => {
        $crate::errors::BuiltinError::TypeError($crate::errors::TypeError::NotCallableError(
            $value.value_type(),
        ))
    };
}

#[macro_export]
macro_rules! call_arguments_error {
    ($value:expr, $require_ident:ident($require_value:expr), $give:expr) => {
        $crate::errors::BuiltinError::TypeError($crate::errors::TypeError::CallArgumentsError {
            value: $value,
            required: $crate::errors::CallArgumentsErrorKind::$require_ident($require_value),
            given: $give,
        })
    };
}

#[macro_export]
macro_rules! build_table_error {
    ($value:expr) => {
        $crate::errors::BuiltinError::TypeError($crate::errors::TypeError::BuildTableError(
            $value.value_type(),
        ))
    };
}
