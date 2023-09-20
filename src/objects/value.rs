use std::{
    fmt,
    hash::{Hash, Hasher},
    num::NonZeroUsize,
};

use gc_arena::{Collect, Gc};

use crate::{
    objects::{AnyCallback, AnyUserData, Closure, GcError, Str, Table},
    utils::escape_str,
};

// canonical raw float bit
const CANONICAL_NAN_BITS: u64 = 0x7ff8000000000000u64;
const CANONICAL_ZERO_BITS: u64 = 0x0u64;

/// Enum of lucia function (Closure / Callback).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Collect)]
#[collect(no_drop)]
pub enum Function<'gc> {
    Closure(Closure<'gc>),
    Callback(AnyCallback<'gc>),
}

impl<'gc> From<Closure<'gc>> for Function<'gc> {
    fn from(closure: Closure<'gc>) -> Self {
        Self::Closure(closure)
    }
}

impl<'gc> From<AnyCallback<'gc>> for Function<'gc> {
    fn from(callback: AnyCallback<'gc>) -> Self {
        Self::Callback(callback)
    }
}

/// Enum of all lucia values.
#[derive(Debug, Copy, Clone, Collect, Default)]
#[collect(no_drop)]
pub enum Value<'gc> {
    /// `null` - A null value.
    #[default]
    Null,
    /// `bool` - A `true` / `false` value.
    Bool(bool),
    /// `int` - A 64-bit integer.
    Int(i64),
    /// `float` - A 64-bit floating point number.
    Float(f64),
    /// `str` - A UTF-8 string.
    Str(Str<'gc>),
    /// `table` - A table.
    Table(Table<'gc>),
    /// `function` - A function.
    Function(Function<'gc>),
    /// `userdata` - An UserData.
    UserData(AnyUserData<'gc>),
    /// `error` - An error.
    Error(GcError<'gc>),
}

impl<'gc> Value<'gc> {
    pub fn metatable(&self) -> Option<Table<'gc>> {
        match self {
            Self::Table(t) => t.metatable(),
            Self::UserData(u) => u.metatable(),
            _ => None,
        }
    }

    pub fn id(&self) -> Option<NonZeroUsize> {
        match self {
            Self::Null => None,
            Self::Bool(_) => None,
            Self::Int(_) => None,
            Self::Float(_) => None,
            Self::Str(v) => NonZeroUsize::new(Gc::as_ptr(v.0) as usize),
            Self::Table(v) => NonZeroUsize::new(Gc::as_ptr(v.0) as usize),
            Self::Function(Function::Closure(v)) => NonZeroUsize::new(Gc::as_ptr(v.0) as usize),
            Self::Function(Function::Callback(v)) => NonZeroUsize::new(v.as_ptr() as usize),
            Self::UserData(v) => NonZeroUsize::new(v.as_ptr() as usize),
            Self::Error(v) => NonZeroUsize::new(Gc::as_ptr(v.0) as usize),
        }
    }

    pub fn is(&self, other: &Value<'gc>) -> bool {
        match (self, other) {
            (Self::Null, Self::Null) => true,
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::Int(l0), Self::Int(r0)) => l0 == r0,
            (Self::Float(l0), Self::Float(r0)) => {
                if l0.is_nan() {
                    r0.is_nan()
                } else {
                    l0 == r0
                }
            }
            (Self::Str(l0), Self::Str(r0)) => Gc::ptr_eq(l0.0, r0.0),
            (Self::Table(l0), Self::Table(r0)) => Gc::ptr_eq(l0.0, r0.0),
            (Self::Function(Function::Closure(l0)), Self::Function(Function::Closure(r0))) => {
                Gc::ptr_eq(l0.0, r0.0)
            }
            (Self::Function(Function::Callback(l0)), Self::Function(Function::Callback(r0))) => {
                l0.as_ptr() == r0.as_ptr()
            }
            (Self::UserData(l0), Self::UserData(r0)) => l0.as_ptr() == r0.as_ptr(),
            (Self::Error(l0), Self::Error(r0)) => Gc::ptr_eq(l0.0, r0.0),
            _ => false,
        }
    }

    pub const fn value_type(self) -> ValueType {
        match self {
            Self::Null => ValueType::Null,
            Self::Bool(_) => ValueType::Bool,
            Self::Int(_) => ValueType::Int,
            Self::Float(_) => ValueType::Float,
            Self::Str(_) => ValueType::Str,
            Self::Table(_) => ValueType::Table,
            Self::Function(_) => ValueType::Function,
            Self::UserData(_) => ValueType::UserData,
            Self::Error(_) => ValueType::Error,
        }
    }

    pub fn is_null(self) -> bool {
        matches!(self, Self::Null)
    }

    pub fn repr(&self) -> String {
        if let Self::Str(s) = self {
            format!("\"{}\"", escape_str(s, false))
        } else if let Self::Table(t) = self {
            t.repr_table(self)
        } else {
            self.to_string()
        }
    }
}

impl<'gc> fmt::Display for Value<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Null => write!(f, "null"),
            Self::Bool(v) => write!(f, "{}", v),
            Self::Int(v) => write!(f, "{}", v),
            Self::Float(v) => write!(f, "{}", v),
            Self::Str(v) => write!(f, "{}", v),
            Self::Table(v) => write!(f, "<table {:p}>", v.0),
            Self::Function(Function::Closure(v)) => write!(f, "<function {:p}>", v.0),
            Self::Function(Function::Callback(v)) => write!(f, "<function {:p}>", v.as_ptr()),
            Self::UserData(v) => write!(f, "<userdata {:p}>", v.as_ptr()),
            Self::Error(e) => write!(f, "<error {}>", **e),
        }
    }
}

impl<'gc> PartialEq for Value<'gc> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Null, Self::Null) => true,
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::Int(l0), Self::Int(r0)) => l0 == r0,
            (Self::Float(l0), Self::Float(r0)) => {
                if l0.is_nan() {
                    r0.is_nan()
                } else {
                    l0 == r0
                }
            }
            (Self::Str(l0), Self::Str(r0)) => l0 == r0,
            (Self::Table(l0), Self::Table(r0)) => l0 == r0,
            (Self::Function(l0), Self::Function(r0)) => l0 == r0,
            (Self::UserData(l0), Self::UserData(r0)) => l0 == r0,
            (Self::Error(l0), Self::Error(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl<'gc> Eq for Value<'gc> {}

impl<'gc> Hash for Value<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Null => 0.hash(state),
            Self::Bool(v) => v.hash(state),
            Self::Int(v) => v.hash(state),
            Self::Float(v) => {
                if v.is_nan() {
                    CANONICAL_NAN_BITS.hash(state)
                } else if *v == 0.0f64 {
                    CANONICAL_ZERO_BITS.hash(state)
                } else {
                    (*v).to_bits().hash(state)
                }
            }
            Self::Str(v) => v.hash(state),
            Self::Table(v) => v.hash(state),
            Self::Function(v) => v.hash(state),
            Self::UserData(v) => v.hash(state),
            Self::Error(v) => v.hash(state),
        }
    }
}

/// The type of Value.
#[derive(Debug, Clone, Copy, Collect, PartialEq, Eq, Hash)]
#[collect[require_static]]
pub enum ValueType {
    Null,
    Bool,
    Int,
    Float,
    Str,
    Table,
    Function,
    UserData,
    Error,
}

impl ValueType {
    pub const fn name(self) -> &'static str {
        match self {
            ValueType::Null => "null",
            ValueType::Bool => "bool",
            ValueType::Int => "int",
            ValueType::Float => "float",
            ValueType::Str => "str",
            ValueType::Table => "table",
            ValueType::Function => "function",
            ValueType::UserData => "userdata",
            ValueType::Error => "error",
        }
    }
}

impl fmt::Display for ValueType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}
