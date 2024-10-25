use std::{fmt, num::NonZeroUsize};

use compact_str::{format_compact, CompactString, ToCompactString};
use gc_arena::{Collect, Gc};

use crate::{
    objects::{Function, Str, Table, UserData},
    utils::{escape_str, Float},
};

/// Enum of all lucia values.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Collect)]
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
    Float(Float),
    /// `str` - A UTF-8 string.
    Str(Str<'gc>),
    /// `table` - A table.
    Table(Table<'gc>),
    /// `function` - A function.
    Function(Function<'gc>),
    /// `userdata` - An UserData.
    UserData(UserData<'gc>),
}

impl<'gc> Value<'gc> {
    pub fn metatable(self) -> Option<Table<'gc>> {
        match self {
            Self::Table(t) => t.metatable(),
            Self::UserData(u) => u.metatable(),
            _ => None,
        }
    }

    pub fn id(self) -> Option<NonZeroUsize> {
        match self {
            Self::Null => None,
            Self::Bool(_) => None,
            Self::Int(_) => None,
            Self::Float(_) => None,
            Self::Str(v) => NonZeroUsize::new(Gc::as_ptr(v.into_inner()) as usize),
            Self::Table(v) => NonZeroUsize::new(Gc::as_ptr(v.into_inner()) as usize),
            Self::Function(Function::Closure(v)) => {
                NonZeroUsize::new(Gc::as_ptr(v.into_inner()) as usize)
            }
            Self::Function(Function::Callback(v)) => {
                NonZeroUsize::new(Gc::as_ptr(v.into_inner()) as usize)
            }
            Self::UserData(v) => NonZeroUsize::new(Gc::as_ptr(v.into_inner()) as usize),
        }
    }

    pub fn is(self, other: Value<'gc>) -> bool {
        match (self, other) {
            (Self::Null, Self::Null)
            | (Self::Bool(_), Self::Bool(_))
            | (Self::Int(_), Self::Int(_))
            | (Self::Float(_), Self::Float(_)) => self == other,
            _ => self.id() == other.id(),
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
        }
    }

    pub fn is_null(self) -> bool {
        matches!(self, Self::Null)
    }

    pub fn repr(self) -> CompactString {
        if let Self::Str(s) = self {
            format_compact!("\"{}\"", escape_str(&s, false))
        } else if let Self::Table(t) = self {
            t.repr_table(self)
        } else {
            self.to_compact_string()
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
            Self::Table(v) => write!(f, "<table {:p}>", v.into_inner()),
            Self::Function(Function::Closure(v)) => write!(f, "<function {:p}>", v.into_inner()),
            Self::Function(Function::Callback(v)) => {
                write!(f, "<function {:p}>", Gc::as_ptr(v.into_inner()))
            }
            Self::UserData(v) => write!(f, "<userdata {:p}>", Gc::as_ptr(v.into_inner())),
        }
    }
}

/// The type of Value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Collect)]
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
        }
    }
}

impl fmt::Display for ValueType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

/// A [`Value`] that is not bound to the GC context.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub enum ExternValue {
    #[default]
    Null,
    Bool(bool),
    Int(i64),
    Float(Float),
    Str(CompactString),
    Table(*const ()),
    Function(*const ()),
    UserData(*const ()),
}

impl From<Value<'_>> for ExternValue {
    fn from(value: Value<'_>) -> Self {
        match value {
            Value::Null => ExternValue::Null,
            Value::Bool(v) => ExternValue::Bool(v),
            Value::Int(v) => ExternValue::Int(v),
            Value::Float(v) => ExternValue::Float(v),
            Value::Str(v) => ExternValue::Str(v.to_compact_string()),
            Value::Table(v) => ExternValue::Table(Gc::as_ptr(v.into_inner()) as _),
            Value::Function(Function::Closure(v)) => {
                ExternValue::Function(Gc::as_ptr(v.into_inner()) as _)
            }
            Value::Function(Function::Callback(v)) => {
                ExternValue::Function(Gc::as_ptr(v.into_inner()) as _)
            }
            Value::UserData(v) => ExternValue::UserData(Gc::as_ptr(v.into_inner()) as _),
        }
    }
}

impl fmt::Display for ExternValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExternValue::Null => write!(f, "null"),
            ExternValue::Bool(v) => write!(f, "{}", v),
            ExternValue::Int(v) => write!(f, "{}", v),
            ExternValue::Float(v) => write!(f, "{}", v),
            ExternValue::Str(v) => write!(f, "{}", v),
            ExternValue::Table(v) => write!(f, "<table {:p}>", v),
            ExternValue::Function(v) => write!(f, "<function {:p}>", v),
            ExternValue::UserData(v) => write!(f, "<userdata {:p}>", v),
        }
    }
}
