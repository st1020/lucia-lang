use std::{fmt, num::NonZeroUsize};

use gc_arena::{Collect, Gc};

use crate::{
    objects::{Callback, Closure, Str, Table, UserData},
    utils::{escape_str, Float},
};

/// Enum of lucia function (Closure / Callback).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Collect)]
#[collect(no_drop)]
pub enum Function<'gc> {
    Closure(Closure<'gc>),
    Callback(Callback<'gc>),
}

impl<'gc> From<Closure<'gc>> for Function<'gc> {
    fn from(closure: Closure<'gc>) -> Self {
        Self::Closure(closure)
    }
}

impl<'gc> From<Callback<'gc>> for Function<'gc> {
    fn from(callback: Callback<'gc>) -> Self {
        Self::Callback(callback)
    }
}

/// Enum of all lucia values.
#[derive(Debug, Copy, Clone, Collect, Default, PartialEq, Eq, Hash)]
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

    pub fn is(&self, other: &Value<'gc>) -> bool {
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
