use std::{num::NonZeroUsize, rc::Rc};

use derive_more::{Display, From, IsVariant};

use crate::{
    Context,
    compiler::value::{MetaMethod, MetaName},
    errors::Error,
    objects::{Bytes, Effect, Function, Str, Table, UserData, unexpected_type_error},
    utils::Float,
};

pub use crate::compiler::value::ValueType;

/// Enum of all lucia values.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default, From, Display, IsVariant)]
pub enum Value {
    /// `null` - A null value.
    #[default]
    #[display("null")]
    Null,
    /// `bool` - A `true` / `false` value.
    Bool(bool),
    /// `int` - A 64-bit integer.
    Int(i64),
    /// `float` - A 64-bit floating point number.
    Float(Float),
    /// `str` - A UTF-8 string.
    Str(Str),
    /// `bytes` - A byte array.
    Bytes(Bytes),
    /// `table` - A table.
    Table(Table),
    /// `function` - A function.
    Function(Function),
    /// `effect` - An effect.
    Effect(Effect),
    /// `userdata` - An UserData.
    UserData(UserData),
}

impl Value {
    pub fn metatable(self) -> Option<Table> {
        #[expect(clippy::wildcard_enum_match_arm)]
        match self {
            Self::Table(t) => t.metatable(),
            Self::UserData(u) => u.metatable(),
            _ => None,
        }
    }

    #[expect(clippy::as_conversions)]
    pub fn id(&self) -> Option<NonZeroUsize> {
        match self {
            Self::Null | Self::Bool(_) | Self::Int(_) | Self::Float(_) => None,
            Self::Str(v) => NonZeroUsize::new(Rc::as_ptr(v).cast::<()>() as usize),
            Self::Bytes(v) => NonZeroUsize::new(Rc::as_ptr(v).cast::<()>() as usize),
            Self::Table(v) => NonZeroUsize::new(Rc::as_ptr(v) as usize),
            Self::Function(v) => NonZeroUsize::new(v.const_ptr() as usize),
            Self::Effect(v) => NonZeroUsize::new(Rc::as_ptr(v) as usize),
            Self::UserData(v) => NonZeroUsize::new(Rc::as_ptr(v) as usize),
        }
    }

    pub fn identical(&self, other: &Value) -> bool {
        if let (Some(this), Some(other)) = (self.id(), other.id()) {
            this == other
        } else {
            self == other
        }
    }

    pub const fn value_type(&self) -> ValueType {
        match self {
            Self::Null => ValueType::Null,
            Self::Bool(_) => ValueType::Bool,
            Self::Int(_) => ValueType::Int,
            Self::Float(_) => ValueType::Float,
            Self::Str(_) => ValueType::Str,
            Self::Bytes(_) => ValueType::Bytes,
            Self::Table(_) => ValueType::Table,
            Self::Function(_) => ValueType::Function,
            Self::Effect(_) => ValueType::Effect,
            Self::UserData(_) => ValueType::UserData,
        }
    }
}

macro_rules! value_enum_dispatch {
    ($value:expr, $path:ident($($arg:expr),*)) => {
        match $value {
            Value::Null => ().$path($($arg),*),
            Value::Bool(v) => v.$path($($arg),*),
            Value::Int(v) => v.$path($($arg),*),
            Value::Float(v) => v.$path($($arg),*),
            Value::Str(v) => v.$path($($arg),*),
            Value::Bytes(v) => v.$path($($arg),*),
            Value::Table(v) => v.$path($($arg),*),
            Value::Function(v) => v.$path($($arg),*),
            Value::Effect(v) => v.$path($($arg),*),
            Value::UserData(v) => v.$path($($arg),*),
        }
    };
}

macro_rules! value_enum_dispatch_meta_method {
    (1, $($name:ident),*) => {
        $(
            #[inline]
            fn $name(self, ctx: &Context) -> Result<Self::Result1, Self::Error> {
                value_enum_dispatch!(self, $name(ctx))
            }
        )*
    };
    (2, $($name:ident),*) => {
        $(
            #[inline]
            fn $name(
                self,
                ctx: &Context,
                other: Self::Value,
            ) -> Result<Self::Result2, Self::Error> {
                value_enum_dispatch!(self, $name(ctx, other))
            }
        )*
    };
    (3, $($name:ident),*) => {
        $(
            #[inline]
            fn $name(
                self,
                ctx: &Context,
                key: Self::Value,
                value: Self::Value,
            ) -> Result<Self::Result3, Self::Error> {
                value_enum_dispatch!(self, $name(ctx, key, value))
            }
        )*
    };
}

impl MetaMethod<&Context> for Value {
    type Value = Value;
    type Error = Error;
    type ResultCall = Function;
    type ResultIter = Self::ResultCall;
    type Result1 = MetaResult<1>;
    type Result2 = MetaResult<2>;
    type Result3 = MetaResult<3>;

    #[inline]
    fn meta_call(self, ctx: &Context) -> Result<Self::ResultCall, Self::Error> {
        value_enum_dispatch!(self, meta_call(ctx))
    }

    #[inline]
    fn meta_iter(self, ctx: &Context) -> Result<Self::ResultIter, Self::Error> {
        value_enum_dispatch!(self, meta_iter(ctx))
    }

    value_enum_dispatch_meta_method!(
        1, meta_len, meta_bool, meta_int, meta_float, meta_str, meta_repr, meta_neg
    );

    value_enum_dispatch_meta_method!(
        2,
        meta_add,
        meta_sub,
        meta_mul,
        meta_div,
        meta_rem,
        meta_eq,
        meta_ne,
        meta_gt,
        meta_ge,
        meta_lt,
        meta_le,
        meta_get_attr,
        meta_get_item
    );

    value_enum_dispatch_meta_method!(3, meta_set_attr, meta_set_item);

    #[inline]
    fn meta_error(self, _: &Context, operator: MetaName, args: Vec<Self::Value>) -> Self::Error {
        if args.is_empty() {
            Error::MetaUnOperator {
                operator,
                operand: self.value_type(),
            }
        } else {
            Error::MetaBinOperator {
                operator,
                operand: (self.value_type(), args[0].value_type()),
            }
        }
    }
}

pub trait FromValue: Sized {
    fn from_value(value: Value) -> Result<Self, Error>;
}

impl FromValue for Value {
    fn from_value(value: Value) -> Result<Self, Error> {
        Ok(value)
    }
}

macro_rules! impl_from_value {
    ($($e:ident($t:ty)),* $(,)?) => {
        $(
            impl FromValue for $t {
                fn from_value(value: Value) -> Result<Self, Error> {
                    if let Value::$e(v) = value {
                        Ok(v)
                    } else {
                        Err(unexpected_type_error!(ValueType::$e, value))
                    }
                }
            }
        )*
    };
}
impl_from_value! {
    Bool(bool),
    Int(i64),
    Float(Float),
    Str(Str),
    Bytes(Bytes),
    Table(Table),
    Function(Function),
    UserData(UserData),
}

#[derive(Debug, Clone)]
pub enum MetaResult<const N: usize> {
    Value(Value),
    TailCall(Function, [Value; N]),
    TailEffect(Effect, Vec<Value>),
}

impl<T: Into<Value>, const N: usize> From<T> for MetaResult<N> {
    fn from(value: T) -> Self {
        MetaResult::Value(value.into())
    }
}

#[cfg(test)]
mod tests {
    use std::mem::size_of;

    use super::*;

    #[test]
    fn value_size() {
        assert!(size_of::<Value>() <= 2 * size_of::<usize>());
    }
}
