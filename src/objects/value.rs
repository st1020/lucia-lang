use std::{fmt, num::NonZeroUsize};

use compact_str::{CompactString, ToCompactString};
use gc_arena::{Collect, Gc, static_collect};

use crate::{
    Context,
    compiler::value::{MetaMethod, MetaName},
    errors::{Error, RuntimeError},
    objects::{Bytes, Function, IntoValue, Str, Table, UserData},
    utils::{Float, impl_enum_from},
};

pub use crate::compiler::value::ValueType;

static_collect!(ValueType);

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
    /// `bytes` - A byte array.
    Bytes(Bytes<'gc>),
    /// `table` - A table.
    Table(Table<'gc>),
    /// `function` - A function.
    Function(Function<'gc>),
    /// `userdata` - An UserData.
    UserData(UserData<'gc>),
}

impl From<()> for Value<'_> {
    fn from(_: ()) -> Self {
        Value::Null
    }
}

impl_enum_from!(Value<'gc>, {
    Bool(bool),
    Int(i64),
    Float(Float),
    Str(Str<'gc>),
    Bytes(Bytes<'gc>),
    Table(Table<'gc>),
    Function(Function<'gc>),
    UserData(UserData<'gc>),
});

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
            Self::Bytes(v) => NonZeroUsize::new(Gc::as_ptr(v.into_inner()) as usize),
            Self::Table(v) => NonZeroUsize::new(Gc::as_ptr(v.into_inner()) as usize),
            Self::Function(v) => NonZeroUsize::new(v.const_ptr() as usize),
            Self::UserData(v) => NonZeroUsize::new(Gc::as_ptr(v.into_inner()) as usize),
        }
    }

    pub fn identical(self, other: Value<'gc>) -> bool {
        if let (Some(this), Some(other)) = (self.id(), other.id()) {
            this == other
        } else {
            self == other
        }
    }

    pub const fn value_type(self) -> ValueType {
        match self {
            Self::Null => ValueType::Null,
            Self::Bool(_) => ValueType::Bool,
            Self::Int(_) => ValueType::Int,
            Self::Float(_) => ValueType::Float,
            Self::Str(_) => ValueType::Str,
            Self::Bytes(_) => ValueType::Bytes,
            Self::Table(_) => ValueType::Table,
            Self::Function(_) => ValueType::Function,
            Self::UserData(_) => ValueType::UserData,
        }
    }

    pub fn is_null(self) -> bool {
        matches!(self, Self::Null)
    }
}

impl fmt::Display for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Null => write!(f, "null"),
            Self::Bool(v) => write!(f, "{v}"),
            Self::Int(v) => write!(f, "{v}"),
            Self::Float(v) => write!(f, "{v}"),
            Self::Str(v) => write!(f, "{v}"),
            Self::Bytes(v) => write!(f, "{v}"),
            Self::Table(v) => write!(f, "{v}"),
            Self::Function(v) => write!(f, "{v}"),
            Self::UserData(v) => write!(f, "{v}"),
        }
    }
}

static_collect!(MetaName);

impl<'gc> IntoValue<'gc> for MetaName {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        self.name().into_value(ctx)
    }
}

#[derive(Debug, Clone, Copy, Collect)]
#[collect(no_drop)]
pub enum MetaResult<'gc, const N: usize> {
    Value(Value<'gc>),
    Call(Function<'gc>, [Value<'gc>; N]),
}

pub(crate) trait IntoMetaResult<'gc, const N: usize> {
    fn into_meta_result(self, ctx: Context<'gc>) -> MetaResult<'gc, N>;
}

impl<'gc, T: IntoValue<'gc>, const N: usize> IntoMetaResult<'gc, N> for T {
    fn into_meta_result(self, ctx: Context<'gc>) -> MetaResult<'gc, N> {
        MetaResult::Value(self.into_value(ctx))
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
            Value::UserData(v) => v.$path($($arg),*),
        }
    };
}

macro_rules! value_enum_dispatch_meta_method {
    (1, $($name:ident),*) => {
        $(
            fn $name(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
                value_enum_dispatch!(self, $name(ctx))
            }
        )*
    };
    (2, $($name:ident),*) => {
        $(
            fn $name(
                &self,
                ctx: Context<'gc>,
                other: Self::Value,
            ) -> Result<Self::Result2, Self::Error> {
                value_enum_dispatch!(self, $name(ctx, other))
            }
        )*
    };
    (3, $($name:ident),*) => {
        $(
            fn $name(
                &self,
                ctx: Context<'gc>,
                key: Self::Value,
                value: Self::Value,
            ) -> Result<Self::Result3, Self::Error> {
                value_enum_dispatch!(self, $name(ctx, key, value))
            }
        )*
    };
}

impl<'gc> MetaMethod<Context<'gc>> for Value<'gc> {
    type Value = Value<'gc>;
    type Error = Error<'gc>;
    type ResultCall = Function<'gc>;
    type ResultIter = Self::ResultCall;
    type Result1 = MetaResult<'gc, 1>;
    type Result2 = MetaResult<'gc, 2>;
    type Result3 = MetaResult<'gc, 3>;

    fn meta_call(&self, ctx: Context<'gc>) -> Result<Self::ResultCall, Self::Error> {
        value_enum_dispatch!(self, meta_call(ctx))
    }
    fn meta_iter(&self, ctx: Context<'gc>) -> Result<Self::ResultIter, Self::Error> {
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

    fn meta_error(
        &self,
        _: Context<'gc>,
        operator: MetaName,
        args: Vec<Self::Value>,
    ) -> Self::Error {
        if args.is_empty() {
            Error::new(RuntimeError::MetaUnOperator {
                operator,
                operand: self.value_type(),
            })
        } else {
            Error::new(RuntimeError::MetaBinOperator {
                operator,
                operand: (self.value_type(), args[0].value_type()),
            })
        }
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
    Bytes(Vec<u8>),
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
            Value::Bytes(v) => ExternValue::Bytes(v.to_vec()),
            Value::Table(v) => ExternValue::Table(Gc::as_ptr(v.into_inner()) as _),
            Value::Function(v) => ExternValue::Function(v.const_ptr()),
            Value::UserData(v) => ExternValue::UserData(Gc::as_ptr(v.into_inner()) as _),
        }
    }
}

impl fmt::Display for ExternValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExternValue::Null => write!(f, "null"),
            ExternValue::Bool(v) => write!(f, "{v}"),
            ExternValue::Int(v) => write!(f, "{v}"),
            ExternValue::Float(v) => write!(f, "{v}"),
            ExternValue::Str(v) => write!(f, "{v}"),
            ExternValue::Bytes(v) => write!(f, "{:?}", v),
            ExternValue::Table(v) => write!(f, "<table {v:p}>"),
            ExternValue::Function(v) => write!(f, "<function {v:p}>"),
            ExternValue::UserData(v) => write!(f, "<userdata {v:p}>"),
        }
    }
}

// SAFETY: The pointers in `ExternValue` are not actually dereferenced at all, they are purely
// informational.
unsafe impl Send for ExternValue {}
unsafe impl Sync for ExternValue {}
