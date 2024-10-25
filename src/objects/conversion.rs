use compact_str::CompactString;

use crate::{
    errors::{Error, ErrorKind},
    objects::{
        Callback, Closure, Function, Str, Table, TableEntries, TableState, UserData, Value,
        ValueType,
    },
    utils::Float,
    Context,
};

pub trait IntoValue<'gc> {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc>;
}

pub trait FromValue<'gc>: Sized {
    fn from_value(value: Value<'gc>) -> Result<Self, Error<'gc>>;
}

macro_rules! unexpected_type_error {
    ($expected:expr, $found:expr) => {
        Error::new(ErrorKind::UnexpectedType {
            expected: $expected,
            found: $found.value_type(),
        })
    };
}

macro_rules! impl_base_type {
    ($([$e:ident $t:ty]),* $(,)?) => {
        $(
            impl<'gc> From<$t> for Value<'gc> {
                fn from(v: $t) -> Value<'gc> {
                    Value::$e(v)
                }
            }

            impl<'gc> IntoValue<'gc> for $t {
                fn into_value(self, _: Context<'gc>) -> Value<'gc> {
                    Value::$e(self)
                }
            }

            impl<'gc> FromValue<'gc> for $t {
                fn from_value(value: Value<'gc>) -> Result<Self, Error<'gc>> {
                    if let Value::$e(v) = value {
                        Ok(v)
                    } else {
                        Err(unexpected_type_error!(ValueType::$e, value))
                    }
                }
            }
        )*
    };

    ($([Function $e:ident $t:ty]),* $(,)?) => {
        $(
            impl<'gc> From<$t> for Value<'gc> {
                fn from(v: $t) -> Value<'gc> {
                    Value::Function(Function::$e(v))
                }
            }

            impl<'gc> IntoValue<'gc> for $t {
                fn into_value(self, _: Context<'gc>) -> Value<'gc> {
                    Value::Function(Function::$e(self))
                }
            }

            impl<'gc> FromValue<'gc> for $t {
                fn from_value(value: Value<'gc>) -> Result<Self, Error<'gc>> {
                    if let Value::Function(Function::$e(v)) = value {
                        Ok(v)
                    } else {
                        Err(unexpected_type_error!(ValueType::Function, value))
                    }
                }
            }
        )*
    };
}
impl_base_type! {
    [Bool bool],
    [Int i64],
    [Float Float],
    [Str Str<'gc>],
    [Table Table<'gc>],
    [Function Function<'gc>],
    [UserData UserData<'gc>],
}
impl_base_type! {
    [Function Closure Closure<'gc>],
    [Function Callback Callback<'gc>],
}

macro_rules! impl_int_into {
    ($($i:ty),* $(,)?) => {
        $(
            impl<'gc> From<$i> for Value<'gc> {
                fn from(v: $i) -> Value<'gc> {
                    Value::Int(v.into())
                }
            }

            impl<'gc> IntoValue<'gc> for $i {
                fn into_value(self, _: Context<'gc>) -> Value<'gc> {
                    Value::Int(self.into())
                }
            }
        )*
    };
}
impl_int_into!(i8, u8, i16, u16, i32, u32);

macro_rules! impl_float_into {
    ($($i:ty),* $(,)?) => {
        $(
            impl<'gc> From<$i> for Value<'gc> {
                fn from(v: $i) -> Value<'gc> {
                    Value::Float(Float(v as f64))
                }
            }

            impl<'gc> IntoValue<'gc> for $i {
                fn into_value(self, _: Context<'gc>) -> Value<'gc> {
                    Value::Float(Float(self as f64))
                }
            }
        )*
    };
}
impl_float_into!(f32, f64);

impl<'gc, T: Into<Value<'gc>>> From<Option<T>> for Value<'gc> {
    fn from(value: Option<T>) -> Self {
        value.map_or(Value::Null, |v| v.into())
    }
}

impl<'gc> From<()> for Value<'gc> {
    fn from(_: ()) -> Self {
        Value::Null
    }
}

impl<'gc> IntoValue<'gc> for () {
    fn into_value(self, _: Context<'gc>) -> Value<'gc> {
        Value::Null
    }
}

impl<'gc> IntoValue<'gc> for Value<'gc> {
    fn into_value(self, _: Context<'gc>) -> Value<'gc> {
        self
    }
}

impl<'gc, T: IntoValue<'gc>> IntoValue<'gc> for Option<T> {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        match self {
            Some(t) => t.into_value(ctx),
            None => Value::Null,
        }
    }
}

impl<'a, 'gc, T> IntoValue<'gc> for &'a Option<T>
where
    &'a T: IntoValue<'gc>,
{
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        match self {
            Some(t) => t.into_value(ctx),
            None => Value::Null,
        }
    }
}

impl<'gc> IntoValue<'gc> for CompactString {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        Value::Str(Str::new(&ctx, self))
    }
}

impl<'gc> IntoValue<'gc> for &'static str {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        Value::Str(Str::new(&ctx, CompactString::const_new(self)))
    }
}

impl<'gc> IntoValue<'gc> for TableEntries<'gc> {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        Value::Table(Table::from(
            &ctx,
            TableState {
                entries: self,
                metatable: None,
            },
        ))
    }
}

impl<'gc, T: IntoValue<'gc>> IntoValue<'gc> for Vec<T> {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        TableEntries::from_iter(self.into_iter().map(|x| x.into_value(ctx))).into_value(ctx)
    }
}

impl<'gc, K: IntoValue<'gc>, V: IntoValue<'gc>> IntoValue<'gc> for Vec<(K, V)> {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        TableEntries::from_iter(
            self.into_iter()
                .map(|(k, v)| (k.into_value(ctx), v.into_value(ctx))),
        )
        .into_value(ctx)
    }
}

impl<'gc, const N: usize, T: IntoValue<'gc>> IntoValue<'gc> for [T; N] {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        Value::Table(Table::from(
            &ctx,
            TableState {
                entries: TableEntries::from_iter(self.into_iter().map(|x| x.into_value(ctx))),
                metatable: None,
            },
        ))
    }
}

impl<'gc, const N: usize, K: IntoValue<'gc>, V: IntoValue<'gc>> IntoValue<'gc> for [(K, V); N] {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        TableEntries::from_iter(
            self.into_iter()
                .map(|(k, v)| (k.into_value(ctx), v.into_value(ctx))),
        )
        .into_value(ctx)
    }
}

impl<'gc> FromValue<'gc> for Value<'gc> {
    fn from_value(value: Value<'gc>) -> Result<Self, Error<'gc>> {
        Ok(value)
    }
}

impl<'gc, T: FromValue<'gc>> FromValue<'gc> for Option<T> {
    fn from_value(value: Value<'gc>) -> Result<Self, Error<'gc>> {
        Ok(if value.is_null() {
            None
        } else {
            Some(T::from_value(value)?)
        })
    }
}

impl<'gc, T: FromValue<'gc>> FromValue<'gc> for Vec<T> {
    fn from_value(value: Value<'gc>) -> Result<Self, Error<'gc>> {
        if let Value::Table(table) = value {
            (0..=table.len())
                .map(|i| T::from_value(table.get_index(i).map_or(Value::Null, |x| x.1)))
                .collect()
        } else {
            Err(unexpected_type_error!(ValueType::Table, value))
        }
    }
}

macro_rules! impl_int_from {
    ($($i:ty),* $(,)?) => {
        $(
            impl<'gc> FromValue<'gc> for $i {
                fn from_value(value: Value<'gc>) -> Result<Self, Error<'gc>> {
                    if let Value::Int(i) = value {
                        if let Ok(i) = <$i>::try_from(i) {
                            Ok(i)
                        } else {
                            Err(unexpected_type_error!( ValueType::Int, value))
                        }
                    } else {
                        Err(unexpected_type_error!( ValueType::Int, value))
                    }
                }
            }
        )*
    };
}
impl_int_from!(u64, i32, u32, i16, u16, i8, u8, isize, usize);

macro_rules! impl_float_from {
    ($($f:ty),* $(,)?) => {
        $(
            impl<'gc> FromValue<'gc> for $f {
                fn from_value(value: Value<'gc>) -> Result<Self, Error<'gc>> {
                    if let Value::Float(v) = value {
                        Ok(v.0 as $f)
                    } else {
                        Err(unexpected_type_error!(ValueType::Float, value))
                    }
                }
            }
        )*
    };
}
impl_float_from!(f32, f64);
