use compact_str::{CompactString, ToCompactString};

use crate::{
    Context,
    errors::{Error, RuntimeError},
    objects::{
        Callback, Closure, Function, Str, Table, TableEntries, TableState, UserData, Value,
        ValueType,
    },
    utils::Float,
};

pub trait IntoValue<'gc> {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc>;
}

macro_rules! impl_int_into {
    ($($i:ty),* $(,)?) => {
        $(
            impl<'gc> From<$i> for Value<'gc> {
                fn from(v: $i) -> Value<'gc> {
                    Value::Int(v.into())
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
        )*
    };
}
impl_float_into!(f32, f64);

macro_rules! impl_function_into {
    ($($i:ty),* $(,)?) => {
        $(
            impl<'gc> From<$i> for Value<'gc> {
                fn from(v: $i) -> Value<'gc> {
                    Value::Function(v.into())
                }
            }
        )*
    };
}
impl_function_into!(Closure<'gc>, Callback<'gc>);

macro_rules! impl_into {
    ($($i:ty),* $(,)?) => {
        $(
            impl<'gc> IntoValue<'gc> for $i {
                fn into_value(self, _: Context<'gc>) -> Value<'gc> {
                    self.into()
                }
            }

            impl<'a, 'gc> IntoValue<'gc> for &'a $i {
                fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
                    (*self).into_value(ctx)
                }
            }
        )*
    };
}
impl_into!(
    (),
    bool,
    i8,
    u8,
    i16,
    u16,
    i32,
    u32,
    i64,
    f32,
    f64,
    Float,
    Str<'gc>,
    Table<'gc>,
    Closure<'gc>,
    Callback<'gc>,
    Function<'gc>,
    UserData<'gc>,
    Value<'gc>,
);

impl<'gc> IntoValue<'gc> for &'static str {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        Value::Str(Str::new(&ctx, CompactString::const_new(self)))
    }
}

impl<'gc> IntoValue<'gc> for CompactString {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        Value::Str(Str::new(&ctx, self))
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

impl<'gc, 'a, T> IntoValue<'gc> for &'a [T]
where
    &'a T: IntoValue<'gc>,
{
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        TableEntries::from_iter(self.iter().map(|x| x.into_value(ctx))).into_value(ctx)
    }
}

impl<'gc, T, const N: usize> IntoValue<'gc> for [T; N]
where
    T: IntoValue<'gc>,
{
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        TableEntries::from_iter(self.into_iter().map(|x| x.into_value(ctx))).into_value(ctx)
    }
}

impl<'gc, K, V> IntoValue<'gc> for Vec<(K, V)>
where
    K: IntoValue<'gc>,
    V: IntoValue<'gc>,
{
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        TableEntries::from_iter(
            self.into_iter()
                .map(|(k, v)| (k.into_value(ctx), v.into_value(ctx))),
        )
        .into_value(ctx)
    }
}

impl<'gc, 'a, K, V> IntoValue<'gc> for &'a [(K, V)]
where
    &'a K: IntoValue<'gc>,
    &'a V: IntoValue<'gc>,
{
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        TableEntries::from_iter(
            self.iter()
                .map(|(k, v)| (k.into_value(ctx), v.into_value(ctx))),
        )
        .into_value(ctx)
    }
}

impl<'gc, K, V, const N: usize> IntoValue<'gc> for [(K, V); N]
where
    K: IntoValue<'gc>,
    V: IntoValue<'gc>,
{
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        TableEntries::from_iter(
            self.into_iter()
                .map(|(k, v)| (k.into_value(ctx), v.into_value(ctx))),
        )
        .into_value(ctx)
    }
}

pub trait FromValue<'gc>: Sized {
    fn from_value(value: Value<'gc>) -> Result<Self, Error<'gc>>;
}

macro_rules! unexpected_type_error {
    ($expected:expr, $found:expr) => {
        Error::new(RuntimeError::UnexpectedType {
            expected: $expected,
            found: $found.value_type(),
        })
    };
}

macro_rules! impl_from {
    ($([$e:ident $t:ty]),* $(,)?) => {
        $(
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
impl_from! {
    [Bool bool],
    [Int i64],
    [Float Float],
    [Str Str<'gc>],
    [Table Table<'gc>],
    [Function Function<'gc>],
    [UserData UserData<'gc>],
}
impl_from! {
    [Function Closure Closure<'gc>],
    [Function Callback Callback<'gc>],
}

impl<'gc> FromValue<'gc> for Value<'gc> {
    fn from_value(value: Value<'gc>) -> Result<Self, Error<'gc>> {
        Ok(value)
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

impl<'gc> FromValue<'gc> for CompactString {
    fn from_value(value: Value<'gc>) -> Result<Self, Error<'gc>> {
        if let Value::Str(v) = value {
            Ok(v.to_compact_string())
        } else {
            Err(unexpected_type_error!(ValueType::Float, value))
        }
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
