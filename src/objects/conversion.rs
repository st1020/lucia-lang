use crate::{
    errors::{Error, ErrorKind},
    objects::{
        AnyCallback, Closure, Function, GcError, Str, Table, TableEntries, TableState, Value,
        ValueType,
    },
    Context,
};

impl<'gc> From<bool> for Value<'gc> {
    fn from(v: bool) -> Value<'gc> {
        Value::Bool(v)
    }
}

impl<'gc> From<i64> for Value<'gc> {
    fn from(v: i64) -> Value<'gc> {
        Value::Int(v)
    }
}

impl<'gc> From<f64> for Value<'gc> {
    fn from(v: f64) -> Value<'gc> {
        Value::Float(v)
    }
}

impl<'gc> From<Str<'gc>> for Value<'gc> {
    fn from(v: Str<'gc>) -> Value<'gc> {
        Value::Str(v)
    }
}

impl<'gc> From<Table<'gc>> for Value<'gc> {
    fn from(v: Table<'gc>) -> Value<'gc> {
        Value::Table(v)
    }
}

impl<'gc> From<Function<'gc>> for Value<'gc> {
    fn from(v: Function<'gc>) -> Value<'gc> {
        Value::Function(v)
    }
}

impl<'gc> From<Closure<'gc>> for Value<'gc> {
    fn from(v: Closure<'gc>) -> Value<'gc> {
        Value::Function(Function::Closure(v))
    }
}

impl<'gc> From<AnyCallback<'gc>> for Value<'gc> {
    fn from(v: AnyCallback<'gc>) -> Value<'gc> {
        Value::Function(Function::Callback(v))
    }
}

pub trait IntoValue<'gc> {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc>;
}

impl<'gc, T: Into<Value<'gc>>> IntoValue<'gc> for T {
    fn into_value(self, _ctx: Context<'gc>) -> Value<'gc> {
        self.into()
    }
}

impl<'gc> IntoValue<'gc> for &'static str {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        Value::Str(Str::new(&ctx, self.to_string()))
    }
}

impl<'gc> IntoValue<'gc> for String {
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

impl<'gc> IntoValue<'gc> for Error<'gc> {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        Value::Error(GcError::new(&ctx, self))
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

macro_rules! unexpected_type_error {
    ($expected:expr, $found:expr) => {
        Error::new(ErrorKind::UnexpectedType {
            expected: $expected,
            found: $found.value_type(),
        })
    };
}

pub trait FromValue<'gc>: Sized {
    fn from_value(value: Value<'gc>) -> Result<Self, Error<'gc>>;
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
            (1..=table.len())
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
impl_int_from!(i64, u64, i32, u32, i16, u16, i8, u8, isize, usize);

macro_rules! impl_float_from {
    ($($f:ty),* $(,)?) => {
        $(
            impl<'gc> FromValue<'gc> for $f {
                fn from_value(value: Value<'gc>) -> Result<Self, Error<'gc>> {
                    if let Value::Float(v) = value {
                        Ok(v as $f)
                    } else {
                        Err(unexpected_type_error!(ValueType::Float, value))
                    }
                }
            }
        )*
    };
}
impl_float_from!(f32, f64);

macro_rules! impl_from {
    ($([$e:ident $t:ty]),* $(,)?) => {
        $(
            impl<'gc> FromValue<'gc> for $t {
                fn from_value( value: Value<'gc>) -> Result<Self, Error<'gc>> {
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
impl_from! {
    [Bool bool],
    [Str Str<'gc>],
    [Table Table<'gc>],
    [Function Function<'gc>],
}

impl<'gc> FromValue<'gc> for Closure<'gc> {
    fn from_value(value: Value<'gc>) -> Result<Self, Error<'gc>> {
        match value {
            Value::Function(Function::Closure(c)) => Ok(c),
            _ => Err(unexpected_type_error!(ValueType::Function, value)),
        }
    }
}

impl<'gc> FromValue<'gc> for AnyCallback<'gc> {
    fn from_value(value: Value<'gc>) -> Result<Self, Error<'gc>> {
        match value {
            Value::Function(Function::Callback(c)) => Ok(c),
            _ => Err(unexpected_type_error!(ValueType::Function, value)),
        }
    }
}

impl<'gc> From<Value<'gc>> for bool {
    fn from(value: Value) -> Self {
        match value {
            Value::Null => false,
            Value::Bool(v) => v,
            Value::Int(v) => v != 0,
            Value::Float(v) => v != 0.0,
            _ => true,
        }
    }
}
