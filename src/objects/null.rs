use crate::{
    Context,
    compiler::value::MetaMethod,
    errors::Error,
    objects::{FromValue, Value, ValueType, impl_metamethod, unexpected_type_error},
};

impl MetaMethod<&Context> for () {
    impl_metamethod!(Null);

    #[inline]
    fn meta_bool(self, _: &Context) -> Result<Self::Result1, Self::Error> {
        Ok(false.into())
    }

    #[inline]
    fn meta_int(self, _: &Context) -> Result<Self::Result1, Self::Error> {
        Ok(0.into())
    }

    #[inline]
    fn meta_float(self, _: &Context) -> Result<Self::Result1, Self::Error> {
        Ok(0.0.into())
    }

    #[inline]
    fn meta_str(self, _: &Context) -> Result<Self::Result1, Self::Error> {
        Ok("null".into())
    }

    #[inline]
    fn meta_repr(self, ctx: &Context) -> Result<Self::Result1, Self::Error> {
        self.meta_str(ctx)
    }

    #[inline]
    fn meta_eq(self, _: &Context, other: Self::Value) -> Result<Self::Result2, Self::Error> {
        Ok(matches!(other, Value::Null).into())
    }

    #[inline]
    fn meta_ne(self, _: &Context, other: Self::Value) -> Result<Self::Result2, Self::Error> {
        Ok((!matches!(other, Value::Null)).into())
    }
}

impl From<()> for Value {
    fn from((): ()) -> Self {
        Value::Null
    }
}

impl FromValue for () {
    fn from_value(value: Value) -> Result<Self, Error> {
        if value.is_null() {
            Ok(())
        } else {
            Err(unexpected_type_error!(ValueType::Null, value))
        }
    }
}

impl<T: Into<Value>> From<Option<T>> for Value {
    fn from(value: Option<T>) -> Self {
        match value {
            Some(t) => t.into(),
            None => Value::Null,
        }
    }
}

impl<T: FromValue> FromValue for Option<T> {
    fn from_value(value: Value) -> Result<Self, Error> {
        Ok(if value.is_null() {
            None
        } else {
            Some(T::from_value(value)?)
        })
    }
}
