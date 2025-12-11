use crate::{
    Context,
    compiler::value::{MetaMethod, MetaName},
    errors::Error,
    objects::{FromValue, Value, ValueType, impl_metamethod, unexpected_type_error},
};

impl MetaMethod<&Context> for i64 {
    impl_metamethod!(Int);

    #[inline]
    fn meta_bool(self, _: &Context) -> Result<Self::Result1, Self::Error> {
        Ok((self != 0).into())
    }

    #[inline]
    fn meta_int(self, _: &Context) -> Result<Self::Result1, Self::Error> {
        Ok(self.into())
    }

    #[expect(clippy::as_conversions, clippy::cast_precision_loss)]
    #[inline]
    fn meta_float(self, _: &Context) -> Result<Self::Result1, Self::Error> {
        Ok((self as f64).into())
    }

    impl_metamethod!(Int, str);
    impl_metamethod!(Int, repr);

    impl_metamethod!(Int, neg);
    impl_metamethod!(Int, arithmetic, Add, meta_add, wrapping_add);
    impl_metamethod!(Int, arithmetic, Sub, meta_sub, wrapping_sub);
    impl_metamethod!(Int, arithmetic, Mul, meta_mul, wrapping_mul);

    #[inline]
    fn meta_div(self, ctx: &Context, other: Self::Value) -> Result<Self::Result2, Self::Error> {
        if let Value::Int(other) = other {
            if other == 0 {
                return Err(Error::DivideByZero { value: self });
            }
            Ok(self.wrapping_div(other).into())
        } else {
            Err(self.meta_error(ctx, MetaName::Div, vec![other]))
        }
    }

    #[inline]
    fn meta_rem(self, ctx: &Context, other: Self::Value) -> Result<Self::Result2, Self::Error> {
        if let Value::Int(other) = other {
            if other == 0 {
                return Err(Error::DivideByZero { value: self });
            }
            Ok(self.wrapping_rem(other).into())
        } else {
            Err(self.meta_error(ctx, MetaName::Rem, vec![other]))
        }
    }

    impl_metamethod!(Int, eq_ne);
    impl_metamethod!(Int, compare, Gt, meta_gt, gt);
    impl_metamethod!(Int, compare, Ge, meta_ge, ge);
    impl_metamethod!(Int, compare, Lt, meta_lt, lt);
    impl_metamethod!(Int, compare, Le, meta_le, le);
}

macro_rules! impl_conversion {
    ($($i:ty),*) => {
        $(
            impl From<$i> for Value {
                fn from(value: $i) -> Value {
                    Value::Int(value.try_into().unwrap_or(i64::MAX))
                }
            }

            impl FromValue for $i {
                fn from_value(value: Value) -> Result<Self, Error> {
                    if let Value::Int(i) = value {
                        if let Ok(i) = <$i>::try_from(i) {
                            Ok(i)
                        } else {
                            Err(unexpected_type_error!(ValueType::Int, value))
                        }
                    } else {
                        Err(unexpected_type_error!(ValueType::Int, value))
                    }
                }
            }
        )*
    };
}
impl_conversion!(i16, u16, i32, u32, u64, isize, usize);
