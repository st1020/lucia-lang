use std::ops::{Add, Div, Mul, Rem, Sub};

use crate::{
    Context,
    compiler::value::MetaMethod,
    errors::Error,
    objects::{FromValue, Value, ValueType, impl_metamethod, unexpected_type_error},
    utils::Float,
};

impl MetaMethod<&Context> for Float {
    impl_metamethod!(Float);

    #[inline]
    fn meta_bool(self, _: &Context) -> Result<Self::Result1, Self::Error> {
        Ok((self.0 != 0.0).into())
    }

    #[expect(clippy::as_conversions, clippy::cast_possible_truncation)]
    #[inline]
    fn meta_int(self, _: &Context) -> Result<Self::Result1, Self::Error> {
        Ok((self.0 as i64).into())
    }

    #[inline]
    fn meta_float(self, _: &Context) -> Result<Self::Result1, Self::Error> {
        Ok(self.into())
    }

    impl_metamethod!(Float, str);
    impl_metamethod!(Float, repr);

    impl_metamethod!(Float, neg);
    impl_metamethod!(Float, arithmetic, Add, meta_add, add);
    impl_metamethod!(Float, arithmetic, Sub, meta_sub, sub);
    impl_metamethod!(Float, arithmetic, Mul, meta_mul, mul);
    impl_metamethod!(Float, arithmetic, Div, meta_div, div);
    impl_metamethod!(Float, arithmetic, Rem, meta_rem, rem);

    impl_metamethod!(Float, eq_ne);
    impl_metamethod!(Float, compare, Gt, meta_gt, gt);
    impl_metamethod!(Float, compare, Ge, meta_ge, ge);
    impl_metamethod!(Float, compare, Lt, meta_lt, lt);
    impl_metamethod!(Float, compare, Le, meta_le, le);
}

macro_rules! impl_conversion {
    ($($i:ty),*) => {
        $(
            impl From<$i> for Value {
                fn from(value: $i) -> Value {
                    Value::Float(Float(f64::from(value)))
                }
            }

            #[allow(clippy::cast_possible_truncation, clippy::allow_attributes)]
            impl FromValue for $i {
                fn from_value(value: Value) -> Result<Self, Error> {
                    if let Value::Float(v) = value {
                        Ok(v.0 as $i)
                    } else {
                        Err(unexpected_type_error!(ValueType::Float, value))
                    }
                }
            }
        )*
    };
}
impl_conversion!(f32, f64);
