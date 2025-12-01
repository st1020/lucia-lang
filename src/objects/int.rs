use crate::{
    Context,
    compiler::value::{MetaMethod, MetaName},
    errors::{Error, RuntimeError},
    objects::{IntoMetaResult, Value, impl_metamethod},
};

impl<'gc> MetaMethod<Context<'gc>> for i64 {
    impl_metamethod!(Int);

    fn meta_bool(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok((*self != 0).into_meta_result(ctx))
    }

    fn meta_int(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok(self.into_meta_result(ctx))
    }

    #[expect(clippy::as_conversions, clippy::cast_precision_loss)]
    fn meta_float(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok((*self as f64).into_meta_result(ctx))
    }

    impl_metamethod!(Int, str);
    impl_metamethod!(Int, repr);

    impl_metamethod!(Int, neg);
    impl_metamethod!(Int, arithmetic, Add, meta_add, wrapping_add);
    impl_metamethod!(Int, arithmetic, Sub, meta_sub, wrapping_sub);
    impl_metamethod!(Int, arithmetic, Mul, meta_mul, wrapping_mul);

    fn meta_div(
        &self,
        ctx: Context<'gc>,
        other: Self::Value,
    ) -> Result<Self::Result2, Self::Error> {
        if let Value::Int(other) = other {
            if other == 0 {
                return Err(Error::new(RuntimeError::DivideByZero { value: *self }));
            }
            Ok(self.wrapping_div(other).into_meta_result(ctx))
        } else {
            Err(self.meta_error(ctx, MetaName::Div, vec![other]))
        }
    }

    fn meta_rem(
        &self,
        ctx: Context<'gc>,
        other: Self::Value,
    ) -> Result<Self::Result2, Self::Error> {
        if let Value::Int(other) = other {
            if other == 0 {
                return Err(Error::new(RuntimeError::DivideByZero { value: *self }));
            }
            Ok(self.wrapping_rem(other).into_meta_result(ctx))
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
