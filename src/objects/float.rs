use std::ops::{Add, Div, Mul, Rem, Sub};

use crate::{
    Context,
    compiler::value::MetaMethod,
    objects::{IntoMetaResult, impl_metamethod},
    utils::Float,
};

impl<'gc> MetaMethod<Context<'gc>> for Float {
    impl_metamethod!(Float);

    fn meta_bool(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok((self.0 != 0.0).into_meta_result(ctx))
    }

    fn meta_int(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok((self.0 as i64).into_meta_result(ctx))
    }

    fn meta_float(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok(self.into_meta_result(ctx))
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
