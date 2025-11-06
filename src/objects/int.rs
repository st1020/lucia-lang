use crate::{
    Context,
    compiler::value::MetaMethod,
    objects::{IntoMetaResult, impl_metamethod},
};

impl<'gc> MetaMethod<Context<'gc>> for i64 {
    impl_metamethod!(Int);

    fn meta_bool(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok((*self != 0).into_meta_result(ctx))
    }

    fn meta_int(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok(self.into_meta_result(ctx))
    }

    fn meta_float(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok((*self as f64).into_meta_result(ctx))
    }

    impl_metamethod!(Int, str);
    impl_metamethod!(Int, repr);

    impl_metamethod!(Int, neg);
    impl_metamethod!(Int, arithmetic, Add, meta_add, wrapping_add);
    impl_metamethod!(Int, arithmetic, Sub, meta_sub, wrapping_sub);
    impl_metamethod!(Int, arithmetic, Mul, meta_mul, wrapping_mul);
    impl_metamethod!(Int, arithmetic, Div, meta_div, wrapping_div);
    impl_metamethod!(Int, arithmetic, Rem, meta_rem, wrapping_rem);

    impl_metamethod!(Int, eq_ne);
    impl_metamethod!(Int, compare, Gt, meta_gt, gt);
    impl_metamethod!(Int, compare, Ge, meta_ge, ge);
    impl_metamethod!(Int, compare, Lt, meta_lt, lt);
    impl_metamethod!(Int, compare, Le, meta_le, le);
}
