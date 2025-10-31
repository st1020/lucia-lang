use crate::{
    Context,
    compiler::value::MetaMethod,
    objects::{IntoMetaResult, value_metamethod},
};

impl<'gc> MetaMethod<Context<'gc>> for i64 {
    value_metamethod!(Int);

    fn meta_bool(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok((*self != 0).into_meta_result(ctx))
    }

    fn meta_int(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok(self.into_meta_result(ctx))
    }

    fn meta_float(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok((*self as f64).into_meta_result(ctx))
    }

    value_metamethod!(Int, str);
    value_metamethod!(Int, repr);

    value_metamethod!(Int, neg);
    value_metamethod!(Int, arithmetic, Add, meta_add, wrapping_add);
    value_metamethod!(Int, arithmetic, Sub, meta_sub, wrapping_sub);
    value_metamethod!(Int, arithmetic, Mul, meta_mul, wrapping_mul);
    value_metamethod!(Int, arithmetic, Div, meta_div, wrapping_div);
    value_metamethod!(Int, arithmetic, Rem, meta_rem, wrapping_rem);

    value_metamethod!(Int, eq_ne);
    value_metamethod!(Int, compare, Gt, meta_gt, gt);
    value_metamethod!(Int, compare, Ge, meta_ge, ge);
    value_metamethod!(Int, compare, Lt, meta_lt, lt);
    value_metamethod!(Int, compare, Le, meta_le, le);
}
