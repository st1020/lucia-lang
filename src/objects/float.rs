use std::ops::{Add, Div, Mul, Rem, Sub};

use crate::{
    Context,
    compiler::value::MetaMethod,
    objects::{IntoMetaResult, value_metamethod},
    utils::Float,
};

impl<'gc> MetaMethod<Context<'gc>> for Float {
    value_metamethod!(Float);

    fn meta_bool(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok((self.0 != 0.0).into_meta_result(ctx))
    }

    fn meta_int(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok((self.0 as i64).into_meta_result(ctx))
    }

    fn meta_float(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok(self.into_meta_result(ctx))
    }

    value_metamethod!(Float, str);
    value_metamethod!(Float, repr);

    value_metamethod!(Float, neg);
    value_metamethod!(Float, arithmetic, Add, meta_add, add);
    value_metamethod!(Float, arithmetic, Sub, meta_sub, sub);
    value_metamethod!(Float, arithmetic, Mul, meta_mul, mul);
    value_metamethod!(Float, arithmetic, Div, meta_div, div);
    value_metamethod!(Float, arithmetic, Rem, meta_rem, rem);

    value_metamethod!(Float, eq_ne);
    value_metamethod!(Float, compare, Gt, meta_gt, gt);
    value_metamethod!(Float, compare, Ge, meta_ge, ge);
    value_metamethod!(Float, compare, Lt, meta_lt, lt);
    value_metamethod!(Float, compare, Le, meta_le, le);
}
