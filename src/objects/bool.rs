use crate::{
    Context,
    compiler::value::MetaMethod,
    objects::{IntoMetaResult, value_metamethod},
};

impl<'gc> MetaMethod<Context<'gc>> for bool {
    value_metamethod!(Bool);

    fn meta_bool(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok(self.into_meta_result(ctx))
    }

    fn meta_int(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok((if *self { 1 } else { 0 }).into_meta_result(ctx))
    }

    fn meta_float(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok((if *self { 1.0 } else { 0.0 }).into_meta_result(ctx))
    }

    value_metamethod!(Bool, str);
    value_metamethod!(Bool, repr);

    value_metamethod!(Bool, eq_ne);
}
