use crate::{
    Context,
    compiler::value::MetaMethod,
    objects::{IntoMetaResult, impl_metamethod},
};

impl<'gc> MetaMethod<Context<'gc>> for bool {
    impl_metamethod!(Bool);

    fn meta_bool(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok(self.into_meta_result(ctx))
    }

    fn meta_int(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok((if *self { 1 } else { 0 }).into_meta_result(ctx))
    }

    fn meta_float(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok((if *self { 1.0 } else { 0.0 }).into_meta_result(ctx))
    }

    impl_metamethod!(Bool, str);
    impl_metamethod!(Bool, repr);

    impl_metamethod!(Bool, eq_ne);
}
