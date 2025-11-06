use crate::{
    Context,
    compiler::value::MetaMethod,
    objects::{IntoMetaResult, Value, impl_metamethod},
};

impl<'gc> MetaMethod<Context<'gc>> for () {
    impl_metamethod!(Null);

    fn meta_bool(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok(false.into_meta_result(ctx))
    }

    fn meta_int(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok(0.into_meta_result(ctx))
    }

    fn meta_float(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok(0.0.into_meta_result(ctx))
    }

    fn meta_str(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok("null".into_meta_result(ctx))
    }

    fn meta_repr(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        self.meta_str(ctx)
    }

    fn meta_eq(&self, ctx: Context<'gc>, other: Self::Value) -> Result<Self::Result2, Self::Error> {
        Ok(matches!(other, Value::Null).into_meta_result(ctx))
    }

    fn meta_ne(&self, ctx: Context<'gc>, other: Self::Value) -> Result<Self::Result2, Self::Error> {
        Ok((!matches!(other, Value::Null)).into_meta_result(ctx))
    }
}
