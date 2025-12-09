use crate::{Context, compiler::value::MetaMethod, objects::impl_metamethod};

impl MetaMethod<&Context> for bool {
    impl_metamethod!(Bool);

    #[inline]
    fn meta_bool(self, _: &Context) -> Result<Self::Result1, Self::Error> {
        Ok(self.into())
    }

    #[inline]
    fn meta_int(self, _: &Context) -> Result<Self::Result1, Self::Error> {
        Ok(i32::from(self).into())
    }

    #[inline]
    fn meta_float(self, _: &Context) -> Result<Self::Result1, Self::Error> {
        Ok((if self { 1.0 } else { 0.0 }).into())
    }

    impl_metamethod!(Bool, str);
    impl_metamethod!(Bool, repr);

    impl_metamethod!(Bool, eq_ne);
}
