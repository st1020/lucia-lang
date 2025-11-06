use derive_more::{Deref, Display};
use gc_arena::{Collect, Gc, Mutation};

use crate::{
    Context,
    compiler::value::MetaMethod,
    objects::{IntoMetaResult, define_object, impl_metamethod},
};

define_object!(Bytes, BytesInner, [u8], inner);

impl<'gc> Bytes<'gc> {
    pub fn new(mc: &Mutation<'gc>, s: Vec<u8>) -> Bytes<'gc> {
        Bytes(Gc::new(mc, BytesInner(s)))
    }

    pub fn as_bytes(&self) -> &[u8] {
        self.into_inner().as_ref()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Collect, Display, Deref)]
#[collect(require_static)]
#[display("b\"{}\"", self.escape_ascii())]
#[deref(forward)]
pub struct BytesInner(Vec<u8>);

impl<'gc> MetaMethod<Context<'gc>> for Bytes<'gc> {
    impl_metamethod!(Bytes);

    fn meta_len(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok((self.len() as i64).into_meta_result(ctx))
    }

    impl_metamethod!(Bytes, str);
    impl_metamethod!(Bytes, repr);

    impl_metamethod!(Bytes, eq_ne);
    impl_metamethod!(Bytes, compare, Gt, meta_gt, gt);
    impl_metamethod!(Bytes, compare, Ge, meta_ge, ge);
    impl_metamethod!(Bytes, compare, Lt, meta_lt, lt);
    impl_metamethod!(Bytes, compare, Le, meta_le, le);
}
