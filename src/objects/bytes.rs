use std::{borrow::Borrow, fmt, ops};

use gc_arena::{Collect, Gc, Mutation, static_collect};

use crate::{
    Context,
    compiler::value::MetaMethod,
    objects::{IntoMetaResult, define_object, value_metamethod},
};

define_object!(Bytes, BytesInner, inner);

impl<'gc> Bytes<'gc> {
    pub fn new(mc: &Mutation<'gc>, s: Vec<u8>) -> Bytes<'gc> {
        Bytes(Gc::new(mc, BytesInner(s)))
    }
}

impl AsRef<[u8]> for Bytes<'_> {
    fn as_ref(&self) -> &[u8] {
        &self.0
    }
}

impl Borrow<[u8]> for Bytes<'_> {
    fn borrow(&self) -> &[u8] {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BytesInner(Vec<u8>);

static_collect!(BytesInner);

impl ops::Deref for BytesInner {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl fmt::Display for BytesInner {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "b\"{}\"", self.escape_ascii())
    }
}

impl<'gc> MetaMethod<Context<'gc>> for Bytes<'gc> {
    value_metamethod!(Bytes);

    fn meta_len(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok((self.len() as i64).into_meta_result(ctx))
    }

    value_metamethod!(Bytes, str);
    value_metamethod!(Bytes, repr);

    value_metamethod!(Bytes, eq_ne);
    value_metamethod!(Bytes, compare, Gt, meta_gt, gt);
    value_metamethod!(Bytes, compare, Ge, meta_ge, ge);
    value_metamethod!(Bytes, compare, Lt, meta_lt, lt);
    value_metamethod!(Bytes, compare, Le, meta_le, le);
}
