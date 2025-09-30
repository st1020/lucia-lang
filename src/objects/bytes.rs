use std::{borrow::Borrow, fmt, ops};

use gc_arena::{Collect, Gc, Mutation, static_collect};

use crate::objects::define_object;

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
