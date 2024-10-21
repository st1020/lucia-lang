use std::{fmt, ops};

use gc_arena::{Collect, Gc, Mutation};
use smol_str::SmolStr;

#[derive(Clone, Copy, Hash, Collect)]
#[collect(no_drop)]
pub struct Str<'gc>(Gc<'gc, StringInner>);

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct StringInner(SmolStr);

unsafe impl Collect for StringInner {}

impl ops::Deref for StringInner {
    type Target = SmolStr;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'gc> Str<'gc> {
    pub fn new(mc: &Mutation<'gc>, s: SmolStr) -> Str<'gc> {
        Str(Gc::new(mc, StringInner(s)))
    }

    pub fn from_inner(inner: Gc<'gc, StringInner>) -> Self {
        Self(inner)
    }

    pub fn into_inner(self) -> Gc<'gc, StringInner> {
        self.0
    }
}

impl<'gc> fmt::Debug for Str<'gc> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "Str({})", self)
    }
}

impl<'gc> fmt::Display for Str<'gc> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str(self)
    }
}

impl<'gc> ops::Deref for Str<'gc> {
    type Target = SmolStr;

    fn deref(&self) -> &SmolStr {
        &self.0
    }
}

impl<'gc> AsRef<str> for Str<'gc> {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl<'gc, T> PartialEq<T> for Str<'gc>
where
    T: ?Sized + AsRef<str>,
{
    fn eq(&self, other: &T) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl<'gc> Eq for Str<'gc> {}
