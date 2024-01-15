use std::{borrow::Borrow, fmt, ops::Deref};

use gc_arena::{Collect, Gc, Mutation};

#[derive(Clone, Copy, Collect, Hash)]
#[collect(no_drop)]
pub struct Str<'gc>(Gc<'gc, String>);

impl<'gc> Str<'gc> {
    pub fn new(mc: &Mutation<'gc>, s: String) -> Str<'gc> {
        Str(Gc::new(mc, s))
    }

    pub fn from_inner(inner: Gc<'gc, String>) -> Self {
        Self(inner)
    }

    pub fn into_inner(self) -> Gc<'gc, String> {
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

impl<'gc> Deref for Str<'gc> {
    type Target = String;

    fn deref(&self) -> &String {
        &self.0
    }
}

impl<'gc> AsRef<String> for Str<'gc> {
    fn as_ref(&self) -> &String {
        &self.0
    }
}

impl<'gc> Borrow<String> for Str<'gc> {
    fn borrow(&self) -> &String {
        &self.0
    }
}

impl<'gc, T> PartialEq<T> for Str<'gc>
where
    T: ?Sized + AsRef<String>,
{
    fn eq(&self, other: &T) -> bool {
        self.0.as_ref() == other.as_ref()
    }
}

impl<'gc> Eq for Str<'gc> {}
