use std::{borrow::Borrow, ops::Deref};

use gc_arena::{Collect, Gc, Mutation};

use crate::errors::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Collect)]
#[collect(no_drop)]
pub struct GcError<'gc>(pub Gc<'gc, Error<'gc>>);

impl<'gc> Deref for GcError<'gc> {
    type Target = Error<'gc>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'gc> AsRef<Error<'gc>> for GcError<'gc> {
    fn as_ref(&self) -> &Error<'gc> {
        &self.0
    }
}

impl<'gc> Borrow<Error<'gc>> for GcError<'gc> {
    fn borrow(&self) -> &Error<'gc> {
        &self.0
    }
}

impl<'gc> GcError<'gc> {
    pub fn new(mc: &Mutation<'gc>, e: Error<'gc>) -> GcError<'gc> {
        GcError(Gc::new(mc, e))
    }
}
