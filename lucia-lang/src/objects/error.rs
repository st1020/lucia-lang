use std::{fmt, ops};

use gc_arena::{Collect, Gc, Mutation};

use crate::errors::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Collect)]
#[collect(no_drop)]
pub struct GcError<'gc>(pub Gc<'gc, Error<'gc>>);

impl<'gc> ops::Deref for GcError<'gc> {
    type Target = Error<'gc>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'gc> fmt::Display for GcError<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'gc> GcError<'gc> {
    pub fn new(mc: &Mutation<'gc>, e: Error<'gc>) -> GcError<'gc> {
        GcError(Gc::new(mc, e))
    }
}
