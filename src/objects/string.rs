use std::{borrow::Borrow, collections::HashSet, fmt, ops};

use gc_arena::{Collect, Gc, Mutation};
use rustc_hash::FxBuildHasher;
use smol_str::SmolStr;

use crate::{compiler::interning::StringInterner, Context};

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

impl<'gc> Borrow<str> for Str<'gc> {
    fn borrow(&self) -> &str {
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

pub struct GcStrInterner<'gc> {
    context: Context<'gc>,
    interner: HashSet<Str<'gc>, FxBuildHasher>,
}

impl<'gc> GcStrInterner<'gc> {
    pub fn new(context: Context<'gc>) -> Self {
        GcStrInterner {
            context,
            interner: HashSet::with_hasher(FxBuildHasher),
        }
    }
}

impl<'gc> StringInterner for GcStrInterner<'gc> {
    type String = Str<'gc>;

    fn intern(&mut self, s: &str) -> Self::String {
        if let Some(s) = self.interner.get(s) {
            *s
        } else {
            let s = Str::new(&self.context, s.into());
            self.interner.insert(s);
            s
        }
    }
}

unsafe impl<'gc> Collect for GcStrInterner<'gc> {
    fn needs_trace() -> bool {
        true
    }

    fn trace(&self, cc: &gc_arena::Collection) {
        for s in self.interner.iter() {
            s.trace(cc)
        }
    }
}
