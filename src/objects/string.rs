use std::{borrow::Borrow, collections::HashSet, fmt, ops};

use compact_str::CompactString;
use gc_arena::{static_collect, Collect, Gc, Mutation};
use rustc_hash::FxBuildHasher;

use crate::{compiler::interning::StringInterner, objects::define_object, Context};

define_object!(Str, StrInner, inner);

impl<'gc> Str<'gc> {
    pub fn new(mc: &Mutation<'gc>, s: CompactString) -> Str<'gc> {
        Str(Gc::new(mc, StrInner(s)))
    }
}

impl AsRef<str> for Str<'_> {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl Borrow<str> for Str<'_> {
    fn borrow(&self) -> &str {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StrInner(CompactString);

static_collect!(StrInner);

impl ops::Deref for StrInner {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl fmt::Display for StrInner {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str(self)
    }
}

pub struct StrInterner<'gc> {
    context: Context<'gc>,
    interner: HashSet<Str<'gc>, FxBuildHasher>,
}

impl<'gc> StrInterner<'gc> {
    pub fn new(context: Context<'gc>) -> Self {
        StrInterner {
            context,
            interner: HashSet::with_hasher(FxBuildHasher),
        }
    }
}

impl<'gc> StringInterner for StrInterner<'gc> {
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

unsafe impl Collect for StrInterner<'_> {
    fn needs_trace() -> bool {
        true
    }

    fn trace(&self, cc: &gc_arena::Collection) {
        self.interner.trace(cc)
    }
}
