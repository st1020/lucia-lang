use std::{collections::HashSet, ffi::OsStr, path::Path};

use compact_str::{CompactString, ToCompactString, format_compact};
use derive_more::{Deref, Display};
use gc_arena::{Collect, Gc, Mutation};
use rustc_hash::FxBuildHasher;

use crate::{
    Context,
    compiler::{
        interning::StringInterner,
        value::{MetaMethod, MetaName},
    },
    errors::{Error, RuntimeError},
    objects::{IntoMetaResult, Value, define_object, impl_metamethod},
};

define_object!(Str, StrInner, str, inner, [u8], OsStr, Path);

impl<'gc> Str<'gc> {
    pub fn new(mc: &Mutation<'gc>, s: CompactString) -> Str<'gc> {
        Str(Gc::new(mc, StrInner(s)))
    }

    pub fn as_str(&self) -> &str {
        self.into_inner().as_ref()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Collect, Display, Deref)]
#[collect(require_static)]
#[deref(forward)]
pub struct StrInner(CompactString);

impl<'gc> MetaMethod<Context<'gc>> for Str<'gc> {
    impl_metamethod!(Str);

    fn meta_len(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok(self.len().into_meta_result(ctx))
    }

    fn meta_bool(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok((!self.is_empty()).into_meta_result(ctx))
    }

    fn meta_int(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok(self
            .parse::<i64>()
            .map_err(|e| {
                Error::new(RuntimeError::ParseError {
                    reason: e.to_string(),
                })
            })?
            .into_meta_result(ctx))
    }

    fn meta_float(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok(self
            .parse::<f64>()
            .map_err(|e| {
                Error::new(RuntimeError::ParseError {
                    reason: e.to_string(),
                })
            })?
            .into_meta_result(ctx))
    }

    impl_metamethod!(Str, str);

    fn meta_repr(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok(format_compact!("{:?}", self.as_str()).into_meta_result(ctx))
    }

    fn meta_add(
        &self,
        ctx: Context<'gc>,
        other: Self::Value,
    ) -> Result<Self::Result2, Self::Error> {
        if let Value::Str(other) = other {
            Ok((self.to_compact_string() + other.as_ref()).into_meta_result(ctx))
        } else {
            Err(self.meta_error(ctx, MetaName::Add, vec![other]))
        }
    }

    impl_metamethod!(Str, eq_ne);
    impl_metamethod!(Str, compare, Gt, meta_gt, gt);
    impl_metamethod!(Str, compare, Ge, meta_ge, ge);
    impl_metamethod!(Str, compare, Lt, meta_lt, lt);
    impl_metamethod!(Str, compare, Le, meta_le, le);
}

pub struct StrInterner<'gc> {
    context: Context<'gc>,
    interner: HashSet<Str<'gc>, FxBuildHasher>, // TODO: use weak references?
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

// SAFETY: Str<'gc> is Collect, and HashSet is Collect when its elements are Collect, the
// Context<'gc> does need to be traced.
unsafe impl Collect for StrInterner<'_> {
    fn trace(&self, cc: &gc_arena::Collection) {
        self.interner.trace(cc);
    }
}
