use std::{borrow::Borrow, collections::HashSet, fmt, ops};

use compact_str::{CompactString, ToCompactString, format_compact};
use gc_arena::{Collect, Gc, Mutation, static_collect};
use rustc_hash::FxBuildHasher;

use crate::{
    Context,
    compiler::{
        interning::StringInterner,
        value::{MetaMethod, MetaName},
    },
    objects::{IntoMetaResult, Value, define_object, value_metamethod},
};

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

impl<'gc> MetaMethod<Context<'gc>> for Str<'gc> {
    value_metamethod!(Str);

    fn meta_len(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok((self.len() as i64).into_meta_result(ctx))
    }

    fn meta_bool(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok((!self.is_empty()).into_meta_result(ctx))
    }

    fn meta_int(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok(self
            .parse::<i64>()
            .map_err(|_| self.meta_error(ctx, MetaName::Int, vec![]))?
            .into_meta_result(ctx))
    }

    fn meta_float(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok(self
            .parse::<f64>()
            .map_err(|_| self.meta_error(ctx, MetaName::Float, vec![]))?
            .into_meta_result(ctx))
    }

    value_metamethod!(Str, str);

    fn meta_repr(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        Ok(format_compact!("{:?}", self.as_ref()).into_meta_result(ctx))
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

    value_metamethod!(Str, eq_ne);
    value_metamethod!(Str, compare, Gt, meta_gt, gt);
    value_metamethod!(Str, compare, Ge, meta_ge, ge);
    value_metamethod!(Str, compare, Lt, meta_lt, lt);
    value_metamethod!(Str, compare, Le, meta_le, le);
}
