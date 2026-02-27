use std::{borrow::Borrow, collections::HashSet, ops::Deref, rc::Rc};

use compact_str::{CompactString, ToCompactString, format_compact};
use derive_more::Display;
use rustc_hash::FxBuildHasher;

use crate::{
    Context,
    compiler::{
        interning::StringInterner,
        value::{MetaMethod, MetaName},
    },
    errors::Error,
    objects::{FromValue, Value, ValueType, impl_metamethod, unexpected_type_error},
};

pub type RcStr = Rc<Str>;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub struct Str(CompactString);

impl Str {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl Deref for Str {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl<T: Into<CompactString>> From<T> for Str {
    fn from(value: T) -> Self {
        Self(value.into())
    }
}

impl MetaMethod<&Context> for RcStr {
    impl_metamethod!(Str);

    #[inline]
    fn meta_len(self, _ctx: &Context) -> Result<Self::Result1, Self::Error> {
        Ok(self.len().into())
    }

    #[inline]
    fn meta_bool(self, _ctx: &Context) -> Result<Self::Result1, Self::Error> {
        Ok((!self.is_empty()).into())
    }

    #[inline]
    fn meta_int(self, _ctx: &Context) -> Result<Self::Result1, Self::Error> {
        Ok(self
            .parse::<i64>()
            .map_err(|e| Error::ParseError {
                reason: e.to_compact_string(),
            })?
            .into())
    }

    #[inline]
    fn meta_float(self, _ctx: &Context) -> Result<Self::Result1, Self::Error> {
        Ok(self
            .parse::<f64>()
            .map_err(|e| Error::ParseError {
                reason: e.to_compact_string(),
            })?
            .into())
    }

    impl_metamethod!(Str, str);

    #[inline]
    fn meta_repr(self, _ctx: &Context) -> Result<Self::Result1, Self::Error> {
        Ok(format_compact!("{:?}", self.as_str()).into())
    }

    #[inline]
    fn meta_add(self, ctx: &Context, other: Self::Value) -> Result<Self::Result2, Self::Error> {
        if let Value::Str(other) = other {
            Ok((self.to_compact_string() + other.as_ref()).into())
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

impl From<CompactString> for Value {
    fn from(value: CompactString) -> Value {
        Value::Str(Rc::new(value.into()))
    }
}

impl From<String> for Value {
    fn from(value: String) -> Value {
        Value::Str(Rc::new(value.into()))
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Value {
        Value::Str(Rc::new(value.into()))
    }
}

impl From<ValueType> for Value {
    fn from(value: ValueType) -> Self {
        CompactString::const_new(value.name()).into()
    }
}

impl From<MetaName> for Value {
    fn from(value: MetaName) -> Self {
        CompactString::const_new(value.name()).into()
    }
}

impl FromValue for Str {
    fn from_value(value: Value) -> Result<Self, Error> {
        if let Value::Str(v) = value {
            Ok(Rc::unwrap_or_clone(v))
        } else {
            Err(unexpected_type_error!(ValueType::Str, value))
        }
    }
}

impl FromValue for String {
    fn from_value(value: Value) -> Result<Self, Error> {
        if let Value::Str(v) = value {
            Ok((*Rc::unwrap_or_clone(v)).into())
        } else {
            Err(unexpected_type_error!(ValueType::Str, value))
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct RcStrWrapper(RcStr);

impl Borrow<str> for RcStrWrapper {
    fn borrow(&self) -> &str {
        self.0.as_str()
    }
}

/// A Str interner.
#[derive(Debug, Default)]
pub struct StrInterner(HashSet<RcStrWrapper, FxBuildHasher>);

impl StringInterner for StrInterner {
    type String = RcStr;

    fn intern(&mut self, s: &str) -> Self::String {
        if let Some(s) = self.0.get(s) {
            s.clone().0
        } else {
            let s = Rc::new(Str::from(s));
            self.0.insert(RcStrWrapper(Rc::clone(&s)));
            s
        }
    }
}
