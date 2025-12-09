use std::rc::Rc;

use compact_str::{CompactString, ToCompactString, format_compact};

use crate::{
    Context,
    compiler::value::{MetaMethod, MetaName},
    errors::{Error, ErrorKind},
    objects::{FromValue, Value, ValueType, impl_metamethod, unexpected_type_error},
};

pub type Str = Rc<CompactString>;

impl MetaMethod<&Context> for Str {
    impl_metamethod!(Str);

    #[inline]
    fn meta_len(self, _: &Context) -> Result<Self::Result1, Self::Error> {
        Ok(self.len().into())
    }

    #[inline]
    fn meta_bool(self, _: &Context) -> Result<Self::Result1, Self::Error> {
        Ok((!self.is_empty()).into())
    }

    #[inline]
    fn meta_int(self, _: &Context) -> Result<Self::Result1, Self::Error> {
        Ok(self
            .parse::<i64>()
            .map_err(|e| {
                Error::new(ErrorKind::ParseError {
                    reason: e.to_string(),
                })
            })?
            .into())
    }

    #[inline]
    fn meta_float(self, _: &Context) -> Result<Self::Result1, Self::Error> {
        Ok(self
            .parse::<f64>()
            .map_err(|e| {
                Error::new(ErrorKind::ParseError {
                    reason: e.to_string(),
                })
            })?
            .into())
    }

    impl_metamethod!(Str, str);

    #[inline]
    fn meta_repr(self, _: &Context) -> Result<Self::Result1, Self::Error> {
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
        Value::Str(value.into())
    }
}

impl From<String> for Value {
    fn from(value: String) -> Value {
        Value::Str(value.to_compact_string().into())
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Value {
        Value::Str(value.to_compact_string().into())
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

impl FromValue for CompactString {
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
            Ok(Rc::unwrap_or_clone(v).into())
        } else {
            Err(unexpected_type_error!(ValueType::Str, value))
        }
    }
}
