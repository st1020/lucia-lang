use std::rc::Rc;

use derive_more::{Deref, Display, From};

use crate::{
    Context,
    compiler::value::MetaMethod,
    errors::Error,
    objects::{FromValue, Value, ValueType, impl_metamethod, unexpected_type_error},
};

pub type Bytes = Rc<BytesInner>;

#[derive(Debug, Clone, PartialEq, Eq, Hash, From, Display, Deref)]
#[display("b\"{}\"", self.escape_ascii())]
#[deref(forward)]
pub struct BytesInner(Vec<u8>);

impl MetaMethod<&Context> for Bytes {
    impl_metamethod!(Bytes);

    #[inline]
    fn meta_len(self, _: &Context) -> Result<Self::Result1, Self::Error> {
        Ok(self.len().into())
    }

    impl_metamethod!(Bytes, str);
    impl_metamethod!(Bytes, repr);

    impl_metamethod!(Bytes, eq_ne);
    impl_metamethod!(Bytes, compare, Gt, meta_gt, gt);
    impl_metamethod!(Bytes, compare, Ge, meta_ge, ge);
    impl_metamethod!(Bytes, compare, Lt, meta_lt, lt);
    impl_metamethod!(Bytes, compare, Le, meta_le, le);
}

impl From<Vec<u8>> for Value {
    fn from(value: Vec<u8>) -> Value {
        Value::Bytes(Bytes::new(value.into()))
    }
}

impl From<&[u8]> for Value {
    fn from(value: &[u8]) -> Value {
        value.to_vec().into()
    }
}

impl FromValue for Vec<u8> {
    fn from_value(value: Value) -> Result<Self, Error> {
        if let Value::Bytes(v) = value {
            Ok(v.to_vec())
        } else {
            Err(unexpected_type_error!(ValueType::Bytes, value))
        }
    }
}
