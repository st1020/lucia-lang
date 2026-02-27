use std::{any::Any, hash, ptr, rc::Rc};

use derive_more::{Deref, Display, From};

use crate::{
    Context,
    compiler::value::{MetaMethod, MetaName},
    objects::{RcTable, call_meta_iter, impl_metamethod},
};

pub type RcUserData = Rc<UserData>;

#[derive(Debug, From, Display, Deref)]
#[display("<userdata {:p}>", self.data)]
pub struct UserData {
    #[deref]
    pub data: Box<dyn Any>,
    pub metatable: Option<RcTable>,
}

impl PartialEq for UserData {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(ptr::from_ref(&*self.data), ptr::from_ref(&*other.data))
    }
}

impl Eq for UserData {}

impl hash::Hash for UserData {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        ptr::from_ref(&*self.data).hash(state);
    }
}

impl UserData {
    pub fn new(data: Box<dyn Any>, metatable: Option<RcTable>) -> Self {
        Self { data, metatable }
    }

    pub fn metatable(&self) -> Option<RcTable> {
        self.metatable.clone()
    }
}

impl MetaMethod<&Context> for RcUserData {
    impl_metamethod!(UserData);

    impl_metamethod!(UserData, call);

    #[inline]
    fn meta_iter(self, ctx: &Context) -> Result<Self::ResultIter, Self::Error> {
        call_meta_iter!(UserData, ctx, self);
        Err(self.meta_error(ctx, MetaName::Iter, vec![]))
    }

    impl_metamethod!(UserData, metatable, Len, meta_len, 1);

    impl_metamethod!(UserData, metatable, Bool, meta_bool, 1);
    impl_metamethod!(UserData, metatable, Int, meta_int, 1);
    impl_metamethod!(UserData, metatable, Float, meta_float, 1);
    impl_metamethod!(UserData, metatable, Str, meta_str, 1);
    impl_metamethod!(UserData, metatable, Repr, meta_repr, 1);

    impl_metamethod!(UserData, metatable, Neg, meta_neg, 1);
    impl_metamethod!(UserData, metatable, Add, meta_add, 2);
    impl_metamethod!(UserData, metatable, Sub, meta_sub, 2);
    impl_metamethod!(UserData, metatable, Mul, meta_mul, 2);
    impl_metamethod!(UserData, metatable, Div, meta_div, 2);
    impl_metamethod!(UserData, metatable, Rem, meta_rem, 2);

    impl_metamethod!(UserData, metatable, Eq, meta_eq, 2);
    impl_metamethod!(UserData, metatable, Ne, meta_ne, 2);
    impl_metamethod!(UserData, metatable, Gt, meta_gt, 2);
    impl_metamethod!(UserData, metatable, Ge, meta_ge, 2);
    impl_metamethod!(UserData, metatable, Lt, meta_lt, 2);
    impl_metamethod!(UserData, metatable, Le, meta_le, 2);

    impl_metamethod!(UserData, metatable, GetAttr, meta_get_attr, 2);
    impl_metamethod!(UserData, metatable, GetItem, meta_get_item, 2);
    impl_metamethod!(UserData, metatable, SetAttr, meta_set_attr, 3);
    impl_metamethod!(UserData, metatable, SetItem, meta_set_item, 3);
}
