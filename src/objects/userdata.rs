use std::{any::Any, hash, ptr, rc::Rc};

use derive_more::{Deref, Display, From};

use crate::{
    Context,
    compiler::value::{MetaMethod, MetaName},
    objects::{Table, Value, impl_metamethod},
};

pub type UserData = Rc<UserDataInner>;

#[derive(Debug, From, Display, Deref)]
#[display("<userdata {:p}>", self.data)]
pub struct UserDataInner {
    #[deref]
    pub data: Box<dyn Any>,
    pub metatable: Option<Table>,
}

impl PartialEq for UserDataInner {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(ptr::from_ref(&*self.data), ptr::from_ref(&*other.data))
    }
}

impl Eq for UserDataInner {}

impl hash::Hash for UserDataInner {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        ptr::from_ref(&*self.data).hash(state);
    }
}

impl UserDataInner {
    pub fn new(data: Box<dyn Any>, metatable: Option<Table>) -> Self {
        Self { data, metatable }
    }

    pub fn metatable(&self) -> Option<Table> {
        self.metatable.clone()
    }
}

impl MetaMethod<&Context> for UserData {
    impl_metamethod!(UserData);

    #[inline]
    fn meta_call(self, ctx: &Context) -> Result<Self::ResultCall, Self::Error> {
        if let Some(metatable) = self.metatable() {
            #[expect(clippy::wildcard_enum_match_arm)]
            match metatable.get(MetaName::Call) {
                Value::Function(v) => Ok(v),
                Value::Table(v) => v.meta_call(ctx), // TODO: prevent infinite recursion
                v => Err(v.meta_error(ctx, MetaName::Call, vec![])),
            }
        } else {
            Err(self.meta_error(ctx, MetaName::Call, vec![]))
        }
    }

    // #[inline]
    // fn meta_iter(self, ctx: &Context) -> Result<Self::ResultIter, Self::Error> {
    //     if let Some(metatable) = self.metatable() {
    //         let t = metatable.get(ctx, MetaName::Iter);
    //         if !t.is_null() {
    //             return Ok(Function::Callback(Callback::from_fn_with(
    //                 &ctx,
    //                 (t.meta_call(ctx)?, *self),
    //                 |(f, v), _ctx, _args| Ok(CallbackReturn::TailCall(*f, vec![(*v).into()])),
    //             )));
    //         }
    //     }
    //     Err(self.meta_error(ctx, MetaName::Iter, vec![]))
    // }

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
