use std::{any::Any, hash, ptr, rc::Rc};

use derive_more::{Deref, Display, From};

use crate::{
    Context,
    compiler::value::{MetaMethod, MetaName},
    objects::{Table, Value, call_metamethod_error, impl_metamethod},
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

    call_metamethod_error!(1, meta_len, Len);

    call_metamethod_error!(1, meta_bool, Bool);
    call_metamethod_error!(1, meta_int, Int);
    call_metamethod_error!(1, meta_float, Float);
    call_metamethod_error!(1, meta_str, Str);
    call_metamethod_error!(1, meta_repr, Repr);

    call_metamethod_error!(1, meta_neg, Neg);
    call_metamethod_error!(2, meta_add, Add);
    call_metamethod_error!(2, meta_sub, Sub);
    call_metamethod_error!(2, meta_mul, Mul);
    call_metamethod_error!(2, meta_div, Div);
    call_metamethod_error!(2, meta_rem, Rem);

    call_metamethod_error!(2, meta_eq, Eq);
    call_metamethod_error!(2, meta_ne, Ne);
    call_metamethod_error!(2, meta_gt, Gt);
    call_metamethod_error!(2, meta_ge, Ge);
    call_metamethod_error!(2, meta_lt, Lt);
    call_metamethod_error!(2, meta_le, Le);

    call_metamethod_error!(2, meta_get_attr, GetAttr);
    call_metamethod_error!(2, meta_get_item, GetItem);
    call_metamethod_error!(3, meta_set_attr, SetAttr);
    call_metamethod_error!(3, meta_set_item, SetItem);
}
