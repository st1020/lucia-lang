use std::{
    hash::{Hash, Hasher},
    ops::Deref,
};

use gc_arena::{lock::Lock, Collect, Gc, Mutation};

use crate::{
    compiler::code::{Code, FunctionKind},
    objects::Value,
};

#[derive(Debug, Clone, Copy, Collect)]
#[collect(no_drop)]
pub struct Closure<'gc>(Gc<'gc, ClosureInner<'gc>>);

impl<'gc> PartialEq for Closure<'gc> {
    fn eq(&self, other: &Closure<'gc>) -> bool {
        Gc::ptr_eq(self.0, other.0)
    }
}

impl<'gc> Eq for Closure<'gc> {}

impl<'gc> Hash for Closure<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Gc::as_ptr(self.0).hash(state)
    }
}

impl<'gc> Deref for Closure<'gc> {
    type Target = ClosureInner<'gc>;

    fn deref(&self) -> &ClosureInner<'gc> {
        &self.0
    }
}

impl<'gc> AsRef<ClosureInner<'gc>> for Closure<'gc> {
    fn as_ref(&self) -> &ClosureInner<'gc> {
        &self.0
    }
}

impl<'gc> Closure<'gc> {
    pub fn new(mc: &Mutation<'gc>, function: Code, base_closure: Option<Closure<'gc>>) -> Self {
        let mut upvalues = vec![UpValue::new(mc, Value::Null); function.upvalue_names.len()];

        if function.kind == FunctionKind::Closure {
            if let Some(base_closure) = base_closure {
                for (i, (_, base_closure_upvalue_id)) in function.upvalue_names.iter().enumerate() {
                    if let Some(base_closure_upvalue_id) = base_closure_upvalue_id {
                        upvalues[i] = base_closure.upvalues[*base_closure_upvalue_id];
                    }
                }
            }
        }

        Closure(Gc::new(mc, ClosureInner { upvalues, function }))
    }

    pub fn from_inner(inner: Gc<'gc, ClosureInner<'gc>>) -> Self {
        Self(inner)
    }

    pub fn into_inner(self) -> Gc<'gc, ClosureInner<'gc>> {
        self.0
    }
}

#[derive(Debug, Collect)]
#[collect(no_drop)]
pub struct ClosureInner<'gc> {
    pub function: Code,
    pub upvalues: Vec<UpValue<'gc>>,
}

#[derive(Debug, Clone, Copy, Collect)]
#[collect(no_drop)]
pub struct UpValue<'gc>(pub Gc<'gc, Lock<Value<'gc>>>);

impl<'gc> UpValue<'gc> {
    pub fn new(mc: &Mutation<'gc>, value: Value<'gc>) -> Self {
        UpValue(Gc::new(mc, Lock::new(value)))
    }
}

impl<'gc> AsRef<Gc<'gc, Lock<Value<'gc>>>> for UpValue<'gc> {
    fn as_ref(&self) -> &Gc<'gc, Lock<Value<'gc>>> {
        &self.0
    }
}

impl<'gc> Deref for UpValue<'gc> {
    type Target = Gc<'gc, Lock<Value<'gc>>>;

    fn deref(&self) -> &Gc<'gc, Lock<Value<'gc>>> {
        &self.0
    }
}
