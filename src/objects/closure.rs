use std::{
    hash::{Hash, Hasher},
    ops::Deref,
};

use gc_arena::{lock::Lock, Collect, Gc, Mutation};

use crate::{compiler::code::Code, objects::Value};

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
        Closure(Gc::new(
            mc,
            ClosureInner {
                upvalues: vec![Gc::new(mc, Lock::new(Value::Null)); function.def_upvalue_count],
                function,
                base_closure,
            },
        ))
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
    pub base_closure: Option<Closure<'gc>>,
    pub upvalues: Vec<Gc<'gc, Lock<Value<'gc>>>>,
}
