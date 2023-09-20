use std::hash::{Hash, Hasher};

use gc_arena::{lock::Lock, Collect, Gc, Mutation};

use crate::{compiler::code::Code, objects::Value};

#[derive(Debug, Clone, Copy, Collect)]
#[collect(no_drop)]
pub struct Closure<'gc>(pub Gc<'gc, ClosureState<'gc>>);

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

impl<'gc> Closure<'gc> {
    pub fn new(mc: &Mutation<'gc>, function: Code, base_closure: Option<Closure<'gc>>) -> Self {
        Closure(Gc::new(
            mc,
            ClosureState {
                upvalues: vec![Gc::new(mc, Lock::new(Value::Null)); function.def_upvalue_count],
                function,
                base_closure,
            },
        ))
    }
}

#[derive(Debug, Collect)]
#[collect(no_drop)]
pub struct ClosureState<'gc> {
    pub function: Code,
    pub base_closure: Option<Closure<'gc>>,
    pub upvalues: Vec<Gc<'gc, Lock<Value<'gc>>>>,
}
