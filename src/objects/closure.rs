use std::fmt;

use gc_arena::{
    lock::{Lock, RefLock},
    Collect, Gc, Mutation,
};

use crate::objects::{define_object, RuntimeCode, Value};

define_object!(Closure, ClosureInner<'gc>, ptr, "closure");

impl<'gc> Closure<'gc> {
    pub fn new(mc: &Mutation<'gc>, code: RuntimeCode<'gc>, closure: Option<Closure<'gc>>) -> Self {
        let mut upvalues = Vec::with_capacity(code.upvalue_names.len());

        if let Some(closure) = closure {
            let base_closure_upvalues = closure.upvalues.borrow();
            for (_, base_closure_upvalue_id) in &code.upvalue_names {
                upvalues.push(base_closure_upvalue_id.map_or_else(
                    || UpValue::new(mc, Value::Null),
                    |id| base_closure_upvalues[id],
                ));
            }
        } else {
            for _ in 0..code.upvalue_names.len() {
                upvalues.push(UpValue::new(mc, Value::Null));
            }
        }

        Closure(Gc::new(
            mc,
            ClosureInner {
                upvalues: Gc::new(mc, RefLock::new(upvalues)),
                code,
            },
        ))
    }
}

#[derive(Debug, Collect)]
#[collect(no_drop)]
pub struct ClosureInner<'gc> {
    pub code: RuntimeCode<'gc>,
    pub upvalues: Gc<'gc, RefLock<Vec<UpValue<'gc>>>>,
}

#[derive(Debug, Clone, Copy, Collect)]
#[collect(no_drop)]
pub struct UpValue<'gc>(Gc<'gc, Lock<Value<'gc>>>);

impl<'gc> UpValue<'gc> {
    pub fn new(mc: &Mutation<'gc>, value: Value<'gc>) -> Self {
        UpValue(Gc::new(mc, Lock::new(value)))
    }

    pub fn get(self) -> Value<'gc> {
        self.0.get()
    }

    pub fn set(self, mc: &Mutation<'gc>, value: Value<'gc>) {
        self.0.set(mc, value);
    }
}

impl fmt::Display for UpValue<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.get())
    }
}
