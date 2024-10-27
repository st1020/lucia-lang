use std::fmt;

use gc_arena::{lock::Lock, Collect, Gc, Mutation};

use crate::{
    frame::LuciaFrame,
    objects::{define_object, RuntimeCode, Value},
};

define_object!(Closure, ClosureInner<'gc>, ptr, "closure");

impl<'gc> Closure<'gc> {
    pub fn new(
        mc: &Mutation<'gc>,
        function: RuntimeCode<'gc>,
        frame: Option<&LuciaFrame<'gc>>,
    ) -> Self {
        let mut upvalues = Vec::with_capacity(function.upvalue_names.len());

        if let Some(frame) = frame {
            for (_, base_closure_upvalue_id) in &function.upvalue_names {
                upvalues.push(
                    base_closure_upvalue_id
                        .map_or_else(|| UpValue::new(mc, Value::Null), |id| frame.upvalues[id]),
                );
            }
        } else {
            for _ in 0..function.upvalue_names.len() {
                upvalues.push(UpValue::new(mc, Value::Null));
            }
        }

        Closure(Gc::new(mc, ClosureInner { upvalues, function }))
    }
}

#[derive(Debug, Collect)]
#[collect(no_drop)]
pub struct ClosureInner<'gc> {
    pub function: RuntimeCode<'gc>,
    pub upvalues: Vec<UpValue<'gc>>,
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

impl<'gc> fmt::Display for UpValue<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.get())
    }
}
