use std::{
    fmt,
    hash::{Hash, Hasher},
    ops::Deref,
};

use gc_arena::{lock::Lock, Collect, Gc, Mutation};

use crate::{
    frame::LuciaFrame,
    objects::{RuntimeCode, Value},
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
    pub function: RuntimeCode<'gc>,
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

impl<'gc> Deref for UpValue<'gc> {
    type Target = Gc<'gc, Lock<Value<'gc>>>;

    fn deref(&self) -> &Gc<'gc, Lock<Value<'gc>>> {
        &self.0
    }
}

impl<'gc> fmt::Display for UpValue<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.get())
    }
}
