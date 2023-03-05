use std::fmt::{Debug, Display};

use crate::code::{Code, FunctionKind};
use crate::gc::{Gc, RefCell, Trace};
use crate::utils::ValueVecDebug;

use super::Value;

/// The closure object. Any function is a closure.
#[derive(Clone)]
pub struct Closure {
    pub function: Code,
    pub base_closure: Option<Gc<RefCell<Closure>>>,
    pub upvalues: Vec<Value>,
}

unsafe impl Trace for Closure {
    #[inline]
    unsafe fn trace(&self) {
        for i in &self.upvalues {
            i.trace();
        }
    }
}

impl Debug for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Closure")
            .field("function", &self.function)
            .field(
                "base_closure",
                &self.base_closure.as_ref().map(|v| v.borrow()),
            )
            .field("variables", &ValueVecDebug(&self.upvalues))
            .finish()
    }
}

impl PartialEq for Closure {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

impl Eq for Closure {}

impl Display for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.function.kind {
            FunctionKind::Funciton => write!(f, "<function>"),
            FunctionKind::Closure => write!(f, "<function(closure)>"),
            FunctionKind::Do => write!(f, "<function(do)>"),
        }
    }
}

impl Closure {
    pub fn new(function: Code, base_closure: Option<Gc<RefCell<Closure>>>) -> Self {
        Closure {
            base_closure,
            upvalues: vec![Value::Null; function.def_upvalue_count],
            function,
        }
    }
}
