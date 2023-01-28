use core::ptr::NonNull;
use std::fmt::Display;

use crate::codegen::{Function, FunctionKind};

use super::{GCObject, Value};

/// The closure object. Any function is a closure.
#[derive(Debug, Clone)]
pub struct Closure {
    pub module_id: usize,
    pub function: Function,
    pub base_closure: Option<NonNull<GCObject>>,
    pub variables: Vec<Value>,
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
    pub fn new(
        module_id: usize,
        function: Function,
        base_closure: Option<NonNull<GCObject>>,
    ) -> Self {
        Closure {
            module_id,
            base_closure,
            variables: {
                let mut temp: Vec<Value> = Vec::with_capacity(function.local_names.len());
                for _ in 0..function.local_names.len() {
                    temp.push(Value::Null);
                }
                temp
            },
            function,
        }
    }
}
