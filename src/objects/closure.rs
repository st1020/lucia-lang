use core::ptr::NonNull;
use std::fmt::Display;

use crate::codegen::Function;

use super::{GCObject, LuciaValue};

/// The closure object. Any function is a closure.
#[derive(Debug, Clone)]
pub struct Closure {
    pub module_id: usize,
    pub function: Function,
    pub base_closure: Option<NonNull<GCObject>>,
    pub variables: Vec<LuciaValue>,
}

impl PartialEq for Closure {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

impl Display for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "function")
    }
}
