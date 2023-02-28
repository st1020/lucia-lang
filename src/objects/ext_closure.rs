use std::fmt::{Debug, Display};

use crate::errors::Result;
use crate::gc::Trace;
use crate::lvm::Lvm;
use crate::utils::ValueVecDebug;

use super::Value;

pub type ExtClosureFunc = fn(Vec<Value>, &mut Vec<Value>, &mut Lvm) -> Result<Value>;

#[derive(Clone)]
pub struct ExtClosure {
    pub func: ExtClosureFunc,
    pub upvalues: Vec<Value>,
}

unsafe impl Trace for ExtClosure {
    #[inline]
    unsafe fn trace(&self) {
        for i in &self.upvalues {
            i.trace();
        }
    }
}

impl Debug for ExtClosure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ExtClosure")
            .field("upvalues", &ValueVecDebug(&self.upvalues))
            .finish()
    }
}

impl PartialEq for ExtClosure {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

impl Eq for ExtClosure {}

impl Display for ExtClosure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<function(ext_closure)>")
    }
}

impl ExtClosure {
    pub fn new(func: ExtClosureFunc, upvalues: Vec<Value>) -> Self {
        ExtClosure { func, upvalues }
    }
}
