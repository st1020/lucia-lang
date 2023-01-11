use std::fmt::{Debug, Display};

use crate::errors::Result;
use crate::lvm::Lvm;

use super::Value;

pub type ExtClosureFunc = fn(Vec<Value>, &mut Vec<Value>, &mut Lvm) -> Result<Value>;

#[derive(Clone)]
pub struct ExtClosure {
    pub func: ExtClosureFunc,
    pub upvalues: Vec<Value>,
}

impl Debug for ExtClosure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ExtClosure")
            .field("upvalues", &self.upvalues)
            .finish()
    }
}

impl PartialEq for ExtClosure {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

impl Display for ExtClosure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "function(ext_closure)")
    }
}

impl ExtClosure {
    pub fn new(func: ExtClosureFunc, upvalues: Vec<Value>) -> Self {
        ExtClosure { func, upvalues }
    }
}
