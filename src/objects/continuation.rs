use std::rc::Rc;

use crate::{frame::Frame, objects::Value};

pub type Continuation = Rc<ContinuationInner>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ContinuationInner {
    pub frames: Vec<Frame>,
}

impl ContinuationInner {
    pub fn new(frames: Vec<Frame>) -> Self {
        Self { frames }
    }
}

impl From<ContinuationInner> for Value {
    fn from(value: ContinuationInner) -> Value {
        Value::Function(Rc::new(value).into())
    }
}
