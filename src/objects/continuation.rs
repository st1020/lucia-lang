use std::rc::Rc;

use crate::{frame::Frame, objects::Value};

pub type RcContinuation = Rc<Continuation>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Continuation {
    pub frames: Vec<Frame>,
}

impl Continuation {
    pub fn new(frames: Vec<Frame>) -> Self {
        Self { frames }
    }
}

impl From<Continuation> for Value {
    fn from(value: Continuation) -> Value {
        Value::Function(Rc::new(value).into())
    }
}
