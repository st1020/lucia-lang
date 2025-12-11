use std::rc::Rc;

use crate::{
    compiler::code::{Code, UpvalueCapture},
    frame::LuciaFrame,
    objects::{Str, Value},
};

pub type Closure = Rc<ClosureInner>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClosureInner {
    pub code: Rc<Code<Str>>,
    pub upvalues: Vec<Value>,
}

impl ClosureInner {
    pub fn new(code: Rc<Code<Str>>, frame: Option<&LuciaFrame>) -> Self {
        let mut upvalues = Vec::with_capacity(code.upvalue_names.len());

        if let Some(frame) = frame {
            let base_locals = &frame.locals;
            let base_upvalues = &frame.closure.upvalues;
            for (_, upvalue_capture) in &code.upvalue_names {
                upvalues.push(match *upvalue_capture {
                    UpvalueCapture::Local(i) => base_locals[i].clone(),
                    UpvalueCapture::Upvalue(i) => base_upvalues[i].clone(),
                });
            }
        } else {
            for _ in 0..code.upvalue_names.len() {
                upvalues.push(Value::Null);
            }
        }

        Self { code, upvalues }
    }
}

impl From<ClosureInner> for Value {
    fn from(value: ClosureInner) -> Value {
        Value::Function(Rc::new(value).into())
    }
}
