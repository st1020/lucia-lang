use std::rc::Rc;

use oxc_index::IndexVec;

use crate::{
    compiler::{
        code::{Code, UpvalueCapture},
        index::UpvalueNameId,
    },
    frame::LuciaFrame,
    objects::{RcStr, Value},
};

pub type RcClosure = Rc<Closure>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Closure {
    pub code: Rc<Code<RcStr>>,
    pub upvalues: IndexVec<UpvalueNameId, Value>,
}

impl Closure {
    pub fn new(code: Rc<Code<RcStr>>, frame: Option<&LuciaFrame>) -> Self {
        let mut upvalues = IndexVec::with_capacity(code.upvalue_names.len());

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

impl From<Closure> for Value {
    fn from(value: Closure) -> Value {
        Value::Function(Rc::new(value).into())
    }
}
