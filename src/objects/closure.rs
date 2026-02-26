use std::rc::Rc;

use oxc_index::{IndexVec, index_vec};

use crate::{
    compiler::{
        code::{Code, UpvalueCapture},
        index::{LocalNameId, UpvalueNameId},
    },
    objects::{RcStr, Value},
};

pub type RcClosure = Rc<Closure>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Closure {
    pub code: Rc<Code<RcStr>>,
    pub upvalues: IndexVec<UpvalueNameId, Value>,
}

impl Closure {
    pub fn new(code: Rc<Code<RcStr>>) -> Self {
        Self {
            upvalues: index_vec![Value::Null; code.upvalue_names.len()],
            code,
        }
    }

    pub fn with_base_closure(
        code: Rc<Code<RcStr>>,
        base_locals: &IndexVec<LocalNameId, Value>,
        base_closure: &Closure,
    ) -> Self {
        let mut upvalues = IndexVec::with_capacity(code.upvalue_names.len());

        for (_, upvalue_capture) in &code.upvalue_names {
            upvalues.push(match *upvalue_capture {
                UpvalueCapture::Local(i) => base_locals[i].clone(),
                UpvalueCapture::Upvalue(i) => base_closure.upvalues[i].clone(),
            });
        }

        Self { code, upvalues }
    }
}

impl From<Closure> for Value {
    fn from(value: Closure) -> Value {
        Value::Function(Rc::new(value).into())
    }
}
