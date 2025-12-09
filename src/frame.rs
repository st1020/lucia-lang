use std::{cmp::Ordering, fmt};

use derive_more::Display;
use itertools::Itertools;

use crate::{
    errors::{Error, ErrorKind},
    objects::{Callback, Closure, TableInner, Value},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Display)]
#[display("{self:?}")]
pub enum CallStatusKind {
    Normal,
    Try,
    TryOption,
    TryPanic,
}

#[derive(Debug, Clone)]
pub enum Frame {
    // An running Lucia frame.
    Lucia(LuciaFrame),
    // A callback that has been queued but not called yet.
    Callback {
        callback: Callback,
        args: Vec<Value>,
    },
}

#[derive(Debug, Clone)]
pub struct LuciaFrame {
    pub pc: usize,
    pub closure: Closure,
    pub locals: Vec<Value>,
    pub stack: Vec<Value>,
    pub call_status: CallStatusKind,
}

impl fmt::Display for LuciaFrame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "pc: {}", self.pc)?;
        writeln!(
            f,
            "locals: [{}]",
            self.closure
                .code
                .local_names
                .iter()
                .zip(self.locals.iter())
                .map(|(name, value)| format!("{name}: {value}"))
                .join(", ")
        )?;
        writeln!(f, "stack: [{}]", self.stack.iter().join(", "))?;
        write!(f, "call_status: {}", self.call_status)?;
        Ok(())
    }
}

impl LuciaFrame {
    const MAX_STACK_SIZE: usize = 256;

    pub(crate) fn new(closure: Closure, mut args: Vec<Value>) -> Result<Self, Error> {
        let params_num = closure.code.params.len();
        let has_variadic = closure.code.variadic.is_some();
        let mut stack = Vec::with_capacity(closure.code.stack_size.min(Self::MAX_STACK_SIZE));
        match args.len().cmp(&params_num) {
            Ordering::Less => {
                return Err(Error::new(ErrorKind::CallArguments {
                    required: if has_variadic {
                        (params_num, None).into()
                    } else {
                        params_num.into()
                    },
                    given: args.len(),
                }));
            }
            Ordering::Equal => {
                stack.append(&mut args.clone());
                if has_variadic {
                    stack.push(Value::Table(TableInner::new().into()));
                }
            }
            Ordering::Greater => {
                if !has_variadic {
                    return Err(Error::new(ErrorKind::CallArguments {
                        required: params_num.into(),
                        given: args.len(),
                    }));
                }
                let t = args.split_off(params_num);
                stack.append(&mut args.clone());
                stack.push(t.into());
            }
        }

        Ok(LuciaFrame {
            pc: 0,
            locals: vec![Value::Null; closure.code.local_names.len()],
            closure,
            stack,
            call_status: CallStatusKind::Normal,
        })
    }
}
