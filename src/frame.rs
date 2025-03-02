use std::{cmp::Ordering, fmt};

use gc_arena::Collect;

use crate::{
    Context,
    errors::{Error, RuntimeError},
    objects::{Callback, Closure, IntoValue, Table, UpValue, Value},
    utils::Join,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Collect)]
#[collect[require_static]]
pub enum CallStatusKind {
    Normal,
    IgnoreReturn,
    Try,
    TryOption,
    TryPanic,
}

impl fmt::Display for CallStatusKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self, f)
    }
}

#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub enum Frame<'gc> {
    // An running Lucia frame.
    Lucia(LuciaFrame<'gc>),
    // A callback that has been queued but not called yet.
    Callback(Callback<'gc>, Vec<Value<'gc>>),
}

#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub struct LuciaFrame<'gc> {
    pub pc: usize,
    pub closure: Closure<'gc>,
    pub locals: Vec<Value<'gc>>,
    pub stack: Vec<Value<'gc>>,
    pub call_status: CallStatusKind,
}

impl fmt::Display for LuciaFrame<'_> {
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
        write!(f, "call_status: {:?}", self.call_status)?;
        Ok(())
    }
}

impl<'gc> LuciaFrame<'gc> {
    const MAX_STACK_SIZE: usize = 256;

    pub(crate) fn new(
        ctx: Context<'gc>,
        closure: Closure<'gc>,
        mut args: Vec<Value<'gc>>,
    ) -> Result<Self, Error<'gc>> {
        let code = &closure.code;
        let params_num = code.params.len();
        let mut stack = Vec::with_capacity(code.stack_size.min(Self::MAX_STACK_SIZE));
        match args.len().cmp(&params_num) {
            Ordering::Less => {
                return Err(Error::new(RuntimeError::CallArguments {
                    required: if code.variadic.is_none() {
                        params_num.into()
                    } else {
                        (params_num, None).into()
                    },
                    given: args.len(),
                }));
            }
            Ordering::Equal => {
                stack.append(&mut args.clone());
                if code.variadic.is_some() {
                    stack.push(Value::Table(Table::new(&ctx)));
                }
            }
            Ordering::Greater => {
                if code.variadic.is_none() {
                    return Err(Error::new(RuntimeError::CallArguments {
                        required: params_num.into(),
                        given: args.len(),
                    }));
                } else {
                    let t = args.split_off(params_num);
                    stack.append(&mut args.clone());
                    stack.push(t.into_value(ctx));
                }
            }
        }

        let mut upvalues = closure.upvalues.borrow_mut(&ctx);
        for (i, (_, base_closure_upvalue_id)) in code.upvalue_names.iter().enumerate() {
            if base_closure_upvalue_id.is_none() {
                upvalues[i] = UpValue::new(&ctx, Value::Null);
            }
        }

        Ok(LuciaFrame {
            pc: 0,
            closure,
            locals: vec![Value::Null; code.local_names.len()],
            stack,
            call_status: CallStatusKind::Normal,
        })
    }
}
