use std::{cmp::Ordering, fmt};

use gc_arena::Collect;

use crate::{
    errors::{Error, RuntimeError},
    objects::{Callback, Closure, IntoValue, Table, UpValue, Value},
    utils::Join,
    Context,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Collect)]
#[collect[require_static]]
pub enum CatchErrorKind {
    None,
    Try,
    TryOption,
    TryPanic,
}

impl fmt::Display for CatchErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self, f)
    }
}

#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub enum Frame<'gc> {
    // An running Lua frame.
    Lua(LuciaFrame<'gc>),
    // A callback that has been queued but not called yet.
    Callback(Callback<'gc>, Vec<Value<'gc>>),
}

#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub struct LuciaFrame<'gc> {
    pub pc: usize,
    pub closure: Closure<'gc>,
    pub locals: Vec<Value<'gc>>,
    pub upvalues: Vec<UpValue<'gc>>,
    pub stack: Vec<Value<'gc>>,
    pub catch_error: CatchErrorKind,
}

impl<'gc> fmt::Display for LuciaFrame<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "pc: {}", self.pc)?;
        writeln!(
            f,
            "upvalues: [{}]",
            self.closure
                .function
                .upvalue_names
                .iter()
                .zip(self.closure.upvalues.iter())
                .map(|((name, _), value)| format!("{name}: {value}"))
                .join(", ")
        )?;
        writeln!(
            f,
            "locals: [{}]",
            self.closure
                .function
                .local_names
                .iter()
                .zip(self.locals.iter())
                .map(|(name, value)| format!("{name}: {value}"))
                .join(", ")
        )?;
        writeln!(f, "stack: [{}]", self.stack.iter().join(", "))?;
        write!(f, "catch_error: {:?}", self.catch_error)?;
        Ok(())
    }
}

impl<'gc> LuciaFrame<'gc> {
    pub(crate) fn new(
        ctx: Context<'gc>,
        closure: Closure<'gc>,
        mut args: Vec<Value<'gc>>,
    ) -> Result<Self, Error<'gc>> {
        let function = &closure.function;
        let params_num = function.params.len();
        let mut stack = vec![Value::Null; params_num];
        match args.len().cmp(&params_num) {
            Ordering::Less => {
                return Err(Error::new(RuntimeError::CallArguments {
                    required: if function.variadic.is_none() {
                        params_num.into()
                    } else {
                        (params_num, None).into()
                    },
                    given: args.len(),
                }));
            }
            Ordering::Equal => {
                stack[..params_num].copy_from_slice(&args[..]);
                if function.variadic.is_some() {
                    stack.push(Value::Table(Table::new(&ctx)));
                }
            }
            Ordering::Greater => {
                if function.variadic.is_none() {
                    return Err(Error::new(RuntimeError::CallArguments {
                        required: params_num.into(),
                        given: args.len(),
                    }));
                } else {
                    let t = args.split_off(params_num);
                    stack[..params_num].copy_from_slice(&args[..]);
                    stack.push(t.into_value(ctx));
                }
            }
        }

        let mut upvalues = Vec::with_capacity(function.upvalue_names.len());
        for (_, base_closure_upvalue_id) in &function.upvalue_names {
            upvalues.push(base_closure_upvalue_id.map_or_else(
                || UpValue::new(&ctx, Value::Null),
                |id| closure.upvalues[id],
            ));
        }

        Ok(LuciaFrame {
            pc: 0,
            closure,
            locals: vec![Value::Null; function.local_names.len()],
            upvalues,
            stack,
            catch_error: CatchErrorKind::None,
        })
    }
}
