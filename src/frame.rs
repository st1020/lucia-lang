use std::{cmp::Ordering, fmt};

use itertools::Itertools;
use ordermap::OrderMap;
use oxc_index::{IndexVec, index_vec};

use crate::{
    compiler::{
        code::CodeParamsInfo,
        index::{CodeId, LocalNameId},
    },
    errors::Error,
    objects::{Callback, Closure, Effect, TableInner, Value},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EffectHandlerInfo {
    pub jump_target: CodeId,
    pub stack_size: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Frame {
    /// A running Lucia frame.
    Lucia(LuciaFrame),
    /// A callback that has been queued but not called yet.
    Callback {
        callback: Callback,
        args: Vec<Value>,
    },
    /// The result of the continuation.
    /// Must be the top frame.
    Result { value: Value },
    /// An error that has occurred.
    /// Must be the top frame of the stack.
    Error { error: Error },
    /// An effect that has been performed but not handled.
    /// Must be the top frame.
    Effect { effect: Effect, args: Vec<Value> },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LuciaFrame {
    pub pc: CodeId,
    pub closure: Closure,
    pub locals: IndexVec<LocalNameId, Value>,
    pub stack: Vec<Value>,
    pub effect_handlers: OrderMap<Effect, EffectHandlerInfo>,
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
        writeln!(
            f,
            "effect_handlers: [{}]",
            self.effect_handlers
                .iter()
                .map(|(effect, jump_target)| format!("{effect} -> {jump_target:?}"))
                .join(", ")
        )?;
        Ok(())
    }
}

impl LuciaFrame {
    const MAX_STACK_SIZE: usize = 256;

    pub(crate) fn new(closure: Closure, args: &[Value]) -> Result<Self, Error> {
        let mut stack = Vec::with_capacity(closure.code.stack_size.min(Self::MAX_STACK_SIZE));
        closure
            .code
            .params
            .info()
            .parse_args_to_stack(&mut stack, args)?;
        Ok(LuciaFrame {
            pc: CodeId::new(0),
            locals: index_vec![Value::Null; closure.code.local_names.len()],
            stack,
            closure,
            effect_handlers: OrderMap::new(),
        })
    }
}

impl CodeParamsInfo {
    pub(crate) fn parse_args_to_stack(
        &self,
        stack: &mut Vec<Value>,
        args: &[Value],
    ) -> Result<(), Error> {
        match args.len().cmp(&self.params_count) {
            Ordering::Less => Err(Error::CallArguments {
                required: if self.has_variadic {
                    (self.params_count, None).into()
                } else {
                    self.params_count.into()
                },
                given: args.len(),
            }),
            Ordering::Equal => {
                stack.extend_from_slice(args);
                if self.has_variadic {
                    stack.push(Value::Table(TableInner::new().into()));
                }
                Ok(())
            }
            Ordering::Greater => {
                if self.has_variadic {
                    let (params, variadic) = args.split_at(self.params_count);
                    stack.extend_from_slice(params);
                    stack.push(variadic.into());
                    Ok(())
                } else {
                    Err(Error::CallArguments {
                        required: self.params_count.into(),
                        given: args.len(),
                    })
                }
            }
        }
    }
}
