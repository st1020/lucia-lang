use std::fmt;

use itertools::Itertools;
use ordermap::{OrderMap, OrderSet};
use oxc_index::IndexVec;

use crate::{
    compiler::index::{CodeId, LocalNameId},
    objects::{Callback, RcClosure, RcEffect, Value},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EffectHandlerInfo {
    pub jump_target: CodeId,
    pub stack_size: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Frame {
    /// A running Lucia frame.
    Lucia {
        pc: CodeId,
        closure: RcClosure,
        locals: IndexVec<LocalNameId, Value>,
        stack: Vec<Value>,
        effect_handlers: OrderMap<RcEffect, EffectHandlerInfo>,
    },
    /// A callback that has been queued but not called yet.
    Callback {
        callback: Callback,
        stack: Vec<Value>,
        effect_handlers: OrderSet<RcEffect>,
    },
}

impl fmt::Display for Frame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Frame::Lucia {
                pc,
                closure,
                locals,
                stack,
                effect_handlers,
            } => {
                writeln!(f, "pc: {pc}")?;
                writeln!(
                    f,
                    "locals: [{}]",
                    closure
                        .code
                        .local_names
                        .iter()
                        .zip(locals.iter())
                        .map(|(name, value)| format!("{name}: {value}"))
                        .join(", ")
                )?;
                writeln!(f, "stack: [{}]", stack.iter().join(", "))?;
                writeln!(
                    f,
                    "effect_handlers: [{}]",
                    effect_handlers
                        .iter()
                        .map(|(effect, jump_target)| format!("{effect} -> {jump_target:?}"))
                        .join(", ")
                )?;
            }
            Frame::Callback {
                callback,
                stack,
                effect_handlers,
            } => {
                writeln!(f, "callback: {callback}")?;
                writeln!(f, "stack: [{}]", stack.iter().join(", "))?;
                writeln!(
                    f,
                    "effect_handlers: [{}]",
                    effect_handlers
                        .iter()
                        .map(|effect| format!("{effect}"))
                        .join(", ")
                )?;
            }
        }
        Ok(())
    }
}
