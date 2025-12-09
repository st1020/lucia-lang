use std::{fmt, sync::OnceLock};

use itertools::Itertools;
use text_size::TextRange;

use crate::compiler::index::FunctionId;

use super::*;

/// A function.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function<S> {
    pub name: Option<S>,
    pub kind: FunctionKind,
    pub params: Vec<TypedIdent<S>>,
    pub variadic: Option<Box<TypedIdent<S>>>,
    pub returns: Option<Box<Ty<S>>>,
    pub throws: Option<Box<Ty<S>>>,
    pub body: Box<Block<S>>,
    pub range: TextRange,
    pub function_id: OnceLock<FunctionId>,
}

impl_locatable!(Function);

impl<S: fmt::Display> fmt::Display for Function<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let params_str = self
            .params
            .iter()
            .map(ToString::to_string)
            .chain(
                self.variadic
                    .iter()
                    .map(|variadic| format!("...{variadic}")),
            )
            .join(", ");
        let returns_str = self
            .returns
            .as_ref()
            .map(|returns| format!(" -> {returns}"))
            .unwrap_or_default();
        let throws_str = self
            .throws
            .as_ref()
            .map(|throws| format!(" throw {throws}"))
            .unwrap_or_default();
        match self.kind {
            FunctionKind::Function => write!(
                f,
                "({}){}{} {}",
                params_str, returns_str, throws_str, self.body
            ),
            FunctionKind::Closure => {
                write!(
                    f,
                    "|{}|{}{} {}",
                    params_str, returns_str, throws_str, self.body
                )
            }
            FunctionKind::Do => write!(f, "do {}", self.body),
        }
    }
}

/// Kind of function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum FunctionKind {
    #[default]
    Function,
    Closure,
    Do,
}

impl fmt::Display for FunctionKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionKind::Function => write!(f, "Function"),
            FunctionKind::Closure => write!(f, "Closure"),
            FunctionKind::Do => write!(f, "Do"),
        }
    }
}
