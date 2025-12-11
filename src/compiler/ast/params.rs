use std::fmt;

use itertools::Itertools;

use super::*;

/// Parameters of callable value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Params<S> {
    pub params: Vec<TypedIdent<S>>,
    pub variadic: Option<Box<TypedIdent<S>>>,
}

impl<S> Params<S> {
    pub(crate) fn empty() -> Self {
        Self {
            params: Vec::new(),
            variadic: None,
        }
    }
}

impl<S: fmt::Display> fmt::Display for Params<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.params
                .iter()
                .map(ToString::to_string)
                .chain(
                    self.variadic
                        .iter()
                        .map(|variadic| format!("...{variadic}")),
                )
                .join(", ")
        )
    }
}
