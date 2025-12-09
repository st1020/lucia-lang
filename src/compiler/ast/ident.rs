use std::{fmt, sync::OnceLock};

use text_size::TextRange;

use crate::compiler::index::ReferenceId;

use super::*;

/// An ident.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident<S> {
    pub name: S,
    pub range: TextRange,
    pub reference_id: OnceLock<ReferenceId>,
}

impl_locatable!(Ident);

impl<S: fmt::Display> fmt::Display for Ident<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

/// The ident with type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedIdent<S> {
    pub ident: Box<Ident<S>>,
    pub ty: Option<Ty<S>>,
    pub range: TextRange,
}

impl_locatable!(TypedIdent);

impl<S: fmt::Display> fmt::Display for TypedIdent<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(t) = &self.ty {
            write!(f, "{}: {}", self.ident, t)
        } else {
            write!(f, "{}", self.ident)
        }
    }
}
