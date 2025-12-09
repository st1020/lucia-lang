use std::fmt;

use text_size::TextRange;

use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssignLeft<S> {
    pub kind: AssignLeftKind<S>,
    pub range: TextRange,
}

impl_locatable!(AssignLeft);
impl_kind_display!(AssignLeft);

/// The left part of assign.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssignLeftKind<S> {
    Ident(Box<TypedIdent<S>>),
    Member {
        ident: Box<Ident<S>>,
        property: MemberKind<S>,
    },
}

impl<S: fmt::Display> fmt::Display for AssignLeftKind<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AssignLeftKind::Ident(ident) => write!(f, "{ident}"),
            AssignLeftKind::Member { ident, property } => write!(f, "{ident}{property}"),
        }
    }
}
