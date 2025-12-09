use std::fmt;

use super::*;

/// Kind of member expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MemberKind<S> {
    /// `[]`
    Bracket(Box<Expr<S>>),
    /// `.`
    Dot(Box<Ident<S>>),
    /// `::`
    DoubleColon(Box<Ident<S>>),
    /// `[#]`
    BracketMeta,
    /// `.#`
    DotMeta,
    /// `::#`
    DoubleColonMeta,
}

impl<S: fmt::Display> fmt::Display for MemberKind<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MemberKind::Bracket(expr) => write!(f, "[{expr}]"),
            MemberKind::Dot(ident) => write!(f, ".{ident}"),
            MemberKind::DoubleColon(ident) => write!(f, "::{ident}"),
            MemberKind::BracketMeta => write!(f, "[#]"),
            MemberKind::DotMeta => write!(f, ".#"),
            MemberKind::DoubleColonMeta => write!(f, "::#"),
        }
    }
}
