use std::fmt;

use itertools::Itertools;
use text_size::TextRange;

use super::*;

/// A type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ty<S> {
    pub kind: TyKind<S>,
    pub range: TextRange,
}

impl_locatable!(Ty);
impl_kind_display!(Ty);

/// Kind of type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyKind<S> {
    Lit(Box<Lit<S>>),
    Ident(Box<Ident<S>>),
    Paren(Box<Ty<S>>),
    Table {
        pairs: Vec<TableTyPair<S>>,
        others: Option<Box<TableTyOther<S>>>,
    },
    Function {
        params: Vec<Ty<S>>,
        variadic: Option<Box<Ty<S>>>,
        returns: Option<Box<Ty<S>>>,
        throws: Option<Box<Ty<S>>>,
    },
    Union(Vec<Ty<S>>),
}

impl<S: AsRef<str>> fmt::Display for TyKind<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TyKind::Lit(lit) => write!(f, "{lit}"),
            TyKind::Ident(ident) => write!(f, "{ident}"),
            TyKind::Paren(ty) => write!(f, "({ty})"),
            TyKind::Table { pairs, others } => match (pairs.len(), others) {
                (0, None) => write!(f, "{{}}"),
                (0, Some(others)) => write!(f, "{{{others}}}"),
                (_, None) => write!(f, "{{{}}}", pairs.iter().join(", ")),
                (_, Some(others)) => write!(f, "{{{}, {}}}", pairs.iter().join(", "), others),
            },
            TyKind::Function {
                params,
                variadic,
                returns,
                throws,
            } => {
                let params_str = params
                    .iter()
                    .map(|param| param.to_string())
                    .chain(variadic.iter().map(|variadic| format!("...{variadic}")))
                    .join(", ");
                let returns_str = returns
                    .as_ref()
                    .map(|returns| format!(" -> {returns}"))
                    .unwrap_or_default();
                let throws_str = throws
                    .as_ref()
                    .map(|throws| format!(" throw {throws}"))
                    .unwrap_or_default();
                write!(f, "fn({params_str}){returns_str}{throws_str}")
            }
            TyKind::Union(union) => write!(f, "{}", union.iter().join(" | ")),
        }
    }
}

/// A table type pair.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TableTyPair<S> {
    pub key: Ident<S>,
    pub value: Ty<S>,
    pub range: TextRange,
}

impl_locatable!(TableTyPair);

impl<S: AsRef<str>> fmt::Display for TableTyPair<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.key, self.value)
    }
}

/// A table type other.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TableTyOther<S> {
    pub key: Ty<S>,
    pub value: Ty<S>,
    pub range: TextRange,
}

impl_locatable!(TableTyOther);

impl<S: AsRef<str>> fmt::Display for TableTyOther<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}]: {}", self.key, self.value)
    }
}
