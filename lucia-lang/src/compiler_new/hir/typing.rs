//! The Lucia Type System.

use std::{fmt, ops::BitOr};

use smol_str::SmolStr;
use thiserror::Error;

use crate::utils::{Float, Join};

use super::BinOp;

/// The Literal Type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LiteralType {
    Bool(bool),
    Int(i64),
    Float(Float),
    Str(SmolStr),
}

impl fmt::Display for LiteralType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LiteralType::Bool(v) => write!(f, "{}", v),
            LiteralType::Int(v) => write!(f, "{}", v),
            LiteralType::Float(v) => write!(f, "{}", v),
            LiteralType::Str(v) => write!(f, "\"{}\"", v),
        }
    }
}

/// All types used in type checker.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    // Special Atom Types
    Unknown,
    Never,
    Any,

    // Literal Type
    Literal(LiteralType),

    // Atom Types
    Null,
    Bool,
    Int,
    Float,
    Str,
    // Table Types
    Table {
        pairs: Vec<(LiteralType, Type)>,
        others: Option<(Box<Type>, Box<Type>)>,
    },
    // Function Types
    Function {
        params: Vec<Type>,
        variadic: Box<Type>,
        returns: Box<Type>,
        throws: Box<Type>,
    },
    // UserData Types
    UserData(SmolStr),

    // Union Types
    Union(Vec<Type>),
}

impl Type {
    pub fn any_table() -> Type {
        Type::Table {
            pairs: Vec::new(),
            others: Some((Box::new(Type::Any), Box::new(Type::Any))),
        }
    }

    pub fn any_function() -> Type {
        Type::Function {
            params: Vec::new(),
            variadic: Box::new(Type::Any),
            returns: Box::new(Type::Any),
            throws: Box::new(Type::Any),
        }
    }

    pub fn union(&self, other: &Type) -> Type {
        if self == other {
            return self.clone();
        }
        match (self, other) {
            (Type::Union(union1), Type::Union(union2)) => {
                let mut new_union = union1.clone();
                new_union.append(&mut union2.clone());
                new_union.sort();
                new_union.dedup();
                Type::Union(new_union)
            }
            (Type::Any, _) | (_, Type::Any) => Type::Any,
            (Type::Never, t) | (t, Type::Never) => t.clone(),
            (Type::Union(union), t) | (t, Type::Union(union)) => {
                let mut new_union = union.clone();
                new_union.push(t.clone());
                new_union.sort();
                new_union.dedup();
                Type::Union(new_union)
            }
            (t1, t2) => Type::Union(vec![t1.clone(), t2.clone()]),
        }
    }

    pub fn optional(&self) -> Type {
        self.union(&Type::Null)
    }

    pub fn is_sub_type_of(&self, other: &Type) -> bool {
        if self == other {
            return true;
        }
        match (self, other) {
            (_, Type::Unknown) => true,
            (Type::Unknown, _) => false,
            (Type::Any, _) | (_, Type::Any) => true,
            (Type::Never, _) => false,
            (
                Type::Table {
                    pairs: pairs1,
                    others: others1,
                },
                Type::Table {
                    pairs: pairs2,
                    others: others2,
                },
            ) => {
                pairs1.iter().all(|(k1, v1)| {
                    pairs2
                        .iter()
                        .find(|(k2, _)| k1 == k2)
                        .map(|(_, v2)| v2)
                        .or_else(|| others2.as_ref().map(|(_, v)| &**v))
                        .is_some_and(|v2| v1.is_sub_type_of(v2))
                }) && match (others1, others2) {
                    (None, _) => true,
                    (Some((key1, value1)), Some((key2, value2))) => {
                        key1.is_sub_type_of(key2) && value1.is_sub_type_of(value2)
                    }
                    _ => false,
                }
            }
            (
                Type::Function {
                    params: params1,
                    variadic: variadic1,
                    returns: returns1,
                    throws: throws1,
                },
                Type::Function {
                    params: params2,
                    variadic: variadic2,
                    returns: returns2,
                    throws: throws2,
                },
            ) => {
                if params1.len() != params2.len() {
                    false
                } else {
                    params1
                        .iter()
                        .zip(params2)
                        .all(|(p1, p2)| p2.is_sub_type_of(p1))
                        && variadic2.is_sub_type_of(variadic1)
                        && returns1.is_sub_type_of(returns2)
                        && throws1.is_sub_type_of(throws2)
                }
            }
            (Type::Literal(literal), t) => match literal {
                LiteralType::Bool(_) => Type::Bool.is_sub_type_of(t),
                LiteralType::Int(_) => Type::Int.is_sub_type_of(t),
                LiteralType::Float(_) => Type::Float.is_sub_type_of(t),
                LiteralType::Str(_) => Type::Str.is_sub_type_of(t),
            },
            (t, Type::Union(types)) => types.iter().any(|x| t.is_sub_type_of(x)),
            (t1, t2) => t1 == t2,
        }
    }

    pub fn expect_is_sub_type_of(&self, other: &Type) -> Result<(), TypeCheckError> {
        if self.is_sub_type_of(other) {
            Ok(())
        } else {
            Err(TypeCheckError::ExpectIsSubtypeOf {
                t: Box::new(self.clone()),
                expected: Box::new(other.clone()),
            })
        }
    }

    pub fn get_member_type(&self, property: &Type) -> Result<Type, TypeCheckError> {
        match self {
            Type::Any => Some(Type::Any),
            Type::Table { pairs, others } => pairs
                .iter()
                .find(|&(key, _)| {
                    if let Type::Literal(lit) = property {
                        lit == key
                    } else {
                        false
                    }
                })
                .map(|(_, t)| t.clone())
                .or_else(|| {
                    others.clone().and_then(|(key, value)| {
                        if property.is_sub_type_of(&key) {
                            Some(*value)
                        } else {
                            None
                        }
                    })
                }),
            _ => None,
        }
        .ok_or_else(|| TypeCheckError::TableMemberNotFound {
            table: Box::new(self.clone()),
            property: Box::new(property.clone()),
        })
    }
}

impl BitOr for Type {
    type Output = Type;

    fn bitor(self, rhs: Self) -> Self::Output {
        self.union(&rhs)
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unknown => write!(f, "unknown"),
            Type::Never => write!(f, "never"),
            Type::Any => write!(f, "any"),
            Type::Literal(literal) => write!(f, "{:?}", literal),
            Type::Null => write!(f, "null"),
            Type::Bool => write!(f, "bool"),
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Str => write!(f, "str"),
            Type::Table { pairs, others } => match (pairs.len(), others) {
                (0, None) => write!(f, "{{}}"),
                (0, Some(others)) => write!(f, "{{[{}]: {}}}", others.0, others.1),
                (_, None) => write!(
                    f,
                    "{{{}}}",
                    pairs
                        .iter()
                        .map(|(name, t)| format!("{name}: {t}"))
                        .join(", ")
                ),
                (_, Some(others)) => write!(
                    f,
                    "{{{}, [{}]: {}}}",
                    pairs
                        .iter()
                        .map(|(name, t)| format!("{name}: {t}"))
                        .join(", "),
                    others.0,
                    others.1
                ),
            },
            Type::Function {
                params,
                variadic,
                returns,
                throws,
            } => {
                if variadic.as_ref() == &Type::Null {
                    write!(
                        f,
                        "fn({}) -> {} throw {}",
                        params.iter().join(", "),
                        returns,
                        throws
                    )
                } else {
                    write!(
                        f,
                        "fn({}, *{}) -> {} throw {}",
                        params.iter().join(", "),
                        variadic,
                        returns,
                        throws
                    )
                }
            }
            Type::UserData(v) => write!(f, "{}", v),
            Type::Union(types) => write!(f, "({})", types.iter().join(" | ")),
        }
    }
}

/// Kind of TypeCheckError.
#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum TypeCheckError {
    #[error("unexpected {t} is subtype of {expected} ")]
    ExpectIsSubtypeOf { t: Box<Type>, expected: Box<Type> },
    #[error("{table} dose not has member {property} ")]
    TableMemberNotFound {
        table: Box<Type>,
        property: Box<Type>,
    },
    #[error("unsupported operand type(s) for {operator}: {} and {}", .operand.0, .operand.1)]
    UnsupportedBinOperator {
        operator: BinOp,
        operand: (Box<Type>, Box<Type>),
    },
}
