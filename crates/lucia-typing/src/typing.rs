use std::{fmt, ops};

use itertools::Itertools;
use lucia_lang::compiler::ast::*;
use sorted_vec::SortedSet;

use crate::error::TypeError;

/// All types used in type checker.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type<S: Eq + Ord> {
    // Special Atom Types
    Unknown,
    Never,
    Any,

    // Literal Type
    Literal(LitKind<S>),

    // Atom Types
    Bool,
    Int,
    Float,
    Str,
    Bytes,
    Table(Box<TableType<S>>),
    Function(Box<FunctionType<S>>),
    UserData(S),

    // Union Types
    Union(SortedSet<Type<S>>),
}

impl<S: Clone + Eq + Ord> Type<S> {
    pub const NULL: Self = Type::Literal(LitKind::Null);

    pub fn union(&self, other: &Type<S>) -> Type<S> {
        if self.is_subtype_of(other) {
            return other.clone();
        }
        if other.is_subtype_of(self) {
            return self.clone();
        }
        match (self, other) {
            (Type::Any, _) | (_, Type::Any) => Type::Any,
            (Type::Never, t) | (t, Type::Never) => t.clone(),
            (Type::Union(union1), Type::Union(union2)) => {
                let mut new_union = union1.clone();
                new_union.mutate_vec(|new_union| new_union.append(&mut union2.to_vec()));
                Type::Union(new_union)
            }
            (Type::Union(union), t) | (t, Type::Union(union)) => {
                let mut new_union = union.clone();
                new_union.push(t.clone());
                Type::Union(new_union)
            }
            (t1, t2) => Type::Union(vec![t1.clone(), t2.clone()].into()),
        }
    }

    pub fn is_subtype_of(&self, other: &Type<S>) -> bool {
        match (self, other) {
            (_, Type::Unknown) => true,
            (Type::Unknown, _) => false,
            (Type::Any, _) | (_, Type::Any) => true,
            (Type::Never, Type::Never) => true,
            (Type::Never, _) => false,
            (Type::Literal(literal), ty) => match literal {
                LitKind::Null => self == other,
                LitKind::Bool(_) => Type::Bool.is_subtype_of(ty),
                LitKind::Int(_) => Type::Int.is_subtype_of(ty),
                LitKind::Float(_) => Type::Float.is_subtype_of(ty),
                LitKind::Str(_) => Type::Str.is_subtype_of(ty),
                LitKind::Bytes(_) => Type::Bytes.is_subtype_of(ty),
            },
            (Type::Table(table1), Type::Table(table2)) => table1 == table2,
            (Type::Function(function1), Type::Function(function2)) => {
                for i in 0..function1.params.len().min(function2.params.len()) {
                    if !function2.params[i]
                        .ty
                        .is_subtype_of(&function1.params[i].ty)
                    {
                        return false;
                    }
                }
                if !function1.variadic.ty.is_subtype_of(&function2.variadic.ty) {
                    return false;
                }
                if function1.params.len() > function2.params.len() {
                    for param in &function1.params[function2.params.len()..] {
                        if !function2.variadic.ty.is_subtype_of(&param.ty) {
                            return false;
                        }
                    }
                }
                if function2.params.len() > function1.params.len() {
                    for param in &function2.params[function2.params.len()..] {
                        if !param.ty.is_subtype_of(&function1.variadic.ty) {
                            return false;
                        }
                    }
                }
                function1.returns.is_subtype_of(&function2.returns)
                    && function1.throws.is_subtype_of(&function2.throws)
            }
            (ty, Type::Union(union)) => union.iter().any(|x| ty.is_subtype_of(x)),
            (t1, t2) => t1 == t2,
        }
    }

    pub fn expect_is_subtype_of(&self, other: &Type<S>) -> Result<(), TypeError<S>> {
        if self.is_subtype_of(other) {
            Ok(())
        } else {
            Err(TypeError::ExpectIsSubtypeOf {
                ty: Box::new(self.clone()),
                expected: Box::new(other.clone()),
            })
        }
    }

    pub fn is_null(&self) -> bool {
        matches!(self, Type::Literal(LitKind::Null))
    }

    pub fn metatable(&self) -> Option<Box<TableType<S>>> {
        if let Type::Table(table) = self {
            table.metatable.clone()
        } else {
            None
        }
    }
}

impl<S: Clone + Eq + Ord> ops::BitOr for Type<S> {
    type Output = Type<S>;

    fn bitor(self, rhs: Self) -> Self::Output {
        self.union(&rhs)
    }
}

impl<S: AsRef<str> + Eq + Ord + fmt::Display> fmt::Display for Type<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unknown => write!(f, "unknown"),
            Type::Never => write!(f, "never"),
            Type::Any => write!(f, "any"),
            Type::Literal(literal_type) => write!(f, "{literal_type}"),
            Type::Bool => write!(f, "bool"),
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Str => write!(f, "str"),
            Type::Bytes => write!(f, "bytes"),
            Type::Table(table) => write!(f, "{table}"),
            Type::Function(function) => write!(f, "{function}"),
            Type::UserData(userdata) => write!(f, "{}", userdata.as_ref()),
            Type::Union(union) => write!(f, "({})", union.iter().join(" | ")),
        }
    }
}

impl<S: AsRef<str> + Clone + Eq + Ord> From<TyKind<S>> for Type<S> {
    fn from(value: TyKind<S>) -> Self {
        match value {
            TyKind::Lit(lit) => lit.kind.into(),
            TyKind::Ident(ident) => match ident.name.as_ref() {
                "never" => Type::Never,
                "any" => Type::Any,
                "bool" => Type::Bool,
                "int" => Type::Int,
                "float" => Type::Float,
                "str" => Type::Str,
                _ => Type::UserData(ident.name),
            },
            TyKind::Paren(ty) => ty.kind.into(),
            TyKind::Table { pairs, others } => TableType {
                pairs: pairs
                    .into_iter()
                    .map(|pair| (LitKind::Str(pair.key.name), pair.value.kind.into()))
                    .collect(),
                others: others.map(|t| (t.key.kind.into(), t.value.kind.into())),
                metatable: None,
            }
            .into(),
            TyKind::Function {
                params,
                variadic,
                returns,
                throws,
            } => FunctionType {
                params: params
                    .into_iter()
                    .map(|x| ParmaType::from(Type::from(x.kind)))
                    .collect(),
                variadic: ParmaType::from(variadic.map(|x| x.kind.into()).unwrap_or(Type::NULL)),
                returns: returns.map(|x| x.kind.into()).unwrap_or(Type::NULL),
                throws: throws.map(|x| x.kind.into()).unwrap_or(Type::NULL),
            }
            .into(),
            TyKind::Union(t) => Type::Union(SortedSet::from_unsorted(
                t.into_iter().map(|x| x.kind.into()).collect(),
            )),
        }
    }
}

impl<S: Eq + Ord> From<Vec<Type<S>>> for Type<S> {
    fn from(value: Vec<Type<S>>) -> Self {
        Type::Union(SortedSet::from_unsorted(value))
    }
}

impl<S: Eq + Ord> From<LitKind<S>> for Type<S> {
    fn from(value: LitKind<S>) -> Self {
        Type::Literal(value)
    }
}

impl<S: Eq + Ord> From<TableType<S>> for Type<S> {
    fn from(value: TableType<S>) -> Self {
        Type::Table(Box::new(value))
    }
}

impl<S: Eq + Ord> From<FunctionType<S>> for Type<S> {
    fn from(value: FunctionType<S>) -> Self {
        Type::Function(Box::new(value))
    }
}

/// The Table Type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TableType<S: Eq + Ord> {
    pub pairs: Vec<(LitKind<S>, Type<S>)>,
    pub others: Option<(Type<S>, Type<S>)>,
    pub metatable: Option<Box<TableType<S>>>,
}

impl<S: Eq + Ord> TableType<S> {
    pub const ANY: Self = TableType {
        pairs: Vec::new(),
        others: Some((Type::Any, Type::Any)),
        metatable: None,
    };

    pub fn list(item: Type<S>) -> Self {
        Self {
            pairs: Vec::new(),
            others: Some((Type::Int, item)),
            metatable: None,
        }
    }
}

impl<S: AsRef<str> + Eq + Ord + Clone> TableType<S> {
    pub fn get_str<K: AsRef<str>>(&self, key: K) -> Type<S> {
        self.pairs
            .iter()
            .find_map(|(k, v)| {
                if let LitKind::Str(k) = k
                    && k.as_ref() == key.as_ref()
                {
                    return Some(v);
                }
                None
            })
            .cloned()
            .unwrap_or(Type::NULL)
    }

    pub fn get_literal_option(&self, key: &LitKind<S>) -> Option<Type<S>> {
        self.pairs
            .iter()
            .find_map(|(k, v)| if k == key { Some(v) } else { None })
            .cloned()
    }

    pub fn get_literal(&self, key: &LitKind<S>) -> Type<S> {
        self.get_literal_option(key).unwrap_or(Type::NULL)
    }

    pub fn get(&self, key: Type<S>) -> Type<S> {
        if let Type::Literal(key) = &key
            && let Some(value) = self.get_literal_option(key)
        {
            return value;
        }
        if let Some((k, v)) = &self.others
            && key.is_subtype_of(k)
        {
            return v.clone();
        }
        Type::NULL
    }

    pub fn set(&self, key: Type<S>, value: Type<S>) -> Result<(), TypeError<S>> {
        if let Type::Literal(key) = &key
            && let Some(v) = self.get_literal_option(key)
        {
            value.expect_is_subtype_of(&v)?;
        }
        if let Some((k, v)) = &self.others {
            key.expect_is_subtype_of(k)?;
            value.expect_is_subtype_of(v)?;
        }
        Ok(())
    }
}

impl<S: AsRef<str> + Eq + Ord + fmt::Display> fmt::Display for TableType<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut items = Vec::new();
        for (k, v) in &self.pairs {
            items.push(format!("{k}: {v}"));
        }
        if let Some((k, v)) = &self.others {
            items.push(format!("[{k}]: {v}"));
        }
        if let Some(t) = &self.metatable {
            items.push(format!("[#]: {t}"));
        }
        write!(f, "{{{}}}", items.iter().join(", "))
    }
}

/// The Function Type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionType<S: Eq + Ord> {
    pub params: Vec<ParmaType<S>>,
    pub variadic: ParmaType<S>,
    pub returns: Type<S>,
    pub throws: Type<S>,
}

impl<S: Clone + Eq + Ord> FunctionType<S> {
    pub const ANY: Self = Self {
        params: Vec::new(),
        variadic: ParmaType {
            name: None,
            ty: Type::Any,
        },
        returns: Type::Any,
        throws: Type::Any,
    };

    pub fn check_args(&self, args: &[Type<S>]) -> Result<(), TypeError<S>> {
        for (i, arg) in args.iter().enumerate() {
            arg.expect_is_subtype_of(&self.params.get(i).unwrap_or(&self.variadic).ty)?;
        }
        Ok(())
    }
}

impl<S: AsRef<str> + Eq + Ord + fmt::Display> fmt::Display for FunctionType<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "fn({}, ...{}) -> {} throw {}",
            self.params.iter().join(", "),
            self.variadic,
            self.returns,
            self.throws,
        )
    }
}

/// The type of function param.
#[derive(Debug, Clone)]
pub struct ParmaType<S: Eq + Ord> {
    pub name: Option<S>,
    pub ty: Type<S>,
}

impl<S: Eq + Ord> PartialEq for ParmaType<S> {
    fn eq(&self, other: &Self) -> bool {
        self.ty == other.ty
    }
}

impl<S: Eq + Ord> Eq for ParmaType<S> {}

impl<S: Eq + Ord> PartialOrd for ParmaType<S> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<S: Eq + Ord> Ord for ParmaType<S> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.ty.cmp(&other.ty)
    }
}

impl<S: AsRef<str> + Eq + Ord + fmt::Display> fmt::Display for ParmaType<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(name) = &self.name {
            write!(f, "{}: {}", name.as_ref(), self.ty)
        } else {
            write!(f, "{}", self.ty)
        }
    }
}

impl<S: Eq + Ord> From<Type<S>> for ParmaType<S> {
    fn from(value: Type<S>) -> Self {
        ParmaType {
            name: None,
            ty: value,
        }
    }
}
