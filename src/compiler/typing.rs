//! The Lucia Type Checker.

use std::{fmt, ops};

use index_vec::{IndexVec, index_vec};
use indexmap::IndexMap;
use rustc_hash::FxBuildHasher;
use sorted_vec::SortedSet;
use thiserror::Error;

use crate::utils::Join;

use super::{
    ast::*,
    index::{FunctionId, SymbolId},
    semantic::Semantic,
    value::{MetaMethod, MetaName, ValueType},
};

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
    Table(Box<TableType<S>>),
    Function(Box<FunctionType<S>>),
    UserData(S),

    // Union Types
    Union(SortedSet<Type<S>>),
}

impl<S: Clone + Eq + Ord> Type<S> {
    const NULL: Self = Type::Literal(LitKind::Null);

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

impl<S: AsRef<str> + Eq + Ord> fmt::Display for Type<S> {
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
            TyKind::Option(t) => Type::from(t.kind) | Type::NULL,
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
    pairs: Vec<(LitKind<S>, Type<S>)>,
    others: Option<(Type<S>, Type<S>)>,
    metatable: Option<Box<TableType<S>>>,
}

impl<S: Eq + Ord> TableType<S> {
    pub fn list(item: Type<S>) -> Self {
        Self {
            pairs: Vec::new(),
            others: Some((Type::Int, item)),
            metatable: None,
        }
    }

    pub fn any_table() -> Self {
        Self {
            pairs: Vec::new(),
            others: Some((Type::Any, Type::Any)),
            metatable: None,
        }
    }
}

impl<S: AsRef<str> + Eq + Ord + Clone> TableType<S> {
    pub fn get_str<K: AsRef<str>>(&self, key: K) -> Type<S> {
        self.pairs
            .iter()
            .find_map(|(k, v)| {
                if let LitKind::Str(k) = k {
                    if k.as_ref() == key.as_ref() {
                        return Some(v);
                    }
                }
                None
            })
            .cloned()
            .unwrap_or(Type::NULL)
    }

    fn get_literal_option(&self, key: &LitKind<S>) -> Option<Type<S>> {
        self.pairs
            .iter()
            .find_map(|(k, v)| if k == key { Some(v) } else { None })
            .cloned()
    }

    fn get_literal(&self, key: &LitKind<S>) -> Type<S> {
        self.get_literal_option(key).unwrap_or(Type::NULL)
    }

    pub fn get(&self, key: Type<S>) -> Type<S> {
        if let Type::Literal(key) = &key {
            if let Some(value) = self.get_literal_option(key) {
                return value;
            }
        }
        if let Some((k, v)) = &self.others {
            if key.is_subtype_of(k) {
                return v.clone();
            }
        }
        Type::NULL
    }

    pub fn set(&self, key: Type<S>, value: Type<S>) -> Result<(), TypeError<S>> {
        if let Type::Literal(key) = &key {
            if let Some(v) = self.get_literal_option(key) {
                value.expect_is_subtype_of(&v)?;
            }
        }
        if let Some((k, v)) = &self.others {
            key.expect_is_subtype_of(k)?;
            value.expect_is_subtype_of(v)?;
        }
        Ok(())
    }
}

impl<S: AsRef<str> + Eq + Ord> fmt::Display for TableType<S> {
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
    params: Vec<ParmaType<S>>,
    variadic: ParmaType<S>,
    returns: Type<S>,
    throws: Type<S>,
}

impl<S: Clone + Eq + Ord> FunctionType<S> {
    pub fn any_function() -> Self {
        Self {
            params: Vec::new(),
            variadic: Type::Any.into(),
            returns: Type::Any,
            throws: Type::Any,
        }
    }

    pub fn check_args(&self, args: &[Type<S>]) -> Result<(), TypeError<S>> {
        for (i, arg) in args.iter().enumerate() {
            arg.expect_is_subtype_of(&self.params.get(i).unwrap_or(&self.variadic).ty)?;
        }
        Ok(())
    }
}

impl<S: AsRef<str> + Eq + Ord> fmt::Display for FunctionType<S> {
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
    name: Option<S>,
    ty: Type<S>,
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

impl<S: AsRef<str> + Eq + Ord> fmt::Display for ParmaType<S> {
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

/// Kind of TypeCheckError.
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum TypeError<S: Eq + Ord> {
    #[error("{ty} is not subtype of {expected} ")]
    ExpectIsSubtypeOf {
        ty: Box<Type<S>>,
        expected: Box<Type<S>>,
    },
    #[error("unsupported operand type for {operator}: {operand}")]
    MetaUnOperator {
        operator: MetaName,
        operand: Box<Type<S>>,
    },
    #[error("unsupported operand types for {operator}: {} and {}", .operand.0, .operand.1)]
    MetaBinOperator {
        operator: MetaName,
        operand: (Box<Type<S>>, Box<Type<S>>),
    },
}

macro_rules! meta_operator_error {
    ($operator:expr, $arg1:expr) => {
        TypeError::MetaUnOperator {
            operator: $operator,
            operand: Box::new($arg1),
        }
    };
    ($operator:expr, $arg1:expr, $arg2:expr) => {
        TypeError::MetaBinOperator {
            operator: $operator,
            operand: (Box::new($arg1), Box::new($arg2)),
        }
    };
}

/// Check type.
pub fn check_type<S: AsRef<str> + Clone + Eq + Ord>(
    program: &Program<S>,
    semantic: &Semantic<S>,
) -> Vec<TypeError<S>> {
    TypeChecker::new(semantic).check_type(program)
}

struct TypeChecker<'a, S: Clone + Eq + Ord> {
    semantic: &'a Semantic<S>,

    current_function_id: FunctionId,
    function_type: IndexVec<FunctionId, Option<FunctionType<S>>>,
    symbol_type: IndexMap<SymbolId, Type<S>, FxBuildHasher>,
    errors: Vec<TypeError<S>>,
}

struct MetaMethodType;

macro_rules! call_metamethod {
    ($ctx:ident, $meta_name:expr, $v1:ident $(,)? $($arg:ident),*) => {
        if let Some(metatable) = $v1.metatable() {
            let metamethod = metatable.get_str($meta_name);
            if !metamethod.is_null() {
                let function = $ctx.call(metamethod)?;
                function.check_args(&[$v1, $($arg),*])?;
                return Ok(function.returns.clone());
            }
        }
    };
}

impl MetaMethodType {
    pub fn arithmetic<S: AsRef<str> + Clone + Eq + Ord>(
        self,
        meta_name: MetaName,
        lhs: Type<S>,
        rhs: Type<S>,
    ) -> Result<Type<S>, TypeError<S>> {
        call_metamethod!(self, meta_name, lhs, rhs);
        if lhs == Type::Any || rhs == Type::Any {
            Ok(Type::Any)
        } else if lhs.is_subtype_of(&Type::Int) && rhs.is_subtype_of(&Type::Int) {
            Ok(Type::Int)
        } else if lhs.is_subtype_of(&Type::Float) && rhs.is_subtype_of(&Type::Float) {
            Ok(Type::Float)
        } else {
            Err(meta_operator_error!(meta_name, lhs, rhs))
        }
    }
}

impl<S: AsRef<str> + Clone + Eq + Ord> MetaMethod<Type<S>> for MetaMethodType {
    type ResultCall = Result<Box<FunctionType<S>>, TypeError<S>>;
    type ResultIter = Result<Type<S>, TypeError<S>>;
    type Result1 = Result<Type<S>, TypeError<S>>;
    type Result2 = Self::Result1;
    type Result3 = Self::Result1;

    fn call(self, value: Type<S>) -> Self::ResultCall {
        if let Type::Function(function) = value {
            return Ok(function);
        }
        let metatable = value
            .metatable()
            .ok_or_else(|| meta_operator_error!(MetaName::Call, value))?;
        match metatable.get_str(MetaName::Call) {
            Type::Function(f) => Ok(f),
            v @ Type::Table(_) => self.call(v),
            v => Err(meta_operator_error!(MetaName::Call, v)),
        }
    }

    fn iter(self, value: Type<S>) -> Self::ResultIter {
        call_metamethod!(self, MetaName::Iter, value);
        match value {
            Type::Table(table) => {
                let mut key: Type<S> = table
                    .pairs
                    .iter()
                    .map(|(k, _)| Type::Literal(k.clone()))
                    .collect::<Vec<_>>()
                    .into();
                let mut value: Type<S> = table
                    .pairs
                    .iter()
                    .map(|(_, v)| v.clone())
                    .collect::<Vec<_>>()
                    .into();
                if let Some((k, v)) = &table.others {
                    key = key.union(k);
                    value = value.union(v);
                }
                Ok(TableType {
                    pairs: vec![(LitKind::Int(0), key), (LitKind::Int(1), value)],
                    others: None,
                    metatable: None,
                }
                .into())
            }
            Type::Function(function) => {
                function.check_args(&[])?;
                Ok(function.returns.clone())
            }
            _ => Err(meta_operator_error!(MetaName::Iter, value)),
        }
    }

    fn get_attr(self, table: Type<S>, key: Type<S>) -> Self::Result2 {
        call_metamethod!(self, MetaName::GetAttr, table, key);
        match table {
            Type::Table(v) => Ok(v.get(key)),
            _ => Err(meta_operator_error!(MetaName::GetAttr, table, key)),
        }
    }

    fn get_item(self, table: Type<S>, key: Type<S>) -> Self::Result2 {
        call_metamethod!(self, MetaName::GetItem, table, key);
        match table {
            Type::Table(v) => Ok(v.get(key)),
            _ => Err(meta_operator_error!(MetaName::GetItem, table, key)),
        }
    }

    fn set_attr(self, table: Type<S>, key: Type<S>, value: Type<S>) -> Self::Result3 {
        call_metamethod!(self, MetaName::GetAttr, table, key, value);
        match table {
            Type::Table(v) => {
                v.set(key, value)?;
                Ok(Type::NULL)
            }
            _ => Err(meta_operator_error!(MetaName::GetAttr, table, key)),
        }
    }

    fn set_item(self, table: Type<S>, key: Type<S>, value: Type<S>) -> Self::Result3 {
        call_metamethod!(self, MetaName::SetItem, table, key, value);
        match table {
            Type::Table(v) => {
                v.set(key, value)?;
                Ok(Type::NULL)
            }
            _ => Err(meta_operator_error!(MetaName::SetItem, table, key)),
        }
    }

    fn neg(self, value: Type<S>) -> Self::Result1 {
        call_metamethod!(self, MetaName::Neg, value);
        value
            .expect_is_subtype_of(&(Type::Int | Type::Float))
            .map(|_| value)
    }

    fn add(self, lhs: Type<S>, rhs: Type<S>) -> Self::Result2 {
        call_metamethod!(self, MetaName::Add, lhs, rhs);
        if lhs == Type::Any || rhs == Type::Any {
            Ok(Type::Any)
        } else if lhs.is_subtype_of(&Type::Int) && rhs.is_subtype_of(&Type::Int) {
            Ok(Type::Int)
        } else if lhs.is_subtype_of(&Type::Float) && rhs.is_subtype_of(&Type::Float) {
            Ok(Type::Float)
        } else if lhs.is_subtype_of(&Type::Str) && rhs.is_subtype_of(&Type::Str) {
            Ok(Type::Str)
        } else {
            Err(meta_operator_error!(MetaName::Add, lhs, rhs))
        }
    }

    fn sub(self, lhs: Type<S>, rhs: Type<S>) -> Self::Result2 {
        self.arithmetic(MetaName::Sub, lhs, rhs)
    }

    fn mul(self, lhs: Type<S>, rhs: Type<S>) -> Self::Result2 {
        self.arithmetic(MetaName::Mul, lhs, rhs)
    }

    fn div(self, lhs: Type<S>, rhs: Type<S>) -> Self::Result2 {
        self.arithmetic(MetaName::Div, lhs, rhs)
    }

    fn rem(self, lhs: Type<S>, rhs: Type<S>) -> Self::Result2 {
        self.arithmetic(MetaName::Rem, lhs, rhs)
    }

    fn eq(self, lhs: Type<S>, rhs: Type<S>) -> Self::Result2 {
        call_metamethod!(self, MetaName::Eq, lhs, rhs);
        Ok(Type::Bool)
    }

    fn ne(self, lhs: Type<S>, rhs: Type<S>) -> Self::Result2 {
        call_metamethod!(self, MetaName::Ne, lhs, rhs);
        Ok(Type::Bool)
    }

    fn gt(self, lhs: Type<S>, rhs: Type<S>) -> Self::Result2 {
        self.arithmetic(MetaName::Gt, lhs, rhs)
    }

    fn ge(self, lhs: Type<S>, rhs: Type<S>) -> Self::Result2 {
        self.arithmetic(MetaName::Ge, lhs, rhs)
    }

    fn lt(self, lhs: Type<S>, rhs: Type<S>) -> Self::Result2 {
        self.arithmetic(MetaName::Lt, lhs, rhs)
    }

    fn le(self, lhs: Type<S>, rhs: Type<S>) -> Self::Result2 {
        self.arithmetic(MetaName::Le, lhs, rhs)
    }

    fn len(self, value: Type<S>) -> Self::Result1 {
        call_metamethod!(self, MetaName::Len, value);
        if !(value.is_subtype_of(&Type::Str) || matches!(value, Type::Table(_))) {
            return Err(meta_operator_error!(MetaName::Len, value));
        }
        Ok(Type::Int)
    }

    fn bool(self, value: Type<S>) -> Self::Result1 {
        call_metamethod!(self, MetaName::Bool, value);
        value.expect_is_subtype_of(&Type::Any)?;
        Ok(Type::Bool)
    }

    fn int(self, value: Type<S>) -> Self::Result1 {
        call_metamethod!(self, MetaName::Int, value);
        value.expect_is_subtype_of(
            &(Type::NULL | Type::Bool | Type::Int | Type::Float | Type::Str),
        )?;
        Ok(Type::Int)
    }

    fn float(self, value: Type<S>) -> Self::Result1 {
        call_metamethod!(self, MetaName::Float, value);
        value.expect_is_subtype_of(
            &(Type::NULL | Type::Bool | Type::Int | Type::Float | Type::Str),
        )?;
        Ok(Type::Float)
    }

    fn str(self, value: Type<S>) -> Self::Result1 {
        call_metamethod!(self, MetaName::Str, value);
        value.expect_is_subtype_of(&Type::Any)?;
        Ok(Type::Str)
    }

    fn repr(self, value: Type<S>) -> Self::Result1 {
        call_metamethod!(self, MetaName::Repr, value);
        value.expect_is_subtype_of(&Type::Any)?;
        Ok(Type::Str)
    }
}

impl BinOp {
    #[allow(clippy::type_complexity)]
    fn check_type<S: AsRef<str> + Clone + Eq + Ord>(
        self,
    ) -> fn(Type<S>, Type<S>) -> Result<Type<S>, TypeError<S>> {
        match self {
            BinOp::Add => |lhs, rhs| MetaMethodType.add(lhs, rhs),
            BinOp::Sub => |lhs, rhs| MetaMethodType.sub(lhs, rhs),
            BinOp::Mul => |lhs, rhs| MetaMethodType.mul(lhs, rhs),
            BinOp::Div => |lhs, rhs| MetaMethodType.div(lhs, rhs),
            BinOp::Rem => |lhs, rhs| MetaMethodType.rem(lhs, rhs),
            BinOp::And => |_, _| Ok(Type::Bool),
            BinOp::Or => |_, _| Ok(Type::Bool),
            BinOp::Eq => |lhs, rhs| MetaMethodType.eq(lhs, rhs),
            BinOp::Ne => |lhs, rhs| MetaMethodType.ne(lhs, rhs),
            BinOp::Lt => |lhs, rhs| MetaMethodType.lt(lhs, rhs),
            BinOp::Le => |lhs, rhs| MetaMethodType.le(lhs, rhs),
            BinOp::Gt => |lhs, rhs| MetaMethodType.gt(lhs, rhs),
            BinOp::Ge => |lhs, rhs| MetaMethodType.ge(lhs, rhs),
            BinOp::Identical => |_, _| Ok(Type::Bool),
            BinOp::NotIdentical => |_, _| Ok(Type::Bool),
            BinOp::Is => |_, _| Ok(Type::Bool),
        }
    }
}

impl<'a, S: AsRef<str> + Clone + Eq + Ord> TypeChecker<'a, S> {
    fn new(semantic: &'a Semantic<S>) -> Self {
        Self {
            semantic,
            current_function_id: FunctionId::new(0),
            function_type: index_vec![None; semantic.functions.len()],
            symbol_type: IndexMap::with_hasher(FxBuildHasher),
            errors: Vec::new(),
        }
    }

    fn get_symbol_type(&self, ident: &Ident<S>) -> Option<&Type<S>> {
        let symbol_id =
            self.semantic.references[ident.reference_id.get().copied().unwrap()].symbol_id;
        self.symbol_type.get(&symbol_id)
    }

    fn set_symbol_type(&mut self, ident: &Ident<S>, ty: Type<S>) -> Result<(), TypeError<S>> {
        if let Some(old_ty) = self.get_symbol_type(ident) {
            if ty.is_subtype_of(old_ty) {
                Ok(())
            } else {
                if let (Type::Table(ty), Type::Table(old_ty)) = (&ty, &old_ty) {
                    if ty.pairs.iter().all(|(k1, v1)| {
                        old_ty
                            .pairs
                            .iter()
                            .find(|(k2, _)| k1 == k2)
                            .map(|(_, v2)| v2)
                            .or_else(|| old_ty.others.as_ref().map(|(_, v)| v))
                            .is_some_and(|v2| v1.is_subtype_of(v2))
                    }) && match (&ty.others, &old_ty.others) {
                        (None, _) => true,
                        (Some((key1, value1)), Some((key2, value2))) => {
                            key1.is_subtype_of(key2) && value1.is_subtype_of(value2)
                        }
                        _ => false,
                    } {
                        return Ok(());
                    }
                }
                Err(TypeError::ExpectIsSubtypeOf {
                    ty: Box::new(ty.clone()),
                    expected: Box::new(old_ty.clone()),
                })
            }
        } else {
            let symbol_id =
                self.semantic.references[ident.reference_id.get().copied().unwrap()].symbol_id;
            self.symbol_type.insert(symbol_id, ty);
            Ok(())
        }
    }

    fn check_type(mut self, program: &Program<S>) -> Vec<TypeError<S>> {
        if let Err(error) = self.check_function(&program.function) {
            self.errors.push(error);
        }
        self.errors
    }

    fn check_function(&mut self, function: &Function<S>) -> Result<Type<S>, TypeError<S>> {
        self.current_function_id = function.function_id.get().copied().unwrap();

        let param_types = function
            .params
            .iter()
            .map(|param| ParmaType {
                name: Some(param.ident.name.clone()),
                ty: param
                    .ty
                    .as_ref()
                    .map(|t| t.kind.clone().into())
                    .unwrap_or(Type::Any),
            })
            .collect::<Vec<_>>();
        let variadic_type = function
            .variadic
            .as_ref()
            .map(|t| ParmaType {
                name: Some(t.ident.name.clone()),
                ty: t
                    .ty
                    .as_ref()
                    .map(|t| t.kind.clone().into())
                    .unwrap_or(Type::Any),
            })
            .unwrap_or(Type::NULL.into());
        let returns_type = function
            .returns
            .as_ref()
            .map(|t| t.kind.clone().into())
            .unwrap_or(Type::Any);
        let throws_type = function
            .throws
            .as_ref()
            .map(|t| t.kind.clone().into())
            .unwrap_or(Type::Any);

        for (param, t) in function.params.iter().zip(param_types.iter()) {
            self.set_symbol_type(&param.ident, t.clone().ty)?;
        }
        if let Some(variadic) = &function.variadic {
            self.set_symbol_type(
                &variadic.ident,
                TableType::list(variadic_type.ty.clone()).into(),
            )?;
        }

        let function_type = FunctionType {
            params: param_types,
            variadic: variadic_type,
            returns: returns_type,
            throws: throws_type,
        };
        self.function_type[self.current_function_id] = Some(function_type.clone());

        self.check_block(&function.body);

        Ok(function_type.into())
    }

    fn check_block(&mut self, block: &Block<S>) {
        for stmt in &block.body {
            if let Err(e) = self.check_stmt(stmt) {
                self.errors.push(e)
            }
        }
    }

    fn check_stmt(&mut self, stmt: &Stmt<S>) -> Result<(), TypeError<S>> {
        match &stmt.kind {
            StmtKind::If {
                test,
                consequent,
                alternate,
            } => {
                self.check_expr(test)?.expect_is_subtype_of(&Type::Bool)?;
                self.check_block(consequent);
                if let Some(alternate) = alternate {
                    self.check_stmt(alternate)?;
                }
            }
            StmtKind::Match { expr, cases } => {
                self.check_expr(expr)?;
                for case in cases {
                    // TODO: check pattern
                    self.check_block(&case.body);
                }
            }
            StmtKind::Loop { body } => {
                self.check_block(body);
            }
            StmtKind::While { test, body } => {
                self.check_expr(test)?.expect_is_subtype_of(&Type::Bool)?;
                self.check_block(body);
            }
            StmtKind::For { left, right, body } => {
                let right_type = MetaMethodType.iter(self.check_expr(right)?)?;
                if left.len() == 1 {
                    self.set_symbol_type(&left[0], right_type)?;
                } else if let Type::Table(table) = right_type {
                    for (i, l) in left.iter().enumerate() {
                        self.set_symbol_type(l, table.get_literal(&LitKind::Int(i as i64)))?;
                    }
                } else {
                    return Err(TypeError::ExpectIsSubtypeOf {
                        ty: Box::new(right_type),
                        expected: Box::new(TableType::any_table().into()),
                    });
                }
                self.check_block(body);
            }
            StmtKind::Break => (),
            StmtKind::Continue => (),
            StmtKind::Return { argument } => self.check_expr(argument)?.expect_is_subtype_of(
                &self.function_type[self.current_function_id]
                    .as_ref()
                    .unwrap()
                    .returns,
            )?,
            StmtKind::Throw { argument } => self.check_expr(argument)?.expect_is_subtype_of(
                &self.function_type[self.current_function_id]
                    .as_ref()
                    .unwrap()
                    .throws,
            )?,
            StmtKind::Import { .. } => {
                // TODO: support import module
            }
            StmtKind::Fn {
                glo: _,
                name,
                function,
            } => {
                let ty = self.check_function(function)?;
                self.set_symbol_type(name, ty)?;
            }
            StmtKind::GloAssign { left, right } => {
                let right_type = self.check_expr(right)?;
                if let Some(t) = &left.ty {
                    self.set_symbol_type(&left.ident, t.kind.clone().into())?;
                }
                self.set_symbol_type(&left.ident, right_type)?;
            }
            StmtKind::Assign { left, right } => {
                let right_type = self.check_expr(right)?;
                self.check_assign(left, right_type)?;
            }
            StmtKind::AssignOp {
                operator,
                left,
                right,
            } => {
                let right_type = self.check_expr(right)?;
                let left_type = match &left.kind {
                    AssignLeftKind::Ident(ident) => self
                        .get_symbol_type(&ident.ident)
                        .cloned()
                        .unwrap_or(Type::Unknown),
                    AssignLeftKind::Member { table, property } => {
                        let meta_method = match property {
                            MemberKind::Bracket(_) => MetaMethodType::get_item,
                            MemberKind::Dot(_) | MemberKind::DoubleColon(_) => {
                                MetaMethodType::get_attr
                            }
                        };
                        meta_method(
                            MetaMethodType,
                            self.check_expr(table)?,
                            self.check_member_property(property)?,
                        )?
                    }
                    AssignLeftKind::MetaMember { table, property: _ } => {
                        let table_type = self.check_expr(table)?;
                        match table_type {
                            Type::Table(table) => table
                                .metatable
                                .clone()
                                .map(Type::Table)
                                .unwrap_or(Type::NULL),
                            ty => {
                                return Err(TypeError::ExpectIsSubtypeOf {
                                    ty: Box::new(ty),
                                    expected: Box::new(TableType::any_table().into()),
                                });
                            }
                        }
                    }
                };
                self.check_assign(left, operator.check_type()(left_type, right_type)?)?;
            }
            StmtKind::AssignUnpack { left, right } => {
                let right_type = self.check_expr(right)?;
                if let Type::Table(table) = right_type {
                    for (i, l) in left.iter().enumerate() {
                        self.check_assign(l, table.get_literal(&LitKind::Int(i as i64)))?;
                    }
                } else {
                    return Err(TypeError::ExpectIsSubtypeOf {
                        ty: Box::new(right_type),
                        expected: Box::new(TableType::any_table().into()),
                    });
                }
            }
            StmtKind::AssignMulti { left, right } => {
                assert!(left.len() != right.len());
                for (left, right) in left.iter().zip(right) {
                    let right_type = self.check_expr(right)?;
                    self.check_assign(left, right_type)?;
                }
            }
            StmtKind::Block(block) => {
                self.check_block(block);
            }
            StmtKind::Expr(expr) => {
                self.check_expr(expr)?;
            }
        }
        Ok(())
    }

    fn check_assign(
        &mut self,
        left: &AssignLeft<S>,
        right_type: Type<S>,
    ) -> Result<(), TypeError<S>> {
        match &left.kind {
            AssignLeftKind::Ident(ident) => {
                if let Some(t) = &ident.ty {
                    self.set_symbol_type(&ident.ident, t.kind.clone().into())?;
                }
                self.set_symbol_type(&ident.ident, right_type)?;
            }
            AssignLeftKind::Member { table, property } => {
                let meta_method = match property {
                    MemberKind::Bracket(_) => MetaMethodType::get_item,
                    MemberKind::Dot(_) | MemberKind::DoubleColon(_) => MetaMethodType::get_attr,
                };
                right_type.expect_is_subtype_of(&meta_method(
                    MetaMethodType,
                    self.check_expr(table)?,
                    self.check_member_property(property)?,
                )?)?;
            }
            AssignLeftKind::MetaMember { table, property: _ } => match self.check_expr(table)? {
                Type::Table(table) => {
                    if let Some(metatable) = &table.metatable {
                        right_type.expect_is_subtype_of(&Type::Table(metatable.clone()))?;
                    } else {
                        right_type.expect_is_subtype_of(&Type::NULL)?;
                    }
                }
                ty => {
                    return Err(TypeError::ExpectIsSubtypeOf {
                        ty: Box::new(ty),
                        expected: Box::new(TableType::any_table().into()),
                    });
                }
            },
        }
        Ok(())
    }

    fn check_expr(&mut self, expr: &Expr<S>) -> Result<Type<S>, TypeError<S>> {
        Ok(match &expr.kind {
            ExprKind::Lit(lit) => lit.kind.clone().into(),
            ExprKind::Ident(ident) => self
                .get_symbol_type(ident)
                .cloned()
                .unwrap_or(Type::Unknown),
            ExprKind::Paren(expr) => self.check_expr(expr)?,
            ExprKind::Function(function) => self.check_function(function)?,
            ExprKind::Table { properties } => {
                let mut pairs = Vec::new();
                for property in properties {
                    let key_type = self.check_expr(&property.key)?;
                    let value_type = self.check_expr(&property.value)?;
                    if let Type::Literal(lit) = key_type {
                        pairs.push((lit, value_type));
                    }
                }
                TableType {
                    pairs,
                    others: None,
                    metatable: None,
                }
                .into()
            }
            ExprKind::List { items } => {
                let mut pairs = Vec::new();
                for (i, item) in items.iter().enumerate() {
                    pairs.push((LitKind::Int(i as i64), self.check_expr(item)?));
                }
                TableType {
                    pairs,
                    others: None,
                    metatable: None,
                }
                .into()
            }
            ExprKind::Unary { operator, argument } => match operator {
                UnOp::Not => {
                    self.check_expr(argument)?
                        .expect_is_subtype_of(&Type::Bool)?;
                    Type::Bool
                }
                UnOp::Neg => {
                    let ty = self.check_expr(argument)?;
                    if ty == Type::Any {
                        Type::Any
                    } else if ty.is_subtype_of(&Type::Int) {
                        Type::Int
                    } else if ty.is_subtype_of(&Type::Float) {
                        Type::Float
                    } else {
                        return Err(TypeError::ExpectIsSubtypeOf {
                            ty: Box::new(ty),
                            expected: Box::new(Type::Int | Type::Float),
                        });
                    }
                }
            },
            ExprKind::Binary {
                operator,
                left,
                right,
            } => {
                let left_type = self.check_expr(left)?;
                let right_type = self.check_expr(right)?;
                operator.check_type()(left_type, right_type)?
            }
            ExprKind::TypeCheck { left, right } => {
                let left_type = self.check_expr(left)?;
                match (left_type, right) {
                    (Type::Literal(LitKind::Null), ValueType::Null)
                    | (Type::Bool, ValueType::Bool)
                    | (Type::Int, ValueType::Int)
                    | (Type::Float, ValueType::Float)
                    | (Type::Str, ValueType::Str)
                    | (Type::Table { .. }, ValueType::Table)
                    | (Type::Function { .. }, ValueType::Function)
                    | (Type::UserData(_), ValueType::UserData) => {
                        Type::Literal(LitKind::Bool(true))
                    }
                    _ => Type::Literal(LitKind::Bool(false)),
                }
            }
            ExprKind::Member {
                table,
                property,
                safe,
            } => {
                let meta_method = match property {
                    MemberKind::Bracket(_) => MetaMethodType::get_item,
                    MemberKind::Dot(_) | MemberKind::DoubleColon(_) => MetaMethodType::get_attr,
                };
                let ty = meta_method(
                    MetaMethodType,
                    self.check_expr(table)?,
                    self.check_member_property(property)?,
                )?;
                if safe.is_some() { ty | Type::NULL } else { ty }
            }
            ExprKind::MetaMember {
                table,
                property: _,
                safe,
            } => {
                let table_type = self.check_expr(table)?;
                match table_type {
                    Type::Table(table) => {
                        let ty = table
                            .metatable
                            .clone()
                            .map(Type::Table)
                            .unwrap_or(Type::NULL);
                        if safe.is_some() { ty | Type::NULL } else { ty }
                    }
                    ty => {
                        return Err(TypeError::ExpectIsSubtypeOf {
                            ty: Box::new(ty),
                            expected: Box::new(TableType::any_table().into()),
                        });
                    }
                }
            }
            ExprKind::Call {
                callee,
                arguments,
                kind,
            } => {
                let function_type = MetaMethodType.call(self.check_expr(callee)?)?;
                let argument_types = arguments
                    .iter()
                    .map(|x| self.check_expr(x))
                    .collect::<Result<Vec<_>, _>>()?;

                for (i, param) in function_type.params.iter().enumerate() {
                    argument_types
                        .get(i)
                        .unwrap_or(&Type::Never)
                        .expect_is_subtype_of(&param.ty)?;
                }
                for argument_type in argument_types.iter().skip(function_type.params.len()) {
                    argument_type.expect_is_subtype_of(&function_type.variadic.ty)?;
                }
                match kind {
                    CallKind::None | CallKind::TryPanic => function_type.returns.clone(),
                    CallKind::Try => TableType {
                        pairs: vec![
                            (LitKind::Int(0), function_type.returns.clone() | Type::NULL),
                            (LitKind::Int(1), function_type.throws.clone() | Type::NULL),
                        ],
                        others: None,
                        metatable: None,
                    }
                    .into(),
                    CallKind::TryOption => function_type.returns.clone() | Type::NULL,
                }
            }
        })
    }

    fn check_member_property(&mut self, property: &MemberKind<S>) -> Result<Type<S>, TypeError<S>> {
        match property {
            MemberKind::Bracket(expr) => self.check_expr(expr),
            MemberKind::Dot(ident) | MemberKind::DoubleColon(ident) => {
                Ok(Type::Literal(LitKind::Str(ident.name.clone())))
            }
        }
    }
}
