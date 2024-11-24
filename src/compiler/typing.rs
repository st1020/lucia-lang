//! The Lucia Type System and Type Checker.

use std::{fmt, ops};

use index_vec::{index_vec, IndexVec};
use indexmap::IndexMap;
use rustc_hash::FxBuildHasher;
use thiserror::Error;

use crate::utils::{Float, Join};

use super::{
    ast::*,
    index::{FunctionId, SymbolId},
    semantic::Semantic,
    value::ValueType,
};

/// The Literal Type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LiteralType<S> {
    Bool(bool),
    Int(i64),
    Float(Float),
    Str(S),
}

impl<S: AsRef<str>> fmt::Display for LiteralType<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LiteralType::Bool(v) => write!(f, "{}", v),
            LiteralType::Int(v) => write!(f, "{}", v),
            LiteralType::Float(v) => write!(f, "{}", v),
            LiteralType::Str(v) => write!(f, "\"{}\"", v.as_ref()),
        }
    }
}

/// All types used in type checker.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(clippy::type_complexity)]
pub enum Type<S> {
    // Special Atom Types
    Unknown,
    Never,
    Any,

    // Literal Type
    Literal(LiteralType<S>),

    // Atom Types
    Null,
    Bool,
    Int,
    Float,
    Str,
    // Table Types
    Table {
        pairs: Vec<(LiteralType<S>, Type<S>)>,
        others: Option<(Box<Type<S>>, Box<Type<S>>)>,
    },
    // Function Types
    Function {
        params: Vec<Type<S>>,
        variadic: Box<Type<S>>,
        returns: Box<Type<S>>,
        throws: Box<Type<S>>,
    },
    // UserData Types
    UserData(S),

    // Union Types
    Union(Vec<Type<S>>),
}

impl<S: AsRef<str> + Clone + Eq + Ord> Type<S> {
    pub fn any_table() -> Type<S> {
        Type::Table {
            pairs: Vec::new(),
            others: Some((Box::new(Type::Any), Box::new(Type::Any))),
        }
    }

    pub fn any_function() -> Type<S> {
        Type::Function {
            params: Vec::new(),
            variadic: Box::new(Type::Any),
            returns: Box::new(Type::Any),
            throws: Box::new(Type::Any),
        }
    }

    pub fn union(&self, other: &Type<S>) -> Type<S> {
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

    pub fn optional(&self) -> Type<S> {
        self.union(&Type::Null)
    }

    pub fn is_sub_type_of(&self, other: &Type<S>) -> bool {
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

    pub fn expect_is_sub_type_of(&self, other: &Type<S>) -> Result<(), TypeError<S>> {
        if self.is_sub_type_of(other) {
            Ok(())
        } else {
            Err(TypeError::ExpectIsSubtypeOf {
                ty: Box::new(self.clone()),
                expected: Box::new(other.clone()),
            })
        }
    }

    pub fn get_member_type(&self, property: &Type<S>) -> Result<Type<S>, TypeError<S>> {
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
        .ok_or_else(|| TypeError::TableMemberNotFound {
            table: Box::new(self.clone()),
            property: Box::new(property.clone()),
        })
    }
}

impl<S: AsRef<str> + Clone + Eq + Ord> ops::BitOr for Type<S> {
    type Output = Type<S>;

    fn bitor(self, rhs: Self) -> Self::Output {
        self.union(&rhs)
    }
}

impl<S: AsRef<str> + Clone + Eq + Ord> fmt::Display for Type<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unknown => write!(f, "unknown"),
            Type::Never => write!(f, "never"),
            Type::Any => write!(f, "any"),
            Type::Literal(literal) => write!(f, "{}", literal),
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
            Type::UserData(v) => write!(f, "{}", v.as_ref()),
            Type::Union(types) => write!(f, "({})", types.iter().join(" | ")),
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
            TyKind::Table { pairs, others } => Type::Table {
                pairs: pairs
                    .into_iter()
                    .map(|(name, t)| (LiteralType::Str(name), t.kind.into()))
                    .collect(),
                others: others.map(|t| (Box::new(t.0.kind.into()), Box::new(t.1.kind.into()))),
            },
            TyKind::Function {
                params,
                variadic,
                returns,
                throws,
            } => Type::Function {
                params: params.into_iter().map(|x| x.kind.into()).collect(),
                variadic: Box::new(variadic.map(|x| x.kind.into()).unwrap_or(Type::Null)),
                returns: Box::new(returns.map(|x| x.kind.into()).unwrap_or(Type::Null)),
                throws: Box::new(throws.map(|x| x.kind.into()).unwrap_or(Type::Null)),
            },
            TyKind::Option(t) => Type::from(t.kind).optional(),
            TyKind::Union(t) => Type::Union(t.into_iter().map(|x| x.kind.into()).collect()),
        }
    }
}

impl<S: AsRef<str> + Clone> From<LitKind<S>> for Type<S> {
    fn from(value: LitKind<S>) -> Self {
        match value {
            LitKind::Null => Type::Null,
            LitKind::Bool(v) => Type::Literal(LiteralType::Bool(v)),
            LitKind::Int(v) => Type::Literal(LiteralType::Int(v)),
            LitKind::Float(v) => Type::Literal(LiteralType::Float(v)),
            LitKind::Str(v) => Type::Literal(LiteralType::Str(v)),
        }
    }
}

/// Kind of TypeCheckError.
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum TypeError<S> {
    #[error("unexpected {ty} is subtype of {expected} ")]
    ExpectIsSubtypeOf {
        ty: Box<Type<S>>,
        expected: Box<Type<S>>,
    },
    #[error("{table} dose not has member {property} ")]
    TableMemberNotFound {
        table: Box<Type<S>>,
        property: Box<Type<S>>,
    },
    #[error("unsupported operand type(s) for {operator}: {} and {}", .operand.0, .operand.1)]
    UnsupportedBinOperator {
        operator: BinOp,
        operand: (Box<Type<S>>, Box<Type<S>>),
    },
}

#[derive(Debug, Clone)]
struct Context<S> {
    returns_type: Option<Type<S>>,
    explicit_returns_type: bool,
    throws_type: Option<Type<S>>,
    explicit_throws_type: bool,
}

impl<S> Context<S> {
    fn new() -> Self {
        Self {
            returns_type: None,
            explicit_returns_type: false,
            throws_type: None,
            explicit_throws_type: false,
        }
    }
}

/// Check type.
pub fn check_type<S: AsRef<str> + Clone + Eq + Ord>(
    program: &Program<S>,
    semantic: &Semantic<S>,
) -> Vec<TypeError<S>> {
    TypeChecker::new(semantic).check_type(program)
}

struct TypeChecker<'a, S> {
    semantic: &'a Semantic<S>,

    current_function_id: FunctionId,
    contexts: IndexVec<FunctionId, Context<S>>,
    symbol_type: IndexMap<SymbolId, Type<S>, FxBuildHasher>,
    errors: Vec<TypeError<S>>,
}

impl<'a, S: AsRef<str> + Clone + Eq + Ord> TypeChecker<'a, S> {
    fn new(semantic: &'a Semantic<S>) -> Self {
        Self {
            semantic,
            current_function_id: FunctionId::new(0),
            contexts: index_vec![Context::new(); semantic.functions.len()],
            symbol_type: IndexMap::with_hasher(FxBuildHasher),
            errors: Vec::new(),
        }
    }

    fn context(&mut self) -> &mut Context<S> {
        &mut self.contexts[self.current_function_id]
    }

    fn get_symbol_type(&self, ident: &Ident<S>) -> Option<&Type<S>> {
        let symbol_id =
            self.semantic.references[ident.reference_id.get().copied().unwrap()].symbol_id;
        self.symbol_type.get(&symbol_id)
    }

    fn set_symbol_type(&mut self, ident: &Ident<S>, ty: Type<S>) -> Result<(), TypeError<S>> {
        if let Some(old_type) = self.get_symbol_type(ident) {
            ty.expect_is_sub_type_of(old_type)?;
        } else {
            let symbol_id =
                self.semantic.references[ident.reference_id.get().copied().unwrap()].symbol_id;
            self.symbol_type.insert(symbol_id, ty);
        }
        Ok(())
    }

    fn check_type(mut self, program: &Program<S>) -> Vec<TypeError<S>> {
        if let Err(error) = self.check_function(&program.function) {
            self.errors.push(error);
        }
        self.errors
    }

    fn check_function(&mut self, function: &Function<S>) -> Result<Type<S>, TypeError<S>> {
        self.current_function_id = function.function_id.get().copied().unwrap();

        let param_types: Vec<Type<S>> = function
            .params
            .iter()
            .map(|param| {
                param
                    .ty
                    .as_ref()
                    .map(|t| t.kind.clone().into())
                    .unwrap_or(Type::Any)
            })
            .collect();
        let variadic_type: Option<Type<S>> = function
            .variadic
            .as_ref()
            .and_then(|x| x.ty.as_ref())
            .map(|t| t.kind.clone().into());
        let returns_type: Option<Type<S>> =
            function.returns.as_ref().map(|t| t.kind.clone().into());
        let throws_type: Option<Type<S>> = function.throws.as_ref().map(|t| t.kind.clone().into());

        for (param, t) in function.params.iter().zip(param_types.iter()) {
            self.set_symbol_type(&param.ident, t.clone())?;
        }
        if let Some(variadic_type) = &variadic_type {
            self.set_symbol_type(
                &function.variadic.as_ref().unwrap().ident,
                Type::Table {
                    pairs: Vec::new(),
                    others: Some((Box::new(Type::Int), Box::new(variadic_type.clone()))),
                },
            )?;
        }
        if let Some(returns_type) = &returns_type {
            self.context().returns_type = Some(returns_type.clone());
            self.context().explicit_returns_type = true;
        }
        if let Some(throws_type) = &throws_type {
            self.context().throws_type = Some(throws_type.clone());
            self.context().explicit_throws_type = true;
        }
        self.check_block(&function.body);

        if let Some(parent_id) = self.semantic.functions[self.current_function_id].parent_id {
            self.current_function_id = parent_id;
        }

        Ok(Type::Function {
            params: param_types,
            variadic: Box::new(variadic_type.unwrap_or(Type::Any)),
            returns: Box::new(returns_type.unwrap_or(Type::Any)),
            throws: Box::new(throws_type.unwrap_or(Type::Any)),
        })
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
                self.check_expr(test)?.expect_is_sub_type_of(&Type::Bool)?;
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
                self.check_expr(test)?.expect_is_sub_type_of(&Type::Bool)?;
                self.check_block(body);
            }
            StmtKind::For { left, right, body } => {
                let mut right_type = match self.check_expr(right)? {
                    Type::Any => Type::Any,
                    Type::Table { pairs, others } => {
                        let (k, v) = pairs.iter().fold(
                            others
                                .map(|(k, v)| (*k, *v))
                                .unwrap_or((Type::Never, Type::Never)),
                            |(acc_k, acc_v), (k, v)| {
                                (acc_k.union(&Type::Literal(k.clone())), acc_v.union(v))
                            },
                        );
                        Type::Table {
                            pairs: vec![(LiteralType::Int(0), k), (LiteralType::Int(1), v)],
                            others: None,
                        }
                    }
                    Type::Function { returns, .. } => *returns,
                    ty => {
                        return Err(TypeError::ExpectIsSubtypeOf {
                            ty: Box::new(ty),
                            expected: Box::new(Type::any_function()),
                        })
                    }
                };
                if let Type::Union(union) = &mut right_type {
                    union.retain(|x| x != &Type::Null);
                    match union.len() {
                        0 => right_type = Type::Never,
                        1 => right_type = union[0].clone(),
                        _ => (),
                    }
                }
                if left.len() == 1 {
                    self.set_symbol_type(&left[0], right_type)?;
                } else {
                    for (i, l) in left.iter().enumerate() {
                        self.set_symbol_type(
                            l,
                            right_type
                                .get_member_type(&Type::Literal(LiteralType::Int(i as i64)))?,
                        )?;
                    }
                }
                self.check_block(body);
            }
            StmtKind::Break => (),
            StmtKind::Continue => (),
            StmtKind::Return { argument } => {
                let return_type = self.check_expr(argument)?;
                if let Some(expect_return_type) = self.context().returns_type.clone() {
                    if self.context().explicit_returns_type {
                        return_type.expect_is_sub_type_of(&expect_return_type)?;
                    } else {
                        self.context().returns_type = Some(expect_return_type.union(&return_type));
                    }
                } else {
                    self.context().returns_type = Some(return_type);
                }
            }
            StmtKind::Throw { argument } => {
                let throw_type = self.check_expr(argument)?;
                if let Some(expect_throw_type) = self.context().throws_type.clone() {
                    if self.context().explicit_throws_type {
                        throw_type.expect_is_sub_type_of(&expect_throw_type)?;
                    } else {
                        self.context().throws_type = Some(expect_throw_type.union(&throw_type));
                    }
                } else {
                    self.context().throws_type = Some(throw_type);
                }
            }
            StmtKind::Import {
                path: _,
                path_str: _,
                kind: _,
            } => (),
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
                operator: _,
                left,
                right,
            } => {
                let right_type = self.check_expr(right)?;
                self.check_assign(left, right_type)?;
            }
            StmtKind::AssignUnpack { left, right } => {
                let right_type = self.check_expr(right)?;
                for (i, l) in left.iter().enumerate() {
                    self.check_assign(
                        l,
                        right_type.get_member_type(&Type::Literal(LiteralType::Int(i as i64)))?,
                    )?;
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
        match left {
            AssignLeft::Ident(ident) => {
                if let Some(t) = &ident.ty {
                    self.set_symbol_type(&ident.ident, t.kind.clone().into())?;
                }
                self.set_symbol_type(&ident.ident, right_type)?;
            }
            AssignLeft::Member { table, property } => {
                right_type.is_sub_type_of(
                    &self
                        .check_expr(table)?
                        .get_member_type(&self.check_member_kind(property)?)?,
                );
            }
            AssignLeft::MetaMember { table } => {
                self.check_expr(table)?
                    .expect_is_sub_type_of(&Type::any_table().optional())?;
                right_type.expect_is_sub_type_of(&(Type::any_table() | Type::Null))?;
            }
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
                let mut key = Type::Never;
                let mut value = Type::Never;
                for property in properties {
                    let key_type = self.check_expr(&property.key)?;
                    let value_type = self.check_expr(&property.value)?;
                    if let Type::Literal(lit) = key_type {
                        pairs.push((lit, value_type));
                    } else {
                        key = key.union(&key_type);
                        value = value.union(&value_type)
                    }
                }
                Type::Table {
                    pairs,
                    others: if key == Type::Never {
                        None
                    } else {
                        Some((Box::new(key), Box::new(value)))
                    },
                }
            }
            ExprKind::List { items } => {
                let mut pairs = Vec::new();
                for (i, item) in items.iter().enumerate() {
                    pairs.push((LiteralType::Int(i as i64), self.check_expr(item)?));
                }
                Type::Table {
                    pairs,
                    others: None,
                }
            }
            ExprKind::Unary { operator, argument } => match operator {
                UnOp::Not => {
                    self.check_expr(argument)?
                        .expect_is_sub_type_of(&Type::Bool)?;
                    Type::Bool
                }
                UnOp::Neg => {
                    let ty = self.check_expr(argument)?;
                    if ty == Type::Any {
                        Type::Any
                    } else if ty.is_sub_type_of(&Type::Int) {
                        Type::Int
                    } else if ty.is_sub_type_of(&Type::Float) {
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

                match operator {
                    BinOp::Add => {
                        if left_type == Type::Any && right_type == Type::Any {
                            Type::Any
                        } else if left_type.is_sub_type_of(&Type::Int)
                            && right_type.is_sub_type_of(&Type::Int)
                        {
                            Type::Int
                        } else if left_type.is_sub_type_of(&Type::Float)
                            && right_type.is_sub_type_of(&Type::Float)
                        {
                            Type::Float
                        } else if left_type.is_sub_type_of(&Type::Str)
                            && right_type.is_sub_type_of(&Type::Str)
                        {
                            Type::Str
                        } else {
                            return Err(TypeError::UnsupportedBinOperator {
                                operator: *operator,
                                operand: (Box::new(left_type), Box::new(right_type)),
                            });
                        }
                    }
                    BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem => {
                        if left_type == Type::Any && right_type == Type::Any {
                            Type::Any
                        } else if left_type.is_sub_type_of(&Type::Int)
                            && right_type.is_sub_type_of(&Type::Int)
                        {
                            Type::Int
                        } else if left_type.is_sub_type_of(&Type::Float)
                            && right_type.is_sub_type_of(&Type::Float)
                        {
                            Type::Float
                        } else {
                            return Err(TypeError::UnsupportedBinOperator {
                                operator: *operator,
                                operand: (Box::new(left_type), Box::new(right_type)),
                            });
                        }
                    }
                    BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                        if left_type == Type::Any && right_type == Type::Any
                            || left_type.is_sub_type_of(&Type::Int)
                                && right_type.is_sub_type_of(&Type::Int)
                            || left_type.is_sub_type_of(&Type::Float)
                                && right_type.is_sub_type_of(&Type::Float)
                        {
                            Type::Bool
                        } else {
                            return Err(TypeError::UnsupportedBinOperator {
                                operator: *operator,
                                operand: (Box::new(left_type), Box::new(right_type)),
                            });
                        }
                    }
                    BinOp::Eq | BinOp::Ne | BinOp::Identical | BinOp::NotIdentical => Type::Bool,
                    BinOp::And | BinOp::Or => {
                        left_type.expect_is_sub_type_of(&Type::Bool)?;
                        right_type.expect_is_sub_type_of(&Type::Bool)?;
                        Type::Bool
                    }
                    BinOp::Is => unreachable!(),
                }
            }
            ExprKind::TypeCheck { left, right } => {
                // type narrowing?
                let left_type = self.check_expr(left)?;
                match (left_type, right) {
                    (Type::Null, ValueType::Null)
                    | (Type::Bool, ValueType::Bool)
                    | (Type::Int, ValueType::Int)
                    | (Type::Float, ValueType::Float)
                    | (Type::Str, ValueType::Str)
                    | (Type::Table { .. }, ValueType::Table)
                    | (Type::Function { .. }, ValueType::Function)
                    | (Type::UserData(_), ValueType::UserData) => {
                        Type::Literal(LiteralType::Bool(true))
                    }
                    _ => Type::Bool,
                }
            }
            ExprKind::Member {
                table,
                property,
                safe,
            } => self
                .check_expr(table)?
                .get_member_type(&self.check_member_kind(property)?)
                .map(|t| if *safe { t.optional() } else { t })?,
            ExprKind::MetaMember { table, safe: _ } => {
                self.check_expr(table)?
                    .expect_is_sub_type_of(&Type::any_table())?;
                Type::any_table().optional()
            }
            ExprKind::Call {
                callee,
                arguments,
                kind,
            } => match self.check_expr(callee)? {
                Type::Any => Type::Any,
                Type::Function {
                    params,
                    variadic,
                    returns,
                    throws,
                } => {
                    let argument_types = arguments
                        .iter()
                        .map(|x| self.check_expr(x))
                        .collect::<Result<Vec<_>, _>>()?;

                    for (i, param) in params.iter().enumerate() {
                        argument_types
                            .get(i)
                            .unwrap_or(&Type::Never)
                            .expect_is_sub_type_of(param)?;
                    }
                    for argument_type in argument_types.iter().skip(params.len()) {
                        argument_type.expect_is_sub_type_of(&variadic)?;
                    }
                    match kind {
                        CallKind::None | CallKind::TryPanic => *returns,
                        CallKind::Try => Type::Table {
                            pairs: vec![
                                (LiteralType::Int(0), returns.optional()),
                                (LiteralType::Int(1), throws.optional()),
                            ],
                            others: None,
                        },
                        CallKind::TryOption => returns.optional(),
                    }
                }
                ty => {
                    return Err(TypeError::ExpectIsSubtypeOf {
                        ty: Box::new(ty),
                        expected: Box::new(Type::any_function()),
                    })
                }
            },
        })
    }

    fn check_member_kind(&mut self, member_kind: &MemberKind<S>) -> Result<Type<S>, TypeError<S>> {
        match member_kind {
            MemberKind::Bracket(expr) => self.check_expr(expr),
            MemberKind::Dot(ident) | MemberKind::DoubleColon(ident) => {
                Ok(Type::Literal(LiteralType::Str(ident.name.clone())))
            }
        }
    }
}
