pub mod error;
pub mod meta_ops;
pub mod typing;

use index_vec::{IndexVec, index_vec};
use indexmap::IndexMap;
use lucia_lang::compiler::{
    analyzer::analyze,
    ast::*,
    error::CompilerError,
    index::{FunctionId, SymbolId},
    interning::StringInterner,
    parser::parse,
    semantic::Semantic,
    value::{MetaMethod, ValueType},
};
use rustc_hash::FxBuildHasher;

use crate::{
    error::TypeError,
    meta_ops::MetaMethodType,
    typing::{FunctionType, ParmaType, TableType, Type},
};

/// Check the type of the input source code.
pub fn check_type_source<S: StringInterner<String: Eq + Ord>>(
    interner: S,
    input: &str,
) -> (Vec<CompilerError>, Vec<TypeError<S::String>>) {
    let (ast, errors) = parse(interner, input);
    let semantic = analyze(&ast);
    (errors, check_type(&ast, &semantic))
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
                if let (Type::Table(ty), Type::Table(old_ty)) = (&ty, &old_ty)
                    && ty.pairs.iter().all(|(k1, v1)| {
                        old_ty
                            .pairs
                            .iter()
                            .find(|(k2, _)| k1 == k2)
                            .map(|(_, v2)| v2)
                            .or_else(|| old_ty.others.as_ref().map(|(_, v)| v))
                            .is_some_and(|v2| v1.is_subtype_of(v2))
                    })
                    && match (&ty.others, &old_ty.others) {
                        (None, _) => true,
                        (Some((key1, value1)), Some((key2, value2))) => {
                            key1.is_subtype_of(key2) && value1.is_subtype_of(value2)
                        }
                        _ => false,
                    }
                {
                    return Ok(());
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
                if right_type == Type::Any {
                    for l in left {
                        self.set_symbol_type(l, Type::Any)?;
                    }
                } else if left.len() == 1 {
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
                            &MetaMethodType,
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
                let right_type = self.check_bin_op(*operator, left_type, right_type)?;
                self.check_assign(left, right_type)?;
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
                    &MetaMethodType,
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
                self.check_bin_op(*operator, left_type, right_type)?
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
                    &MetaMethodType,
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

    fn check_bin_op(
        &mut self,
        operator: BinOp,
        left_type: Type<S>,
        right_type: Type<S>,
    ) -> Result<Type<S>, TypeError<S>> {
        match operator {
            BinOp::Add => MetaMethodType.add(left_type, right_type),
            BinOp::Sub => MetaMethodType.sub(left_type, right_type),
            BinOp::Mul => MetaMethodType.mul(left_type, right_type),
            BinOp::Div => MetaMethodType.div(left_type, right_type),
            BinOp::Rem => MetaMethodType.rem(left_type, right_type),
            BinOp::And | BinOp::Or => Ok(Type::Bool),
            BinOp::Eq => MetaMethodType.eq(left_type, right_type),
            BinOp::Ne => MetaMethodType.ne(left_type, right_type),
            BinOp::Lt => MetaMethodType.lt(left_type, right_type),
            BinOp::Le => MetaMethodType.le(left_type, right_type),
            BinOp::Gt => MetaMethodType.gt(left_type, right_type),
            BinOp::Ge => MetaMethodType.ge(left_type, right_type),
            BinOp::Identical | BinOp::NotIdentical | BinOp::Is => Ok(Type::Bool),
        }
    }
}
