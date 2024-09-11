//! The semantic analyzer.
//!
//! Lowers the AST to `Vec<Function>` and add semantic information.
//!
//! Functions in the AST will be identified and processed.
//! The kind of names within the namespace will be determined.

use std::{collections::HashMap, fmt, mem};

use indexmap::IndexMap;
use smol_str::SmolStr;

use crate::utils::Join;

use super::{
    ast::*,
    code::ConstValue,
    opcode::{JumpTarget, OpCode},
    typing::{LiteralType, Type, TypeCheckError},
};

#[derive(Debug, Clone)]
pub struct GlobalName {
    pub is_writable: bool,
}

#[derive(Debug, Clone)]
pub struct UpvalueName {
    pub defined_func_id: usize,
    pub base_closure_upvalue_id: usize,
}

/// A Function.
#[derive(Debug, Clone, Default)]
pub struct Function {
    /// Function id.
    pub func_id: usize,
    /// Kind of Function.
    pub kind: FunctionKind,
    /// Name of parameters.
    pub params: Vec<TypedIdent>,
    /// Name of variadic parameter.
    pub variadic: Option<Box<TypedIdent>>,
    /// AST of the function.
    pub body: Box<Block>,
    /// The base function.
    pub base_function: Option<usize>,

    /// Local names.
    pub local_names: IndexMap<SmolStr, Option<Type>>,
    /// Global names.
    pub global_names: IndexMap<SmolStr, GlobalName>,
    /// Upvalue names.
    pub upvalue_names: IndexMap<SmolStr, UpvalueName>,

    // used by type check
    pub(crate) returns_type: Option<Box<Type>>,
    pub(crate) explicit_returns_type: bool,
    pub(crate) throws_type: Option<Box<Type>>,
    pub(crate) explicit_throws_type: bool,

    // used by codegen
    pub(crate) code: Vec<OpCode>,
    pub(crate) consts: Vec<ConstValue>,
    pub(crate) jump_target_count: usize,
    pub(crate) continue_stack: Vec<JumpTarget>,
    pub(crate) break_stack: Vec<JumpTarget>,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "func_id: {}", self.func_id)?;
        writeln!(f, "kind: {}", self.kind)?;
        if let Some(v) = &self.variadic {
            writeln!(f, "params: ({}), *{}", self.params.iter().join(", "), v)?;
        } else {
            writeln!(f, "params: ({})", self.params.iter().join(", "))?;
        }
        writeln!(f, "base_function: {:?}", self.base_function)?;
        writeln!(f, "body: {}", self.body)?;

        writeln!(f, "local_names: {:?}", self.local_names)?;
        writeln!(f, "global_names: {:?}", self.global_names)?;
        writeln!(f, "upvalue_names: {:?}", self.upvalue_names)?;
        Ok(())
    }
}

impl Function {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        func_id: usize,
        kind: FunctionKind,
        params: Vec<TypedIdent>,
        variadic: Option<Box<TypedIdent>>,
        body: Box<Block>,
        base_function: Option<usize>,
        returns_type: Option<Box<Type>>,
        throws_type: Option<Box<Type>>,
    ) -> Self {
        Function {
            func_id,
            kind,
            params,
            variadic,
            body,
            base_function,
            local_names: IndexMap::new(),
            global_names: IndexMap::new(),
            upvalue_names: IndexMap::new(),
            explicit_returns_type: returns_type.is_some(),
            returns_type,
            explicit_throws_type: throws_type.is_some(),
            throws_type,
            code: Vec::new(),
            consts: vec![ConstValue::Null],
            jump_target_count: 0,
            continue_stack: Vec::new(),
            break_stack: Vec::new(),
        }
    }

    pub fn upvalues(&self) -> impl Iterator<Item = (SmolStr, Option<usize>)> + '_ {
        self.upvalue_names.iter().map(
            |(
                k,
                UpvalueName {
                    defined_func_id,
                    base_closure_upvalue_id,
                },
            )| {
                (
                    k.clone(),
                    if defined_func_id == &self.func_id {
                        None
                    } else {
                        Some(*base_closure_upvalue_id)
                    },
                )
            },
        )
    }

    fn to_type(&self) -> Type {
        Type::Function {
            params: self
                .params
                .iter()
                .map(|x| x.t.clone().unwrap_or(Type::Any).clone())
                .collect(),
            variadic: Box::new(
                self.variadic
                    .clone()
                    .map(|x| x.t.unwrap_or(Type::Any))
                    .unwrap_or(Type::Null),
            ),
            returns: self.returns_type.clone().unwrap_or(Box::new(Type::Any)),
            throws: self.throws_type.clone().unwrap_or(Box::new(Type::Any)),
        }
    }
}

/// Semantic Analyze. Lowers the AST to `Vec<Function>`.
pub fn analyze(ast: AST) -> Result<Vec<Function>, Vec<TypeCheckError>> {
    let first_comment = ast.first_comment.trim();
    let enable_type_check =
        first_comment.contains("type-check: on") || first_comment.contains("type-check: strict");
    let mut analyzer = SemanticAnalyzer::new(ast);
    analyzer.analyze_name();
    if enable_type_check {
        analyzer.type_check();
    }
    if analyzer.errors.is_empty() {
        Ok(analyzer.func_list)
    } else {
        Err(analyzer.errors)
    }
}

#[derive(Debug)]
struct SemanticAnalyzer {
    func_list: Vec<Function>,
    global_types: HashMap<SmolStr, Option<Type>>,
    upvalue_types: HashMap<(usize, SmolStr), Option<Type>>,
    errors: Vec<TypeCheckError>,
}

impl SemanticAnalyzer {
    fn new(ast: AST) -> Self {
        let mut analyzer = SemanticAnalyzer {
            func_list: Vec::new(),
            global_types: HashMap::new(),
            upvalue_types: HashMap::new(),
            errors: Vec::new(),
        };
        Handle::new(0, &mut analyzer).build(Function::new(
            0,
            FunctionKind::Function,
            Vec::new(),
            None,
            ast.body,
            None,
            None,
            None,
        ));
        analyzer
    }

    fn analyze_name(&mut self) {
        for func_id in 0..self.func_list.len() {
            for param in self.func_list[func_id].params.clone() {
                self.func_list[func_id]
                    .local_names
                    .insert(param.ident.name, param.t);
            }
            if let Some(variadic) = self.func_list[func_id].variadic.clone() {
                self.func_list[func_id].local_names.insert(
                    variadic.ident.name,
                    variadic.t.map(|t| Type::Table {
                        pairs: Vec::new(),
                        others: Some((Box::new(Type::Int), Box::new(t))),
                    }),
                );
            }
            let mut body = mem::take(&mut self.func_list[func_id].body);
            Handle::new(func_id, self).analyze_name_block(&body);
            mem::swap(&mut self.func_list[func_id].body, &mut body);
        }
    }

    fn type_check(&mut self) {
        for func_id in 0..self.func_list.len() {
            let mut body = mem::take(&mut self.func_list[func_id].body);
            Handle::new(func_id, self).type_check_block(&mut body);
            mem::swap(&mut self.func_list[func_id].body, &mut body);
        }
    }
}

#[derive(Debug)]
struct Handle<'a> {
    func_id: usize,
    analyzer: &'a mut SemanticAnalyzer,
}

impl<'a> Handle<'a> {
    fn new(func_id: usize, analyzer: &'a mut SemanticAnalyzer) -> Self {
        Handle { func_id, analyzer }
    }

    fn current(&self) -> &Function {
        &self.analyzer.func_list[self.func_id]
    }

    fn current_mut(&mut self) -> &mut Function {
        &mut self.analyzer.func_list[self.func_id]
    }
}

impl<'a> Handle<'a> {
    fn build(&mut self, mut func: Function) {
        self.analyzer.func_list.push(Function::default());
        self.build_block(&mut func.body);
        self.analyzer.func_list[self.func_id] = func;
    }

    fn build_block(&mut self, ast_node: &mut Block) {
        for stmt in &mut ast_node.body {
            self.build_stmt(stmt);
        }
    }

    fn build_stmt(&mut self, ast_node: &mut Stmt) {
        match &mut ast_node.kind {
            StmtKind::If {
                test,
                consequent,
                alternate,
            } => {
                self.build_expr(test);
                self.build_block(consequent);
                if let Some(alternate) = alternate {
                    self.build_stmt(alternate);
                }
            }
            StmtKind::Loop { body } => self.build_block(body),
            StmtKind::While { test, body } => {
                self.build_expr(test);
                self.build_block(body);
            }
            StmtKind::For {
                left: _,
                right,
                body,
            } => {
                self.build_expr(right);
                self.build_block(body);
            }
            StmtKind::Break => (),
            StmtKind::Continue => (),
            StmtKind::Return { argument } => self.build_expr(argument),
            StmtKind::Throw { argument } => self.build_expr(argument),
            StmtKind::Global { arguments: _ } => (),
            StmtKind::Import { path: _, kind: _ } => (),
            StmtKind::Assign { left, right } => {
                self.build_assign_left(left);
                self.build_expr(right);
            }
            StmtKind::AssignOp {
                operator: _,
                left,
                right,
            } => {
                self.build_assign_left(left);
                self.build_expr(right);
            }
            StmtKind::AssignUnpack { left, right } => {
                for left in left {
                    self.build_assign_left(left);
                }
                self.build_expr(right);
            }
            StmtKind::AssignMulti { left, right } => {
                for left in left {
                    self.build_assign_left(left);
                }
                for right in right {
                    self.build_expr(right);
                }
            }
            StmtKind::Block(block) => self.build_block(block),
            StmtKind::Expr(expr) => self.build_expr(expr),
        }
    }

    fn build_expr(&mut self, ast_node: &mut Expr) {
        match &mut ast_node.kind {
            ExprKind::Lit(_) => (),
            ExprKind::Ident(_) => (),
            ExprKind::Function { .. } => {
                let func_id = self.analyzer.func_list.len();
                if let ExprKind::Function {
                    kind,
                    params,
                    variadic,
                    body,
                    returns,
                    throws,
                } = mem::replace(&mut ast_node.kind, ExprKind::FunctionId(func_id))
                {
                    Handle::new(func_id, self.analyzer).build(Function::new(
                        func_id,
                        kind,
                        params,
                        variadic,
                        body,
                        Some(self.func_id),
                        returns,
                        throws,
                    ));
                }
            }
            ExprKind::FunctionId(_) => (),
            ExprKind::Table { properties } => {
                for TableProperty { key, value, .. } in properties {
                    self.build_expr(key);
                    self.build_expr(value);
                }
            }
            ExprKind::List { items } => {
                for item in items {
                    self.build_expr(item);
                }
            }
            ExprKind::Unary {
                operator: _,
                argument,
            } => self.build_expr(argument),
            ExprKind::Binary {
                operator: _,
                left,
                right,
            } => {
                self.build_expr(left);
                self.build_expr(right);
            }
            ExprKind::Member {
                table,
                property,
                safe: _,
            } => {
                self.build_expr(table);
                self.build_member_property(property);
            }
            ExprKind::MetaMember { table, safe: _ } => self.build_expr(table),
            ExprKind::Call {
                callee,
                arguments,
                kind: _,
            } => {
                self.build_expr(callee);
                for arg in arguments {
                    self.build_expr(arg);
                }
            }
        }
    }

    fn build_assign_left(&mut self, ast_node: &mut AssignLeft) {
        match ast_node {
            AssignLeft::Ident { .. } => (),
            AssignLeft::Member { table, property } => {
                self.build_expr(table);
                self.build_member_property(property);
            }
            AssignLeft::MetaMember { table } => self.build_expr(table),
        }
    }

    fn build_member_property(&mut self, ast_node: &mut MemberKind) {
        match ast_node {
            MemberKind::Bracket(expr) => self.build_expr(expr),
            MemberKind::Dot(_) | MemberKind::DoubleColon(_) => (),
        }
    }
}

impl<'a> Handle<'a> {
    fn analyze_name_block(&mut self, ast_node: &Block) {
        for stmt in &ast_node.body {
            self.analyze_name_stmt(stmt);
        }
    }

    fn analyze_name_stmt(&mut self, ast_node: &Stmt) {
        match &ast_node.kind {
            StmtKind::If {
                test,
                consequent,
                alternate,
            } => {
                self.analyze_name_expr(test);
                self.analyze_name_block(consequent);
                if let Some(alternate) = alternate {
                    self.analyze_name_stmt(alternate);
                }
            }
            StmtKind::Loop { body } => self.analyze_name_block(body),
            StmtKind::While { test, body } => {
                self.analyze_name_expr(test);
                self.analyze_name_block(body);
            }
            StmtKind::For { left, right, body } => {
                for left in left {
                    self.store_name(&left.name, None);
                }
                self.analyze_name_expr(right);
                self.analyze_name_block(body);
            }
            StmtKind::Break => (),
            StmtKind::Continue => (),
            StmtKind::Return { argument } => self.analyze_name_expr(argument),
            StmtKind::Throw { argument } => self.analyze_name_expr(argument),
            StmtKind::Global { arguments } => {
                for arg in arguments {
                    self.current_mut()
                        .global_names
                        .insert(arg.ident.name.clone(), GlobalName { is_writable: true });
                    if !self.analyzer.global_types.contains_key(&arg.ident.name) {
                        self.analyzer
                            .global_types
                            .insert(arg.ident.name.clone(), arg.t.clone());
                    }
                }
            }
            StmtKind::Import { path: _, kind } => match kind {
                ImportKind::Simple(alias) => {
                    self.current_mut()
                        .global_names
                        .insert(alias.name.clone(), GlobalName { is_writable: false });
                }
                ImportKind::Nested(items) => {
                    for (_, alias) in items {
                        self.current_mut()
                            .global_names
                            .insert(alias.name.clone(), GlobalName { is_writable: false });
                    }
                }
                ImportKind::Glob => (),
            },
            StmtKind::Assign { left, right } => {
                self.analyze_name_assign_left(left);
                self.analyze_name_expr(right);
            }
            StmtKind::AssignOp {
                operator: _,
                left,
                right,
            } => {
                self.analyze_name_assign_left(left);
                self.analyze_name_expr(right);
            }
            StmtKind::AssignUnpack { left, right } => {
                for left in left {
                    self.analyze_name_assign_left(left);
                }
                self.analyze_name_expr(right);
            }
            StmtKind::AssignMulti { left, right } => {
                for left in left {
                    self.analyze_name_assign_left(left);
                }
                for right in right {
                    self.analyze_name_expr(right);
                }
            }
            StmtKind::Block(block) => self.analyze_name_block(block),
            StmtKind::Expr(expr) => self.analyze_name_expr(expr),
        }
    }

    fn analyze_name_expr(&mut self, ast_node: &Expr) {
        match &ast_node.kind {
            ExprKind::Lit(_) => (),
            ExprKind::Ident(ident) => self.load_name(&ident.name),
            ExprKind::Function { .. } => panic!(),
            ExprKind::FunctionId(_) => (),
            ExprKind::Table { properties } => {
                for TableProperty { key, value, .. } in properties {
                    self.analyze_name_expr(key);
                    self.analyze_name_expr(value);
                }
            }
            ExprKind::List { items } => {
                for item in items {
                    self.analyze_name_expr(item);
                }
            }
            ExprKind::Unary {
                operator: _,
                argument,
            } => self.analyze_name_expr(argument),
            ExprKind::Binary {
                operator: _,
                left,
                right,
            } => {
                self.analyze_name_expr(left);
                self.analyze_name_expr(right);
            }
            ExprKind::Member {
                table,
                property,
                safe: _,
            } => {
                self.analyze_name_expr(table);
                self.analyze_name_member_property(property);
            }
            ExprKind::MetaMember { table, safe: _ } => self.analyze_name_expr(table),
            ExprKind::Call {
                callee,
                arguments,
                kind: _,
            } => {
                self.analyze_name_expr(callee);
                for arg in arguments {
                    self.analyze_name_expr(arg);
                }
            }
        }
    }

    fn analyze_name_assign_left(&mut self, ast_node: &AssignLeft) {
        match ast_node {
            AssignLeft::Ident(ident) => self.store_name(&ident.ident.name, ident.t.clone()),
            AssignLeft::Member { table, property } => {
                self.analyze_name_expr(table);
                self.analyze_name_member_property(property);
            }
            AssignLeft::MetaMember { table } => self.analyze_name_expr(table),
        }
    }

    fn analyze_name_member_property(&mut self, ast_node: &MemberKind) {
        match ast_node {
            MemberKind::Bracket(expr) => self.analyze_name_expr(expr),
            MemberKind::Dot(_) | MemberKind::DoubleColon(_) => (),
        }
    }

    fn load_name(&mut self, name: &str) {
        if self.current().local_names.contains_key(name)
            || self.current().global_names.contains_key(name)
            || self.current().upvalue_names.contains_key(name)
        {
            return;
        }

        if self.current().kind != FunctionKind::Closure {
            self.current_mut()
                .global_names
                .insert(name.into(), GlobalName { is_writable: false });
            return;
        }

        self.find_upvalue(name, |handle| {
            handle
                .current_mut()
                .global_names
                .insert(name.into(), GlobalName { is_writable: false });
        });
    }

    fn store_name(&mut self, name: &str, t: Option<Type>) {
        if self.current().local_names.contains_key(name)
            || matches!(
                self.current().global_names.get(name),
                Some(GlobalName { is_writable: true })
            )
            || self.current().upvalue_names.contains_key(name)
        {
            return;
        }

        if self.current().kind != FunctionKind::Closure {
            self.current_mut().local_names.insert(name.into(), t);
            return;
        }

        self.find_upvalue(name, |handle| {
            handle
                .current_mut()
                .local_names
                .insert(name.into(), t.clone());
        });
    }

    fn find_upvalue<T: Fn(&mut Self)>(&mut self, name: &str, default: T) {
        let mut func_id_stack = vec![self.func_id];
        let mut base_func = self.current_mut();
        loop {
            if let Some(func) = base_func.base_function {
                base_func = &mut self.analyzer.func_list[func];
                func_id_stack.push(base_func.func_id);
            } else {
                default(self);
                break;
            }

            let defined_func_id;
            if let Some(t) = base_func.local_names.shift_remove(name) {
                defined_func_id = base_func.func_id;
                self.analyzer
                    .upvalue_types
                    .insert((defined_func_id, name.into()), t);
                base_func.upvalue_names.insert(
                    name.into(),
                    UpvalueName {
                        defined_func_id,
                        base_closure_upvalue_id: 0,
                    },
                );
            } else if let Some(UpvalueName {
                defined_func_id: func_id,
                base_closure_upvalue_id: _,
            }) = base_func.upvalue_names.get(name)
            {
                defined_func_id = *func_id;
            } else if base_func.kind != FunctionKind::Closure {
                default(self);
                break;
            } else {
                continue;
            }

            let mut base_func_id = func_id_stack.pop().unwrap();
            for func_id in func_id_stack.into_iter().rev() {
                let base_closure_upvalue_id = self.analyzer.func_list[base_func_id]
                    .upvalue_names
                    .get_index_of(name)
                    .unwrap();
                self.analyzer.func_list[func_id].upvalue_names.insert(
                    name.into(),
                    UpvalueName {
                        defined_func_id,
                        base_closure_upvalue_id,
                    },
                );
                base_func_id = func_id;
            }
            break;
        }
    }
}

impl<'a> Handle<'a> {
    fn type_check_block(&mut self, ast_node: &mut Block) {
        for stmt in &mut ast_node.body {
            if let Err(e) = self.type_check_stmt(stmt) {
                self.analyzer.errors.push(e)
            }
        }
    }

    fn type_check_stmt(&mut self, ast_node: &mut Stmt) -> Result<(), TypeCheckError> {
        match &mut ast_node.kind {
            StmtKind::If {
                test,
                consequent,
                alternate,
            } => {
                self.type_check_expr(test)?
                    .expect_is_sub_type_of(&Type::Bool)?;
                self.type_check_block(consequent);
                if let Some(alternate) = alternate {
                    self.type_check_stmt(alternate)?;
                }
            }
            StmtKind::Loop { body } => {
                self.type_check_block(body);
            }
            StmtKind::While { test, body } => {
                self.type_check_expr(test)?
                    .expect_is_sub_type_of(&Type::Bool)?;
                self.type_check_block(body);
            }
            StmtKind::For { left, right, body } => {
                let mut right_type = match self.type_check_expr(right)? {
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
                    t => {
                        return Err(TypeCheckError::ExpectIsSubtypeOf {
                            t: Box::new(t),
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
                    self.set_name_type(&left[0].name, right_type)?;
                } else {
                    for (i, l) in left.iter().enumerate() {
                        self.set_name_type(
                            &l.name,
                            right_type
                                .get_member_type(&Type::Literal(LiteralType::Int(i as i64)))?,
                        )?;
                    }
                }
                self.type_check_block(body);
            }
            StmtKind::Break => (),
            StmtKind::Continue => (),
            StmtKind::Return { argument } => {
                let return_type = self.type_check_expr(argument)?;
                if let Some(expect_return_type) = self.current().returns_type.clone() {
                    if self.current().explicit_returns_type {
                        return_type.expect_is_sub_type_of(&expect_return_type)?;
                    } else {
                        self.current_mut().returns_type =
                            Some(Box::new(expect_return_type.union(&return_type)));
                    }
                } else {
                    self.current_mut().returns_type = Some(Box::new(return_type));
                }
            }
            StmtKind::Throw { argument } => {
                let throw_type = self.type_check_expr(argument)?;
                if let Some(expect_throw_type) = self.current().throws_type.clone() {
                    if self.current().explicit_throws_type {
                        throw_type.expect_is_sub_type_of(&expect_throw_type)?;
                    } else {
                        self.current_mut().throws_type =
                            Some(Box::new(expect_throw_type.union(&throw_type)));
                    }
                } else {
                    self.current_mut().throws_type = Some(Box::new(throw_type));
                }
            }
            StmtKind::Global { arguments: _ } => (),
            StmtKind::Import { path: _, kind: _ } => (),
            StmtKind::Assign { left, right } => {
                let right_type = self.type_check_expr(right)?;
                self.type_check_assign(left, right_type)?;
            }
            StmtKind::AssignOp {
                operator,
                left,
                right,
            } => {
                let right_type = self.type_check_expr(&mut Expr {
                    kind: ExprKind::Binary {
                        operator: *operator,
                        left: Box::new(left.clone().into()),
                        right: right.clone(),
                    },
                    start: ast_node.start,
                    end: ast_node.end,
                })?;
                self.type_check_assign(left, right_type)?;
            }
            StmtKind::AssignUnpack { left, right } => {
                let right_type = self.type_check_expr(right)?;
                for (i, l) in left.iter_mut().enumerate() {
                    self.type_check_assign(
                        l,
                        right_type.get_member_type(&Type::Literal(LiteralType::Int(i as i64)))?,
                    )?;
                }
            }
            StmtKind::AssignMulti { left, right } => {
                assert!(left.len() != right.len());
                for (left, right) in left.iter_mut().zip(right) {
                    let right_type = self.type_check_expr(right)?;
                    self.type_check_assign(left, right_type)?;
                }
            }
            StmtKind::Block(block) => {
                self.type_check_block(block);
            }
            StmtKind::Expr(expr) => {
                self.type_check_expr(expr)?;
            }
        }
        Ok(())
    }

    fn type_check_assign(
        &mut self,
        left: &mut AssignLeft,
        right_type: Type,
    ) -> Result<(), TypeCheckError> {
        match left {
            AssignLeft::Ident(ident) => {
                if let Some(t) = &ident.t {
                    self.set_name_type(&ident.ident.name, t.clone())?;
                }
                self.set_name_type(&ident.ident.name, right_type)?;
            }
            AssignLeft::Member { table, property } => {
                right_type.is_sub_type_of(
                    &self
                        .type_check_expr(table)?
                        .get_member_type(&self.type_check_member_kind(property)?)?,
                );
            }
            AssignLeft::MetaMember { table } => {
                self.type_check_expr(table)?
                    .expect_is_sub_type_of(&Type::any_table().optional())?;
                right_type.expect_is_sub_type_of(&(Type::any_table() | Type::Null))?;
            }
        }
        Ok(())
    }

    fn type_check_expr(&mut self, ast_node: &mut Expr) -> Result<Type, TypeCheckError> {
        Ok(match &mut ast_node.kind {
            ExprKind::Lit(lit) => lit.value.clone().into(),
            ExprKind::Ident(ident) => self.get_name_type(&ident.name).unwrap_or(Type::Unknown),
            ExprKind::Function { .. } => panic!(),
            ExprKind::FunctionId(i) => self.analyzer.func_list[*i].to_type(),
            ExprKind::Table { properties } => {
                let mut pairs = Vec::new();
                let mut key = Type::Never;
                let mut value = Type::Never;
                for property in properties {
                    let key_type = self.type_check_expr(&mut property.key)?;
                    let value_type = self.type_check_expr(&mut property.value)?;
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
                for (i, item) in items.iter_mut().enumerate() {
                    pairs.push((LiteralType::Int(i as i64), self.type_check_expr(item)?));
                }
                Type::Table {
                    pairs,
                    others: None,
                }
            }
            ExprKind::Unary { operator, argument } => match operator {
                UnOp::Not => {
                    self.type_check_expr(argument)?
                        .expect_is_sub_type_of(&Type::Bool)?;
                    Type::Bool
                }
                UnOp::Neg => {
                    let t = self.type_check_expr(argument)?;
                    if t == Type::Any {
                        Type::Any
                    } else if t.is_sub_type_of(&Type::Int) {
                        Type::Int
                    } else if t.is_sub_type_of(&Type::Float) {
                        Type::Float
                    } else {
                        return Err(TypeCheckError::ExpectIsSubtypeOf {
                            t: Box::new(t),
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
                let left_type = self.type_check_expr(left)?;
                let right_type = self.type_check_expr(right)?;

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
                            return Err(TypeCheckError::UnsupportedBinOperator {
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
                            return Err(TypeCheckError::UnsupportedBinOperator {
                                operator: *operator,
                                operand: (Box::new(left_type), Box::new(right_type)),
                            });
                        }
                    }
                    BinOp::Lt | BinOp::Le | BinOp::Ge | BinOp::Gt => {
                        if left_type == Type::Any && right_type == Type::Any
                            || left_type.is_sub_type_of(&Type::Int)
                                && right_type.is_sub_type_of(&Type::Int)
                            || left_type.is_sub_type_of(&Type::Float)
                                && right_type.is_sub_type_of(&Type::Float)
                        {
                            Type::Bool
                        } else {
                            return Err(TypeCheckError::UnsupportedBinOperator {
                                operator: *operator,
                                operand: (Box::new(left_type), Box::new(right_type)),
                            });
                        }
                    }
                    BinOp::Eq | BinOp::Ne | BinOp::Is => Type::Bool,
                    BinOp::And | BinOp::Or => {
                        left_type.expect_is_sub_type_of(&Type::Bool)?;
                        right_type.expect_is_sub_type_of(&Type::Bool)?;
                        Type::Bool
                    }
                }
            }
            ExprKind::Member {
                table,
                property,
                safe,
            } => self
                .type_check_expr(table)?
                .get_member_type(&self.type_check_member_kind(property)?)
                .map(|t| if *safe { t.optional() } else { t })?,
            ExprKind::MetaMember { table, safe: _ } => {
                self.type_check_expr(table)?
                    .expect_is_sub_type_of(&Type::any_table())?;
                Type::any_table().optional()
            }
            ExprKind::Call {
                callee,
                arguments,
                kind,
            } => match self.type_check_expr(callee)? {
                Type::Any => Type::Any,
                Type::Function {
                    params,
                    variadic,
                    returns,
                    throws,
                } => {
                    let argument_types = arguments
                        .iter_mut()
                        .map(|x| self.type_check_expr(x))
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
                t => {
                    return Err(TypeCheckError::ExpectIsSubtypeOf {
                        t: Box::new(t),
                        expected: Box::new(Type::any_function()),
                    })
                }
            },
        })
    }

    fn type_check_member_kind(
        &mut self,
        ast_node: &mut MemberKind,
    ) -> Result<Type, TypeCheckError> {
        match ast_node {
            MemberKind::Bracket(expr) => self.type_check_expr(expr),
            MemberKind::Dot(ident) | MemberKind::DoubleColon(ident) => {
                Ok(Type::Literal(LiteralType::Str(ident.name.clone())))
            }
        }
    }

    fn get_name_type(&self, name: &str) -> Option<Type> {
        if let Some(t) = self.current().local_names.get(name) {
            t.clone()
        } else if self.current().global_names.contains_key(name) {
            self.analyzer
                .global_types
                .get(name)
                .cloned()
                .unwrap_or(None)
        } else if let Some(UpvalueName {
            defined_func_id,
            base_closure_upvalue_id: _,
        }) = self.current().upvalue_names.get(name)
        {
            self.analyzer
                .upvalue_types
                .get(&(*defined_func_id, name.into()))
                .cloned()
                .unwrap_or(None)
        } else {
            None
        }
    }

    fn set_name_type(&mut self, name: &str, t: Type) -> Result<(), TypeCheckError> {
        if let Some(old_type) = self.get_name_type(name) {
            t.expect_is_sub_type_of(&old_type)?;
        } else {
            let t = match t {
                Type::Literal(LiteralType::Bool(_)) => Type::Bool,
                Type::Literal(LiteralType::Int(_)) => Type::Int,
                Type::Literal(LiteralType::Float(_)) => Type::Float,
                Type::Literal(LiteralType::Str(_)) => Type::Str,
                _ => t,
            };
            if let Some(x) = self.current_mut().local_names.get_mut(name) {
                *x = Some(t);
            } else if self.current().global_names.contains_key(name) {
                self.analyzer.global_types.insert(name.into(), Some(t));
            } else if let Some(UpvalueName {
                defined_func_id,
                base_closure_upvalue_id: _,
            }) = self.current().upvalue_names.get(name)
            {
                let func_id = *defined_func_id;
                self.analyzer
                    .upvalue_types
                    .insert((func_id, name.into()), Some(t));
            }
        }

        Ok(())
    }
}
