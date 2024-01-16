//! The semantic analyzer.
//!
//! Lowers the AST to `Vec<Function>` and add semantic information.
//!
//! Functions in the AST will be identified and processed.
//! The kind of names within the namespace will be determined.

use std::{fmt, mem};

use indexmap::IndexMap;

use super::{
    ast::*,
    code::ConstValue,
    opcode::{JumpTarget, OpCode},
};

#[derive(Debug, Clone)]
pub enum NameKind {
    Local,
    Global {
        is_writable: bool,
    },
    Upvalue {
        func_count: usize,
        upvalue_id: usize,
    },
}

/// A Function.
#[derive(Debug, Clone, Default)]
pub struct Function {
    /// Function id.
    pub func_id: usize,
    /// Kind of Function.
    pub kind: FunctionKind,
    /// Name of parameters.
    pub params: Vec<String>,
    /// Name of variadic parameter.
    pub variadic: Option<String>,
    /// AST of the function.
    pub body: Box<Block>,
    /// The base function.
    pub base_function: Option<usize>,

    /// Names.
    pub names: IndexMap<String, NameKind>,

    /// The count of upvalues defined in the function.
    pub def_upvalue_count: usize,

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
            writeln!(f, "params: ({}), *{}", self.params.join(", "), v)?;
        } else {
            writeln!(f, "params: ({})", self.params.join(", "))?;
        }
        writeln!(f, "base_function: {:?}", self.base_function)?;
        writeln!(f, "def_upvalue_count: {}", self.def_upvalue_count)?;
        writeln!(f, "body: {}", self.body)?;

        writeln!(f, "names: {:?}", self.names)?;
        Ok(())
    }
}

impl Function {
    pub fn new(
        func_id: usize,
        kind: FunctionKind,
        params: Vec<Ident>,
        variadic: Option<Box<Ident>>,
        body: Box<Block>,
        base_function: Option<usize>,
    ) -> Self {
        Function {
            func_id,
            kind,
            params: params.into_iter().map(|x| x.name).collect(),
            variadic: variadic.map(|x| x.name),
            body,
            base_function,
            names: IndexMap::new(),
            def_upvalue_count: 0,
            code: Vec::new(),
            consts: vec![ConstValue::Null],
            jump_target_count: 0,
            continue_stack: Vec::new(),
            break_stack: Vec::new(),
        }
    }

    pub fn upvalues(&self) -> impl Iterator<Item = (String, usize, usize)> + '_ {
        self.names.iter().filter_map(|(k, v)| match v {
            NameKind::Upvalue {
                func_count,
                upvalue_id,
            } => Some((k.clone(), *func_count, *upvalue_id)),
            _ => None,
        })
    }
}

macro_rules! impl_names_iter {
    ($name:ident, $pattern:pat) => {
        impl Function {
            pub fn $name(&self) -> impl Iterator<Item = &String> + '_ {
                self.names.iter().filter_map(
                    |(k, v)| {
                        if matches!(v, $pattern) {
                            Some(k)
                        } else {
                            None
                        }
                    },
                )
            }
        }
    };
}

impl_names_iter!(local_names, NameKind::Local);
impl_names_iter!(global_names, NameKind::Global { .. });
impl_names_iter!(upvalue_names, NameKind::Upvalue { .. });

/// Semantic Analyze. Lowers the AST to `Vec<Function>`.
pub fn analyze(ast: Box<Block>) -> Vec<Function> {
    let mut func_list: Vec<Function> = Vec::new();

    Handle::new(0, &mut func_list).build(Function::new(
        0,
        FunctionKind::Function,
        Vec::new(),
        None,
        ast,
        None,
    ));

    for func_id in 0..func_list.len() {
        for param in func_list[func_id].params.clone() {
            func_list[func_id].names.insert(param, NameKind::Local);
        }
        if let Some(variadic) = func_list[func_id].variadic.clone() {
            func_list[func_id].names.insert(variadic, NameKind::Local);
        }
        let mut body = mem::take(&mut func_list[func_id].body);
        Handle::new(func_id, &mut func_list).analyze_name_block(&body);
        mem::swap(&mut func_list[func_id].body, &mut body);
    }

    func_list
}

#[derive(Debug)]
struct Handle<'a> {
    func_id: usize,
    func_list: &'a mut Vec<Function>,
}

impl<'a> Handle<'a> {
    fn new(func_id: usize, func_list: &'a mut Vec<Function>) -> Self {
        Handle { func_id, func_list }
    }

    fn current(&self) -> &Function {
        &self.func_list[self.func_id]
    }

    fn current_mut(&mut self) -> &mut Function {
        &mut self.func_list[self.func_id]
    }

    fn build(&mut self, mut func: Function) {
        self.func_list.push(Function::default());
        self.build_block(&mut func.body);
        self.func_list[self.func_id] = func;
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
                let func_id = self.func_list.len();
                if let ExprKind::Function {
                    kind,
                    params,
                    variadic,
                    body,
                } = mem::replace(&mut ast_node.kind, ExprKind::FunctionId(func_id))
                {
                    Handle::new(func_id, self.func_list).build(Function::new(
                        func_id,
                        kind,
                        params,
                        variadic,
                        body,
                        Some(self.func_id),
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
            AssignLeft::Ident(_) => (),
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
                    self.store_name(&left.name);
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
                        .names
                        .insert(arg.name.clone(), NameKind::Global { is_writable: true });
                }
            }
            StmtKind::Import { path: _, kind } => match kind {
                ImportKind::Simple(alias) => {
                    self.current_mut()
                        .names
                        .insert(alias.name.clone(), NameKind::Global { is_writable: false });
                }
                ImportKind::Nested(items) => {
                    for (_, alias) in items {
                        self.current_mut()
                            .names
                            .insert(alias.name.clone(), NameKind::Global { is_writable: false });
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
            AssignLeft::Ident(ident) => self.store_name(&ident.name),
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
        if self.current().names.contains_key(name) {
            return;
        }

        if self.current().kind != FunctionKind::Closure {
            self.current_mut()
                .names
                .insert(name.to_owned(), NameKind::Global { is_writable: false });
            return;
        }

        self.find_upvalue(name, NameKind::Global { is_writable: false });
    }

    fn store_name(&mut self, name: &str) {
        match self.current().names.get(name) {
            Some(NameKind::Local)
            | Some(NameKind::Global { is_writable: true })
            | Some(NameKind::Upvalue { .. }) => return,
            Some(NameKind::Global { is_writable: false }) | None => (),
        }

        if self.current().kind != FunctionKind::Closure {
            self.current_mut()
                .names
                .insert(name.to_owned(), NameKind::Local);
            return;
        }

        self.find_upvalue(name, NameKind::Local);
    }

    fn find_upvalue(&mut self, name: &str, default: NameKind) {
        let mut base_func_count = 0;
        let mut base_func = self.current_mut();
        loop {
            if let Some(func) = base_func.base_function {
                base_func = &mut self.func_list[func];
                base_func_count += 1;
            } else {
                self.current_mut().names.insert(name.to_owned(), default);
                break;
            }

            match base_func.names.get(name).cloned() {
                Some(NameKind::Upvalue {
                    func_count,
                    upvalue_id,
                }) => {
                    let func_count = func_count + base_func_count;
                    self.current_mut().names.insert(
                        name.to_owned(),
                        NameKind::Upvalue {
                            func_count,
                            upvalue_id,
                        },
                    );
                    break;
                }
                Some(NameKind::Local) => {
                    base_func.names.insert(
                        name.to_owned(),
                        NameKind::Upvalue {
                            func_count: 0,
                            upvalue_id: base_func.def_upvalue_count,
                        },
                    );
                    let upvalue_id = base_func.def_upvalue_count;
                    base_func.def_upvalue_count += 1;
                    self.current_mut().names.insert(
                        name.to_owned(),
                        NameKind::Upvalue {
                            func_count: base_func_count,
                            upvalue_id,
                        },
                    );
                    break;
                }
                _ => (),
            }

            if base_func.kind != FunctionKind::Closure {
                self.current_mut().names.insert(name.to_owned(), default);
                break;
            }
        }
    }
}
