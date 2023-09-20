//! The semantic analyzer.
//!
//! Lowers the AST to `Vec<Function>` and add semantic information.
//!
//! Functions in the AST will be identified and processed.
//! The kind of names within the namespace will be determined.

use std::{fmt, hash::Hash, mem};

use indexmap::IndexSet;

use super::{
    ast::*,
    code::ConstValue,
    opcode::{JumpTarget, OpCode},
};

/// Global name information.
#[derive(Debug, Clone)]
pub struct GlobalNameInfo {
    pub name: String,
    pub is_writable: bool,
}

impl Hash for GlobalNameInfo {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl PartialEq for GlobalNameInfo {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for GlobalNameInfo {}

impl From<String> for GlobalNameInfo {
    fn from(value: String) -> Self {
        GlobalNameInfo {
            name: value,
            is_writable: false,
        }
    }
}

/// Upvalue name information.
#[derive(Debug, Clone)]
pub struct UpvalueNameInfo {
    pub name: String,
    pub func_count: usize,
    pub upvalue_id: usize,
}

impl Hash for UpvalueNameInfo {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl PartialEq for UpvalueNameInfo {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for UpvalueNameInfo {}

impl From<String> for UpvalueNameInfo {
    fn from(value: String) -> Self {
        UpvalueNameInfo {
            name: value,
            func_count: 0,
            upvalue_id: 0,
        }
    }
}

impl From<UpvalueNameInfo> for (String, usize, usize) {
    fn from(value: UpvalueNameInfo) -> Self {
        (value.name, value.func_count, value.upvalue_id)
    }
}

/// A Function.
#[derive(Debug, Clone)]
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

    /// Local names.
    pub local_names: IndexSet<String>,
    /// Global names.
    pub global_names: IndexSet<GlobalNameInfo>,
    /// Upvalue names.
    pub upvalue_names: IndexSet<UpvalueNameInfo>,

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

        writeln!(f, "local_names: {:?}", self.local_names)?;
        writeln!(f, "global_names: {:?}", self.global_names)?;
        writeln!(f, "upvalue_names: {:?}", self.upvalue_names)?;
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
            local_names: IndexSet::new(),
            global_names: IndexSet::new(),
            upvalue_names: IndexSet::new(),
            def_upvalue_count: 0,
            code: Vec::new(),
            consts: vec![ConstValue::Null],
            jump_target_count: 0,
            continue_stack: Vec::new(),
            break_stack: Vec::new(),
        }
    }
}

/// Semantic Analyze. Lowers the AST to `Vec<Function>`.
pub fn analyze(ast: Box<Block>) -> Vec<Function> {
    let mut func = Function::new(0, FunctionKind::Function, Vec::new(), None, ast, None);
    let mut analyzer = SemanticAnalyzer::new(&mut func);
    analyzer.func_list.insert(0, func);
    analyzer.analyze_name();
    analyzer.func_list
}

#[derive(Debug, Clone)]
struct SemanticAnalyzer {
    func_id: usize,
    pub func_list: Vec<Function>,
}

impl SemanticAnalyzer {
    fn new(func: &mut Function) -> Self {
        let mut analyzer = SemanticAnalyzer {
            func_id: func.func_id,
            func_list: Vec::new(),
        };
        analyzer.build_block(&mut func.body);
        analyzer
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
                self.build_expr(left);
                self.build_expr(right);
            }
            StmtKind::AssignOp {
                operator: _,
                left,
                right,
            } => {
                self.build_expr(left);
                self.build_expr(right);
            }
            StmtKind::AssignUnpack { left, right } => {
                for left in left {
                    self.build_expr(left);
                }
                self.build_expr(right);
            }
            StmtKind::AssignMulti { left, right } => {
                for left in left {
                    self.build_expr(left);
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
                let func_id = self.func_id + self.func_list.len() + 1;
                if let ExprKind::Function {
                    kind,
                    params,
                    variadic,
                    body,
                } = mem::replace(&mut ast_node.kind, ExprKind::FunctionId(func_id))
                {
                    let mut func =
                        Function::new(func_id, kind, params, variadic, body, Some(self.func_id));
                    let mut analyzer = SemanticAnalyzer::new(&mut func);
                    self.func_list.push(func);
                    self.func_list.append(&mut analyzer.func_list);
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
                kind: _,
                safe: _,
            } => {
                self.build_expr(table);
                self.build_expr(property);
            }
            ExprKind::MetaMember { table, safe: _ } => self.build_expr(table),
            ExprKind::Call {
                callee,
                arguments,
                propagating_error: _,
            } => {
                self.build_expr(callee);
                for arg in arguments {
                    self.build_expr(arg);
                }
            }
        }
    }

    pub fn analyze_name(&mut self) {
        for i in 0..self.func_list.len() {
            for param in self.func_list[i].params.clone() {
                self.func_list[i].local_names.insert(param);
            }
            if let Some(variadic) = self.func_list[i].variadic.clone() {
                self.func_list[i].local_names.insert(variadic);
            }
            let mut t = self.func_list[i].body.clone();
            self.analyze_name_block(i, &mut t);
        }
    }

    fn analyze_name_block(&mut self, func_id: usize, ast_node: &mut Block) {
        for stmt in &mut ast_node.body {
            self.analyze_name_stmt(func_id, stmt);
        }
    }

    fn analyze_name_stmt(&mut self, func_id: usize, ast_node: &mut Stmt) {
        match &mut ast_node.kind {
            StmtKind::If {
                test,
                consequent,
                alternate,
            } => {
                self.analyze_name_expr(func_id, test);
                self.analyze_name_block(func_id, consequent);
                if let Some(alternate) = alternate {
                    self.analyze_name_stmt(func_id, alternate);
                }
            }
            StmtKind::Loop { body } => self.analyze_name_block(func_id, body),
            StmtKind::While { test, body } => {
                self.analyze_name_expr(func_id, test);
                self.analyze_name_block(func_id, body);
            }
            StmtKind::For { left, right, body } => {
                for left in left {
                    self.store_name(func_id, &left.name);
                }
                self.analyze_name_expr(func_id, right);
                self.analyze_name_block(func_id, body);
            }
            StmtKind::Break => (),
            StmtKind::Continue => (),
            StmtKind::Return { argument } => self.analyze_name_expr(func_id, argument),
            StmtKind::Throw { argument } => self.analyze_name_expr(func_id, argument),
            StmtKind::Global { arguments } => {
                for arg in arguments {
                    self.func_list[func_id]
                        .global_names
                        .replace(GlobalNameInfo {
                            name: arg.name.clone(),
                            is_writable: true,
                        });
                }
            }
            StmtKind::Import { path: _, kind } => match kind {
                ImportKind::Simple(alias) => {
                    self.func_list[func_id]
                        .global_names
                        .insert(GlobalNameInfo::from(alias.name.clone()));
                }
                ImportKind::Nested(items) => {
                    for (_, alias) in items {
                        self.func_list[func_id]
                            .global_names
                            .insert(GlobalNameInfo::from(alias.name.clone()));
                    }
                }
                ImportKind::Glob => (),
            },
            StmtKind::Assign { left, right } => {
                if let ExprKind::Ident(ident) = &left.kind {
                    self.store_name(func_id, &ident.name);
                } else {
                    self.analyze_name_expr(func_id, left);
                }
                self.analyze_name_expr(func_id, right);
            }
            StmtKind::AssignOp {
                operator: _,
                left,
                right,
            } => {
                if let ExprKind::Ident(ident) = &left.kind {
                    self.store_name(func_id, &ident.name);
                } else {
                    self.analyze_name_expr(func_id, left);
                }
                self.analyze_name_expr(func_id, left);
                self.analyze_name_expr(func_id, right);
            }
            StmtKind::AssignUnpack { left, right } => {
                for left in left {
                    if let ExprKind::Ident(ident) = &left.kind {
                        self.store_name(func_id, &ident.name);
                    } else {
                        self.analyze_name_expr(func_id, left);
                    }
                }
                self.analyze_name_expr(func_id, right);
            }
            StmtKind::AssignMulti { left, right } => {
                for left in left {
                    if let ExprKind::Ident(ident) = &left.kind {
                        self.store_name(func_id, &ident.name);
                    } else {
                        self.analyze_name_expr(func_id, left);
                    }
                }
                for right in right {
                    self.analyze_name_expr(func_id, right);
                }
            }
            StmtKind::Block(block) => self.analyze_name_block(func_id, block),
            StmtKind::Expr(expr) => self.analyze_name_expr(func_id, expr),
        }
    }

    fn analyze_name_expr(&mut self, func_id: usize, ast_node: &mut Expr) {
        match &mut ast_node.kind {
            ExprKind::Lit(_) => (),
            ExprKind::Ident(ident) => self.load_name(func_id, &ident.name),
            ExprKind::Function { .. } => {
                let func_id = self.func_id + self.func_list.len() + 1;
                if let ExprKind::Function {
                    kind,
                    params,
                    variadic,
                    body,
                } = mem::replace(&mut ast_node.kind, ExprKind::FunctionId(func_id))
                {
                    let mut func =
                        Function::new(func_id, kind, params, variadic, body, Some(self.func_id));
                    let mut analyzer = SemanticAnalyzer::new(&mut func);
                    self.func_list.push(func);
                    self.func_list.append(&mut analyzer.func_list);
                }
            }
            ExprKind::FunctionId(_) => (),
            ExprKind::Table { properties } => {
                for TableProperty { key, value, .. } in properties {
                    self.analyze_name_expr(func_id, key);
                    self.analyze_name_expr(func_id, value);
                }
            }
            ExprKind::List { items } => {
                for item in items {
                    self.analyze_name_expr(func_id, item);
                }
            }
            ExprKind::Unary {
                operator: _,
                argument,
            } => self.analyze_name_expr(func_id, argument),
            ExprKind::Binary {
                operator: _,
                left,
                right,
            } => {
                self.analyze_name_expr(func_id, left);
                self.analyze_name_expr(func_id, right);
            }
            ExprKind::Member {
                table,
                property,
                kind: _,
                safe: _,
            } => {
                self.analyze_name_expr(func_id, table);
                self.analyze_name_expr(func_id, property);
            }
            ExprKind::MetaMember { table, safe: _ } => self.analyze_name_expr(func_id, table),
            ExprKind::Call {
                callee,
                arguments,
                propagating_error: _,
            } => {
                self.analyze_name_expr(func_id, callee);
                for arg in arguments {
                    self.analyze_name_expr(func_id, arg);
                }
            }
        }
    }

    fn load_name(&mut self, func_id: usize, name: &str) {
        if self.func_list[func_id].local_names.contains(name)
            || self.func_list[func_id]
                .global_names
                .contains(&GlobalNameInfo::from(name.to_owned()))
            || self.func_list[func_id]
                .upvalue_names
                .contains(&UpvalueNameInfo::from(name.to_owned()))
        {
            return;
        }
        if self.func_list[func_id].kind != FunctionKind::Closure {
            self.func_list[func_id]
                .global_names
                .insert(GlobalNameInfo::from(name.to_owned()));
        } else {
            let mut base_func_count = 0;
            let mut base_func = &mut self.func_list[func_id];
            loop {
                if let Some(func) = base_func.base_function {
                    base_func = &mut self.func_list[func];
                    base_func_count += 1;
                } else {
                    self.func_list[func_id]
                        .global_names
                        .insert(GlobalNameInfo::from(name.to_owned()));
                    break;
                }
                if let Some(upvalue_name) = base_func
                    .upvalue_names
                    .get(&UpvalueNameInfo::from(name.to_owned()))
                {
                    let func_count = upvalue_name.func_count + base_func_count;
                    let upvalue_id = upvalue_name.upvalue_id;
                    self.func_list[func_id]
                        .upvalue_names
                        .insert(UpvalueNameInfo {
                            name: name.to_owned(),
                            func_count,
                            upvalue_id,
                        });
                    break;
                }
                if base_func.local_names.contains(name) {
                    base_func.local_names.remove(name);
                    base_func.upvalue_names.insert(UpvalueNameInfo {
                        name: name.to_owned(),
                        func_count: 0,
                        upvalue_id: base_func.def_upvalue_count,
                    });
                    let upvalue_id = base_func.def_upvalue_count;
                    base_func.def_upvalue_count += 1;
                    self.func_list[func_id]
                        .upvalue_names
                        .insert(UpvalueNameInfo {
                            name: name.to_owned(),
                            func_count: base_func_count,
                            upvalue_id,
                        });
                    break;
                }
                if base_func.kind != FunctionKind::Closure {
                    self.func_list[func_id]
                        .global_names
                        .insert(GlobalNameInfo::from(name.to_owned()));
                    break;
                }
            }
        }
    }

    fn store_name(&mut self, func_id: usize, name: &str) {
        if self.func_list[func_id].local_names.contains(name)
            || self.func_list[func_id]
                .upvalue_names
                .contains(&UpvalueNameInfo::from(name.to_owned()))
        {
            return;
        } else if let Some(name_info) = self.func_list[func_id]
            .global_names
            .get(&GlobalNameInfo::from(name.to_owned()))
        {
            if name_info.is_writable {
                return;
            }
        }
        if self.func_list[func_id].kind != FunctionKind::Closure {
            self.func_list[func_id].local_names.insert(name.to_owned());
        } else {
            let mut base_func_count = 0;
            let mut base_func = &mut self.func_list[func_id];
            loop {
                if let Some(func) = base_func.base_function {
                    base_func = &mut self.func_list[func];
                    base_func_count += 1;
                } else {
                    self.func_list[func_id].local_names.insert(name.to_owned());
                    break;
                }
                if let Some(upvalue_name) = base_func
                    .upvalue_names
                    .get(&UpvalueNameInfo::from(name.to_owned()))
                {
                    let func_count = upvalue_name.func_count + base_func_count;
                    let upvalue_id = upvalue_name.upvalue_id;
                    self.func_list[func_id]
                        .upvalue_names
                        .insert(UpvalueNameInfo {
                            name: name.to_owned(),
                            func_count,
                            upvalue_id,
                        });
                    break;
                }
                if base_func.local_names.contains(name) {
                    base_func.local_names.remove(name);
                    base_func.upvalue_names.insert(UpvalueNameInfo {
                        name: name.to_owned(),
                        func_count: 0,
                        upvalue_id: base_func.def_upvalue_count,
                    });
                    let upvalue_id = base_func.def_upvalue_count;
                    base_func.def_upvalue_count += 1;
                    self.func_list[func_id]
                        .upvalue_names
                        .insert(UpvalueNameInfo {
                            name: name.to_owned(),
                            func_count: base_func_count,
                            upvalue_id,
                        });
                    break;
                }
                if base_func.kind != FunctionKind::Closure {
                    self.func_list[func_id].local_names.insert(name.to_owned());
                    break;
                }
            }
        }
    }
}
