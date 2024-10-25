//! The semantic analyzer.
//!
//! Functions in the AST will be identified and processed.
//! The kind of symbols within the scope will be determined.

use std::cell::Cell;

use index_vec::IndexVec;
use indexmap::{IndexMap, IndexSet};
use rustc_hash::FxBuildHasher;

use super::{
    ast::*,
    index::{FunctionId, ScopeId, SymbolId},
    interning::InternedString,
};

/// The semantic information of a function.
#[derive(Debug, Clone)]
pub struct FunctionSemantic {
    /// The kind of function.
    pub kind: FunctionKind,
    /// The parent function it belongs in.
    pub parent_id: Option<FunctionId>,
    /// Symbols in a function.
    pub symbols: IndexSet<SymbolId, FxBuildHasher>,
}

impl FunctionSemantic {
    pub fn new(kind: FunctionKind, parent_id: Option<FunctionId>) -> Self {
        Self {
            kind,
            parent_id,
            symbols: IndexSet::with_hasher(FxBuildHasher),
        }
    }
}

/// The semantic information of a scope.
#[derive(Debug, Clone)]
pub struct Scope<S> {
    /// The kind of scope.
    pub kind: ScopeKind,
    /// The parent scope it belongs in.
    pub parent_id: Option<ScopeId>,
    /// The function it belongs in.
    pub function_id: FunctionId,
    /// Symbol bindings in a scope.
    ///
    /// A binding is a mapping from an identifier name to its [`SymbolId`]
    pub bindings: IndexMap<InternedString<S>, SymbolId, FxBuildHasher>,
}

impl<S> Scope<S> {
    pub fn new(kind: ScopeKind, parent_id: Option<ScopeId>, function_id: FunctionId) -> Self {
        Self {
            kind,
            parent_id,
            function_id,
            bindings: IndexMap::with_hasher(FxBuildHasher),
        }
    }
}

/// The kind of scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScopeKind {
    Function,
    Closure,
    Block,
}

/// The semantic information of a symbol.
#[derive(Debug, Clone)]
pub struct Symbol<S> {
    /// The name of the symbol.
    pub name: S,
    /// The scope it belongs in.
    pub scope_id: ScopeId,
    /// The kind of symbol.
    pub kind: SymbolKind,
}

/// The kind of symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SymbolKind {
    Local,
    Upvalue,
    Global { writable: bool },
}

/// Semantic information of a Lucia program.
///
/// [`Semantic`] contains the results of analyzing a program, including the
/// [function] table, [scope] tree and [symbol] table.
///
/// [function]: FunctionSemantic
/// [scope]: Scope
/// [symbol]: Symbol
#[derive(Debug, Clone)]
pub struct Semantic<S> {
    pub functions: IndexVec<FunctionId, FunctionSemantic>,
    pub scopes: IndexVec<ScopeId, Scope<S>>,
    pub symbols: IndexVec<SymbolId, Symbol<S>>,
}

/// Semantic Analyze.
pub fn analyze<S: AsRef<str> + Copy>(program: &Program<'_, S>) -> Semantic<S> {
    SemanticAnalyzer::new().analyze(program)
}

#[derive(Debug)]
struct SemanticAnalyzer<S> {
    current_function_id: FunctionId,
    current_scope_id: ScopeId,

    functions: IndexVec<FunctionId, FunctionSemantic>,
    scopes: IndexVec<ScopeId, Scope<S>>,
    symbols: IndexVec<SymbolId, Symbol<S>>,
    globals: IndexMap<InternedString<S>, SymbolId, FxBuildHasher>,
}

impl<S: AsRef<str> + Copy> SemanticAnalyzer<S> {
    fn new() -> Self {
        Self {
            current_function_id: FunctionId::new(0),
            current_scope_id: ScopeId::new(0),
            functions: IndexVec::new(),
            scopes: IndexVec::new(),
            symbols: IndexVec::new(),
            globals: IndexMap::with_hasher(FxBuildHasher),
        }
    }

    fn analyze(mut self, program: &Program<'_, S>) -> Semantic<S> {
        self.analyze_program(program);
        Semantic {
            functions: self.functions,
            scopes: self.scopes,
            symbols: self.symbols,
        }
    }

    fn declare_symbol(&mut self, ident: &Ident<'_, S>, kind: SymbolKind) {
        let symbol = Symbol {
            name: ident.name,
            scope_id: self.current_scope_id,
            kind,
        };
        let symbol_id = if matches!(kind, SymbolKind::Global { .. }) {
            if let Some(symbol_id) = self.globals.get(&InternedString(ident.name)).copied() {
                symbol_id
            } else {
                let symbol_id = self.symbols.push(symbol);
                self.globals.insert(ident.name.into(), symbol_id);
                symbol_id
            }
        } else {
            self.symbols.push(symbol)
        };
        self.scopes[self.current_scope_id]
            .bindings
            .insert(ident.name.into(), symbol_id);
        self.functions[self.current_function_id]
            .symbols
            .insert(symbol_id);
        ident.symbol_id.set(Some(symbol_id));
    }

    fn declare_read(&mut self, ident: &Ident<'_, S>) {
        self.declare_reference(ident, false, SymbolKind::Global { writable: false });
    }

    fn declare_write(&mut self, ident: &Ident<'_, S>) {
        self.declare_reference(ident, true, SymbolKind::Local);
    }

    fn declare_reference(
        &mut self,
        ident: &Ident<'_, S>,
        need_writable: bool,
        default: SymbolKind,
    ) {
        let mut scope_id = self.current_scope_id;
        loop {
            let scope = &self.scopes[scope_id];
            if let Some(symbol_id) = scope.bindings.get(&InternedString(ident.name)).copied() {
                let symbol = &mut self.symbols[symbol_id];
                match symbol.kind {
                    SymbolKind::Local => {
                        if scope.function_id != self.current_function_id {
                            symbol.kind = SymbolKind::Upvalue;
                        }
                        self.declare_upvalue(symbol_id, scope_id);
                        ident.symbol_id.set(Some(symbol_id));
                        return;
                    }
                    SymbolKind::Upvalue => {
                        self.declare_upvalue(symbol_id, scope_id);
                        ident.symbol_id.set(Some(symbol_id));
                        return;
                    }
                    SymbolKind::Global { writable } => {
                        if scope_id == self.current_scope_id && (!need_writable || writable) {
                            ident.symbol_id.set(Some(symbol_id));
                            return;
                        }
                    }
                }
            }
            if scope.kind == ScopeKind::Function {
                break;
            }
            if let Some(parent_id) = scope.parent_id {
                scope_id = parent_id;
            } else {
                break;
            }
        }
        self.declare_symbol(ident, default);
    }

    fn declare_upvalue(&mut self, symbol_id: SymbolId, def_scope_id: ScopeId) {
        let mut scope_id = self.current_scope_id;
        while scope_id != def_scope_id {
            let function_id = self.scopes[scope_id].function_id;
            self.functions[function_id].symbols.insert(symbol_id);
            scope_id = self.scopes[scope_id].parent_id.unwrap();
        }
    }

    fn enter_function(&mut self, kind: FunctionKind, function_id: &Cell<Option<FunctionId>>) {
        let parent_function_id = self.current_function_id;

        let function = FunctionSemantic::new(kind, Some(parent_function_id));
        self.current_function_id = self.functions.push(function);

        function_id.set(Some(self.current_function_id));
    }

    fn leave_function(&mut self) {
        if let Some(parent_id) = self.functions[self.current_function_id].parent_id {
            self.current_function_id = parent_id;
        }
    }

    fn enter_scope(&mut self, kind: ScopeKind, scope_id: &Cell<Option<ScopeId>>) {
        let parent_scope_id = self.current_scope_id;

        let scope = Scope::new(kind, Some(parent_scope_id), self.current_function_id);
        self.current_scope_id = self.scopes.push(scope);

        scope_id.set(Some(self.current_scope_id));
    }

    fn leave_scope(&mut self) {
        if let Some(parent_id) = self.scopes[self.current_scope_id].parent_id {
            self.current_scope_id = parent_id;
        }
    }

    fn analyze_program(&mut self, program: &Program<'_, S>) {
        let function_id = self
            .functions
            .push(FunctionSemantic::new(FunctionKind::Function, None));
        let scope = Scope::new(ScopeKind::Function, None, function_id);
        let scope_id = self.scopes.push(scope);
        program.function.function_id.set(Some(function_id));
        program.function.body.scope_id.set(Some(scope_id));
        self.analyze_stmts(&program.function.body);
    }

    fn analyze_function(&mut self, function: &Function<'_, S>) {
        self.enter_function(function.kind, &function.function_id);
        let scope_kind = match function.kind {
            FunctionKind::Function => ScopeKind::Function,
            FunctionKind::Closure => ScopeKind::Closure,
            FunctionKind::Do => ScopeKind::Function,
        };
        self.enter_scope(scope_kind, &function.body.scope_id);

        for param in &function.params {
            self.declare_symbol(&param.ident, SymbolKind::Local);
        }
        if let Some(variadic) = &function.variadic {
            self.declare_symbol(&variadic.ident, SymbolKind::Local);
        }
        self.analyze_stmts(&function.body);

        self.leave_scope();
        self.leave_function();
    }

    fn analyze_stmts(&mut self, block: &Block<'_, S>) {
        for stmt in &block.body {
            self.analyze_stmt(stmt);
        }
    }

    fn analyze_block(&mut self, block: &Block<'_, S>) {
        self.enter_scope(ScopeKind::Block, &block.scope_id);
        self.analyze_stmts(block);
        self.leave_scope();
    }

    fn analyze_stmt(&mut self, stmt: &Stmt<'_, S>) {
        match &stmt.kind {
            StmtKind::If {
                test,
                consequent,
                alternate,
            } => {
                self.analyze_expr(test);
                self.analyze_block(consequent);
                if let Some(alternate) = alternate {
                    self.analyze_stmt(alternate);
                }
            }
            StmtKind::Loop { body } => self.analyze_block(body),
            StmtKind::While { test, body } => {
                self.analyze_expr(test);
                self.analyze_block(body);
            }
            StmtKind::For { left, right, body } => {
                self.analyze_expr(right);
                self.enter_scope(ScopeKind::Block, &body.scope_id);
                for ident in left {
                    self.declare_symbol(ident, SymbolKind::Local);
                }
                self.analyze_stmts(body);
                self.leave_scope();
            }
            StmtKind::Break => (),
            StmtKind::Continue => (),
            StmtKind::Return { argument } => self.analyze_expr(argument),
            StmtKind::Throw { argument } => self.analyze_expr(argument),
            StmtKind::Global { arguments } => {
                for argument in arguments {
                    self.declare_symbol(&argument.ident, SymbolKind::Global { writable: true });
                }
            }
            StmtKind::Import {
                path,
                path_str: _,
                kind,
            } => match kind {
                ImportKind::Simple(alias) => {
                    let ident = alias.as_ref().map_or(path.last().unwrap(), |v| v);
                    self.declare_symbol(ident, SymbolKind::Global { writable: true });
                }
                ImportKind::Nested(items) => {
                    for (name, alias) in items {
                        let ident = alias.as_ref().unwrap_or(name);
                        self.declare_symbol(ident, SymbolKind::Global { writable: true });
                    }
                }
                ImportKind::Glob => (),
            },
            StmtKind::Fn { name, function } => {
                self.analyze_function(function);
                self.declare_write(name);
            }
            StmtKind::Assign { left, right } => {
                self.analyze_expr(right);
                self.analyze_assign_left(left);
            }
            StmtKind::AssignOp {
                operator: _,
                left,
                right,
            } => {
                self.analyze_expr(right);
                self.analyze_assign_left(left);
            }
            StmtKind::AssignUnpack { left, right } => {
                self.analyze_expr(right);
                for left in left {
                    self.analyze_assign_left(left);
                }
            }
            StmtKind::AssignMulti { left, right } => {
                for right in right {
                    self.analyze_expr(right);
                }
                for left in left {
                    self.analyze_assign_left(left);
                }
            }
            StmtKind::Block(block) => self.analyze_block(block),
            StmtKind::Expr(expr) => self.analyze_expr(expr),
        }
    }

    fn analyze_assign_left(&mut self, left: &AssignLeft<'_, S>) {
        match left {
            AssignLeft::Ident(ident) => self.declare_write(&ident.ident),
            AssignLeft::Member { table, property } => {
                self.analyze_expr(table);
                self.analyze_member_property(property);
            }
            AssignLeft::MetaMember { table } => self.analyze_expr(table),
        }
    }

    fn analyze_expr(&mut self, expr: &Expr<'_, S>) {
        match &expr.kind {
            ExprKind::Lit(_) => (),
            ExprKind::Ident(ident) => self.declare_read(ident),
            ExprKind::Function(function) => self.analyze_function(function),
            ExprKind::Table { properties } => {
                for property in properties {
                    self.analyze_expr(&property.key);
                    self.analyze_expr(&property.value);
                }
            }
            ExprKind::List { items } => {
                for item in items {
                    self.analyze_expr(item);
                }
            }
            ExprKind::Unary {
                operator: _,
                argument,
            } => self.analyze_expr(argument),
            ExprKind::Binary {
                operator: _,
                left,
                right,
            } => {
                self.analyze_expr(left);
                self.analyze_expr(right);
            }
            ExprKind::Member {
                table,
                property,
                safe: _,
            } => {
                self.analyze_expr(table);
                self.analyze_member_property(property);
            }
            ExprKind::MetaMember { table, safe: _ } => self.analyze_expr(table),
            ExprKind::Call {
                callee,
                arguments,
                kind: _,
            } => {
                self.analyze_expr(callee);
                for argument in arguments {
                    self.analyze_expr(argument);
                }
            }
        }
    }

    fn analyze_member_property(&mut self, property: &MemberKind<'_, S>) {
        match property {
            MemberKind::Bracket(expr) => self.analyze_expr(expr),
            MemberKind::Dot(_) | MemberKind::DoubleColon(_) => (),
        }
    }
}
