//! The semantic analyzer.
//!
//! Functions in the AST will be identified and processed.
//! The kind of symbols within the scope will be determined.

use std::cell::Cell;

use index_vec::IndexVec;
use indexmap::IndexMap;
use rustc_hash::FxBuildHasher;

use super::{
    ast::*,
    index::{FunctionId, ReferenceId, ScopeId, SymbolId},
    interning::InternedString,
    semantic::*,
};

/// Semantic Analyze.
pub fn analyze<S: AsRef<str> + Clone>(program: &Program<S>) -> Semantic<S> {
    SemanticAnalyzer::new().analyze(program)
}

#[derive(Debug)]
struct SemanticAnalyzer<S> {
    current_function_id: FunctionId,
    current_scope_id: ScopeId,

    functions: IndexVec<FunctionId, FunctionSemantic>,
    scopes: IndexVec<ScopeId, Scope<S>>,
    symbols: IndexVec<SymbolId, Symbol<S>>,
    references: IndexVec<ReferenceId, Reference>,
    globals: IndexMap<InternedString<S>, SymbolId, FxBuildHasher>,
}

impl<S: AsRef<str> + Clone> SemanticAnalyzer<S> {
    fn new() -> Self {
        Self {
            current_function_id: FunctionId::new(0),
            current_scope_id: ScopeId::new(0),
            functions: IndexVec::new(),
            scopes: IndexVec::new(),
            symbols: IndexVec::new(),
            references: IndexVec::new(),
            globals: IndexMap::with_hasher(FxBuildHasher),
        }
    }

    fn analyze(mut self, program: &Program<S>) -> Semantic<S> {
        self.analyze_program(program);
        Semantic {
            functions: self.functions,
            scopes: self.scopes,
            symbols: self.symbols,
            references: self.references,
        }
    }

    fn declare_symbol(&mut self, ident: &Ident<S>, kind: SymbolKind) -> SymbolId {
        let symbol = Symbol {
            name: ident.name.clone(),
            range: ident.range,
            scope_id: self.current_scope_id,
            kind,
            references: Vec::new(),
        };
        let symbol_id = if matches!(kind, SymbolKind::Global { .. }) {
            if let Some(symbol_id) = self.globals.get(ident.name.as_ref()).copied() {
                symbol_id
            } else {
                let symbol_id = self.symbols.push(symbol);
                self.globals.insert(ident.name.clone().into(), symbol_id);
                symbol_id
            }
        } else {
            self.symbols.push(symbol)
        };
        self.scopes[self.current_scope_id]
            .bindings
            .insert(ident.name.clone().into(), symbol_id);
        self.functions[self.current_function_id]
            .symbols
            .insert(symbol_id);
        symbol_id
    }

    fn declare_reference(
        &mut self,
        symbol_id: SymbolId,
        ident: &Ident<S>,
        kind: ReferenceKind,
    ) -> ReferenceId {
        let reference = Reference {
            symbol_id,
            range: ident.range,
            kind,
        };
        let reference_id = self.references.push(reference);
        self.symbols[symbol_id].references.push(reference_id);
        ident.reference_id.set(Some(reference_id));
        reference_id
    }

    fn declare_symbol_and_write_reference(&mut self, ident: &Ident<S>, kind: SymbolKind) {
        let symbol_id = self.declare_symbol(ident, kind);
        self.declare_reference(symbol_id, ident, ReferenceKind::Write);
    }

    fn declare_read_reference(&mut self, ident: &Ident<S>) {
        let symbol_id = self.get_or_declare_symbol(ident, SymbolKind::Global);
        self.declare_reference(symbol_id, ident, ReferenceKind::Read);
    }

    fn declare_write_reference(&mut self, ident: &Ident<S>) {
        let symbol_id = self.get_or_declare_symbol(ident, SymbolKind::Local);
        self.declare_reference(symbol_id, ident, ReferenceKind::Write);
    }

    fn get_or_declare_symbol(&mut self, ident: &Ident<S>, kind: SymbolKind) -> SymbolId {
        let mut scope_id = self.current_scope_id;
        loop {
            let scope = &self.scopes[scope_id];
            if let Some(symbol_id) = scope.bindings.get(ident.name.as_ref()).copied() {
                let symbol = &mut self.symbols[symbol_id];
                match symbol.kind {
                    SymbolKind::Local => {
                        if scope.function_id != self.current_function_id {
                            symbol.kind = SymbolKind::Upvalue;
                            self.declare_upvalue(symbol_id, scope_id);
                        }
                        return symbol_id;
                    }
                    SymbolKind::Upvalue => {
                        self.declare_upvalue(symbol_id, scope_id);
                        return symbol_id;
                    }
                    SymbolKind::Global => (),
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
        self.declare_symbol(ident, kind)
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

    fn analyze_program(&mut self, program: &Program<S>) {
        let function_id = self
            .functions
            .push(FunctionSemantic::new(FunctionKind::Function, None));
        let scope = Scope::new(ScopeKind::Function, None, function_id);
        let scope_id = self.scopes.push(scope);
        program.function.function_id.set(Some(function_id));
        program.function.body.scope_id.set(Some(scope_id));
        self.analyze_stmts(&program.function.body);
    }

    fn analyze_function(&mut self, function: &Function<S>) {
        self.enter_function(function.kind, &function.function_id);
        let scope_kind = match function.kind {
            FunctionKind::Function => ScopeKind::Function,
            FunctionKind::Closure => ScopeKind::Closure,
            FunctionKind::Do => ScopeKind::Function,
        };
        self.enter_scope(scope_kind, &function.body.scope_id);

        for param in &function.params {
            self.declare_symbol_and_write_reference(&param.ident, SymbolKind::Local);
        }
        if let Some(variadic) = &function.variadic {
            self.declare_symbol_and_write_reference(&variadic.ident, SymbolKind::Local);
        }
        self.analyze_stmts(&function.body);

        self.leave_scope();
        self.leave_function();
    }

    fn analyze_stmts(&mut self, block: &Block<S>) {
        for stmt in &block.body {
            self.analyze_stmt(stmt);
        }
    }

    fn analyze_block(&mut self, block: &Block<S>) {
        self.enter_scope(ScopeKind::Block, &block.scope_id);
        self.analyze_stmts(block);
        self.leave_scope();
    }

    fn analyze_stmt(&mut self, stmt: &Stmt<S>) {
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
            StmtKind::Match { expr, cases } => {
                self.analyze_expr(expr);
                for case in cases {
                    self.enter_scope(ScopeKind::Block, &case.body.scope_id);
                    for pattern in &case.patterns.patterns {
                        self.analyze_pattern(pattern);
                    }
                    self.analyze_stmts(&case.body);
                    self.leave_scope();
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
                    self.declare_symbol_and_write_reference(ident, SymbolKind::Local);
                }
                self.analyze_stmts(body);
                self.leave_scope();
            }
            StmtKind::Break => (),
            StmtKind::Continue => (),
            StmtKind::Return { argument } => self.analyze_expr(argument),
            StmtKind::Throw { argument } => self.analyze_expr(argument),
            StmtKind::Import {
                path,
                path_str: _,
                kind,
            } => match kind {
                ImportKind::Simple(alias) => {
                    let ident = alias.as_ref().map_or(path.last().unwrap(), |v| v);
                    self.declare_symbol_and_write_reference(ident, SymbolKind::Global);
                }
                ImportKind::Nested(items) => {
                    for (name, alias) in items {
                        let ident = alias.as_ref().unwrap_or(name);
                        self.declare_symbol_and_write_reference(ident, SymbolKind::Global);
                    }
                }
                ImportKind::Glob => (),
            },
            StmtKind::Fn {
                glo,
                name,
                function,
            } => {
                self.analyze_function(function);
                if *glo {
                    self.declare_symbol_and_write_reference(name, SymbolKind::Global);
                } else {
                    self.declare_write_reference(name);
                }
            }
            StmtKind::GloAssign { left, right } => {
                self.analyze_expr(right);
                self.declare_symbol_and_write_reference(&left.ident, SymbolKind::Global);
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

    fn analyze_assign_left(&mut self, left: &AssignLeft<S>) {
        match left {
            AssignLeft::Ident(ident) => self.declare_write_reference(&ident.ident),
            AssignLeft::Member { table, property } => {
                self.analyze_expr(table);
                self.analyze_member_property(property);
            }
            AssignLeft::MetaMember { table } => self.analyze_expr(table),
        }
    }

    fn analyze_expr(&mut self, expr: &Expr<S>) {
        match &expr.kind {
            ExprKind::Lit(_) => (),
            ExprKind::Ident(ident) => self.declare_read_reference(ident),
            ExprKind::Paren(expr) => self.analyze_expr(expr),
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
            ExprKind::TypeCheck { left, right: _ } => {
                self.analyze_expr(left);
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

    fn analyze_member_property(&mut self, property: &MemberKind<S>) {
        match property {
            MemberKind::Bracket(expr) => self.analyze_expr(expr),
            MemberKind::Dot(_) | MemberKind::DoubleColon(_) => (),
        }
    }

    fn analyze_pattern(&mut self, pattern: &Pattern<S>) {
        match &pattern.kind {
            PatternKind::Lit(_) => (),
            PatternKind::Ident(ident) => {
                self.declare_symbol_and_write_reference(ident, SymbolKind::Local);
            }
            PatternKind::Table { pairs, others: _ } => {
                for (_, v) in pairs {
                    self.analyze_pattern(v);
                }
            }
            PatternKind::List { items, others: _ } => {
                for item in items {
                    self.analyze_pattern(item);
                }
            }
        }
    }
}
