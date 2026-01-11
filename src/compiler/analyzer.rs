//! The semantic analyzer.
//!
//! Functions in the AST will be identified and processed.
//! The kind of symbols within the scope will be determined.

use std::{hash::Hash, sync::OnceLock};

use super::{
    ast::*,
    error::CompilerError,
    index::{FunctionId, ReferenceId, ScopeId, SymbolId},
    semantic::*,
};

/// Semantic Analyze.
pub fn analyze<S: Clone + Eq + Hash>(program: &Program<S>) -> (Semantic<S>, Vec<CompilerError>) {
    SemanticAnalyzer::new().analyze(program)
}

#[derive(Debug)]
struct SemanticAnalyzer<S> {
    current_function_id: FunctionId,
    current_scope_id: ScopeId,
    semantic: Semantic<S>,
}

impl<S: Clone + Eq + Hash> SemanticAnalyzer<S> {
    fn new() -> Self {
        Self {
            current_function_id: FunctionId::new(0),
            current_scope_id: ScopeId::new(0),
            semantic: Semantic::new(),
        }
    }

    fn analyze(mut self, program: &Program<S>) -> (Semantic<S>, Vec<CompilerError>) {
        self.visit_program(program);
        // SemanticAnalyzer does not produce errors now, but can be extended to do in the future.
        (self.semantic, Vec::new())
    }

    fn declare_symbol(&mut self, ident: &Ident<S>, kind: SymbolKind) -> SymbolId {
        let symbol = Symbol {
            name: ident.name.clone(),
            range: ident.range,
            scope_id: self.current_scope_id,
            kind,
            references: Vec::new(),
        };
        let symbol_id = if matches!(kind, SymbolKind::Global) {
            if let Some(symbol_id) = self.semantic.globals.get(&ident.name).copied() {
                symbol_id
            } else {
                let symbol_id = self.semantic.symbols.push(symbol);
                self.semantic.globals.insert(ident.name.clone(), symbol_id);
                symbol_id
            }
        } else {
            self.semantic.symbols.push(symbol)
        };
        self.semantic.scopes[self.current_scope_id]
            .bindings
            .insert(ident.name.clone(), symbol_id);
        self.semantic.functions[self.current_function_id]
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
        let reference_id = self.semantic.references.push(reference);
        self.semantic.symbols[symbol_id]
            .references
            .push(reference_id);
        ident.reference_id.set(reference_id).unwrap();
        reference_id
    }

    fn declare_symbol_and_write_reference(&mut self, ident: &Ident<S>, kind: SymbolKind) {
        let symbol_id = self.declare_symbol(ident, kind);
        self.declare_reference(symbol_id, ident, ReferenceKind::Write);
    }

    fn declare_read_reference(&mut self, ident: &Ident<S>) {
        let symbol_id = {
            let mut scope_id = self.current_scope_id;
            loop {
                let scope = &self.semantic.scopes[scope_id];
                if let Some(symbol_id) = scope.bindings.get(&ident.name).copied() {
                    let symbol = &mut self.semantic.symbols[symbol_id];
                    match symbol.kind {
                        SymbolKind::Local => {
                            if scope.function_id != self.current_function_id {
                                self.declare_upvalue(symbol_id, scope_id);
                            }
                            break Some(symbol_id);
                        }
                        SymbolKind::Global => (),
                    }
                }
                if scope.kind == ScopeKind::Function {
                    break None;
                }
                if let Some(parent_id) = scope.parent_id {
                    scope_id = parent_id;
                } else {
                    break None;
                }
            }
            .unwrap_or_else(|| self.declare_symbol(ident, SymbolKind::Global))
        };
        self.declare_reference(symbol_id, ident, ReferenceKind::Read);
    }

    fn declare_write_reference(&mut self, ident: &Ident<S>) {
        let symbol_id = {
            let mut scope_id = self.current_scope_id;
            loop {
                let scope = &self.semantic.scopes[scope_id];
                if scope.function_id != self.current_function_id {
                    break None;
                }
                if let Some(symbol_id) = scope.bindings.get(&ident.name).copied() {
                    let symbol = &mut self.semantic.symbols[symbol_id];
                    match symbol.kind {
                        SymbolKind::Local => break Some(symbol_id),
                        SymbolKind::Global => (),
                    }
                }
                if scope.kind == ScopeKind::Function {
                    break None;
                }
                if let Some(parent_id) = scope.parent_id {
                    scope_id = parent_id;
                } else {
                    break None;
                }
            }
            .unwrap_or_else(|| self.declare_symbol(ident, SymbolKind::Local))
        };
        self.declare_reference(symbol_id, ident, ReferenceKind::Write);
    }

    fn declare_upvalue(&mut self, symbol_id: SymbolId, def_scope_id: ScopeId) {
        let mut scope_id = self.current_scope_id;
        while scope_id != def_scope_id {
            let function_id = self.semantic.scopes[scope_id].function_id;
            self.semantic.functions[function_id]
                .symbols
                .insert(symbol_id);
            scope_id = self.semantic.scopes[scope_id].parent_id.unwrap();
        }
    }

    fn enter_function(&mut self, kind: FunctionKind, function_id: &OnceLock<FunctionId>) {
        let parent_function_id = self.current_function_id;

        let function = FunctionSemantic::new(kind, Some(parent_function_id));
        self.current_function_id = self.semantic.functions.push(function);

        function_id.set(self.current_function_id).unwrap();
    }

    fn leave_function(&mut self) {
        if let Some(parent_id) = self.semantic.functions[self.current_function_id].parent_id {
            self.current_function_id = parent_id;
        }
    }

    fn enter_scope(&mut self, kind: ScopeKind, scope_id: &OnceLock<ScopeId>) {
        let parent_scope_id = self.current_scope_id;

        let scope = Scope::new(kind, Some(parent_scope_id), self.current_function_id);
        self.current_scope_id = self.semantic.scopes.push(scope);

        scope_id.set(self.current_scope_id).unwrap();
    }

    fn leave_scope(&mut self) {
        if let Some(parent_id) = self.semantic.scopes[self.current_scope_id].parent_id {
            self.current_scope_id = parent_id;
        }
    }

    fn visit_exprs(&mut self, exprs: &[Expr<S>]) {
        for expr in exprs {
            self.visit_expr(expr);
        }
    }
}

impl<S: Clone + Eq + Hash> Visit<S> for SemanticAnalyzer<S> {
    type Return = ();

    fn visit_program(&mut self, program: &Program<S>) -> Self::Return {
        let function_id = self
            .semantic
            .functions
            .push(FunctionSemantic::new(FunctionKind::Function, None));
        let scope = Scope::new(ScopeKind::Function, None, function_id);
        let scope_id = self.semantic.scopes.push(scope);
        program.function.function_id.set(function_id).unwrap();
        program.function.body.scope_id.set(scope_id).unwrap();
        self.visit_exprs(&program.function.body.body);
    }

    fn visit_function(&mut self, function: &Function<S>) -> Self::Return {
        self.enter_function(function.kind, &function.function_id);
        let scope_kind = match function.kind {
            FunctionKind::Closure => ScopeKind::Closure,
            FunctionKind::Function | FunctionKind::Do => ScopeKind::Function,
        };
        self.enter_scope(scope_kind, &function.body.scope_id);

        self.visit_params(&function.params);
        self.visit_exprs(&function.body.body);

        self.leave_scope();
        self.leave_function();
    }

    fn visit_block(&mut self, block: &Block<S>) -> Self::Return {
        self.enter_scope(ScopeKind::Block, &block.scope_id);
        self.visit_exprs(&block.body);
        self.leave_scope();
    }

    #[expect(clippy::match_same_arms)]
    fn visit_expr(&mut self, expr: &Expr<S>) -> Self::Return {
        match &expr.kind {
            ExprKind::Lit(_lit) => (),
            ExprKind::Ident(ident) => self.declare_read_reference(ident),
            ExprKind::Paren(expr) => self.visit_expr(expr),
            ExprKind::Block(block) => self.visit_block(block),
            ExprKind::Fn {
                glo,
                name,
                function,
            } => {
                if let Some(name) = name {
                    if glo.is_some() {
                        self.declare_symbol_and_write_reference(name, SymbolKind::Global);
                    } else {
                        self.declare_write_reference(name);
                    }
                }
                self.visit_function(function);
            }
            ExprKind::Effect {
                glo,
                name,
                effect: _,
            } => {
                if glo.is_some() {
                    self.declare_symbol_and_write_reference(name, SymbolKind::Global);
                } else {
                    self.declare_write_reference(name);
                }
            }
            ExprKind::Table { properties } => {
                for property in properties {
                    self.visit_expr(&property.key);
                    self.visit_expr(&property.value);
                }
            }
            ExprKind::List { items } => {
                for item in items {
                    self.visit_expr(item);
                }
            }
            ExprKind::Unary {
                operator: _,
                argument,
            } => self.visit_expr(argument),
            ExprKind::Binary {
                operator: _,
                left,
                right,
            } => {
                self.visit_expr(left);
                self.visit_expr(right);
            }
            ExprKind::TypeCheck { left, right: _ } => self.visit_expr(left),
            ExprKind::Member {
                table,
                property,
                safe: _,
            } => {
                self.visit_expr(table);
                self.visit_member_property(property);
            }
            ExprKind::Call {
                callee,
                arguments,
                trailing_lambda,
                handlers,
            } => {
                for handler in handlers {
                    self.declare_read_reference(&handler.effect);
                }
                self.visit_expr(callee);
                for argument in arguments {
                    self.visit_expr(argument);
                }
                if let Some(trailing_lambda) = trailing_lambda {
                    self.visit_function(trailing_lambda);
                }
                for handler in handlers {
                    self.enter_scope(ScopeKind::Block, &handler.body.scope_id);
                    self.visit_params(&handler.params);
                    self.visit_exprs(&handler.body.body);
                    self.leave_scope();
                }
            }
            ExprKind::If {
                test,
                consequent,
                alternate,
            } => {
                self.visit_expr(test);
                self.visit_block(consequent);
                if let Some(alternate) = alternate {
                    self.visit_expr(alternate);
                }
            }
            ExprKind::Match { expr, cases } => {
                self.visit_expr(expr);
                for case in cases {
                    self.enter_scope(ScopeKind::Block, &case.scope_id);
                    for pattern in &case.patterns {
                        self.visit_pattern(pattern);
                    }
                    self.visit_expr(&case.body);
                    self.leave_scope();
                }
            }
            ExprKind::Loop { body } => self.visit_block(body),
            ExprKind::While { test, body } => {
                self.visit_expr(test);
                self.visit_block(body);
            }
            ExprKind::For { left, right, body } => {
                self.visit_expr(right);
                self.enter_scope(ScopeKind::Block, &body.scope_id);
                for ident in left {
                    self.declare_symbol_and_write_reference(ident, SymbolKind::Local);
                }
                self.visit_exprs(&body.body);
                self.leave_scope();
            }
            ExprKind::Break | ExprKind::Continue => (),
            ExprKind::Return { argument } => self.visit_expr(argument),
            ExprKind::Import {
                path,
                path_str: _,
                kind,
            } => match kind {
                ImportKind::Simple(alias) => {
                    let ident = alias.as_ref().map_or(path.last().unwrap(), |v| v);
                    self.declare_symbol_and_write_reference(ident, SymbolKind::Global);
                }
                ImportKind::Nested(items) => {
                    for item in items {
                        let ident = item.alias.as_ref().unwrap_or(&item.name);
                        self.declare_symbol_and_write_reference(ident, SymbolKind::Global);
                    }
                }
                ImportKind::Glob => (),
            },
            ExprKind::GloAssign { left, right } => {
                self.declare_symbol_and_write_reference(&left.ident, SymbolKind::Global);
                self.visit_expr(right);
            }
            ExprKind::Assign { left, right } => {
                self.visit_expr(right);
                self.visit_assign_left(left);
            }
            ExprKind::AssignOp {
                operator: _,
                left,
                right,
            } => {
                self.visit_expr(right);
                self.visit_assign_left(left);
            }
            ExprKind::AssignUnpack { left, right } => {
                self.visit_expr(right);
                for assign_left in left {
                    self.visit_assign_left(assign_left);
                }
            }
            ExprKind::AssignMulti { left, right } => {
                for right_expr in right {
                    self.visit_expr(right_expr);
                }
                for assign_left in left {
                    self.visit_assign_left(assign_left);
                }
            }
        }
    }

    fn visit_assign_left(&mut self, assign_left: &AssignLeft<S>) -> Self::Return {
        match &assign_left.kind {
            AssignLeftKind::Ident(ident) => self.declare_write_reference(&ident.ident),
            AssignLeftKind::Member { ident, property } => {
                self.declare_write_reference(ident);
                self.visit_member_property(property);
            }
        }
    }

    fn visit_params(&mut self, params: &Params<S>) -> Self::Return {
        for param in &params.params {
            self.declare_symbol_and_write_reference(&param.ident, SymbolKind::Local);
        }
        if let Some(variadic) = &params.variadic {
            self.declare_symbol_and_write_reference(&variadic.ident, SymbolKind::Local);
        }
    }

    fn visit_pattern(&mut self, pattern: &Pattern<S>) -> Self::Return {
        match &pattern.kind {
            PatternKind::Lit(_) => (),
            PatternKind::Ident(ident) => {
                self.declare_symbol_and_write_reference(ident, SymbolKind::Local);
            }
            PatternKind::Table { pairs, others: _ } => {
                for pair in pairs {
                    self.visit_pattern(&pair.value);
                }
            }
            PatternKind::List { items, others: _ } => {
                for item in items {
                    self.visit_pattern(item);
                }
            }
        }
    }

    fn visit_member_property(&mut self, property: &MemberKind<S>) -> Self::Return {
        match property {
            MemberKind::Bracket(expr) => self.visit_expr(expr),
            MemberKind::Dot(_)
            | MemberKind::DoubleColon(_)
            | MemberKind::BracketMeta
            | MemberKind::DotMeta
            | MemberKind::DoubleColonMeta => (),
        }
    }

    fn visit_lit(&mut self, _lit: &Lit<S>) -> Self::Return {
        unreachable!()
    }

    fn visit_ident(&mut self, _ident: &Ident<S>) -> Self::Return {
        unreachable!()
    }

    fn visit_typed_ident(&mut self, _ident: &TypedIdent<S>) -> Self::Return {
        unreachable!()
    }

    fn visit_ty(&mut self, _ty: &Ty<S>) -> Self::Return {
        unreachable!()
    }
}
