//! The Code Generator.

use std::{hash::Hash, iter, rc::Rc};

use ordermap::{OrderMap, OrderSet};
use oxc_index::IndexVec;
use rustc_hash::FxBuildHasher;

use super::{
    ast::*,
    code::{CodeParams, ConstValue, UpvalueCapture, UserEffect},
    error::CompilerError,
    index::{FunctionId, SymbolId},
    ir::{ControlFlowGraph, FunctionIR},
    opcode::{JumpTarget, OpCode},
    semantic::{FunctionSemantic, Semantic, SymbolKind},
    value::{BuiltinEffect, ValueType},
};

/// Generate code.
pub fn gen_code<S: Clone + Eq + Hash>(
    program: &Program<S>,
    semantic: &Semantic<S>,
) -> (IndexVec<FunctionId, FunctionIR<S>>, Vec<CompilerError>) {
    CodeGenerator::new(semantic).gen_code(program)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum CodeMarker {
    OpCode(OpCode),
    JumpTarget(JumpTarget),
}

impl From<OpCode> for CodeMarker {
    fn from(value: OpCode) -> Self {
        CodeMarker::OpCode(value)
    }
}

impl From<JumpTarget> for CodeMarker {
    fn from(value: JumpTarget) -> Self {
        CodeMarker::JumpTarget(value)
    }
}

impl From<BinOp> for OpCode {
    fn from(value: BinOp) -> Self {
        match value {
            BinOp::Add => OpCode::Add,
            BinOp::Sub => OpCode::Sub,
            BinOp::Mul => OpCode::Mul,
            BinOp::Div => OpCode::Div,
            BinOp::Rem => OpCode::Rem,
            BinOp::Eq => OpCode::Eq,
            BinOp::Ne => OpCode::Ne,
            BinOp::Lt => OpCode::Lt,
            BinOp::Le => OpCode::Le,
            BinOp::Gt => OpCode::Gt,
            BinOp::Ge => OpCode::Ge,
            BinOp::Identical => OpCode::Identical,
            BinOp::NotIdentical => OpCode::NotIdentical,
            BinOp::And | BinOp::Or | BinOp::Is => unreachable!(),
        }
    }
}

impl From<UnOp> for OpCode {
    fn from(value: UnOp) -> Self {
        match value {
            UnOp::Not => OpCode::Not,
            UnOp::Neg => OpCode::Neg,
        }
    }
}

impl<S> MemberKind<S> {
    fn get_opcode(&self) -> OpCode {
        match self {
            MemberKind::Bracket(_) => OpCode::GetItem,
            MemberKind::Dot(_) | MemberKind::DoubleColon(_) => OpCode::GetAttr,
            MemberKind::BracketMeta | MemberKind::DotMeta | MemberKind::DoubleColonMeta => {
                OpCode::GetMeta
            }
        }
    }

    fn set_opcode(&self) -> OpCode {
        match self {
            MemberKind::Bracket(_) => OpCode::SetItem,
            MemberKind::Dot(_) | MemberKind::DoubleColon(_) => OpCode::SetAttr,
            MemberKind::BracketMeta | MemberKind::DotMeta | MemberKind::DoubleColonMeta => {
                OpCode::SetMeta
            }
        }
    }
}

impl<S> From<usize> for ConstValue<S> {
    fn from(value: usize) -> Self {
        ConstValue::Int(value.try_into().unwrap_or(i64::MAX)) // TODO: handle overflow
    }
}

impl<S> From<LitKind<S>> for ConstValue<S> {
    fn from(value: LitKind<S>) -> Self {
        match value {
            LitKind::Null => ConstValue::Null,
            LitKind::Bool(v) => ConstValue::Bool(v),
            LitKind::Int(v) => ConstValue::Int(v),
            LitKind::Float(v) => ConstValue::Float(v),
            LitKind::Str(v) => ConstValue::Str(v),
            LitKind::Bytes(v) => ConstValue::Bytes(v),
        }
    }
}

impl<S> From<Params<S>> for CodeParams<S> {
    fn from(value: Params<S>) -> Self {
        CodeParams {
            params: value.params.into_iter().map(|x| x.ident.name).collect(),
            variadic: value.variadic.map(|x| x.ident.name),
        }
    }
}

#[derive(Debug, Clone, Default)]
struct Context {
    code: Vec<CodeMarker>,
    jump_target_count: usize,
    continue_stack: Vec<JumpTarget>,
    break_stack: Vec<JumpTarget>,
}

struct CodeGenerator<'a, S> {
    semantic: &'a Semantic<S>,
    current_function_id: FunctionId,
    ir: IndexVec<FunctionId, FunctionIR<S>>,
    contexts: IndexVec<FunctionId, Context>,
    errors: Vec<CompilerError>,
}

impl<'a, S: Clone + Eq + Hash> CodeGenerator<'a, S> {
    fn new(semantic: &'a Semantic<S>) -> Self {
        Self {
            semantic,
            current_function_id: FunctionId::new(0),
            ir: IndexVec::with_capacity(semantic.functions.len()),
            contexts: IndexVec::with_capacity(semantic.functions.len()),
            errors: Vec::new(),
        }
    }

    fn function_semantic(&self) -> &FunctionSemantic {
        &self.semantic.functions[self.current_function_id]
    }

    fn local_names(&mut self) -> &OrderMap<SymbolId, S, FxBuildHasher> {
        &self.ir[self.current_function_id].local_names
    }

    fn global_names(&mut self) -> &OrderMap<SymbolId, S, FxBuildHasher> {
        &self.ir[self.current_function_id].global_names
    }

    fn upvalue_names(&mut self) -> &OrderMap<SymbolId, (S, UpvalueCapture), FxBuildHasher> {
        &self.ir[self.current_function_id].upvalue_names
    }

    fn continue_stack(&mut self) -> &mut Vec<JumpTarget> {
        &mut self.contexts[self.current_function_id].continue_stack
    }

    fn break_stack(&mut self) -> &mut Vec<JumpTarget> {
        &mut self.contexts[self.current_function_id].break_stack
    }

    fn code(&mut self) -> &mut Vec<CodeMarker> {
        &mut self.contexts[self.current_function_id].code
    }

    fn push_code<T: Into<CodeMarker>>(&mut self, code: T) {
        let code = code.into();
        if code == CodeMarker::OpCode(OpCode::Pop)
            && matches!(self.code().last().copied(), Some(CodeMarker::OpCode(opcode)) if opcode.is_load())
        {
            self.code().pop();
        } else {
            self.code().push(code);
        }
    }

    fn push_load_const<T: Into<ConstValue<S>>>(&mut self, value: T) {
        let const_id = self.add_const(value.into());
        self.push_code(OpCode::LoadConst(const_id));
    }

    fn get_jump_target(&mut self) -> JumpTarget {
        let jump_target = self.contexts[self.current_function_id].jump_target_count;
        self.contexts[self.current_function_id].jump_target_count += 1;
        JumpTarget(jump_target)
    }

    fn add_const(&mut self, value: ConstValue<S>) -> usize {
        let consts = &mut self.ir[self.current_function_id].consts;
        if let Some(index) = consts.get_index_of(&value) {
            index
        } else {
            consts.insert(value);
            consts.len() - 1
        }
    }

    fn load(&mut self, ident: &Ident<S>) {
        let symbol_id =
            self.semantic.references[ident.reference_id.get().copied().unwrap()].symbol_id;
        let opcode = match self.semantic.symbols[symbol_id].kind {
            SymbolKind::Local => {
                let index = self.local_names().get_index_of(&symbol_id).unwrap();
                OpCode::LoadLocal(index)
            }
            SymbolKind::Upvalue => {
                if let Some(index) = self.local_names().get_index_of(&symbol_id) {
                    OpCode::LoadLocal(index)
                } else if let Some(index) = self.upvalue_names().get_index_of(&symbol_id) {
                    OpCode::LoadUpvalue(index)
                } else {
                    panic!("upvalue symbol not found in local or upvalue names");
                }
            }
            SymbolKind::Global => {
                let index = self.global_names().get_index_of(&symbol_id).unwrap();
                OpCode::LoadGlobal(index)
            }
        };
        self.push_code(opcode);
    }

    fn store(&mut self, ident: &Ident<S>) {
        let symbol_id =
            self.semantic.references[ident.reference_id.get().copied().unwrap()].symbol_id;
        let opcode = match self.semantic.symbols[symbol_id].kind {
            SymbolKind::Local | SymbolKind::Upvalue => {
                // Upvalue is stored as local variable. Because upvalue can't be assigned in closure
                // which captures it. It can only be assigned in the closure where it is declared.
                // In that closure, it is just a normal local variable.
                let index = self.local_names().get_index_of(&symbol_id).unwrap();
                OpCode::StoreLocal(index)
            }
            SymbolKind::Global => {
                let index = self.global_names().get_index_of(&symbol_id).unwrap();
                OpCode::StoreGlobal(index)
            }
        };
        self.push_code(opcode);
    }

    fn gen_code(
        mut self,
        program: &Program<S>,
    ) -> (IndexVec<FunctionId, FunctionIR<S>>, Vec<CompilerError>) {
        self.gen_function_code(&program.function);
        (self.ir, self.errors)
    }

    fn gen_function_code(&mut self, function: &Function<S>) -> FunctionId {
        let function_id = function.function_id.get().copied().unwrap();
        self.current_function_id = function_id;

        let mut local_names = OrderMap::with_hasher(FxBuildHasher);
        let mut global_names = OrderMap::with_hasher(FxBuildHasher);
        let mut upvalue_names = OrderMap::with_hasher(FxBuildHasher);
        for symbol_id in self.semantic.functions[self.current_function_id]
            .symbols
            .iter()
            .copied()
        {
            let symbol = &self.semantic.symbols[symbol_id];
            match symbol.kind {
                SymbolKind::Local => {
                    local_names.insert(symbol_id, symbol.name.clone());
                }
                SymbolKind::Upvalue => {
                    let capture = if let Some(parent_id) = self.function_semantic().parent_id {
                        let base_closure_local_id = self.ir[parent_id]
                            .local_names
                            .get_index_of(&symbol_id)
                            .map(UpvalueCapture::Local);
                        let base_closure_upvalue_id = self.ir[parent_id]
                            .upvalue_names
                            .get_index_of(&symbol_id)
                            .map(UpvalueCapture::Upvalue);
                        base_closure_local_id.or(base_closure_upvalue_id)
                    } else {
                        None
                    };
                    if let Some(capture) = capture {
                        upvalue_names.insert(symbol_id, (symbol.name.clone(), capture));
                    } else {
                        // If the current closure has no parent, or the symbol can not be found in
                        // the parent closure, it means the upvalue symbol is declared in current
                        // closure. So we just create a local variable for it.
                        // For child closure which use the upvalue symbol, it can capture it from
                        // the local variable.
                        local_names.insert(symbol_id, symbol.name.clone());
                    }
                }
                SymbolKind::Global => {
                    global_names.insert(symbol_id, symbol.name.clone());
                }
            }
        }
        self.ir.push(FunctionIR {
            name: function.name.clone(),
            params: (*function.params.clone()).into(),
            kind: function.kind,
            cfg: ControlFlowGraph::default(),
            consts: {
                let mut consts = OrderSet::with_hasher(FxBuildHasher);
                consts.insert(ConstValue::Null);
                consts
            },
            local_names,
            global_names,
            upvalue_names,
        });
        self.contexts.push(Context::default());

        self.visit_params(&function.params).unwrap();
        self.visit_block(&function.body).unwrap();
        if function.kind == FunctionKind::Do {
            self.push_code(OpCode::Pop);
            self.push_code(OpCode::LoadLocals);
            self.push_code(OpCode::Return);
        }
        if self.code().last().copied() != Some(OpCode::Return.into()) {
            self.push_code(OpCode::Return);
        }

        let jump_target_count = self.contexts[self.current_function_id].jump_target_count;
        self.ir[self.current_function_id].cfg = ControlFlowGraph::from_bytecode(
            self.code(),
            jump_target_count,
            function.params.params.len() + usize::from(function.params.variadic.is_some()),
        );

        if let Some(parent_id) = self.function_semantic().parent_id {
            self.current_function_id = parent_id;
        }

        function_id
    }

    fn visit_pattern_depth(
        &mut self,
        pattern: &Pattern<S>,
        pattern_depth: usize,
        clean_stack_label_stack: &mut Vec<JumpTarget>,
    ) -> Result<(), CompilerError> {
        let mut get_or_push_pattern_clean_stack_label = |index: usize| {
            if let Some(clean_stack_label) = clean_stack_label_stack.get(index).copied() {
                clean_stack_label
            } else {
                let clean_stack_label = self.get_jump_target();
                clean_stack_label_stack.push(clean_stack_label);
                clean_stack_label
            }
        };
        match &pattern.kind {
            PatternKind::Lit(lit) => {
                let next_pattern_label = get_or_push_pattern_clean_stack_label(pattern_depth);
                self.push_load_const(lit.kind.clone());
                self.push_code(OpCode::Eq);
                self.push_code(OpCode::PopJumpIfFalse(next_pattern_label));
            }
            PatternKind::Ident(ident) => {
                let next_pattern_label = get_or_push_pattern_clean_stack_label(pattern_depth);
                self.push_code(OpCode::JumpPopIfNull(next_pattern_label));
                self.store(ident);
            }
            PatternKind::Table { pairs, others } => {
                let clean_stack_label = get_or_push_pattern_clean_stack_label(pattern_depth + 1);
                self.push_code(OpCode::Copy(1));
                self.push_code(OpCode::TypeCheck(ValueType::Table));
                self.push_code(OpCode::PopJumpIfFalse(clean_stack_label));
                if others.is_none() {
                    self.push_code(OpCode::Copy(1));
                    self.push_code(OpCode::GetLen);
                    self.push_load_const(pairs.len());
                    self.push_code(OpCode::Eq);
                    self.push_code(OpCode::PopJumpIfFalse(clean_stack_label));
                }
                for pair in pairs {
                    self.push_code(OpCode::Copy(1));
                    self.push_load_const(pair.key.kind.clone());
                    self.push_code(OpCode::GetItem);
                    self.visit_pattern_depth(
                        &pair.value,
                        pattern_depth + 1,
                        clean_stack_label_stack,
                    )?;
                }
                self.push_code(OpCode::Pop);
            }
            PatternKind::List { items, others } => {
                let clean_stack_label = get_or_push_pattern_clean_stack_label(pattern_depth + 1);
                self.push_code(OpCode::Copy(1));
                self.push_code(OpCode::TypeCheck(ValueType::Table));
                self.push_code(OpCode::PopJumpIfFalse(clean_stack_label));
                if others.is_none() {
                    self.push_code(OpCode::Copy(1));
                    self.push_code(OpCode::GetLen);
                    self.push_load_const(items.len());
                    self.push_code(OpCode::Eq);
                    self.push_code(OpCode::PopJumpIfFalse(clean_stack_label));
                }
                for (i, item) in items.iter().enumerate() {
                    self.push_code(OpCode::Copy(1));
                    self.push_load_const(i);
                    self.push_code(OpCode::GetItem);
                    self.visit_pattern_depth(item, pattern_depth + 1, clean_stack_label_stack)?;
                }
                self.push_code(OpCode::Pop);
            }
        }
        Ok(())
    }
}

impl<S: Clone + Eq + Hash> Visit<S> for CodeGenerator<'_, S> {
    type Return = Result<(), CompilerError>;

    fn visit_program(&mut self, _program: &Program<S>) -> Self::Return {
        unreachable!()
    }

    fn visit_function(&mut self, function: &Function<S>) -> Self::Return {
        let function_id = self.gen_function_code(function);
        self.push_load_const(ConstValue::Function(function_id.into()));
        if function.kind == FunctionKind::Do {
            self.push_code(OpCode::Call(0));
        }
        Ok(())
    }

    fn visit_block(&mut self, block: &Block<S>) -> Self::Return {
        if block.body.is_empty() {
            self.push_load_const(ConstValue::Null);
        } else {
            for (i, expr) in block.body.iter().enumerate() {
                self.visit_expr(expr)
                    .unwrap_or_else(|err| self.errors.push(err));
                if i != block.body.len() - 1 {
                    self.push_code(OpCode::Pop);
                }
            }
        }
        Ok(())
    }

    fn visit_expr(&mut self, expr: &Expr<S>) -> Self::Return {
        match &expr.kind {
            ExprKind::Lit(lit) => self.visit_lit(lit)?,
            ExprKind::Ident(ident) => self.visit_ident(ident)?,
            ExprKind::Paren(expr) => self.visit_expr(expr)?,
            ExprKind::Block(block) => self.visit_block(block)?,
            ExprKind::Fn {
                glo: _,
                name,
                function,
            } => {
                self.visit_function(function)?;
                if let Some(name) = name {
                    self.store(name);
                    self.push_load_const(ConstValue::Null);
                }
            }
            ExprKind::Effect {
                glo: _,
                name,
                effect,
            } => {
                self.push_load_const(Rc::new(UserEffect {
                    name: effect.name.clone(),
                    params: (*effect.params.clone()).into(),
                }));
                self.store(name);
                self.push_load_const(ConstValue::Null);
            }
            ExprKind::Table { properties } => {
                for TableProperty { key, value, .. } in properties {
                    self.visit_expr(key)?;
                    self.visit_expr(value)?;
                }
                self.push_code(OpCode::BuildTable(properties.len()));
            }
            ExprKind::List { items } => {
                for item in items {
                    self.visit_expr(item)?;
                }
                self.push_code(OpCode::BuildList(items.len()));
            }
            ExprKind::Unary { operator, argument } => {
                self.visit_expr(argument)?;
                self.push_code(OpCode::from(*operator));
            }
            #[expect(clippy::wildcard_enum_match_arm)]
            ExprKind::Binary {
                operator,
                left,
                right,
            } => match operator {
                BinOp::And => {
                    let label = self.get_jump_target();
                    self.visit_expr(left)?;
                    self.push_code(OpCode::JumpIfFalseOrPop(label));
                    self.visit_expr(right)?;
                    self.push_code(label);
                }
                BinOp::Or => {
                    let label = self.get_jump_target();
                    self.visit_expr(left)?;
                    self.push_code(OpCode::JumpIfTrueOrPop(label));
                    self.visit_expr(right)?;
                    self.push_code(label);
                }
                _ => {
                    self.visit_expr(left)?;
                    self.visit_expr(right)?;
                    self.push_code(OpCode::from(*operator));
                }
            },
            ExprKind::TypeCheck { left, right } => {
                self.visit_expr(left)?;
                self.push_code(OpCode::TypeCheck(*right));
            }
            ExprKind::Member {
                table,
                property,
                safe,
            } => {
                self.visit_expr(table)?;
                let safe_label = self.get_jump_target();
                if safe.is_some() {
                    self.push_code(OpCode::JumpPopIfNull(safe_label));
                }
                self.visit_member_property(property)?;
                self.push_code(property.get_opcode());
                self.push_code(safe_label);
            }
            ExprKind::Call {
                callee,
                arguments,
                trailing_lambda,
                handlers,
            } => {
                let mut argument_count = arguments.len();
                #[expect(clippy::wildcard_enum_match_arm)]
                match &callee.kind {
                    ExprKind::Member {
                        table,
                        property,
                        safe,
                    } => {
                        self.visit_expr(table)?;
                        let safe_label = self.get_jump_target();
                        if safe.is_some() {
                            self.push_code(OpCode::JumpPopIfNull(safe_label));
                        }
                        match property {
                            MemberKind::Dot(_) | MemberKind::DotMeta => {
                                self.push_code(OpCode::Copy(1));
                                self.visit_member_property(property)?;
                                self.push_code(property.get_opcode());
                                self.push_code(OpCode::Swap(2));
                                argument_count += 1;
                            }
                            MemberKind::Bracket(_)
                            | MemberKind::DoubleColon(_)
                            | MemberKind::BracketMeta
                            | MemberKind::DoubleColonMeta => {
                                self.visit_member_property(property)?;
                                self.push_code(property.get_opcode());
                            }
                        }
                        self.push_code(safe_label);
                    }
                    _ => self.visit_expr(callee)?,
                }
                for arg in arguments {
                    self.visit_expr(arg)?;
                }
                if let Some(trailing_lambda) = trailing_lambda {
                    self.visit_function(trailing_lambda)?;
                    argument_count += 1;
                }

                if handlers.is_empty() {
                    self.push_code(OpCode::Call(argument_count));
                } else {
                    let end_label = self.get_jump_target();
                    let mut handler_labels = Vec::new();
                    for handler in handlers {
                        let handler_label = self.get_jump_target();
                        self.load(&handler.effect);
                        self.push_code(OpCode::RegisterHandler(handler_label));
                        handler_labels.push(handler_label);
                    }

                    self.push_code(OpCode::Call(argument_count));
                    self.push_code(OpCode::Jump(end_label));

                    for (handler, handler_label) in handlers.iter().zip(handler_labels) {
                        self.push_code(handler_label);
                        self.push_load_const(Rc::new(UserEffect {
                            name: handler.effect.name.clone(),
                            params: (*handler.params.clone()).into(),
                        }));
                        self.push_code(OpCode::CheckEffect(
                            handler.params.params.len()
                                + usize::from(handler.params.variadic.is_some()),
                        ));
                        self.visit_params(&handler.params)?;
                        self.visit_block(&handler.body)?;
                        self.push_code(OpCode::Jump(end_label));
                    }
                    self.push_code(end_label);
                }
            }
            ExprKind::If {
                test,
                consequent,
                alternate,
            } => {
                let false_label = self.get_jump_target();
                let end_label = self.get_jump_target();
                self.visit_expr(test)?;
                self.push_code(OpCode::PopJumpIfFalse(false_label));
                self.visit_block(consequent)?;
                self.push_code(OpCode::Jump(end_label));
                self.push_code(false_label);
                if let Some(alternate) = alternate {
                    self.visit_expr(alternate)?;
                } else {
                    self.push_load_const(ConstValue::Null);
                }
                self.push_code(end_label);
            }
            ExprKind::Match { expr, cases } => {
                let end_label = self.get_jump_target();
                self.visit_expr(expr)?;
                let block_labels = iter::repeat_with(|| self.get_jump_target())
                    .take(cases.len())
                    .collect::<Vec<_>>();
                for (case, block_label) in cases.iter().zip(block_labels.iter().copied()) {
                    for pattern in &case.patterns {
                        let mut clean_stack_label_stack = vec![self.get_jump_target()];

                        self.push_code(OpCode::Copy(1));
                        self.visit_pattern_depth(pattern, 0, &mut clean_stack_label_stack)?;
                        self.push_code(OpCode::Pop);
                        self.push_code(OpCode::Jump(block_label));

                        for (clean_stack_index, clean_stack_label) in
                            clean_stack_label_stack.into_iter().rev().enumerate()
                        {
                            if clean_stack_index != 0 {
                                self.push_code(OpCode::Pop);
                            }
                            self.push_code(clean_stack_label);
                        }
                    }
                }
                self.push_load_const(ConstValue::Null);
                self.push_code(OpCode::Jump(end_label));
                for (case, block_label) in cases.iter().zip(block_labels.iter().copied()) {
                    self.push_code(block_label);
                    self.visit_expr(&case.body)?;
                    self.push_code(OpCode::Jump(end_label));
                }
                self.push_code(end_label);
                self.push_code(OpCode::Swap(2));
                self.push_code(OpCode::Pop);
            }
            ExprKind::Loop { body } => {
                let continue_label = self.get_jump_target();
                let break_label = self.get_jump_target();
                self.continue_stack().push(continue_label);
                self.break_stack().push(break_label);

                self.push_code(continue_label);
                self.visit_block(body)?;
                self.push_code(OpCode::Pop);
                self.push_code(OpCode::JumpBackEdge(continue_label));
                self.push_code(break_label);
                self.push_load_const(ConstValue::Null);

                self.continue_stack().pop();
                self.break_stack().pop();
            }
            ExprKind::While { test, body } => {
                let continue_label = self.get_jump_target();
                let break_label = self.get_jump_target();
                self.continue_stack().push(continue_label);
                self.break_stack().push(break_label);

                self.push_code(continue_label);
                self.visit_expr(test)?;
                self.push_code(OpCode::PopJumpIfFalse(break_label));
                self.visit_block(body)?;
                self.push_code(OpCode::Pop);
                self.push_code(OpCode::JumpBackEdge(continue_label));
                self.push_code(break_label);
                self.push_load_const(ConstValue::Null);

                self.continue_stack().pop();
                self.break_stack().pop();
            }
            ExprKind::For { left, right, body } => {
                let continue_label = self.get_jump_target();
                let break_label = self.get_jump_target();
                let body_label = self.get_jump_target();
                self.continue_stack().push(continue_label);
                self.break_stack().push(break_label);

                self.visit_expr(right)?;
                self.push_code(OpCode::Iter);
                self.push_code(continue_label);
                self.push_load_const(ConstValue::Null);
                self.push_load_const(BuiltinEffect::Yield);
                self.push_code(OpCode::RegisterHandler(body_label));
                self.push_code(OpCode::Call(1));
                self.push_code(OpCode::Jump(break_label));
                self.push_code(body_label);
                self.push_code(OpCode::MarkAddStackSize(3));
                self.push_code(OpCode::Pop);
                if left.len() == 1 {
                    self.store(&left[0]);
                } else {
                    for (i, l) in left.iter().enumerate() {
                        self.push_code(OpCode::Copy(1));
                        self.push_load_const(i);
                        self.push_code(OpCode::GetItem);
                        self.store(l);
                    }
                    self.push_code(OpCode::Pop);
                }
                self.visit_block(body)?;
                self.push_code(OpCode::Pop);
                self.push_code(OpCode::JumpBackEdge(continue_label));
                self.push_code(break_label);
                self.push_code(OpCode::Pop);
                self.push_load_const(ConstValue::Null);

                self.continue_stack().pop();
                self.break_stack().pop();
            }
            ExprKind::Break => {
                let opcode = match self.break_stack().last().copied() {
                    Some(v) => OpCode::Break(v),
                    None => return Err(CompilerError::BreakOutsideLoop { range: expr.range }),
                };
                self.push_code(opcode);
                self.push_load_const(ConstValue::Null);
            }
            ExprKind::Continue => {
                let opcode = match self.continue_stack().last().copied() {
                    Some(v) => OpCode::Continue(v),
                    None => return Err(CompilerError::ContinueOutsideLoop { range: expr.range }),
                };
                self.push_code(opcode);
                self.push_load_const(ConstValue::Null);
            }
            ExprKind::Return { argument } => {
                if self.function_semantic().kind == FunctionKind::Do {
                    return Err(CompilerError::ReturnOutsideFunction { range: expr.range });
                }

                // This is actually useless, the noop jump is help to keep the return expression
                // in a separate basic block, so the CFG construction can insert stack cleanup code
                // at starting of the return basic block. The jump opcode will be removed when
                // converting CFG to bytecode.
                let return_label = self.get_jump_target();
                self.push_code(OpCode::Jump(return_label));
                self.push_code(return_label);

                if let ExprKind::Call { handlers, .. } = &argument.kind
                    && handlers.is_empty()
                {
                    self.visit_expr(argument)?;
                    if let Some(CodeMarker::OpCode(OpCode::Call(i))) = self.code().last().copied() {
                        *self.code().last_mut().unwrap() = OpCode::ReturnCall(i).into();
                    } else {
                        self.push_code(OpCode::Return);
                    }
                } else {
                    self.visit_expr(argument)?;
                    self.push_code(OpCode::Return);
                }
                self.push_load_const(ConstValue::Null);
            }
            ExprKind::Import {
                path,
                path_str,
                kind,
            } => {
                let path_const_id = self.add_const(ConstValue::Str(path_str.clone()));
                self.push_code(OpCode::Import(path_const_id));
                match kind {
                    ImportKind::Simple(alias) => {
                        self.store(alias.as_ref().map_or(path.last().unwrap(), |v| v));
                    }
                    ImportKind::Nested(items) => {
                        for item in items {
                            let const_id = self.add_const(ConstValue::Str(item.name.name.clone()));
                            self.push_code(OpCode::ImportFrom(const_id));
                            self.store(item.alias.as_ref().unwrap_or(&item.name));
                        }
                        self.push_code(OpCode::Pop);
                    }
                    ImportKind::Glob => {
                        self.push_code(OpCode::ImportGlob);
                    }
                }
                self.push_load_const(ConstValue::Null);
            }
            ExprKind::GloAssign { left, right } => {
                self.visit_expr(right)?;
                self.store(&left.ident);
                self.push_load_const(ConstValue::Null);
            }
            ExprKind::Assign { left, right } => {
                self.visit_expr(right)?;
                self.visit_assign_left(left)?;
                self.push_load_const(ConstValue::Null);
            }
            ExprKind::AssignOp {
                operator,
                left,
                right,
            } => {
                match &left.kind {
                    AssignLeftKind::Ident(ident) => {
                        self.load(&ident.ident);
                        self.visit_expr(right)?;
                        self.push_code(OpCode::from(*operator));
                        self.store(&ident.ident);
                    }
                    AssignLeftKind::Member { ident, property } => match property {
                        MemberKind::Bracket(_)
                        | MemberKind::Dot(_)
                        | MemberKind::DoubleColon(_) => {
                            self.visit_ident(ident)?;
                            self.visit_member_property(property)?;
                            self.push_code(OpCode::Copy(2));
                            self.push_code(OpCode::Copy(2));
                            self.push_code(property.get_opcode());
                            self.visit_expr(right)?;
                            self.push_code(OpCode::from(*operator));
                            self.push_code(OpCode::Swap(3));
                            self.push_code(OpCode::Swap(2));
                            self.push_code(property.set_opcode());
                            self.store(ident);
                        }
                        MemberKind::BracketMeta
                        | MemberKind::DotMeta
                        | MemberKind::DoubleColonMeta => {
                            self.visit_ident(ident)?;
                            self.push_code(OpCode::Copy(1));
                            self.push_code(property.get_opcode());
                            self.visit_expr(right)?;
                            self.push_code(OpCode::from(*operator));
                            self.push_code(OpCode::Swap(2));
                            self.push_code(property.set_opcode());
                            self.store(ident);
                        }
                    },
                }
                self.push_load_const(ConstValue::Null);
            }
            ExprKind::AssignUnpack { left, right } => {
                self.visit_expr(right)?;
                for _ in 1..left.len() {
                    self.push_code(OpCode::Copy(1));
                }
                for (i, l) in left.iter().enumerate() {
                    self.push_load_const(i);
                    self.push_code(OpCode::GetItem);
                    self.visit_assign_left(l)?;
                }
                self.push_load_const(ConstValue::Null);
            }
            ExprKind::AssignMulti { left, right } => {
                assert!(left.len() == right.len());
                for right_expr in right {
                    self.visit_expr(right_expr)?;
                }
                for assign_left in left.iter().rev() {
                    self.visit_assign_left(assign_left)?;
                }
                self.push_load_const(ConstValue::Null);
            }
        }
        Ok(())
    }

    fn visit_assign_left(&mut self, assign_left: &AssignLeft<S>) -> Self::Return {
        match &assign_left.kind {
            AssignLeftKind::Ident(ident) => self.store(&ident.ident),
            AssignLeftKind::Member { ident, property } => {
                self.visit_ident(ident)?;
                self.visit_member_property(property)?;
                self.push_code(property.set_opcode());
                self.store(ident);
            }
        }
        Ok(())
    }

    fn visit_params(&mut self, params: &Params<S>) -> Self::Return {
        if let Some(variadic) = &params.variadic {
            self.store(&variadic.ident);
        }
        for param in params.params.iter().rev() {
            self.store(&param.ident);
        }
        Ok(())
    }

    /// See [CodeGenerator::visit_pattern_depth].
    fn visit_pattern(&mut self, _pattern: &Pattern<S>) -> Self::Return {
        unreachable!()
    }

    fn visit_member_property(&mut self, property: &MemberKind<S>) -> Self::Return {
        match property {
            MemberKind::Bracket(property) => self.visit_expr(property)?,
            MemberKind::Dot(ident) | MemberKind::DoubleColon(ident) => {
                self.push_load_const(ConstValue::Str(ident.name.clone()));
            }
            MemberKind::BracketMeta | MemberKind::DotMeta | MemberKind::DoubleColonMeta => (),
        }
        Ok(())
    }

    fn visit_lit(&mut self, lit: &Lit<S>) -> Self::Return {
        self.push_load_const(lit.kind.clone());
        Ok(())
    }

    fn visit_ident(&mut self, ident: &Ident<S>) -> Self::Return {
        self.load(ident);
        Ok(())
    }

    fn visit_typed_ident(&mut self, _ident: &TypedIdent<S>) -> Self::Return {
        unreachable!()
    }

    fn visit_ty(&mut self, _ty: &Ty<S>) -> Self::Return {
        unreachable!()
    }
}
