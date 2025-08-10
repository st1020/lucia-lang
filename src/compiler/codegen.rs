//! The Code Generator.

use std::iter;

use index_vec::{IndexVec, index_vec};
use indexmap::{IndexMap, IndexSet};
use rustc_hash::FxBuildHasher;

use super::{
    ast::*,
    code::{Code, ConstValue},
    error::CompilerError,
    index::{FunctionId, SymbolId},
    opcode::{JumpTarget, OpCode},
    semantic::{FunctionSemantic, Semantic, SymbolKind},
    value::ValueType,
};

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
        ConstValue::Int(value as i64)
    }
}

impl<S: AsRef<str>> From<LitKind<S>> for ConstValue<S> {
    fn from(value: LitKind<S>) -> Self {
        match value {
            LitKind::Null => ConstValue::Null,
            LitKind::Bool(v) => ConstValue::Bool(v),
            LitKind::Int(v) => ConstValue::Int(v),
            LitKind::Float(v) => ConstValue::Float(v),
            LitKind::Str(v) => ConstValue::Str(v),
        }
    }
}

#[derive(Debug, Clone)]
struct Context<S> {
    code: Vec<OpCode>,
    consts: IndexSet<ConstValue<S>, FxBuildHasher>,
    local_names: IndexMap<SymbolId, S, FxBuildHasher>,
    global_names: IndexMap<SymbolId, S, FxBuildHasher>,
    upvalue_names: IndexMap<SymbolId, (S, Option<usize>), FxBuildHasher>,

    jump_target_count: usize,
    need_clear_stack_count: usize,
    continue_stack: Vec<JumpTarget>,
    break_stack: Vec<JumpTarget>,
}

impl<S: AsRef<str>> Context<S> {
    fn new() -> Self {
        let mut consts = IndexSet::with_hasher(FxBuildHasher);
        consts.insert(ConstValue::Null);
        Self {
            code: Vec::new(),
            consts,
            local_names: IndexMap::with_hasher(FxBuildHasher),
            global_names: IndexMap::with_hasher(FxBuildHasher),
            upvalue_names: IndexMap::with_hasher(FxBuildHasher),
            jump_target_count: 0,
            need_clear_stack_count: 0,
            continue_stack: Vec::new(),
            break_stack: Vec::new(),
        }
    }
}

/// Generate code.
pub fn gen_code<S: AsRef<str> + Clone>(
    program: &Program<S>,
    semantic: &Semantic<S>,
) -> (Code<S>, Vec<CompilerError>) {
    CodeGenerator::new(semantic).gen_code(program)
}

struct CodeGenerator<'a, S> {
    semantic: &'a Semantic<S>,

    contexts: IndexVec<FunctionId, Context<S>>,
    current_function_id: FunctionId,
    errors: Vec<CompilerError>,
}

impl<'a, S: AsRef<str> + Clone> CodeGenerator<'a, S> {
    fn new(semantic: &'a Semantic<S>) -> Self {
        Self {
            semantic,
            contexts: index_vec![Context::new(); semantic.functions.len()],
            current_function_id: FunctionId::new(0),
            errors: Vec::new(),
        }
    }

    fn context(&mut self) -> &mut Context<S> {
        &mut self.contexts[self.current_function_id]
    }

    fn function_semantic(&self) -> &FunctionSemantic {
        &self.semantic.functions[self.current_function_id]
    }

    fn continue_stack(&mut self) -> &mut Vec<JumpTarget> {
        &mut self.context().continue_stack
    }

    fn break_stack(&mut self) -> &mut Vec<JumpTarget> {
        &mut self.context().break_stack
    }

    fn code(&mut self) -> &mut Vec<OpCode> {
        &mut self.context().code
    }

    fn push_code(&mut self, opcode: OpCode) {
        if opcode == OpCode::Pop && self.code().last().copied().is_some_and(OpCode::is_load) {
            self.code().pop();
        } else {
            self.code().push(opcode);
        }
    }

    fn push_load_const<T: Into<ConstValue<S>>>(&mut self, value: T) {
        let const_id = self.add_const(value.into());
        self.push_code(OpCode::LoadConst(const_id));
    }

    fn get_jump_target(&mut self) -> JumpTarget {
        let jump_target = self.context().jump_target_count;
        self.context().jump_target_count += 1;
        JumpTarget(jump_target)
    }

    fn add_const(&mut self, value: ConstValue<S>) -> usize {
        let consts = &mut self.context().consts;
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
                let index = self.context().local_names.get_index_of(&symbol_id).unwrap();
                OpCode::LoadLocal(index)
            }
            SymbolKind::Upvalue => {
                let index = self
                    .context()
                    .upvalue_names
                    .get_index_of(&symbol_id)
                    .unwrap();
                OpCode::LoadUpvalue(index)
            }
            SymbolKind::Global => {
                let index = self
                    .context()
                    .global_names
                    .get_index_of(&symbol_id)
                    .unwrap();
                OpCode::LoadGlobal(index)
            }
        };
        self.push_code(opcode);
    }

    fn store(&mut self, ident: &Ident<S>) {
        let symbol_id =
            self.semantic.references[ident.reference_id.get().copied().unwrap()].symbol_id;
        let opcode = match self.semantic.symbols[symbol_id].kind {
            SymbolKind::Local => {
                let index = self.context().local_names.get_index_of(&symbol_id).unwrap();
                OpCode::StoreLocal(index)
            }
            SymbolKind::Upvalue => {
                let index = self
                    .context()
                    .upvalue_names
                    .get_index_of(&symbol_id)
                    .unwrap();
                OpCode::StoreUpvalue(index)
            }
            SymbolKind::Global => {
                let index = self
                    .context()
                    .global_names
                    .get_index_of(&symbol_id)
                    .unwrap();
                OpCode::StoreGlobal(index)
            }
        };
        self.push_code(opcode);
    }

    fn gen_code(mut self, program: &Program<S>) -> (Code<S>, Vec<CompilerError>) {
        let code = self.gen_function_code(&program.function);
        (code, self.errors)
    }

    fn gen_function_code(&mut self, function: &Function<S>) -> Code<S> {
        self.current_function_id = function.function_id.get().copied().unwrap();
        for symbol_id in self.semantic.functions[self.current_function_id]
            .symbols
            .iter()
            .copied()
        {
            let symbol = &self.semantic.symbols[symbol_id];
            match symbol.kind {
                SymbolKind::Local => {
                    self.context()
                        .local_names
                        .insert(symbol_id, symbol.name.clone());
                }
                SymbolKind::Upvalue => {
                    let base_closure_upvalue_id =
                        self.function_semantic().parent_id.and_then(|parent_id| {
                            self.contexts[parent_id]
                                .upvalue_names
                                .get_index_of(&symbol_id)
                        });
                    self.context()
                        .upvalue_names
                        .insert(symbol_id, (symbol.name.clone(), base_closure_upvalue_id));
                }
                SymbolKind::Global => {
                    self.context()
                        .global_names
                        .insert(symbol_id, symbol.name.clone());
                }
            }
        }
        if let Some(variadic) = &function.variadic {
            self.store(&variadic.ident);
        }
        for param in function.params.iter().rev() {
            self.store(&param.ident);
        }
        self.visit_block(&function.body).unwrap();
        if function.kind == FunctionKind::Do {
            self.push_code(OpCode::Pop);
            self.push_code(OpCode::LoadLocals);
            self.push_code(OpCode::Return);
        } else if !self
            .context()
            .code
            .last()
            .is_some_and(|&opcode| opcode == OpCode::Return)
        {
            self.push_code(OpCode::Return);
        }

        let mut jump_target_table = vec![0; self.context().jump_target_count];
        let mut i = 0;
        while i < self.code().len() {
            match self.code()[i] {
                OpCode::JumpTarget(JumpTarget(index)) => {
                    jump_target_table[index] = i;
                    self.code().remove(i);
                }
                _ => i += 1,
            }
        }
        for opcode in self.code().iter_mut() {
            match opcode {
                OpCode::Jump(JumpTarget(v)) => *v = jump_target_table[*v],
                OpCode::JumpPopIfNull(JumpTarget(v)) => *v = jump_target_table[*v],
                OpCode::PopJumpIfFalse(JumpTarget(v)) => *v = jump_target_table[*v],
                OpCode::JumpIfTrueOrPop(JumpTarget(v)) => *v = jump_target_table[*v],
                OpCode::JumpIfFalseOrPop(JumpTarget(v)) => *v = jump_target_table[*v],
                _ => (),
            }
        }

        let stack_size = Self::get_stack_size(
            self.code(),
            function.params.len() + if function.variadic.is_some() { 1 } else { 0 },
        );
        let code = Code {
            name: function.name.clone(),
            params: function
                .params
                .iter()
                .map(|x| x.ident.name.clone())
                .collect(),
            variadic: function.variadic.as_ref().map(|x| x.ident.name.clone()),
            kind: function.kind,
            code: self.code().clone(),
            consts: self.context().consts.iter().cloned().collect(),
            local_names: self.context().local_names.values().cloned().collect(),
            global_names: self.context().global_names.values().cloned().collect(),
            upvalue_names: self.context().upvalue_names.values().cloned().collect(),
            stack_size,
        };

        if let Some(parent_id) = self.function_semantic().parent_id {
            self.current_function_id = parent_id;
        }

        code
    }

    /// Try estimate function stack size.
    fn get_stack_size(code: &[OpCode], init_size: usize) -> usize {
        let mut max_stack_depth = init_size;
        let mut stack_depths = vec![None; code.len()];
        let mut stack = vec![(0, init_size)]; // (offset, current_depth)
        while let Some((offset, current_depth)) = stack.pop() {
            if offset >= code.len() {
                continue;
            }
            if let Some(prev_depth) = stack_depths[offset]
                && current_depth <= prev_depth
            {
                continue;
            }
            stack_depths[offset] = Some(current_depth);
            max_stack_depth = max_stack_depth.max(current_depth);
            let mut new_depth = current_depth;
            match code[offset] {
                OpCode::Pop => new_depth -= 1,
                OpCode::Copy(_) => new_depth += 1,
                OpCode::Swap(_) => (),
                OpCode::LoadLocal(_)
                | OpCode::LoadGlobal(_)
                | OpCode::LoadUpvalue(_)
                | OpCode::LoadConst(_) => new_depth += 1,
                OpCode::StoreLocal(_) | OpCode::StoreGlobal(_) | OpCode::StoreUpvalue(_) => {
                    new_depth -= 1
                }
                OpCode::Import(_) => new_depth += 1,
                OpCode::ImportFrom(_) => new_depth += 1,
                OpCode::ImportGlob => new_depth -= 1,
                OpCode::BuildTable(i) => new_depth = new_depth - i * 2 + 1,
                OpCode::BuildList(i) => new_depth = new_depth - i + 1,
                OpCode::GetAttr | OpCode::GetItem => new_depth -= 1,
                OpCode::GetMeta => (),
                OpCode::SetAttr | OpCode::SetItem => new_depth -= 3,
                OpCode::SetMeta => new_depth -= 2,
                OpCode::Neg | OpCode::Not => (),
                OpCode::Add
                | OpCode::Sub
                | OpCode::Mul
                | OpCode::Div
                | OpCode::Rem
                | OpCode::Eq
                | OpCode::Ne
                | OpCode::Gt
                | OpCode::Ge
                | OpCode::Lt
                | OpCode::Le
                | OpCode::Identical
                | OpCode::NotIdentical => new_depth -= 1,
                OpCode::TypeCheck(_) => (),
                OpCode::GetLen => (),
                OpCode::Iter => (),
                OpCode::Jump(JumpTarget(i)) => {
                    stack.push((i, new_depth));
                    continue;
                }
                OpCode::JumpPopIfNull(JumpTarget(i)) => {
                    stack.push((i, new_depth - 1));
                }
                OpCode::PopJumpIfTrue(JumpTarget(i)) | OpCode::PopJumpIfFalse(JumpTarget(i)) => {
                    new_depth -= 1;
                    stack.push((i, new_depth));
                }
                OpCode::JumpIfTrueOrPop(JumpTarget(i))
                | OpCode::JumpIfFalseOrPop(JumpTarget(i)) => {
                    stack.push((i, new_depth));
                    new_depth -= 1;
                }
                OpCode::Call(i)
                | OpCode::TryCall(i)
                | OpCode::TryOptionCall(i)
                | OpCode::TryPanicCall(i) => new_depth -= i,
                OpCode::Return | OpCode::Throw => {
                    debug_assert_eq!(new_depth, 1);
                    continue;
                }
                OpCode::ReturnCall(i) => {
                    debug_assert_eq!(new_depth, i + 1);
                    continue;
                }
                OpCode::LoadLocals => new_depth += 1,
                OpCode::JumpTarget(_) => unreachable!(),
            }
            stack.push((offset + 1, new_depth));
        }
        max_stack_depth
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
                self.push_code(match lit.kind {
                    LitKind::Null | LitKind::Bool(_) | LitKind::Int(_) | LitKind::Float(_) => {
                        OpCode::Identical
                    }
                    LitKind::Str(_) => OpCode::Eq,
                });
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

impl<S: AsRef<str> + Clone> Visit<S> for CodeGenerator<'_, S> {
    type Return = Result<(), CompilerError>;

    fn visit_program(&mut self, _program: &Program<S>) -> Self::Return {
        Ok(())
    }

    fn visit_function(&mut self, function: &Function<S>) -> Self::Return {
        let code = self.gen_function_code(function);
        self.push_load_const(ConstValue::Code(Box::new(code)));
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

    fn visit_assign_left(&mut self, assign_left: &AssignLeft<S>) -> Self::Return {
        match &assign_left.kind {
            AssignLeftKind::Ident(ident) => self.store(&ident.ident),
            AssignLeftKind::Member { table, property } => {
                self.visit_expr(table)?;
                self.visit_member_property(property)?;
                self.push_code(property.set_opcode());
            }
        }
        Ok(())
    }

    /// See [CodeGenerator::visit_pattern_depth].
    fn visit_pattern(&mut self, _pattern: &Pattern<S>) -> Self::Return {
        unimplemented!()
    }

    fn visit_expr(&mut self, expr: &Expr<S>) -> Self::Return {
        match &expr.kind {
            ExprKind::Lit(lit) => self.visit_lit(lit)?,
            ExprKind::Ident(ident) => self.visit_ident(ident)?,
            ExprKind::Paren(expr) => self.visit_expr(expr)?,
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
                    self.push_code(OpCode::JumpTarget(label));
                }
                BinOp::Or => {
                    let label = self.get_jump_target();
                    self.visit_expr(left)?;
                    self.push_code(OpCode::JumpIfTrueOrPop(label));
                    self.visit_expr(right)?;
                    self.push_code(OpCode::JumpTarget(label));
                }
                operator => {
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
                self.push_code(OpCode::JumpTarget(safe_label));
            }
            ExprKind::Call {
                callee,
                arguments,
                kind,
            } => {
                let mut argument_count = arguments.len();
                let safe_label = self.get_jump_target();
                match &callee.kind {
                    ExprKind::Member {
                        table,
                        property,
                        safe,
                    } => {
                        self.visit_expr(table)?;
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
                    }
                    _ => self.visit_expr(callee)?,
                }
                for arg in arguments {
                    self.visit_expr(arg)?;
                }
                self.push_code(match kind {
                    CallKind::None => OpCode::Call(argument_count),
                    CallKind::Try => OpCode::TryCall(argument_count),
                    CallKind::TryOption => OpCode::TryOptionCall(argument_count),
                    CallKind::TryPanic => OpCode::TryPanicCall(argument_count),
                });
                self.push_code(OpCode::JumpTarget(safe_label));
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
                self.push_code(OpCode::JumpTarget(false_label));
                if let Some(alternate) = alternate {
                    self.visit_expr(alternate)?;
                } else {
                    self.push_load_const(ConstValue::Null);
                }
                self.push_code(OpCode::JumpTarget(end_label));
            }
            ExprKind::Match { expr, cases } => {
                let end_label = self.get_jump_target();
                self.visit_expr(expr)?;
                let block_labels: Vec<_> = iter::repeat_with(|| self.get_jump_target())
                    .take(cases.len())
                    .collect();
                for (i, case) in cases.iter().enumerate() {
                    let block_label = block_labels[i];
                    for pattern in &case.patterns {
                        let clean_stack_label = self.get_jump_target();
                        let mut clean_stack_label_stack = vec![clean_stack_label];

                        self.push_code(OpCode::Copy(1));
                        self.visit_pattern_depth(pattern, 0, &mut clean_stack_label_stack)?;
                        self.push_code(OpCode::Pop);
                        self.push_code(OpCode::Jump(block_label));

                        for (i, clean_stack_label) in
                            clean_stack_label_stack.into_iter().rev().enumerate()
                        {
                            if i != 0 {
                                self.push_code(OpCode::Pop);
                            }
                            self.push_code(OpCode::JumpTarget(clean_stack_label));
                        }
                    }
                }
                self.push_load_const(ConstValue::Null);
                self.push_code(OpCode::Jump(end_label));
                for (i, case) in cases.iter().enumerate() {
                    self.push_code(OpCode::JumpTarget(block_labels[i]));
                    self.visit_block(&case.body)?;
                    self.push_code(OpCode::Jump(end_label));
                }
                self.push_code(OpCode::JumpTarget(end_label));
                self.push_code(OpCode::Swap(2));
                self.push_code(OpCode::Pop);
            }
            ExprKind::Loop { body } => {
                let continue_label = self.get_jump_target();
                let break_label = self.get_jump_target();
                self.continue_stack().push(continue_label);
                self.break_stack().push(break_label);

                self.push_code(OpCode::JumpTarget(continue_label));
                self.visit_block(body)?;
                self.push_code(OpCode::Pop);
                self.push_code(OpCode::Jump(continue_label));
                self.push_code(OpCode::JumpTarget(break_label));
                self.push_load_const(ConstValue::Null);

                self.continue_stack().pop();
                self.break_stack().pop();
            }
            ExprKind::While { test, body } => {
                let continue_label = self.get_jump_target();
                let break_label = self.get_jump_target();
                self.continue_stack().push(continue_label);
                self.break_stack().push(break_label);

                self.push_code(OpCode::JumpTarget(continue_label));
                self.visit_expr(test)?;
                self.push_code(OpCode::PopJumpIfFalse(break_label));
                self.visit_block(body)?;
                self.push_code(OpCode::Pop);
                self.push_code(OpCode::Jump(continue_label));
                self.push_code(OpCode::JumpTarget(break_label));
                self.push_load_const(ConstValue::Null);

                self.continue_stack().pop();
                self.break_stack().pop();
            }
            ExprKind::For { left, right, body } => {
                let continue_label = self.get_jump_target();
                let break_label = self.get_jump_target();
                self.continue_stack().push(continue_label);
                self.break_stack().push(break_label);
                self.context().need_clear_stack_count += 1;

                self.visit_expr(right)?;
                self.push_code(OpCode::Iter);
                self.push_code(OpCode::JumpTarget(continue_label));
                self.push_code(OpCode::Copy(1));
                self.push_code(OpCode::Call(0));
                self.push_code(OpCode::JumpPopIfNull(break_label));
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
                self.push_code(OpCode::Jump(continue_label));
                self.push_code(OpCode::JumpTarget(break_label));
                self.push_code(OpCode::Pop);
                self.push_load_const(ConstValue::Null);

                self.continue_stack().pop();
                self.break_stack().pop();
                self.context().need_clear_stack_count -= 1;
            }
            ExprKind::Break => {
                let opcode = OpCode::Jump(match self.break_stack().last().copied() {
                    Some(v) => v,
                    None => return Err(CompilerError::BreakOutsideLoop { range: expr.range }),
                });
                self.push_code(opcode);
                self.push_load_const(ConstValue::Null);
            }
            ExprKind::Continue => {
                let opcode = OpCode::Jump(match self.continue_stack().last().copied() {
                    Some(v) => v,
                    None => return Err(CompilerError::ContinueOutsideLoop { range: expr.range }),
                });
                self.push_code(opcode);
                self.push_load_const(ConstValue::Null);
            }
            ExprKind::Return { argument } => {
                if self.function_semantic().kind == FunctionKind::Do {
                    return Err(CompilerError::ReturnOutsideFunction { range: expr.range });
                }
                for _ in 0..self.context().need_clear_stack_count {
                    self.push_code(OpCode::Pop);
                }
                if let ExprKind::Call { kind, .. } = argument.kind {
                    self.visit_expr(argument)?;
                    if kind != CallKind::None {
                        self.push_code(OpCode::Return);
                    } else {
                        let code_len = self.code().len();
                        if let OpCode::Call(i) = self.code()[code_len - 2] {
                            self.code()[code_len - 2] = OpCode::ReturnCall(i);
                        } else {
                            self.push_code(OpCode::Return);
                        }
                    }
                } else {
                    self.visit_expr(argument)?;
                    self.push_code(OpCode::Return);
                }
                self.push_load_const(ConstValue::Null);
            }
            ExprKind::Throw { argument } => {
                if self.function_semantic().kind == FunctionKind::Do {
                    return Err(CompilerError::ThrowOutsideFunction { range: expr.range });
                }
                for _ in 0..self.context().need_clear_stack_count {
                    self.push_code(OpCode::Pop);
                }
                self.visit_expr(argument)?;
                self.push_code(OpCode::Throw);
                self.push_load_const(ConstValue::Null);
            }
            ExprKind::Import {
                path,
                path_str,
                kind,
            } => {
                let const_id = self.add_const(ConstValue::Str(path_str.clone()));
                self.push_code(OpCode::Import(const_id));
                match kind {
                    ImportKind::Simple(alias) => {
                        self.store(alias.as_ref().map_or(path.last().unwrap(), |v| v))
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
                    AssignLeftKind::Member { table, property } => match property {
                        MemberKind::Bracket(_)
                        | MemberKind::Dot(_)
                        | MemberKind::DoubleColon(_) => {
                            self.visit_expr(table)?;
                            self.visit_member_property(property)?;
                            self.push_code(OpCode::Copy(2));
                            self.push_code(OpCode::Copy(2));
                            self.push_code(property.get_opcode());
                            self.visit_expr(right)?;
                            self.push_code(OpCode::from(*operator));
                            self.push_code(OpCode::Swap(3));
                            self.push_code(OpCode::Swap(2));
                            self.push_code(property.set_opcode());
                        }
                        MemberKind::BracketMeta
                        | MemberKind::DotMeta
                        | MemberKind::DoubleColonMeta => {
                            self.visit_expr(table)?;
                            self.push_code(OpCode::Copy(1));
                            self.push_code(property.get_opcode());
                            self.visit_expr(right)?;
                            self.push_code(OpCode::from(*operator));
                            self.push_code(OpCode::Swap(2));
                            self.push_code(property.set_opcode());
                        }
                    },
                };
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
                for right in right {
                    self.visit_expr(right)?;
                }
                for left in left.iter().rev() {
                    self.visit_assign_left(left)?;
                }
                self.push_load_const(ConstValue::Null);
            }
            ExprKind::Block(block) => self.visit_block(block)?,
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

    fn visit_typed_ident(&mut self, _ident: &TypedIdent<S>) -> Self::Return {
        unimplemented!()
    }

    fn visit_ty(&mut self, _ty: &Ty<S>) -> Self::Return {
        unimplemented!()
    }
}
