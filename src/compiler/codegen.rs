//! The Code Generator.

use std::iter;

use index_vec::{index_vec, IndexVec};
use indexmap::{IndexMap, IndexSet};
use rustc_hash::FxBuildHasher;

use super::{
    analyzer::{FunctionSemantic, Semantic, SymbolKind},
    ast::*,
    code::{Code, ConstValue},
    error::CompilerError,
    index::{FunctionId, SymbolId},
    opcode::{JumpTarget, OpCode},
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
            BinOp::Lt => OpCode::Lt,
            BinOp::Le => OpCode::Le,
            BinOp::Ne => OpCode::Ne,
            BinOp::Ge => OpCode::Ge,
            BinOp::Gt => OpCode::Gt,
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

impl<S> MemberKind<'_, S> {
    fn get_opcode(&self) -> OpCode {
        match self {
            MemberKind::Bracket(_) => OpCode::GetItem,
            MemberKind::Dot(_) | MemberKind::DoubleColon(_) => OpCode::GetAttr,
        }
    }

    fn set_opcode(&self) -> OpCode {
        match self {
            MemberKind::Bracket(_) => OpCode::SetItem,
            MemberKind::Dot(_) | MemberKind::DoubleColon(_) => OpCode::SetAttr,
        }
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
            continue_stack: Vec::new(),
            break_stack: Vec::new(),
        }
    }
}

/// Generate code.
pub fn gen_code<S: AsRef<str> + Copy>(
    program: &Program<'_, S>,
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

impl<'a, S: AsRef<str> + Copy> CodeGenerator<'a, S> {
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
        self.code().push(opcode);
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

    fn load(&mut self, ident: &Ident<'_, S>) {
        let symbol_id = ident.symbol_id.get().unwrap();
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

    fn store(&mut self, ident: &Ident<'_, S>) {
        let symbol_id = ident.symbol_id.get().unwrap();
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

    fn gen_code(mut self, program: &Program<'_, S>) -> (Code<S>, Vec<CompilerError>) {
        let code = self.gen_function_code(&program.function);
        (code, self.errors)
    }

    fn gen_function_code(&mut self, function: &Function<'_, S>) -> Code<S> {
        self.current_function_id = function.function_id.get().unwrap();
        for symbol_id in self.semantic.functions[self.current_function_id]
            .symbols
            .iter()
            .copied()
        {
            let symbol = &self.semantic.symbols[symbol_id];
            match symbol.kind {
                SymbolKind::Local => {
                    self.context().local_names.insert(symbol_id, symbol.name);
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
                        .insert(symbol_id, (symbol.name, base_closure_upvalue_id));
                }
                SymbolKind::Global => {
                    self.context().global_names.insert(symbol_id, symbol.name);
                }
            }
        }
        if let Some(variadic) = &function.variadic {
            self.store(&variadic.ident);
        }
        for param in function.params.iter().rev() {
            self.store(&param.ident);
        }
        self.gen_block(&function.body);
        if function.kind == FunctionKind::Do {
            self.push_code(OpCode::Return);
        } else if !self
            .context()
            .code
            .last()
            .is_some_and(|&opcode| opcode == OpCode::Return)
        {
            let const_id = self.add_const(ConstValue::Null);
            self.push_code(OpCode::LoadConst(const_id));
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
                OpCode::JumpIfNull(JumpTarget(v)) => *v = jump_target_table[*v],
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
            name: function.name,
            params: function.params.iter().map(|x| x.ident.name).collect(),
            variadic: function.variadic.as_ref().map(|x| x.ident.name),
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
            if let Some(prev_depth) = stack_depths[offset] {
                if current_depth <= prev_depth {
                    continue;
                }
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
                OpCode::ImportGlob => (),
                OpCode::BuildTable(i) => new_depth = new_depth - i * 2 + 1,
                OpCode::BuildList(i) => new_depth = new_depth - i + 1,
                OpCode::GetAttr | OpCode::GetItem => new_depth -= 1,
                OpCode::GetMeta => (),
                OpCode::SetAttr | OpCode::SetItem => new_depth -= 3,
                OpCode::SetMeta => new_depth -= 1,
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
                OpCode::JumpIfNull(JumpTarget(i)) => {
                    stack.push((i, new_depth));
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
                OpCode::Return | OpCode::Throw | OpCode::ReturnCall(_) => continue,
                OpCode::JumpTarget(_) => unreachable!(),
            }
            stack.push((offset + 1, new_depth));
        }
        max_stack_depth
    }

    fn gen_function(&mut self, function: &Function<'_, S>) {
        let code = self.gen_function_code(function);
        let const_id = self.add_const(ConstValue::Func(Box::new(code)));
        self.push_code(OpCode::LoadConst(const_id));
        if function.kind == FunctionKind::Do {
            self.push_code(OpCode::Call(0));
        }
    }

    fn gen_block(&mut self, block: &Block<'_, S>) {
        for stmt in &block.body {
            self.gen_stmt(stmt)
                .unwrap_or_else(|err| self.errors.push(err));
        }
    }

    fn gen_stmt(&mut self, stmt: &Stmt<'_, S>) -> Result<(), CompilerError> {
        match &stmt.kind {
            StmtKind::If {
                test,
                consequent,
                alternate,
            } => {
                if let Some(alternate) = alternate {
                    let false_label = self.get_jump_target();
                    let end_label = self.get_jump_target();
                    self.gen_expr(test)?;
                    self.push_code(OpCode::PopJumpIfFalse(false_label));
                    self.gen_block(consequent);
                    self.push_code(OpCode::Jump(end_label));
                    self.push_code(OpCode::JumpTarget(false_label));
                    self.gen_stmt(alternate)?;
                    self.push_code(OpCode::JumpTarget(end_label));
                } else {
                    let end_label = self.get_jump_target();
                    self.gen_expr(test)?;
                    self.push_code(OpCode::PopJumpIfFalse(end_label));
                    self.gen_block(consequent);
                    self.push_code(OpCode::JumpTarget(end_label));
                }
            }
            StmtKind::Match { expr, cases } => {
                let end_label = self.get_jump_target();
                self.gen_expr(expr)?;
                let block_labels: Vec<_> = iter::repeat_with(|| self.get_jump_target())
                    .take(cases.len())
                    .collect();
                for (i, case) in cases.iter().enumerate() {
                    let block_label = block_labels[i];
                    for pattern in &case.patterns.patterns {
                        let next_pattern_label = self.get_jump_target();
                        self.push_code(OpCode::Copy(1));
                        self.gen_pattern(pattern, next_pattern_label)?;
                        self.push_code(OpCode::Jump(block_label));
                        self.push_code(OpCode::JumpTarget(next_pattern_label));
                    }
                }
                self.push_code(OpCode::Jump(end_label));
                for (i, case) in cases.iter().enumerate() {
                    self.push_code(OpCode::JumpTarget(block_labels[i]));
                    self.gen_block(&case.body);
                    self.push_code(OpCode::Jump(end_label));
                }
                self.push_code(OpCode::JumpTarget(end_label));
                self.push_code(OpCode::Pop);
            }
            StmtKind::Loop { body } => {
                let continue_label = self.get_jump_target();
                let break_label = self.get_jump_target();
                self.continue_stack().push(continue_label);
                self.break_stack().push(break_label);

                self.push_code(OpCode::JumpTarget(continue_label));
                self.gen_block(body);
                self.push_code(OpCode::Jump(continue_label));
                self.push_code(OpCode::JumpTarget(break_label));

                self.continue_stack().pop();
                self.break_stack().pop();
            }
            StmtKind::While { test, body } => {
                let continue_label = self.get_jump_target();
                let break_label = self.get_jump_target();
                self.continue_stack().push(continue_label);
                self.break_stack().push(break_label);

                self.push_code(OpCode::JumpTarget(continue_label));
                self.gen_expr(test)?;
                self.push_code(OpCode::PopJumpIfFalse(break_label));
                self.gen_block(body);
                self.push_code(OpCode::Jump(continue_label));
                self.push_code(OpCode::JumpTarget(break_label));

                self.continue_stack().pop();
                self.break_stack().pop();
            }
            StmtKind::For { left, right, body } => {
                let continue_label = self.get_jump_target();
                let break_label = self.get_jump_target();
                self.continue_stack().push(continue_label);
                self.break_stack().push(break_label);

                self.gen_expr(right)?;
                self.push_code(OpCode::Iter);
                self.push_code(OpCode::JumpTarget(continue_label));
                self.push_code(OpCode::Copy(1));
                self.push_code(OpCode::Call(0));
                self.push_code(OpCode::JumpIfNull(break_label));
                if left.len() == 1 {
                    self.store(&left[0]);
                } else {
                    for (i, l) in left.iter().enumerate() {
                        self.push_code(OpCode::Copy(1));
                        let const_id = self.add_const(ConstValue::Int(i.try_into().unwrap()));
                        self.push_code(OpCode::LoadConst(const_id));
                        self.push_code(OpCode::GetItem);
                        self.store(l);
                    }
                    self.push_code(OpCode::Pop);
                }
                self.gen_block(body);
                self.push_code(OpCode::Jump(continue_label));
                self.push_code(OpCode::JumpTarget(break_label));
                self.push_code(OpCode::Pop);
                self.push_code(OpCode::Pop);

                self.continue_stack().pop();
                self.break_stack().pop();
            }
            StmtKind::Break => {
                let opcode = OpCode::Jump(match self.break_stack().last().copied() {
                    Some(v) => v,
                    None => return Err(CompilerError::BreakOutsideLoop { range: stmt.range }),
                });
                self.push_code(opcode);
            }
            StmtKind::Continue => {
                let opcode = OpCode::Jump(match self.continue_stack().last().copied() {
                    Some(v) => v,
                    None => return Err(CompilerError::ContinueOutsideLoop { range: stmt.range }),
                });
                self.push_code(opcode);
            }
            StmtKind::Return { argument } => {
                if self.function_semantic().kind == FunctionKind::Do {
                    return Err(CompilerError::ReturnOutsideFunction { range: stmt.range });
                }
                if let ExprKind::Call { kind, .. } = argument.kind {
                    self.gen_expr(argument)?;
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
                    self.gen_expr(argument)?;
                    self.push_code(OpCode::Return);
                }
            }
            StmtKind::Throw { argument } => {
                if self.function_semantic().kind == FunctionKind::Do {
                    return Err(CompilerError::ThrowOutsideFunction { range: stmt.range });
                }
                self.gen_expr(argument)?;
                self.push_code(OpCode::Throw);
            }
            StmtKind::Import {
                path,
                path_str,
                kind,
            } => {
                let const_id = self.add_const(ConstValue::Str(*path_str));
                self.push_code(OpCode::Import(const_id));
                match kind {
                    ImportKind::Simple(alias) => {
                        self.store(alias.as_ref().map_or(path.last().unwrap(), |v| v))
                    }
                    ImportKind::Nested(items) => {
                        for (name, alias) in items {
                            let const_id = self.add_const(ConstValue::Str(name.name));
                            self.push_code(OpCode::ImportFrom(const_id));
                            self.store(alias.as_ref().unwrap_or(name));
                        }
                        self.push_code(OpCode::Pop);
                    }
                    ImportKind::Glob => {
                        self.push_code(OpCode::ImportGlob);
                    }
                }
            }
            StmtKind::Fn {
                glo: _,
                name,
                function,
            } => {
                self.gen_function(function);
                self.store(name);
            }
            StmtKind::GloAssign { left, right } => {
                self.gen_expr(right)?;
                self.store(&left.ident);
            }
            StmtKind::Assign { left, right } => {
                self.gen_expr(right)?;
                self.gen_assign_left(left)?;
            }
            StmtKind::AssignOp {
                operator,
                left,
                right,
            } => match &left {
                AssignLeft::Ident(ident) => {
                    self.load(&ident.ident);
                    self.gen_expr(right)?;
                    self.push_code(OpCode::from(*operator));
                    self.store(&ident.ident);
                }
                AssignLeft::Member { table, property } => {
                    self.gen_expr_member_without_get(table, property)?;
                    self.push_code(OpCode::Copy(2));
                    self.push_code(OpCode::Copy(2));
                    self.push_code(property.get_opcode());
                    self.gen_expr(right)?;
                    self.push_code(OpCode::from(*operator));
                    self.push_code(OpCode::Swap(3));
                    self.push_code(OpCode::Swap(2));
                    self.push_code(property.set_opcode());
                }
                AssignLeft::MetaMember { table } => {
                    self.gen_expr(table)?;
                    self.push_code(OpCode::Copy(1));
                    self.push_code(OpCode::GetMeta);
                    self.push_code(OpCode::SetMeta);
                }
            },
            StmtKind::AssignUnpack { left, right } => {
                self.gen_expr(right)?;
                for _ in 0..left.len() {
                    self.push_code(OpCode::Copy(1));
                }
                for (i, l) in left.iter().enumerate() {
                    let const_id = self.add_const(ConstValue::Int(i.try_into().unwrap()));
                    self.push_code(OpCode::LoadConst(const_id));
                    self.push_code(OpCode::GetItem);
                    self.gen_assign_left(l)?;
                }
            }
            StmtKind::AssignMulti { left, right } => {
                assert!(left.len() == right.len());
                for right in right {
                    self.gen_expr(right)?;
                }
                for left in left.iter().rev() {
                    self.gen_assign_left(left)?;
                }
            }
            StmtKind::Block(block) => self.gen_block(block),
            StmtKind::Expr(expr) => {
                self.gen_expr(expr)?;
                self.push_code(OpCode::Pop);
            }
        }
        Ok(())
    }

    fn gen_assign_left(&mut self, assign_left: &AssignLeft<'_, S>) -> Result<(), CompilerError> {
        match assign_left {
            AssignLeft::Ident(ident) => self.store(&ident.ident),
            AssignLeft::Member { table, property } => {
                self.gen_expr_member_without_get(table, property)?;
                self.push_code(property.set_opcode());
            }
            AssignLeft::MetaMember { table } => {
                self.gen_expr(table)?;
                self.push_code(OpCode::SetMeta);
            }
        }
        Ok(())
    }

    fn gen_expr(&mut self, expr: &Expr<'_, S>) -> Result<(), CompilerError> {
        match &expr.kind {
            ExprKind::Lit(lit) => {
                let const_id = self.add_const(lit.kind.into());
                self.push_code(OpCode::LoadConst(const_id));
            }
            ExprKind::Ident(ident) => self.load(ident),
            ExprKind::Function(function) => self.gen_function(function),
            ExprKind::Table { properties } => {
                for TableProperty { key, value, .. } in properties {
                    self.gen_expr(key)?;
                    self.gen_expr(value)?;
                }
                self.push_code(OpCode::BuildTable(properties.len()));
            }
            ExprKind::List { items } => {
                for item in items {
                    self.gen_expr(item)?;
                }
                self.push_code(OpCode::BuildList(items.len()));
            }
            ExprKind::Unary { operator, argument } => {
                self.gen_expr(argument)?;
                self.push_code(OpCode::from(*operator));
            }
            ExprKind::Binary {
                operator,
                left,
                right,
            } => match operator {
                BinOp::And => {
                    let label = self.get_jump_target();
                    self.gen_expr(left)?;
                    self.push_code(OpCode::JumpIfFalseOrPop(label));
                    self.gen_expr(right)?;
                    self.push_code(OpCode::JumpTarget(label));
                }
                BinOp::Or => {
                    let label = self.get_jump_target();
                    self.gen_expr(left)?;
                    self.push_code(OpCode::JumpIfTrueOrPop(label));
                    self.gen_expr(right)?;
                    self.push_code(OpCode::JumpTarget(label));
                }
                operator => {
                    self.gen_expr(left)?;
                    self.gen_expr(right)?;
                    self.push_code(OpCode::from(*operator));
                }
            },
            ExprKind::TypeCheck { left, right } => {
                self.gen_expr(left)?;
                self.push_code(OpCode::TypeCheck(*right));
            }
            ExprKind::Member {
                table,
                property,
                safe,
            } => {
                self.gen_expr(table)?;
                let safe_label = self.get_jump_target();
                if *safe {
                    self.push_code(OpCode::JumpIfNull(safe_label));
                }
                match property {
                    MemberKind::Bracket(property) => {
                        self.gen_expr(property)?;
                        self.push_code(OpCode::GetItem);
                    }
                    MemberKind::Dot(ident) | MemberKind::DoubleColon(ident) => {
                        let const_id = self.add_const(ConstValue::Str(ident.name));
                        self.push_code(OpCode::LoadConst(const_id));
                        self.push_code(OpCode::GetAttr);
                    }
                }
                self.push_code(OpCode::JumpTarget(safe_label));
            }
            ExprKind::MetaMember { table, safe } => {
                self.gen_expr(table)?;
                let safe_label = self.get_jump_target();
                if *safe {
                    self.push_code(OpCode::JumpIfNull(safe_label));
                }
                self.push_code(OpCode::GetMeta);
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
                        self.gen_expr(table)?;
                        if *safe {
                            self.push_code(OpCode::JumpIfNull(safe_label));
                        }
                        match property {
                            MemberKind::Bracket(property) => {
                                self.gen_expr(property)?;
                                self.push_code(OpCode::GetItem);
                            }
                            MemberKind::Dot(ident) => {
                                self.push_code(OpCode::Copy(1));
                                let const_id = self.add_const(ConstValue::Str(ident.name));
                                self.push_code(OpCode::LoadConst(const_id));
                                self.push_code(OpCode::GetAttr);
                                self.push_code(OpCode::Swap(2));
                                argument_count += 1;
                            }
                            MemberKind::DoubleColon(ident) => {
                                let const_id = self.add_const(ConstValue::Str(ident.name));
                                self.push_code(OpCode::LoadConst(const_id));
                                self.push_code(OpCode::GetAttr);
                            }
                        }
                    }
                    _ => {
                        self.gen_expr(callee)?;
                    }
                }
                for arg in arguments {
                    self.gen_expr(arg)?;
                }
                self.push_code(match kind {
                    CallKind::None => OpCode::Call(argument_count),
                    CallKind::Try => OpCode::TryCall(argument_count),
                    CallKind::TryOption => OpCode::TryOptionCall(argument_count),
                    CallKind::TryPanic => OpCode::TryPanicCall(argument_count),
                });
                self.push_code(OpCode::JumpTarget(safe_label));
            }
        }
        Ok(())
    }

    fn gen_expr_member_without_get(
        &mut self,
        table: &Expr<'_, S>,
        property: &MemberKind<'_, S>,
    ) -> Result<(), CompilerError> {
        self.gen_expr(table)?;
        match property {
            MemberKind::Bracket(property) => self.gen_expr(property)?,
            MemberKind::Dot(ident) | MemberKind::DoubleColon(ident) => {
                let const_id = self.add_const(ConstValue::Str(ident.name));
                self.push_code(OpCode::LoadConst(const_id));
            }
        }
        Ok(())
    }

    fn gen_pattern(
        &mut self,
        pattern: &Pattern<'_, S>,
        next_pattern_label: JumpTarget,
    ) -> Result<(), CompilerError> {
        match &pattern.kind {
            PatternKind::Lit(lit) => {
                let const_id = self.add_const(lit.kind.into());
                self.push_code(OpCode::LoadConst(const_id));
                self.push_code(match lit.kind {
                    LitKind::Null | LitKind::Bool(_) | LitKind::Int(_) | LitKind::Float(_) => {
                        OpCode::Identical
                    }
                    LitKind::Str(_) => OpCode::Eq,
                });
                self.push_code(OpCode::PopJumpIfFalse(next_pattern_label));
            }
            PatternKind::Ident(ident) => {
                self.push_code(OpCode::JumpPopIfNull(next_pattern_label));
                self.store(ident);
            }
            PatternKind::Table { pairs, others } => {
                self.push_code(OpCode::Copy(1));
                self.push_code(OpCode::TypeCheck(ValueType::Table));
                self.push_code(OpCode::PopJumpIfFalse(next_pattern_label));
                if !others {
                    self.push_code(OpCode::Copy(1));
                    self.push_code(OpCode::GetLen);
                    let const_id = self.add_const(ConstValue::Int(pairs.len().try_into().unwrap()));
                    self.push_code(OpCode::LoadConst(const_id));
                    self.push_code(OpCode::Eq);
                    self.push_code(OpCode::PopJumpIfFalse(next_pattern_label));
                }
                for (k, v) in pairs {
                    self.push_code(OpCode::Copy(1));
                    let const_id = self.add_const(k.kind.into());
                    self.push_code(OpCode::LoadConst(const_id));
                    self.push_code(OpCode::GetItem);
                    self.gen_pattern(v, next_pattern_label)?;
                }
                self.push_code(OpCode::Pop);
            }
            PatternKind::List { items, others } => {
                self.push_code(OpCode::Copy(1));
                self.push_code(OpCode::TypeCheck(ValueType::Table));
                self.push_code(OpCode::PopJumpIfFalse(next_pattern_label));
                if !others {
                    self.push_code(OpCode::Copy(1));
                    self.push_code(OpCode::GetLen);
                    let const_id = self.add_const(ConstValue::Int(items.len().try_into().unwrap()));
                    self.push_code(OpCode::LoadConst(const_id));
                    self.push_code(OpCode::Eq);
                    self.push_code(OpCode::PopJumpIfFalse(next_pattern_label));
                }
                for (i, item) in items.iter().enumerate() {
                    self.push_code(OpCode::Copy(1));
                    let const_id = self.add_const(ConstValue::Int(i.try_into().unwrap()));
                    self.push_code(OpCode::LoadConst(const_id));
                    self.push_code(OpCode::GetItem);
                    self.gen_pattern(item, next_pattern_label)?;
                }
                self.push_code(OpCode::Pop);
            }
        }
        Ok(())
    }
}
