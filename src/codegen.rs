use std::cmp::max;
use std::convert::TryFrom;
use std::vec;

use crate::analyzer::{Function, GlobalNameInfo, UpvalueNameInfo};
use crate::ast::*;
use crate::code::{Code, ConstlValue, FunctionKind};
use crate::errors::{Error, Result, SyntaxError};
use crate::opcode::{JumpTarget, OpCode};
use crate::utils::Join;

impl TryFrom<BinOp> for OpCode {
    type Error = Error;

    fn try_from(value: BinOp) -> Result<Self> {
        Ok(match value {
            BinOp::Add => OpCode::Add,
            BinOp::Sub => OpCode::Sub,
            BinOp::Mul => OpCode::Mul,
            BinOp::Div => OpCode::Div,
            BinOp::Mod => OpCode::Mod,
            BinOp::Eq => OpCode::Eq,
            BinOp::Lt => OpCode::Lt,
            BinOp::Le => OpCode::Le,
            BinOp::Ne => OpCode::Ne,
            BinOp::Ge => OpCode::Ge,
            BinOp::Gt => OpCode::Gt,
            BinOp::Is => OpCode::Is,
            _ => return Err(SyntaxError::IllegalAst.into()),
        })
    }
}

pub fn gen_code(func_list: Vec<Function>) -> Result<Code> {
    CodeGen { func_list }.gen_code(0)
}

struct CodeGen {
    func_list: Vec<Function>,
}

impl CodeGen {
    fn get_jump_target(&mut self, func_id: usize) -> JumpTarget {
        self.func_list[func_id].jump_target_count += 1;
        JumpTarget(self.func_list[func_id].jump_target_count - 1)
    }

    fn add_const(&mut self, func_id: usize, value: ConstlValue) -> usize {
        if let Some(index) = self.func_list[func_id]
            .consts
            .iter()
            .position(|x| *x == value)
        {
            index
        } else {
            self.func_list[func_id].consts.push(value);
            self.func_list[func_id].consts.len() - 1
        }
    }

    fn load(&mut self, func_id: usize, name: &str) -> Result<()> {
        let t = if let Some(i) = self.func_list[func_id].local_names.get_index_of(name) {
            OpCode::LoadLocal(i)
        } else if let Some(i) = self.func_list[func_id]
            .global_names
            .get_index_of(&GlobalNameInfo::from(name.to_owned()))
        {
            OpCode::LoadGlobal(i)
        } else if let Some(i) = self.func_list[func_id]
            .upvalue_names
            .get_index_of(&UpvalueNameInfo::from(name.to_owned()))
        {
            OpCode::LoadUpvalue(i)
        } else {
            return Err(SyntaxError::IllegalAst.into());
        };
        self.func_list[func_id].code.push(t);
        Ok(())
    }

    fn store(&mut self, func_id: usize, name: &str) -> Result<()> {
        let t = if let Some(i) = self.func_list[func_id].local_names.get_index_of(name) {
            OpCode::StoreLocal(i)
        } else if let Some(i) = self.func_list[func_id]
            .global_names
            .get_index_of(&GlobalNameInfo::from(name.to_owned()))
        {
            OpCode::StoreGlobal(i)
        } else if let Some(i) = self.func_list[func_id]
            .upvalue_names
            .get_index_of(&UpvalueNameInfo::from(name.to_owned()))
        {
            OpCode::StoreUpvalue(i)
        } else {
            return Err(SyntaxError::IllegalAst.into());
        };
        self.func_list[func_id].code.push(t);
        Ok(())
    }

    fn gen_code(&mut self, func_id: usize) -> Result<Code> {
        if let Some(variadic) = self.func_list[func_id].variadic.clone() {
            self.store(func_id, &variadic)?;
        }
        for param in self.func_list[func_id].params.clone().into_iter().rev() {
            self.store(func_id, &param)?;
        }
        for stmt in self.func_list[func_id].body.clone().body {
            self.gen_stmt(func_id, &stmt)?;
        }
        if self.func_list[func_id].kind == FunctionKind::Do {
            self.func_list[func_id].code.push(OpCode::Return);
        } else if *self.func_list[func_id].code.last().unwrap_or(&OpCode::Pop) != OpCode::Return {
            let t = self.add_const(func_id, ConstlValue::Null);
            self.func_list[func_id].code.push(OpCode::LoadConst(t));
            self.func_list[func_id].code.push(OpCode::Return);
        }

        let mut temp: Vec<usize> = vec![0; self.func_list[func_id].jump_target_count];
        let mut i = 0;
        while i < self.func_list[func_id].code.len() {
            match self.func_list[func_id].code[i] {
                OpCode::JumpTarget(JumpTarget(index)) => {
                    temp[index] = i;
                    self.func_list[func_id].code.remove(i);
                }
                _ => i += 1,
            }
        }
        let mut i = 0;
        while i < self.func_list[func_id].code.len() {
            match &self.func_list[func_id].code[i] {
                OpCode::For(JumpTarget(v)) => {
                    self.func_list[func_id].code[i] = OpCode::For(JumpTarget(temp[*v]))
                }
                OpCode::Jump(JumpTarget(v)) => {
                    self.func_list[func_id].code[i] = OpCode::Jump(JumpTarget(temp[*v]))
                }
                OpCode::JumpIfNull(JumpTarget(v)) => {
                    self.func_list[func_id].code[i] = OpCode::JumpIfNull(JumpTarget(temp[*v]))
                }
                OpCode::JumpPopIfFalse(JumpTarget(v)) => {
                    self.func_list[func_id].code[i] = OpCode::JumpPopIfFalse(JumpTarget(temp[*v]))
                }
                OpCode::JumpIfTureOrPop(JumpTarget(v)) => {
                    self.func_list[func_id].code[i] = OpCode::JumpIfTureOrPop(JumpTarget(temp[*v]))
                }
                OpCode::JumpIfFalseOrPop(JumpTarget(v)) => {
                    self.func_list[func_id].code[i] = OpCode::JumpIfFalseOrPop(JumpTarget(temp[*v]))
                }
                _ => (),
            }
            i += 1;
        }

        let stack_size = get_stack_size(
            &self.func_list[func_id].code,
            0,
            self.func_list[func_id].params.len()
                + if self.func_list[func_id].variadic.is_some() {
                    1
                } else {
                    0
                },
        );
        Ok(Code {
            params: self.func_list[func_id].params.clone(),
            variadic: self.func_list[func_id].variadic.clone(),
            kind: self.func_list[func_id].kind,
            code: self.func_list[func_id].code.clone(),
            consts: self.func_list[func_id].consts.clone(),
            local_names: self.func_list[func_id]
                .local_names
                .clone()
                .into_iter()
                .collect(),
            global_names: self.func_list[func_id]
                .global_names
                .clone()
                .into_iter()
                .map(|x| x.name)
                .collect(),
            upvalue_names: self.func_list[func_id]
                .upvalue_names
                .clone()
                .into_iter()
                .map(|x| x.into())
                .collect(),
            def_upvalue_count: self.func_list[func_id].def_upvalue_count,
            stack_size,
        })
    }

    fn gen_expr(&mut self, func_id: usize, ast_node: &Expr) -> Result<()> {
        match &ast_node.kind {
            ExprKind::Lit(lit) => {
                let t = self.add_const(
                    func_id,
                    match &lit.value {
                        LitKind::Null => ConstlValue::Null,
                        LitKind::Bool(v) => ConstlValue::Bool(*v),
                        LitKind::Int(v) => ConstlValue::Int(*v),
                        LitKind::Float(v) => ConstlValue::Float(*v),
                        LitKind::Str(v) => ConstlValue::Str(v.clone()),
                    },
                );
                self.func_list[func_id].code.push(OpCode::LoadConst(t));
            }
            ExprKind::Ident(ident) => self.load(func_id, &ident.name)?,
            ExprKind::Function { .. } => return Err(SyntaxError::IllegalAst.into()),
            ExprKind::FunctionId(i) => {
                let code = self.gen_code(*i)?;
                let is_do = code.kind == FunctionKind::Do;
                let t = self.add_const(func_id, ConstlValue::Func(code));
                self.func_list[func_id].code.push(OpCode::LoadConst(t));
                if is_do {
                    self.func_list[func_id].code.push(OpCode::Call(0));
                }
            }
            ExprKind::Table { properties } => {
                let temp = properties.len();
                for TableProperty { key, value, .. } in properties {
                    self.gen_expr(func_id, key)?;
                    self.gen_expr(func_id, value)?;
                }
                self.func_list[func_id].code.push(OpCode::BuildTable(temp));
            }
            ExprKind::Unary { operator, argument } => {
                self.gen_expr(func_id, argument)?;
                self.func_list[func_id].code.push(match operator {
                    UnOp::Not => OpCode::Not,
                    UnOp::Neg => OpCode::Neg,
                });
            }
            ExprKind::Binary {
                operator,
                left,
                right,
            } => match operator {
                BinOp::And => {
                    let label = self.get_jump_target(func_id);
                    self.gen_expr(func_id, left)?;
                    self.func_list[func_id]
                        .code
                        .push(OpCode::JumpIfFalseOrPop(label));
                    self.gen_expr(func_id, right)?;
                    self.func_list[func_id].code.push(OpCode::JumpTarget(label));
                }
                BinOp::Or => {
                    let label = self.get_jump_target(func_id);
                    self.gen_expr(func_id, left)?;
                    self.func_list[func_id]
                        .code
                        .push(OpCode::JumpIfTureOrPop(label));
                    self.gen_expr(func_id, right)?;
                    self.func_list[func_id].code.push(OpCode::JumpTarget(label));
                }
                operator => {
                    self.gen_expr(func_id, left)?;
                    self.gen_expr(func_id, right)?;
                    self.func_list[func_id]
                        .code
                        .push(OpCode::try_from(*operator)?);
                }
            },
            ExprKind::Member {
                table,
                property,
                kind,
                safe,
            } => {
                self.gen_expr(func_id, table)?;
                let safe_label = self.get_jump_target(func_id);
                if *safe {
                    self.func_list[func_id]
                        .code
                        .push(OpCode::JumpIfNull(safe_label));
                }
                match kind {
                    MemberKind::Bracket => {
                        self.gen_expr(func_id, property)?;
                        self.func_list[func_id].code.push(OpCode::GetItem);
                    }
                    MemberKind::Dot | MemberKind::DoubleColon => {
                        match &property.kind {
                            ExprKind::Ident(ident) => {
                                let t =
                                    self.add_const(func_id, ConstlValue::Str(ident.name.clone()));
                                self.func_list[func_id].code.push(OpCode::LoadConst(t));
                            }
                            _ => return Err(SyntaxError::IllegalAst.into()),
                        }
                        self.func_list[func_id].code.push(OpCode::GetAttr);
                    }
                }
                self.func_list[func_id]
                    .code
                    .push(OpCode::JumpTarget(safe_label));
            }
            ExprKind::MetaMember { table, safe } => {
                self.gen_expr(func_id, table)?;
                let safe_label = self.get_jump_target(func_id);
                if *safe {
                    self.func_list[func_id]
                        .code
                        .push(OpCode::JumpIfNull(safe_label));
                }
                self.func_list[func_id].code.push(OpCode::GetMeta);
                self.func_list[func_id]
                    .code
                    .push(OpCode::JumpTarget(safe_label));
            }
            ExprKind::Call {
                callee,
                arguments,
                propagating_error,
            } => {
                let mut temp: usize = arguments.len();
                let safe_label = self.get_jump_target(func_id);
                match callee.kind.clone() {
                    ExprKind::Member {
                        table,
                        property,
                        kind,
                        safe,
                    } => {
                        self.gen_expr(func_id, &table)?;
                        if safe {
                            self.func_list[func_id]
                                .code
                                .push(OpCode::JumpIfNull(safe_label));
                        }
                        match kind {
                            MemberKind::Bracket => {
                                self.gen_expr(func_id, &property)?;
                                self.func_list[func_id].code.push(OpCode::GetItem);
                            }
                            MemberKind::Dot => {
                                self.func_list[func_id].code.push(OpCode::Dup);
                                match property.kind {
                                    ExprKind::Ident(ident) => {
                                        let t =
                                            self.add_const(func_id, ConstlValue::Str(ident.name));
                                        self.func_list[func_id].code.push(OpCode::LoadConst(t));
                                    }
                                    _ => return Err(SyntaxError::IllegalAst.into()),
                                }
                                self.func_list[func_id].code.push(OpCode::GetAttr);
                                self.func_list[func_id].code.push(OpCode::RotTwo);
                                temp += 1;
                            }
                            MemberKind::DoubleColon => {
                                match property.kind {
                                    ExprKind::Ident(ident) => {
                                        let t =
                                            self.add_const(func_id, ConstlValue::Str(ident.name));
                                        self.func_list[func_id].code.push(OpCode::LoadConst(t));
                                    }
                                    _ => return Err(SyntaxError::IllegalAst.into()),
                                }
                                self.func_list[func_id].code.push(OpCode::GetAttr);
                            }
                        }
                    }
                    _ => {
                        self.gen_expr(func_id, callee)?;
                    }
                }
                for arg in arguments {
                    self.gen_expr(func_id, arg)?;
                }
                self.func_list[func_id].code.push(if *propagating_error {
                    OpCode::TryCall(temp)
                } else {
                    OpCode::Call(temp)
                });
                self.func_list[func_id]
                    .code
                    .push(OpCode::JumpTarget(safe_label));
            }
        }
        Ok(())
    }

    fn gen_stmt(&mut self, func_id: usize, ast_node: &Stmt) -> Result<()> {
        macro_rules! gen_block {
            ($block:expr) => {
                for stmt in &$block.body {
                    self.gen_stmt(func_id, stmt)?;
                }
            };
        }
        macro_rules! gen_expr_member_without_get {
            ($table:expr, $property:expr, $kind:expr, $safe:expr) => {{
                if *$safe {
                    return Err(SyntaxError::IllegalAst.into());
                }
                self.gen_expr(func_id, &$table)?;
                match $kind {
                    MemberKind::Bracket => self.gen_expr(func_id, &$property)?,
                    MemberKind::Dot | MemberKind::DoubleColon => match &$property.kind {
                        ExprKind::Ident(ident) => {
                            let t = self.add_const(func_id, ConstlValue::Str(ident.name.clone()));
                            self.func_list[func_id].code.push(OpCode::LoadConst(t));
                        }
                        _ => return Err(SyntaxError::IllegalAst.into()),
                    },
                }
            }};
        }
        macro_rules! assign_left {
            ($left:expr) => {
                match &$left.kind {
                    ExprKind::Ident(ident) => self.store(func_id, &ident.name)?,
                    ExprKind::Member {
                        table,
                        property,
                        kind,
                        safe,
                    } => {
                        gen_expr_member_without_get!(table, property, kind, safe);
                        self.func_list[func_id].code.push(match kind {
                            MemberKind::Bracket => OpCode::SetItem,
                            MemberKind::Dot | MemberKind::DoubleColon => OpCode::SetAttr,
                        });
                    }
                    ExprKind::MetaMember { table, safe } => {
                        if *safe {
                            return Err(SyntaxError::IllegalAst.into());
                        }
                        self.gen_expr(func_id, table)?;
                        self.func_list[func_id].code.push(OpCode::SetMeta);
                    }
                    _ => return Err(SyntaxError::IllegalAst.into()),
                }
            };
        }
        match &ast_node.kind {
            StmtKind::If {
                test,
                consequent,
                alternate,
            } => {
                /*
                if (...) <statement> [else <statement>]

                  if (<cond>)                   <cond>
                                                JUMP_IF_FALSE a
                    <true_statement>   ===>     <true_statement>
                  else:                         JUMP b
                a:                           a:
                    <false_statement>           <false_statement>
                b:                           b:
                */
                if let Some(alternate) = alternate {
                    let false_label = self.get_jump_target(func_id);
                    let end_label = self.get_jump_target(func_id);
                    self.gen_expr(func_id, test)?;
                    self.func_list[func_id]
                        .code
                        .push(OpCode::JumpPopIfFalse(false_label));
                    gen_block!(consequent);
                    self.func_list[func_id].code.push(OpCode::Jump(end_label));
                    self.func_list[func_id]
                        .code
                        .push(OpCode::JumpTarget(false_label));
                    self.gen_stmt(func_id, alternate)?;
                    self.func_list[func_id]
                        .code
                        .push(OpCode::JumpTarget(end_label));
                } else {
                    let end_label = self.get_jump_target(func_id);
                    self.gen_expr(func_id, test)?;
                    self.func_list[func_id]
                        .code
                        .push(OpCode::JumpPopIfFalse(end_label));
                    gen_block!(consequent);
                    self.func_list[func_id]
                        .code
                        .push(OpCode::JumpTarget(end_label));
                }
            }
            StmtKind::Loop { body } => {
                let continue_label = self.get_jump_target(func_id);
                let break_label = self.get_jump_target(func_id);
                self.func_list[func_id].continue_stack.push(continue_label);
                self.func_list[func_id].break_stack.push(break_label);

                self.func_list[func_id]
                    .code
                    .push(OpCode::JumpTarget(continue_label));
                gen_block!(body);
                self.func_list[func_id]
                    .code
                    .push(OpCode::Jump(continue_label));
                self.func_list[func_id]
                    .code
                    .push(OpCode::JumpTarget(break_label));

                self.func_list[func_id].continue_stack.pop();
                self.func_list[func_id].break_stack.pop();
            }
            StmtKind::While { test, body } => {
                let continue_label = self.get_jump_target(func_id);
                let break_label = self.get_jump_target(func_id);
                self.func_list[func_id].continue_stack.push(continue_label);
                self.func_list[func_id].break_stack.push(break_label);

                self.func_list[func_id]
                    .code
                    .push(OpCode::JumpTarget(continue_label));
                self.gen_expr(func_id, test)?;
                self.func_list[func_id]
                    .code
                    .push(OpCode::JumpPopIfFalse(break_label));
                gen_block!(body);
                self.func_list[func_id]
                    .code
                    .push(OpCode::Jump(continue_label));
                self.func_list[func_id]
                    .code
                    .push(OpCode::JumpTarget(break_label));

                self.func_list[func_id].continue_stack.pop();
                self.func_list[func_id].break_stack.pop();
            }
            StmtKind::For { left, right, body } => {
                let continue_label = self.get_jump_target(func_id);
                let break_label = self.get_jump_target(func_id);
                self.func_list[func_id].continue_stack.push(continue_label);
                self.func_list[func_id].break_stack.push(break_label);

                self.gen_expr(func_id, right)?;
                self.func_list[func_id]
                    .code
                    .push(OpCode::JumpTarget(continue_label));
                self.func_list[func_id].code.push(OpCode::For(break_label));
                if left.len() == 1 {
                    self.store(func_id, &left[0].name)?;
                } else {
                    for (i, l) in left.iter().enumerate() {
                        self.func_list[func_id].code.push(OpCode::Dup);
                        let t = self.add_const(func_id, ConstlValue::Int(i.try_into().unwrap()));
                        self.func_list[func_id].code.push(OpCode::LoadConst(t));
                        self.func_list[func_id].code.push(OpCode::GetItem);
                        self.store(func_id, &l.name)?;
                    }
                    self.func_list[func_id].code.push(OpCode::Pop);
                }
                gen_block!(body);
                self.func_list[func_id]
                    .code
                    .push(OpCode::Jump(continue_label));
                self.func_list[func_id]
                    .code
                    .push(OpCode::JumpTarget(break_label));
                self.func_list[func_id].code.push(OpCode::Pop);

                self.func_list[func_id].continue_stack.pop();
                self.func_list[func_id].break_stack.pop();
            }
            StmtKind::Break => {
                let t = OpCode::Jump(match self.func_list[func_id].break_stack.last() {
                    Some(v) => *v,
                    None => return Err(SyntaxError::BreakOutsideLoop.into()),
                });
                self.func_list[func_id].code.push(t);
            }
            StmtKind::Continue => {
                let t = OpCode::Jump(match self.func_list[func_id].continue_stack.last() {
                    Some(v) => *v,
                    None => return Err(SyntaxError::ContinueOutsideLoop.into()),
                });
                self.func_list[func_id].code.push(t);
            }
            StmtKind::Return { argument } => {
                if self.func_list[func_id].kind == FunctionKind::Do {
                    return Err(SyntaxError::ReturnOutsideFunction.into());
                }
                if let ExprKind::Call {
                    propagating_error, ..
                } = argument.kind
                {
                    self.gen_expr(func_id, argument)?;
                    if propagating_error {
                        self.func_list[func_id].code.push(OpCode::Return);
                    } else if let OpCode::Call(i) =
                        self.func_list[func_id].code[self.func_list[func_id].code.len() - 2]
                    {
                        let t = self.func_list[func_id].code.len();
                        self.func_list[func_id].code[t - 2] = OpCode::ReturnCall(i);
                    }
                } else {
                    self.gen_expr(func_id, argument)?;
                    self.func_list[func_id].code.push(OpCode::Return);
                }
            }
            StmtKind::Throw { argument } => {
                if self.func_list[func_id].kind == FunctionKind::Do {
                    return Err(SyntaxError::ThrowOutsideFunction.into());
                }
                self.gen_expr(func_id, argument)?;
                self.func_list[func_id].code.push(OpCode::Throw);
            }
            StmtKind::Global { arguments } => {
                if self.func_list[func_id].kind == FunctionKind::Do {
                    return Err(SyntaxError::GlobalOutsideFunction.into());
                }
                for arg in arguments {
                    self.func_list[func_id].global_names.insert(GlobalNameInfo {
                        name: arg.name.clone(),
                        is_writable: true,
                    });
                }
            }
            StmtKind::Import { path, kind } => {
                let path_str = path.iter().map(|x| x.name.as_str()).join("::");
                let t = self.add_const(func_id, ConstlValue::Str(path_str));
                self.func_list[func_id].code.push(OpCode::Import(t));
                match kind {
                    ImportKind::Simple(alias) => self.store(func_id, &alias.name)?,
                    ImportKind::Nested(items) => {
                        for (name, alias) in items {
                            let t = self.add_const(func_id, ConstlValue::Str(name.name.clone()));
                            self.func_list[func_id].code.push(OpCode::ImportFrom(t));
                            self.store(func_id, &alias.name)?;
                        }
                        self.func_list[func_id].code.push(OpCode::Pop);
                    }
                    ImportKind::Glob => {
                        self.func_list[func_id].code.push(OpCode::ImportGlob);
                    }
                }
            }
            StmtKind::Assign { left, right } => {
                self.gen_expr(func_id, right)?;
                assign_left!(left);
            }
            StmtKind::AssignOp {
                operator,
                left,
                right,
            } => match &left.kind {
                ExprKind::Ident(ident) => {
                    self.gen_expr(func_id, left)?;
                    self.gen_expr(func_id, right)?;
                    self.func_list[func_id]
                        .code
                        .push(OpCode::try_from(*operator)?);
                    self.store(func_id, &ident.name)?;
                }
                ExprKind::Member {
                    table,
                    property,
                    kind,
                    safe,
                } => {
                    gen_expr_member_without_get!(table, property, kind, safe);
                    self.func_list[func_id].code.push(OpCode::DupTwo);
                    self.func_list[func_id].code.push(match kind {
                        MemberKind::Bracket => OpCode::GetItem,
                        MemberKind::Dot | MemberKind::DoubleColon => OpCode::GetAttr,
                    });
                    self.gen_expr(func_id, right)?;
                    self.func_list[func_id]
                        .code
                        .push(OpCode::try_from(*operator)?);
                    self.func_list[func_id].code.push(OpCode::RotThree);
                    self.func_list[func_id].code.push(match kind {
                        MemberKind::Bracket => OpCode::SetItem,
                        MemberKind::Dot | MemberKind::DoubleColon => OpCode::SetAttr,
                    });
                }
                ExprKind::MetaMember { table, safe } => {
                    if *safe {
                        return Err(SyntaxError::IllegalAst.into());
                    }
                    self.gen_expr(func_id, table)?;
                    self.func_list[func_id].code.push(OpCode::Dup);
                    self.func_list[func_id].code.push(OpCode::GetMeta);
                    self.func_list[func_id].code.push(OpCode::SetMeta);
                }
                _ => return Err(SyntaxError::IllegalAst.into()),
            },
            StmtKind::AssignUnpack { left, right } => {
                self.gen_expr(func_id, right)?;
                for (i, l) in left.iter().enumerate() {
                    let t = self.add_const(func_id, ConstlValue::Int(i.try_into().unwrap()));
                    self.func_list[func_id].code.push(OpCode::LoadConst(t));
                    self.func_list[func_id].code.push(OpCode::GetItem);
                    assign_left!(l);
                }
            }
            StmtKind::AssignMulti { left, right } => {
                if left.len() != right.len() {
                    return Err(SyntaxError::IllegalAst.into());
                }
                for right in right {
                    self.gen_expr(func_id, right)?;
                }
                for left in left.iter().rev() {
                    assign_left!(left);
                }
            }
            StmtKind::Block(block) => gen_block!(block),
            StmtKind::Expr(expr) => {
                self.gen_expr(func_id, expr)?;
                self.func_list[func_id].code.push(OpCode::Pop);
            }
        }
        Ok(())
    }
}

/// Try estimate function stack size.
fn get_stack_size(code: &Vec<OpCode>, mut offset: usize, init_size: usize) -> usize {
    let mut stack_size = init_size;
    let mut t = init_size;
    while offset < code.len() {
        match code[offset] {
            OpCode::Pop => t += 1,
            OpCode::Dup => t += 1,
            OpCode::DupTwo => t += 2,
            OpCode::RotTwo | OpCode::RotThree => (),
            OpCode::LoadLocal(_)
            | OpCode::LoadGlobal(_)
            | OpCode::LoadUpvalue(_)
            | OpCode::LoadConst(_) => t += 1,
            OpCode::StoreLocal(_) | OpCode::StoreGlobal(_) | OpCode::StoreUpvalue(_) => t -= 1,
            OpCode::Import(_) => t += 1,
            OpCode::ImportFrom(_) => t += 1,
            OpCode::ImportGlob => (),
            OpCode::BuildTable(i) => t = t - i * 2 + 1,
            OpCode::GetAttr | OpCode::GetItem => t -= 1,
            OpCode::GetMeta => (),
            OpCode::SetAttr | OpCode::SetItem => t -= 2,
            OpCode::SetMeta => t -= 1,
            OpCode::Neg | OpCode::Not => (),
            OpCode::Add
            | OpCode::Sub
            | OpCode::Mul
            | OpCode::Div
            | OpCode::Mod
            | OpCode::Eq
            | OpCode::Ne
            | OpCode::Gt
            | OpCode::Ge
            | OpCode::Lt
            | OpCode::Le
            | OpCode::Is => t -= 1,
            OpCode::For(_) => t += 1,
            OpCode::Jump(JumpTarget(_)) => (),
            OpCode::JumpIfNull(JumpTarget(i)) => {
                stack_size = max(stack_size, get_stack_size(code, i, t));
            }
            OpCode::JumpPopIfFalse(JumpTarget(i)) => {
                t -= 1;
                stack_size = max(stack_size, get_stack_size(code, i, t));
            }
            OpCode::JumpIfTureOrPop(JumpTarget(i)) | OpCode::JumpIfFalseOrPop(JumpTarget(i)) => {
                stack_size = max(stack_size, get_stack_size(code, i, t));
                t -= 1;
            }
            OpCode::Call(i) => t = t - i + 1,
            OpCode::TryCall(i) => t = t - i + 1,
            OpCode::Return | OpCode::Throw => break,
            OpCode::ReturnCall(i) => t = t - i + 1,
            OpCode::JumpTarget(_) => panic!(),
        }
        stack_size = max(stack_size, t);
        offset += 1;
    }
    stack_size
}
