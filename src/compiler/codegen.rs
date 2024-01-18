//! The Code Generator.
//!
//! Turn `Vec<Function>` build by analyzer into `Code`.

use std::{convert::TryFrom, iter, vec};

use thiserror::Error;

use crate::utils::Join;

use super::{
    analyzer::Function,
    ast::*,
    code::{Code, ConstValue, FunctionKind},
    opcode::{JumpTarget, OpCode},
    parser::ParserError,
};

impl TryFrom<BinOp> for OpCode {
    type Error = SyntaxError;

    fn try_from(value: BinOp) -> Result<Self, Self::Error> {
        Ok(match value {
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
            BinOp::Is => OpCode::Is,
            BinOp::And | BinOp::Or => return Err(SyntaxError::IllegalAst),
        })
    }
}

impl MemberKind {
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

/// Generate code.
pub fn gen_code(func_list: Vec<Function>) -> Result<Code, SyntaxError> {
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

    fn add_const(&mut self, func_id: usize, value: ConstValue) -> usize {
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

    fn load(&mut self, func_id: usize, name: &str) -> Result<(), SyntaxError> {
        macro_rules! find_name_index {
            ($name:ident) => {
                self.func_list[func_id]
                    .$name()
                    .enumerate()
                    .find_map(|(i, x)| if x == name { Some(i) } else { None })
            };
        }
        let t = if let Some(i) = find_name_index!(local_names) {
            OpCode::LoadLocal(i)
        } else if let Some(i) = find_name_index!(global_names) {
            OpCode::LoadGlobal(i)
        } else if let Some(i) = find_name_index!(upvalue_names) {
            OpCode::LoadUpvalue(i)
        } else {
            return Err(SyntaxError::IllegalAst);
        };
        self.func_list[func_id].code.push(t);
        Ok(())
    }

    fn store(&mut self, func_id: usize, name: &str) -> Result<(), SyntaxError> {
        macro_rules! find_name_index {
            ($name:ident) => {
                self.func_list[func_id]
                    .$name()
                    .enumerate()
                    .find_map(|(i, x)| if x == name { Some(i) } else { None })
            };
        }
        let t = if let Some(i) = find_name_index!(local_names) {
            OpCode::StoreLocal(i)
        } else if let Some(i) = find_name_index!(global_names) {
            OpCode::StoreGlobal(i)
        } else if let Some(i) = find_name_index!(upvalue_names) {
            OpCode::StoreUpvalue(i)
        } else {
            return Err(SyntaxError::IllegalAst);
        };
        self.func_list[func_id].code.push(t);
        Ok(())
    }

    fn gen_code(&mut self, func_id: usize) -> Result<Code, SyntaxError> {
        if let Some(variadic) = self.func_list[func_id].variadic.clone() {
            self.store(func_id, &variadic.ident.name)?;
        }
        for param in self.func_list[func_id].params.clone().into_iter().rev() {
            self.store(func_id, &param.ident.name)?;
        }
        self.gen_block(func_id, &self.func_list[func_id].body.clone())?;
        if self.func_list[func_id].kind == FunctionKind::Do {
            self.func_list[func_id].code.push(OpCode::Return);
        } else if *self.func_list[func_id].code.last().unwrap_or(&OpCode::Pop) != OpCode::Return {
            let t = self.add_const(func_id, ConstValue::Null);
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
                OpCode::Jump(JumpTarget(v)) => {
                    self.func_list[func_id].code[i] = OpCode::Jump(JumpTarget(temp[*v]))
                }
                OpCode::JumpIfNull(JumpTarget(v)) => {
                    self.func_list[func_id].code[i] = OpCode::JumpIfNull(JumpTarget(temp[*v]))
                }
                OpCode::JumpPopIfFalse(JumpTarget(v)) => {
                    self.func_list[func_id].code[i] = OpCode::JumpPopIfFalse(JumpTarget(temp[*v]))
                }
                OpCode::JumpIfTrueOrPop(JumpTarget(v)) => {
                    self.func_list[func_id].code[i] = OpCode::JumpIfTrueOrPop(JumpTarget(temp[*v]))
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
            params: self.func_list[func_id]
                .params
                .iter()
                .map(|x| x.ident.name.clone())
                .collect(),
            variadic: self.func_list[func_id]
                .variadic
                .clone()
                .map(|x| x.ident.name),
            kind: self.func_list[func_id].kind,
            code: self.func_list[func_id].code.clone(),
            consts: self.func_list[func_id].consts.clone(),
            local_names: self.func_list[func_id].local_names().cloned().collect(),
            global_names: self.func_list[func_id].global_names().cloned().collect(),
            upvalue_names: self.func_list[func_id].upvalues().collect(),
            def_upvalue_count: self.func_list[func_id].def_upvalue_count,
            stack_size,
        })
    }

    fn gen_block(&mut self, func_id: usize, ast_node: &Block) -> Result<(), SyntaxError> {
        for stmt in &ast_node.body {
            self.gen_stmt(func_id, stmt)?;
        }
        Ok(())
    }

    fn gen_expr(&mut self, func_id: usize, ast_node: &Expr) -> Result<(), SyntaxError> {
        match &ast_node.kind {
            ExprKind::Lit(lit) => {
                let t = self.add_const(
                    func_id,
                    match &lit.value {
                        LitKind::Null => ConstValue::Null,
                        LitKind::Bool(v) => ConstValue::Bool(*v),
                        LitKind::Int(v) => ConstValue::Int(*v),
                        LitKind::Float(v) => ConstValue::Float(*v),
                        LitKind::Str(v) => ConstValue::Str(v.clone()),
                    },
                );
                self.func_list[func_id].code.push(OpCode::LoadConst(t));
            }
            ExprKind::Ident(ident) => self.load(func_id, &ident.name)?,
            ExprKind::Function { .. } => return Err(SyntaxError::IllegalAst),
            ExprKind::FunctionId(i) => {
                let code = self.gen_code(*i)?;
                let is_do = code.kind == FunctionKind::Do;
                let t = self.add_const(func_id, ConstValue::Func(code));
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
            ExprKind::List { items } => {
                let temp = items.len();
                for item in items {
                    self.gen_expr(func_id, item)?;
                }
                self.func_list[func_id].code.push(OpCode::BuildList(temp));
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
                        .push(OpCode::JumpIfTrueOrPop(label));
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
                safe,
            } => {
                self.gen_expr(func_id, table)?;
                let safe_label = self.get_jump_target(func_id);
                if *safe {
                    self.func_list[func_id]
                        .code
                        .push(OpCode::JumpIfNull(safe_label));
                }
                match property {
                    MemberKind::Bracket(property) => {
                        self.gen_expr(func_id, property)?;
                        self.func_list[func_id].code.push(OpCode::GetItem);
                    }
                    MemberKind::Dot(ident) | MemberKind::DoubleColon(ident) => {
                        let t = self.add_const(func_id, ConstValue::Str(ident.name.clone()));
                        self.func_list[func_id].code.push(OpCode::LoadConst(t));
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
                kind,
            } => {
                let mut temp: usize = arguments.len();
                let safe_label = self.get_jump_target(func_id);
                match callee.kind.clone() {
                    ExprKind::Member {
                        table,
                        property,

                        safe,
                    } => {
                        self.gen_expr(func_id, &table)?;
                        if safe {
                            self.func_list[func_id]
                                .code
                                .push(OpCode::JumpIfNull(safe_label));
                        }
                        match property {
                            MemberKind::Bracket(property) => {
                                self.gen_expr(func_id, &property)?;
                                self.func_list[func_id].code.push(OpCode::GetItem);
                            }
                            MemberKind::Dot(ident) => {
                                self.func_list[func_id].code.push(OpCode::Copy(1));
                                let t = self.add_const(func_id, ConstValue::Str(ident.name));
                                self.func_list[func_id].code.push(OpCode::LoadConst(t));
                                self.func_list[func_id].code.push(OpCode::GetAttr);
                                self.func_list[func_id].code.push(OpCode::Swap(2));
                                temp += 1;
                            }
                            MemberKind::DoubleColon(ident) => {
                                let t = self.add_const(func_id, ConstValue::Str(ident.name));
                                self.func_list[func_id].code.push(OpCode::LoadConst(t));
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
                self.func_list[func_id].code.push(match kind {
                    CallKind::None => OpCode::Call(temp),
                    CallKind::Try => OpCode::TryCall(temp),
                    CallKind::TryOption => OpCode::TryOptionCall(temp),
                    CallKind::TryPanic => OpCode::TryPanicCall(temp),
                });
                self.func_list[func_id]
                    .code
                    .push(OpCode::JumpTarget(safe_label));
            }
        }
        Ok(())
    }

    fn gen_stmt(&mut self, func_id: usize, ast_node: &Stmt) -> Result<(), SyntaxError> {
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
                    self.gen_block(func_id, consequent)?;
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
                    self.gen_block(func_id, consequent)?;
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
                self.gen_block(func_id, body)?;
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
                self.gen_block(func_id, body)?;
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
                self.func_list[func_id].code.push(OpCode::Iter);
                self.func_list[func_id]
                    .code
                    .push(OpCode::JumpTarget(continue_label));
                self.func_list[func_id].code.push(OpCode::Copy(1));
                self.func_list[func_id].code.push(OpCode::Call(0));
                self.func_list[func_id]
                    .code
                    .push(OpCode::JumpIfNull(break_label));
                if left.len() == 1 {
                    self.store(func_id, &left[0].name)?;
                } else {
                    for (i, l) in left.iter().enumerate() {
                        self.func_list[func_id].code.push(OpCode::Copy(1));
                        let t = self.add_const(func_id, ConstValue::Int(i.try_into().unwrap()));
                        self.func_list[func_id].code.push(OpCode::LoadConst(t));
                        self.func_list[func_id].code.push(OpCode::GetItem);
                        self.store(func_id, &l.name)?;
                    }
                    self.func_list[func_id].code.push(OpCode::Pop);
                }
                self.gen_block(func_id, body)?;
                self.func_list[func_id]
                    .code
                    .push(OpCode::Jump(continue_label));
                self.func_list[func_id]
                    .code
                    .push(OpCode::JumpTarget(break_label));
                self.func_list[func_id].code.push(OpCode::Pop);
                self.func_list[func_id].code.push(OpCode::Pop);

                self.func_list[func_id].continue_stack.pop();
                self.func_list[func_id].break_stack.pop();
            }
            StmtKind::Break => {
                let t = OpCode::Jump(match self.func_list[func_id].break_stack.last() {
                    Some(v) => *v,
                    None => return Err(SyntaxError::BreakOutsideLoop),
                });
                self.func_list[func_id].code.push(t);
            }
            StmtKind::Continue => {
                let t = OpCode::Jump(match self.func_list[func_id].continue_stack.last() {
                    Some(v) => *v,
                    None => return Err(SyntaxError::ContinueOutsideLoop),
                });
                self.func_list[func_id].code.push(t);
            }
            StmtKind::Return { argument } => {
                if self.func_list[func_id].kind == FunctionKind::Do {
                    return Err(SyntaxError::ReturnOutsideFunction);
                }
                if let ExprKind::Call { kind, .. } = argument.kind {
                    self.gen_expr(func_id, argument)?;
                    if kind != CallKind::None {
                        self.func_list[func_id].code.push(OpCode::Return);
                    } else if let OpCode::Call(i) =
                        self.func_list[func_id].code[self.func_list[func_id].code.len() - 2]
                    {
                        let t = self.func_list[func_id].code.len();
                        self.func_list[func_id].code[t - 2] = OpCode::ReturnCall(i);
                    } else {
                        self.func_list[func_id].code.push(OpCode::Return);
                    }
                } else {
                    self.gen_expr(func_id, argument)?;
                    self.func_list[func_id].code.push(OpCode::Return);
                }
            }
            StmtKind::Throw { argument } => {
                if self.func_list[func_id].kind == FunctionKind::Do {
                    return Err(SyntaxError::ThrowOutsideFunction);
                }
                self.gen_expr(func_id, argument)?;
                self.func_list[func_id].code.push(OpCode::Throw);
            }
            StmtKind::Global { arguments: _ } => {
                if self.func_list[func_id].kind == FunctionKind::Do {
                    return Err(SyntaxError::GlobalOutsideFunction);
                }
            }
            StmtKind::Import { path, kind } => {
                let path_str = path.iter().map(|x| x.name.as_str()).join("::");
                let t = self.add_const(func_id, ConstValue::Str(path_str));
                self.func_list[func_id].code.push(OpCode::Import(t));
                match kind {
                    ImportKind::Simple(alias) => self.store(func_id, &alias.name)?,
                    ImportKind::Nested(items) => {
                        for (name, alias) in items {
                            let t = self.add_const(func_id, ConstValue::Str(name.name.clone()));
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
                self.gen_assign_left(func_id, left)?;
            }
            StmtKind::AssignOp {
                operator,
                left,
                right,
            } => match &left {
                AssignLeft::Ident(TypedIdent { ident, t: _ }) => {
                    self.gen_expr(func_id, &left.clone().into())?;
                    self.gen_expr(func_id, right)?;
                    self.func_list[func_id]
                        .code
                        .push(OpCode::try_from(*operator)?);
                    self.store(func_id, &ident.name)?;
                }
                AssignLeft::Member { table, property } => {
                    self.gen_expr_member_without_get(func_id, table, property)?;
                    self.func_list[func_id].code.push(OpCode::Copy(2));
                    self.func_list[func_id].code.push(OpCode::Copy(2));
                    self.func_list[func_id].code.push(property.get_opcode());
                    self.gen_expr(func_id, right)?;
                    self.func_list[func_id]
                        .code
                        .push(OpCode::try_from(*operator)?);
                    self.func_list[func_id].code.push(OpCode::Swap(3));
                    self.func_list[func_id].code.push(OpCode::Swap(2));
                    self.func_list[func_id].code.push(property.set_opcode());
                }
                AssignLeft::MetaMember { table } => {
                    self.gen_expr(func_id, table)?;
                    self.func_list[func_id].code.push(OpCode::Copy(1));
                    self.func_list[func_id].code.push(OpCode::GetMeta);
                    self.func_list[func_id].code.push(OpCode::SetMeta);
                }
            },
            StmtKind::AssignUnpack { left, right } => {
                self.gen_expr(func_id, right)?;
                self.func_list[func_id]
                    .code
                    .extend(iter::repeat(OpCode::Copy(1)).take(left.len()));
                for (i, l) in left.iter().enumerate() {
                    let t = self.add_const(func_id, ConstValue::Int(i.try_into().unwrap()));
                    self.func_list[func_id].code.push(OpCode::LoadConst(t));
                    self.func_list[func_id].code.push(OpCode::GetItem);
                    self.gen_assign_left(func_id, l)?;
                }
            }
            StmtKind::AssignMulti { left, right } => {
                if left.len() != right.len() {
                    return Err(SyntaxError::IllegalAst);
                }
                for right in right {
                    self.gen_expr(func_id, right)?;
                }
                for left in left.iter().rev() {
                    self.gen_assign_left(func_id, left)?;
                }
            }
            StmtKind::Block(block) => self.gen_block(func_id, block)?,
            StmtKind::Expr(expr) => {
                self.gen_expr(func_id, expr)?;
                self.func_list[func_id].code.push(OpCode::Pop);
            }
        }
        Ok(())
    }

    fn gen_assign_left(
        &mut self,
        func_id: usize,
        ast_node: &AssignLeft,
    ) -> Result<(), SyntaxError> {
        match ast_node {
            AssignLeft::Ident(TypedIdent { ident, t: _ }) => self.store(func_id, &ident.name)?,
            AssignLeft::Member { table, property } => {
                self.gen_expr_member_without_get(func_id, table, property)?;
                self.func_list[func_id].code.push(property.set_opcode());
            }
            AssignLeft::MetaMember { table } => {
                self.gen_expr(func_id, table)?;
                self.func_list[func_id].code.push(OpCode::SetMeta);
            }
        }
        Ok(())
    }

    fn gen_expr_member_without_get(
        &mut self,
        func_id: usize,
        table: &Expr,
        property: &MemberKind,
    ) -> Result<(), SyntaxError> {
        self.gen_expr(func_id, table)?;
        match property {
            MemberKind::Bracket(property) => self.gen_expr(func_id, property)?,
            MemberKind::Dot(ident) | MemberKind::DoubleColon(ident) => {
                let t = self.add_const(func_id, ConstValue::Str(ident.name.clone()));
                self.func_list[func_id].code.push(OpCode::LoadConst(t));
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
            OpCode::Copy(_) => t += 1,
            OpCode::Swap(_) => (),
            OpCode::LoadLocal(_)
            | OpCode::LoadGlobal(_)
            | OpCode::LoadUpvalue(_)
            | OpCode::LoadConst(_) => t += 1,
            OpCode::StoreLocal(_) | OpCode::StoreGlobal(_) | OpCode::StoreUpvalue(_) => t -= 1,
            OpCode::Import(_) => t += 1,
            OpCode::ImportFrom(_) => t += 1,
            OpCode::ImportGlob => (),
            OpCode::BuildTable(i) => t = t - i * 2 + 1,
            OpCode::BuildList(i) => t = t - i + 1,
            OpCode::GetAttr | OpCode::GetItem => t -= 1,
            OpCode::GetMeta => (),
            OpCode::SetAttr | OpCode::SetItem => t -= 2,
            OpCode::SetMeta => t -= 1,
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
            | OpCode::Is => t -= 1,
            OpCode::Iter => (),
            OpCode::Jump(JumpTarget(_)) => (),
            OpCode::JumpIfNull(JumpTarget(i)) => {
                stack_size = stack_size.max(get_stack_size(code, i, t));
            }
            OpCode::JumpPopIfFalse(JumpTarget(i)) => {
                t -= 1;
                stack_size = stack_size.max(get_stack_size(code, i, t));
            }
            OpCode::JumpIfTrueOrPop(JumpTarget(i)) | OpCode::JumpIfFalseOrPop(JumpTarget(i)) => {
                stack_size = stack_size.max(get_stack_size(code, i, t));
                t -= 1;
            }
            OpCode::Call(i)
            | OpCode::TryCall(i)
            | OpCode::TryOptionCall(i)
            | OpCode::TryPanicCall(i) => t = t - i + 1,
            OpCode::Return | OpCode::Throw => break,
            OpCode::ReturnCall(i) => t = t - i + 1,
            OpCode::JumpTarget(_) => panic!(),
        }
        stack_size = stack_size.max(t);
        offset += 1;
    }
    stack_size
}

/// Kind of SyntaxError.
#[derive(Error, Debug, Clone, PartialEq)]
pub enum SyntaxError {
    #[error("illegal ast")]
    IllegalAst,
    #[error("break outside loop")]
    BreakOutsideLoop,
    #[error("continue outside loop")]
    ContinueOutsideLoop,
    #[error("global outside function")]
    GlobalOutsideFunction,
    #[error("return outside function")]
    ReturnOutsideFunction,
    #[error("throw outside function")]
    ThrowOutsideFunction,
    #[error(transparent)]
    ParserError(#[from] ParserError),
}
