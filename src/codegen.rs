use std::cmp::max;
use std::convert::TryFrom;
use std::fmt::{Debug, Display};

use crate::ast::*;
use crate::errors::{Error, Result, SyntaxError};
use crate::lexer::tokenize;
use crate::parser::Parser;

/// The const value.
#[derive(Debug, Clone, PartialEq)]
pub enum ConstlValue {
    /// "null"
    Null,
    /// "true", "false"
    Bool(bool),
    /// "12", "0o100", "0b110"
    Int(i64),
    /// "12.34", "0b100.100"
    Float(f64),
    /// ""abc"", ""abc"
    Str(String),
    /// func id
    Func(Function),
}

impl From<LitKind> for ConstlValue {
    fn from(value: LitKind) -> Self {
        match value {
            LitKind::Null => ConstlValue::Null,
            LitKind::Bool(v) => ConstlValue::Bool(v),
            LitKind::Int(v) => ConstlValue::Int(v),
            LitKind::Float(v) => ConstlValue::Float(v),
            LitKind::Str(v) => ConstlValue::Str(v),
        }
    }
}

/// The jump target.
/// Note that this is only used during code generation and will not appear in the `Program`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct JumpTarget(pub usize);

/// The operation code.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum OpCode {
    /// Removes the top-of-stack (TOS) item.
    Pop,
    /// Duplicates the reference on top of the stack.
    Dup,
    /// Duplicates the two references on top of the stack, leaving them in the same order.
    DupTwo,
    /// Swaps the two top-most stack items.
    RotTwo,
    /// Lifts second and third stack item one position up, moves top down to position three.
    RotThree,
    /// Pushes the value associated with `local_names[namei]` onto the stack.
    LoadLocal(usize),
    /// Pushes the value associated with `global_names[namei]` onto the stack.
    LoadGlobal(usize),
    /// Pushes the value associated with `upvalue_names[namei]` onto the stack.
    LoadUpvalue(usize),
    /// Pushes `consts[consti]` onto the stack.
    LoadConst(usize),
    /// Stores TOS into the `local_names[namei]`.
    StoreLocal(usize),
    /// Stores TOS into the `global_names[namei]`.
    StoreGlobal(usize),
    /// Stores TOS into the `upvalue_names[namei]`.
    StoreUpvalue(usize),

    /// Imports the module `consts[consti]` and pushed it onto the stack.
    Import(usize),
    /// Loads the attribute `consts[consti]` the module found in TOS and pushed it onto the stack.
    ImportFrom(usize),
    /// LoLoads all symbols from the module TOS to the global namespace.
    ImportGlob,

    /// Pushes a new table onto the stack. Pops `2 * count` items to build table.
    BuildTable(usize),
    /// Implements `TOS = TOS1::TOS`.
    GetAttr,
    /// Implements `TOS = TOS1[TOS]`.
    GetItem,
    /// Implements `TOS = TOS[#]`.
    GetMeta,
    /// Implements `TOS1::TOS = TOS2`.
    SetAttr,
    /// Implements `TOS1[TOS] = TOS2`.
    SetItem,
    /// Implements `TOS[#] = TOS1`.
    SetMeta,

    /// Implements `TOS = -TOS`.
    Neg,
    /// Implements `TOS = not TOS`.
    Not,

    /// Implements `TOS = TOS1 + TOS`.
    Add,
    /// Implements `TOS = TOS1 - TOS`.
    Sub,
    /// Implements `TOS = TOS1 * TOS`.
    Mul,
    /// Implements `TOS = TOS1 / TOS`.
    Div,
    /// Implements `TOS = TOS1 % TOS`.
    Mod,

    /// Implements `TOS = TOS1 == TOS`.
    Eq,
    /// Implements `TOS = TOS1 != TOS`.
    Ne,
    /// Implements `TOS = TOS1 > TOS`.
    Gt,
    /// Implements `TOS = TOS1 >= TOS`.
    Ge,
    /// Implements `TOS = TOS1 < TOS`.
    Lt,
    /// Implements `TOS = TOS1 <= TOS`.
    Le,
    /// Implements `TOS = TOS1 is TOS`.
    Is,

    ///
    For(JumpTarget),
    /// Sets the bytecode counter to target.
    Jump(JumpTarget),
    /// If TOS is null, sets the bytecode counter to target.
    JumpIfNull(JumpTarget),
    /// If TOS is false, sets the bytecode counter to target. TOS is popped.
    JumpPopIfFalse(JumpTarget),
    /// If TOS is true, sets the bytecode counter to target and leaves TOS on the stack. Otherwise, TOS is popped.
    JumpIfTureOrPop(JumpTarget),
    /// If TOS is false, sets the bytecode counter to target and leaves TOS on the stack. Otherwise, TOS is popped.
    JumpIfFalseOrPop(JumpTarget),

    /// Pops numbers of item for function arguments, then pop an callable value and call it.
    Call(usize),
    /// Call with a shortcut for propagating errors.
    TryCall(usize),
    /// Returns with TOS to the caller of the function.
    Return,
    /// Returns with TOS as a error.
    Throw,
    /// Same as "Call(usize); Return;", this is for tail call optimization.
    ReturnCall(usize),

    /// A jump target, only used during code generation.
    JumpTarget(JumpTarget),
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

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

impl From<UnOp> for OpCode {
    fn from(value: UnOp) -> Self {
        match value {
            UnOp::Not => OpCode::Not,
            UnOp::Neg => OpCode::Neg,
        }
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

/// Kind of function.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionKind {
    Funciton,
    Closure,
    Do,
}

/// A function.
#[derive(Debug, Clone)]
pub struct Function {
    pub params: Vec<String>,
    pub variadic: Option<String>,
    pub code_list: Vec<OpCode>,
    pub kind: FunctionKind,

    pub const_list: Vec<ConstlValue>,
    pub local_names: Vec<String>,
    pub global_names: Vec<String>,
    pub upvalue_names: Vec<(String, usize, usize)>,

    pub stack_size: usize,
}

impl PartialEq for Function {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

impl Function {
    pub fn new() -> Self {
        Function {
            params: Vec::new(),
            variadic: None,
            code_list: Vec::new(),
            kind: FunctionKind::Funciton,
            const_list: Vec::new(),
            local_names: Vec::new(),
            global_names: Vec::new(),
            upvalue_names: Vec::new(),
            stack_size: 0,
        }
    }
}

impl Default for Function {
    fn default() -> Self {
        Self::new()
    }
}

impl TryFrom<&str> for Function {
    type Error = Error;

    fn try_from(value: &str) -> Result<Self> {
        gen_code(Parser::new(&mut tokenize(value)).parse()?)
    }
}

impl TryFrom<&String> for Function {
    type Error = Error;

    fn try_from(value: &String) -> Result<Self> {
        gen_code(Parser::new(&mut tokenize(value)).parse()?)
    }
}

pub fn gen_code(ast_root: Box<Block>) -> Result<Function> {
    let t = FunctionBuilder::new(ast_root, Vec::new(), None, FunctionKind::Funciton);
    t.gen_code()
}

#[derive(Debug, Clone)]
pub struct FunctionBuilder<'a> {
    code: Box<Block>,

    pub params: Vec<String>,
    pub variadic: Option<String>,
    pub code_list: Vec<OpCode>,
    pub kind: FunctionKind,

    pub const_list: Vec<ConstlValue>,
    pub local_names: Vec<String>,
    pub global_names: Vec<(String, bool /* writable */)>,
    pub upvalue_names: Vec<(String, usize, usize)>,

    base_function: Option<&'a FunctionBuilder<'a>>,
    func_list: Vec<FunctionBuilder<'a>>,
    jump_target_count: usize,
    continue_stack: Vec<JumpTarget>,
    break_stack: Vec<JumpTarget>,
}

impl<'a> FunctionBuilder<'a> {
    pub fn new(
        code: Box<Block>,
        params: Vec<String>,
        variadic: Option<String>,
        kind: FunctionKind,
    ) -> Self {
        FunctionBuilder {
            code,
            params,
            variadic,
            code_list: Vec::new(),
            kind,
            base_function: None,
            const_list: Vec::new(),
            local_names: Vec::new(),
            global_names: Vec::new(),
            upvalue_names: Vec::new(),
            func_list: Vec::new(),
            jump_target_count: 0,
            continue_stack: Vec::new(),
            break_stack: Vec::new(),
        }
    }

    fn get_jump_target(&mut self) -> JumpTarget {
        self.jump_target_count += 1;
        JumpTarget(self.jump_target_count - 1)
    }

    fn add_const(&mut self, value: ConstlValue) -> usize {
        if let Some(index) = self.const_list.iter().position(|x| *x == value) {
            index
        } else {
            self.const_list.push(value);
            self.const_list.len() - 1
        }
    }

    fn add_local_name(&mut self, name: &String) -> usize {
        if let Some(index) = self.local_names.iter().position(|x| x == name) {
            index
        } else {
            self.local_names.push(name.clone());
            self.local_names.len() - 1
        }
    }

    fn add_global_name(&mut self, name: &String) -> usize {
        if let Some(index) = self.global_names.iter().position(|(x, _)| x == name) {
            index
        } else {
            self.global_names.push((name.clone(), false));
            self.global_names.len() - 1
        }
    }

    fn load(&mut self, name: &String) -> OpCode {
        if let Some(index) = self.local_names.iter().position(|x| x == name) {
            OpCode::LoadLocal(index)
        } else if let Some(index) = self.global_names.iter().position(|(x, _)| x == name) {
            OpCode::LoadGlobal(index)
        } else if !(self.kind == FunctionKind::Closure) {
            OpCode::LoadGlobal(self.add_global_name(name))
        } else {
            let mut base_func_count = 0;
            let mut base_func = self.base_function.unwrap();
            loop {
                if let Some(i) = base_func.local_names.iter().position(|x| x == name) {
                    self.upvalue_names.push((name.clone(), base_func_count, i));
                    break OpCode::LoadUpvalue(self.upvalue_names.len() - 1);
                }
                if self.kind != FunctionKind::Closure {
                    break OpCode::LoadGlobal(self.add_global_name(name));
                }
                if let Some(func) = base_func.base_function {
                    base_func = func;
                    base_func_count += 1;
                } else {
                    break OpCode::LoadGlobal(self.add_global_name(name));
                }
            }
        }
    }

    fn store(&mut self, name: &String) -> OpCode {
        if let Some(index) = self.local_names.iter().position(|x| x == name) {
            OpCode::StoreLocal(index)
        } else if let Some(index) = self.global_names.iter().position(|(x, _)| x == name) {
            if self.global_names[index].1 {
                OpCode::StoreGlobal(index)
            } else {
                OpCode::StoreLocal(self.add_local_name(name))
            }
        } else if !(self.kind == FunctionKind::Closure) {
            OpCode::StoreLocal(self.add_local_name(name))
        } else {
            let mut base_func_count = 0;
            let mut base_func = self.base_function.unwrap();
            loop {
                if let Some(i) = base_func.local_names.iter().position(|x| x == name) {
                    self.upvalue_names.push((name.clone(), base_func_count, i));
                    break OpCode::StoreUpvalue(self.upvalue_names.len() - 1);
                }
                if self.kind != FunctionKind::Closure {
                    break OpCode::StoreLocal(self.add_local_name(name));
                }
                if let Some(func) = base_func.base_function {
                    base_func = func;
                    base_func_count += 1;
                } else {
                    break OpCode::StoreLocal(self.add_local_name(name));
                }
            }
        }
    }

    pub fn gen_code(mut self) -> Result<Function> {
        for param in self.params.clone() {
            self.add_local_name(&param);
        }
        if let Some(v) = self.variadic.clone() {
            self.add_local_name(&v);
        }
        for stmt in self.code.body.clone() {
            let t = &mut self.gen_stmt(&stmt)?;
            self.code_list.append(t);
        }
        if self.kind == FunctionKind::Do {
            self.code_list.push(OpCode::Return);
        } else if *self.code_list.last().unwrap_or(&OpCode::Pop) != OpCode::Return {
            let t = self.add_const(ConstlValue::Null);
            self.code_list.push(OpCode::LoadConst(t));
            self.code_list.push(OpCode::Return);
        }

        let mut temp: Vec<usize> = vec![0; self.jump_target_count];
        let mut i = 0;
        while i < self.code_list.len() {
            match self.code_list[i] {
                OpCode::JumpTarget(JumpTarget(index)) => {
                    temp[index] = i;
                    self.code_list.remove(i);
                }
                _ => i += 1,
            }
        }
        let mut i = 0;
        while i < self.code_list.len() {
            self.code_list[i] = match &self.code_list[i] {
                OpCode::For(JumpTarget(v)) => OpCode::For(JumpTarget(temp[*v])),
                OpCode::Jump(JumpTarget(v)) => OpCode::Jump(JumpTarget(temp[*v])),
                OpCode::JumpIfNull(JumpTarget(v)) => OpCode::JumpIfNull(JumpTarget(temp[*v])),
                OpCode::JumpPopIfFalse(JumpTarget(v)) => {
                    OpCode::JumpPopIfFalse(JumpTarget(temp[*v]))
                }
                OpCode::JumpIfTureOrPop(JumpTarget(v)) => {
                    OpCode::JumpIfTureOrPop(JumpTarget(temp[*v]))
                }
                OpCode::JumpIfFalseOrPop(JumpTarget(v)) => {
                    OpCode::JumpIfFalseOrPop(JumpTarget(temp[*v]))
                }
                v => v.clone(),
            };
            i += 1;
        }

        let stack_size = get_stack_size(&self.code_list, 0, 0);
        let mut func_list = Vec::new();
        for func in self.func_list.clone() {
            let mut func = func.clone();
            func.base_function = Some(&self);
            func_list.push(func.gen_code()?);
        }

        let mut t = func_list.into_iter();

        for v in &mut self.const_list {
            if let ConstlValue::Func(func) = v {
                *func = t.next().unwrap();
            }
        }

        Ok(Function {
            params: self.params,
            variadic: self.variadic,
            code_list: self.code_list,
            kind: self.kind,
            const_list: self.const_list,
            local_names: self.local_names,
            global_names: self.global_names.iter().map(|(x, _)| x.clone()).collect(),
            upvalue_names: self.upvalue_names,
            stack_size,
        })
    }

    fn gen_expr(&mut self, ast_node: &Expr) -> Result<Vec<OpCode>> {
        let mut code_list = Vec::new();
        match &ast_node.kind {
            ExprKind::Lit(lit) => code_list.push(OpCode::LoadConst(
                self.add_const(ConstlValue::from(lit.value.clone())),
            )),
            ExprKind::Ident(ident) => code_list.push(self.load(&ident.name)),
            ExprKind::Do(body) => {
                self.func_list.push(FunctionBuilder::new(
                    body.clone(),
                    Vec::new(),
                    None,
                    FunctionKind::Do,
                ));
                code_list.push(OpCode::LoadConst(
                    self.add_const(ConstlValue::Func(Function::new())),
                ));
                code_list.push(OpCode::Call(0));
            }
            ExprKind::Function {
                params,
                variadic,
                body,
                is_closure,
            } => {
                self.func_list.push(FunctionBuilder::new(
                    body.clone(),
                    params.iter().map(|x| x.name.clone()).collect(),
                    variadic.clone().map(|v| v.name),
                    if *is_closure {
                        FunctionKind::Closure
                    } else {
                        FunctionKind::Funciton
                    },
                ));
                code_list.push(OpCode::LoadConst(
                    self.add_const(ConstlValue::Func(Function::new())),
                ));
            }
            ExprKind::Table { properties } => {
                let temp = properties.len();
                for TableProperty { key, value, .. } in properties {
                    code_list.append(&mut self.gen_expr(key)?);
                    code_list.append(&mut self.gen_expr(value)?);
                }
                code_list.push(OpCode::BuildTable(temp));
            }
            ExprKind::Unary { operator, argument } => {
                code_list.append(&mut self.gen_expr(argument)?);
                code_list.push(OpCode::from(*operator));
            }
            ExprKind::Binary {
                operator,
                left,
                right,
            } => match operator {
                BinOp::And => {
                    let label = self.get_jump_target();
                    code_list.append(&mut self.gen_expr(left)?);
                    code_list.push(OpCode::JumpIfFalseOrPop(label));
                    code_list.append(&mut self.gen_expr(right)?);
                    code_list.push(OpCode::JumpTarget(label));
                }
                BinOp::Or => {
                    let label = self.get_jump_target();
                    code_list.append(&mut self.gen_expr(left)?);
                    code_list.push(OpCode::JumpIfTureOrPop(label));
                    code_list.append(&mut self.gen_expr(right)?);
                    code_list.push(OpCode::JumpTarget(label));
                }
                operator => {
                    code_list.append(&mut self.gen_expr(left)?);
                    code_list.append(&mut self.gen_expr(right)?);
                    code_list.push(OpCode::try_from(*operator)?);
                }
            },
            ExprKind::Member {
                table,
                property,
                kind,
                safe,
            } => {
                code_list.append(&mut self.gen_expr(table)?);
                let safe_label = self.get_jump_target();
                if *safe {
                    code_list.push(OpCode::JumpIfNull(safe_label));
                }
                match kind {
                    MemberKind::Bracket => {
                        code_list.append(&mut self.gen_expr(property)?);
                        code_list.push(OpCode::GetItem);
                    }
                    MemberKind::Dot | MemberKind::DoubleColon => {
                        match &property.kind {
                            ExprKind::Ident(ident) => {
                                code_list.push(OpCode::LoadConst(
                                    self.add_const(ConstlValue::Str(ident.name.clone())),
                                ));
                            }
                            _ => return Err(SyntaxError::IllegalAst.into()),
                        }
                        code_list.push(OpCode::GetAttr);
                    }
                }
                code_list.push(OpCode::JumpTarget(safe_label));
            }
            ExprKind::MetaMember { table, safe } => {
                code_list.append(&mut self.gen_expr(table)?);
                let safe_label = self.get_jump_target();
                if *safe {
                    code_list.push(OpCode::JumpIfNull(safe_label));
                }
                code_list.push(OpCode::GetMeta);
                code_list.push(OpCode::JumpTarget(safe_label));
            }
            ExprKind::Call {
                callee,
                arguments,
                propagating_error,
            } => {
                let mut temp: usize = arguments.len();
                let safe_label = self.get_jump_target();
                match callee.kind.clone() {
                    ExprKind::Member {
                        table,
                        property,
                        kind,
                        safe,
                    } => {
                        code_list.append(&mut self.gen_expr(&table)?);
                        if safe {
                            code_list.push(OpCode::JumpIfNull(safe_label));
                        }
                        match kind {
                            MemberKind::Bracket => {
                                code_list.append(&mut self.gen_expr(&property)?);
                                code_list.push(OpCode::GetItem);
                            }
                            MemberKind::Dot => {
                                code_list.push(OpCode::Dup);
                                match property.kind {
                                    ExprKind::Ident(ident) => {
                                        code_list.push(OpCode::LoadConst(
                                            self.add_const(ConstlValue::Str(ident.name)),
                                        ));
                                    }
                                    _ => return Err(SyntaxError::IllegalAst.into()),
                                }
                                code_list.push(OpCode::GetAttr);
                                code_list.push(OpCode::RotTwo);
                                temp += 1;
                            }
                            MemberKind::DoubleColon => {
                                match property.kind {
                                    ExprKind::Ident(ident) => {
                                        code_list.push(OpCode::LoadConst(
                                            self.add_const(ConstlValue::Str(ident.name)),
                                        ));
                                    }
                                    _ => return Err(SyntaxError::IllegalAst.into()),
                                }
                                code_list.push(OpCode::GetAttr);
                            }
                        }
                    }
                    _ => {
                        code_list.append(&mut self.gen_expr(callee)?);
                    }
                }
                for arg in arguments {
                    code_list.append(&mut self.gen_expr(arg)?);
                }
                code_list.push(if *propagating_error {
                    OpCode::TryCall(temp)
                } else {
                    OpCode::Call(temp)
                });
                code_list.push(OpCode::JumpTarget(safe_label));
            }
        }
        Ok(code_list)
    }

    fn gen_stmt(&mut self, ast_node: &Stmt) -> Result<Vec<OpCode>> {
        let mut code_list = Vec::new();
        macro_rules! gen_block {
            ($block:expr) => {
                for stmt in &$block.body {
                    code_list.append(&mut self.gen_stmt(stmt)?);
                }
            };
        }
        macro_rules! gen_expr_member_without_get {
            ($table:expr, $property:expr, $kind:expr, $safe:expr) => {{
                if $safe {
                    return Err(SyntaxError::IllegalAst.into());
                }
                code_list.append(&mut self.gen_expr(&$table)?);
                match $kind {
                    MemberKind::Bracket => {
                        code_list.append(&mut self.gen_expr(&$property)?);
                    }
                    MemberKind::Dot | MemberKind::DoubleColon => match $property.kind {
                        ExprKind::Ident(ident) => {
                            code_list.push(OpCode::LoadConst(
                                self.add_const(ConstlValue::Str(ident.name)),
                            ));
                        }
                        _ => return Err(SyntaxError::IllegalAst.into()),
                    },
                }
            }};
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
                    let false_label = self.get_jump_target();
                    let end_label = self.get_jump_target();
                    code_list.append(&mut self.gen_expr(test)?);
                    code_list.push(OpCode::JumpPopIfFalse(false_label));
                    gen_block!(consequent);
                    code_list.push(OpCode::Jump(end_label));
                    code_list.push(OpCode::JumpTarget(false_label));
                    code_list.append(&mut self.gen_stmt(alternate)?);
                    code_list.push(OpCode::JumpTarget(end_label));
                } else {
                    let end_label = self.get_jump_target();
                    code_list.append(&mut self.gen_expr(test)?);
                    code_list.push(OpCode::JumpPopIfFalse(end_label));
                    gen_block!(consequent);
                    code_list.push(OpCode::JumpTarget(end_label));
                }
            }
            StmtKind::Loop { body } => {
                let continue_label = self.get_jump_target();
                let break_label = self.get_jump_target();
                self.continue_stack.push(continue_label);
                self.break_stack.push(break_label);

                code_list.push(OpCode::JumpTarget(continue_label));
                gen_block!(body);
                code_list.push(OpCode::Jump(continue_label));
                code_list.push(OpCode::JumpTarget(break_label));

                self.continue_stack.pop();
                self.break_stack.pop();
            }
            StmtKind::While { test, body } => {
                let continue_label = self.get_jump_target();
                let break_label = self.get_jump_target();
                self.continue_stack.push(continue_label);
                self.break_stack.push(break_label);

                code_list.push(OpCode::JumpTarget(continue_label));
                code_list.append(&mut self.gen_expr(test)?);
                code_list.push(OpCode::JumpPopIfFalse(break_label));
                gen_block!(body);
                code_list.push(OpCode::Jump(continue_label));
                code_list.push(OpCode::JumpTarget(break_label));

                self.continue_stack.pop();
                self.break_stack.pop();
            }
            StmtKind::For { left, right, body } => {
                let continue_label = self.get_jump_target();
                let break_label = self.get_jump_target();
                self.continue_stack.push(continue_label);
                self.break_stack.push(break_label);

                code_list.append(&mut self.gen_expr(right)?);
                code_list.push(OpCode::JumpTarget(continue_label));
                code_list.push(OpCode::For(break_label));
                if left.len() == 1 {
                    code_list.push(OpCode::StoreLocal(self.add_local_name(&left[0].name)));
                } else {
                    for (i, l) in left.iter().enumerate() {
                        code_list.push(OpCode::Dup);
                        code_list.push(OpCode::LoadConst(
                            self.add_const(ConstlValue::Int(i.try_into().unwrap())),
                        ));
                        code_list.push(OpCode::GetItem);
                        code_list.push(OpCode::StoreLocal(self.add_local_name(&l.name)));
                    }
                    code_list.push(OpCode::Pop);
                }
                gen_block!(body);
                code_list.push(OpCode::Jump(continue_label));
                code_list.push(OpCode::JumpTarget(break_label));
                code_list.push(OpCode::Pop);

                self.continue_stack.pop();
                self.break_stack.pop();
            }
            StmtKind::Break => {
                code_list.push(OpCode::Jump(match self.break_stack.last() {
                    Some(v) => *v,
                    None => return Err(SyntaxError::BreakOutsideLoop.into()),
                }));
            }
            StmtKind::Continue => {
                code_list.push(OpCode::Jump(match self.continue_stack.last() {
                    Some(v) => *v,
                    None => return Err(SyntaxError::ContinueOutsideLoop.into()),
                }));
            }
            StmtKind::Return { argument } => {
                if self.kind == FunctionKind::Do {
                    return Err(SyntaxError::ReturnOutsideFunction.into());
                }
                if let ExprKind::Call {
                    propagating_error, ..
                } = argument.kind.clone()
                {
                    code_list.append(&mut self.gen_expr(argument)?);
                    if propagating_error {
                        code_list.push(OpCode::Return);
                    } else if let OpCode::Call(i) = code_list[code_list.len() - 2] {
                        let t = code_list.len();
                        code_list[t - 2] = OpCode::ReturnCall(i);
                    }
                } else {
                    code_list.append(&mut self.gen_expr(argument)?);
                    code_list.push(OpCode::Return);
                }
            }
            StmtKind::Throw { argument } => {
                if self.kind == FunctionKind::Do {
                    return Err(SyntaxError::ThrowOutsideFunction.into());
                }
                code_list.append(&mut self.gen_expr(argument)?);
                code_list.push(OpCode::Throw);
            }
            StmtKind::Global { arguments } => {
                if self.kind == FunctionKind::Do {
                    return Err(SyntaxError::GlobalOutsideFunction.into());
                }
                for arg in arguments {
                    self.global_names.push((arg.name.clone(), true));
                }
            }
            StmtKind::Import { path, kind } => {
                let path_str = path
                    .iter()
                    .map(|x| x.name.clone())
                    .collect::<Vec<String>>()
                    .join("::");
                code_list.push(OpCode::Import(self.add_const(ConstlValue::Str(path_str))));
                match kind {
                    ImportKind::Simple(alias) => {
                        code_list.push(OpCode::StoreGlobal(self.add_global_name(&alias.name)));
                    }
                    ImportKind::Nested(items) => {
                        for (name, alias) in items {
                            code_list.push(OpCode::ImportFrom(
                                self.add_const(ConstlValue::Str(name.name.clone())),
                            ));
                            code_list.push(OpCode::StoreGlobal(self.add_global_name(&alias.name)));
                        }
                        code_list.push(OpCode::Pop);
                    }
                    ImportKind::Glob => {
                        code_list.push(OpCode::ImportGlob);
                    }
                }
            }
            StmtKind::Assign { left, right } => {
                code_list.append(&mut self.gen_expr(right)?);
                match left.kind.clone() {
                    ExprKind::Ident(ident) => code_list.push(self.store(&ident.name)),
                    ExprKind::Member {
                        table,
                        property,
                        kind,
                        safe,
                    } => {
                        gen_expr_member_without_get!(table, property, kind, safe);
                        code_list.push(match kind {
                            MemberKind::Bracket => OpCode::SetItem,
                            MemberKind::Dot | MemberKind::DoubleColon => OpCode::SetAttr,
                        });
                    }
                    ExprKind::MetaMember { table, safe } => {
                        if safe {
                            return Err(SyntaxError::IllegalAst.into());
                        }
                        code_list.append(&mut self.gen_expr(&table)?);
                        code_list.push(OpCode::SetMeta);
                    }
                    _ => return Err(SyntaxError::IllegalAst.into()),
                }
            }
            StmtKind::AssignOp {
                operator,
                left,
                right,
            } => match left.kind.clone() {
                ExprKind::Ident(ident) => {
                    code_list.append(&mut self.gen_expr(left)?);
                    code_list.append(&mut self.gen_expr(right)?);
                    code_list.push(OpCode::try_from(*operator)?);
                    code_list.push(self.store(&ident.name));
                }
                ExprKind::Member {
                    table,
                    property,
                    kind,
                    safe,
                } => {
                    gen_expr_member_without_get!(table, property, kind, safe);
                    code_list.push(OpCode::DupTwo);
                    code_list.push(match kind {
                        MemberKind::Bracket => OpCode::GetItem,
                        MemberKind::Dot | MemberKind::DoubleColon => OpCode::GetAttr,
                    });
                    code_list.append(&mut self.gen_expr(right)?);
                    code_list.push(OpCode::try_from(*operator)?);
                    code_list.push(OpCode::RotThree);
                    code_list.push(match kind {
                        MemberKind::Bracket => OpCode::SetItem,
                        MemberKind::Dot | MemberKind::DoubleColon => OpCode::SetAttr,
                    });
                }
                ExprKind::MetaMember { table, safe } => {
                    if safe {
                        return Err(SyntaxError::IllegalAst.into());
                    }
                    code_list.append(&mut self.gen_expr(&table)?);
                    code_list.push(OpCode::Dup);
                    code_list.push(OpCode::GetMeta);
                    code_list.push(OpCode::SetMeta);
                }
                _ => return Err(SyntaxError::IllegalAst.into()),
            },
            StmtKind::AssignUnpack { left, right } => {
                let right_expr = self.gen_expr(right)?;
                for (i, l) in left.iter().enumerate() {
                    code_list.append(&mut right_expr.clone());
                    code_list.push(OpCode::LoadConst(
                        self.add_const(ConstlValue::Int(i.try_into().unwrap())),
                    ));
                    code_list.push(OpCode::GetItem);
                    match &l.kind {
                        ExprKind::Ident(ident) => {
                            code_list.push(self.store(&ident.name));
                        }
                        ExprKind::Member {
                            table,
                            property,
                            kind,
                            safe,
                        } => {
                            gen_expr_member_without_get!(
                                table.clone(),
                                property.clone(),
                                *kind,
                                *safe
                            );
                            code_list.push(match kind {
                                MemberKind::Bracket => OpCode::SetItem,
                                MemberKind::Dot | MemberKind::DoubleColon => OpCode::SetAttr,
                            });
                        }
                        ExprKind::MetaMember { table, safe } => {
                            if *safe {
                                return Err(SyntaxError::IllegalAst.into());
                            }
                            code_list.append(&mut self.gen_expr(table)?);
                            code_list.push(OpCode::SetMeta);
                        }
                        _ => return Err(SyntaxError::IllegalAst.into()),
                    }
                }
            }
            StmtKind::AssignMulti { left, right } => {
                if left.len() != right.len() {
                    return Err(SyntaxError::IllegalAst.into());
                }
                for right in right {
                    code_list.append(&mut self.gen_expr(right)?);
                }
                for left in left.iter().rev() {
                    match left.kind.clone() {
                        ExprKind::Ident(ident) => {
                            code_list.push(self.store(&ident.name));
                        }
                        ExprKind::Member {
                            table,
                            property,
                            kind,
                            safe,
                        } => {
                            gen_expr_member_without_get!(table, property, kind, safe);
                            code_list.push(match kind {
                                MemberKind::Bracket => OpCode::SetItem,
                                MemberKind::Dot | MemberKind::DoubleColon => OpCode::SetAttr,
                            });
                        }
                        ExprKind::MetaMember { table, safe } => {
                            if safe {
                                return Err(SyntaxError::IllegalAst.into());
                            }
                            code_list.append(&mut self.gen_expr(&table)?);
                            code_list.push(OpCode::SetMeta);
                        }
                        _ => return Err(SyntaxError::IllegalAst.into()),
                    }
                }
            }
            StmtKind::Block(block) => gen_block!(block),
            StmtKind::Expr(expr) => {
                code_list.append(&mut self.gen_expr(expr)?);
                code_list.push(OpCode::Pop);
            }
        }
        Ok(code_list)
    }
}
