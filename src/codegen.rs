use std::cmp::max;
use std::convert::TryFrom;
use std::fmt::{Debug, Display};

use crate::ast::*;
use crate::errors::{Error, Result, SyntaxError};
use crate::lexer::tokenize;
use crate::parser::Parser;

/// The const value.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
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
    Func(usize),
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
pub enum OPCode {
    /// Removes the top-of-stack (TOS) item.
    Pop,
    /// Duplicates the reference on top of the stack.
    Dup,
    /// Duplicates the two references on top of the stack, leaving them in the same order.
    DupTwo,
    /// Swaps the two top-most stack items.
    Rot,
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
    /// Implements `TOS1::TOS = TOS2`.
    SetAttr,
    /// Implements `TOS1[TOS] = TOS2`.
    SetItem,

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

impl Display for OPCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

impl TryFrom<BinOp> for OPCode {
    type Error = Error;

    fn try_from(value: BinOp) -> Result<Self> {
        Ok(match value {
            BinOp::Add => OPCode::Add,
            BinOp::Sub => OPCode::Sub,
            BinOp::Mul => OPCode::Mul,
            BinOp::Div => OPCode::Div,
            BinOp::Mod => OPCode::Mod,
            BinOp::Eq => OPCode::Eq,
            BinOp::Lt => OPCode::Lt,
            BinOp::Le => OPCode::Le,
            BinOp::Ne => OPCode::Ne,
            BinOp::Ge => OPCode::Ge,
            BinOp::Gt => OPCode::Gt,
            BinOp::Is => OPCode::Is,
            _ => return Err(SyntaxError::IllegalAst.into()),
        })
    }
}

impl From<UnOp> for OPCode {
    fn from(value: UnOp) -> Self {
        match value {
            UnOp::Not => OPCode::Not,
            UnOp::Neg => OPCode::Neg,
        }
    }
}

/// Generate code from AST.
pub fn gen_code(ast_root: Box<Block>) -> Result<Program> {
    let mut context = Context::new();
    let func = FunctionBuilder::new(ast_root, 0, None, Vec::new(), None, FunctionKind::Funciton);
    context.func_list.push(func);

    let mut func_count = 0;
    while func_count < context.func_list.len() {
        let mut func = context.func_list[func_count].clone();
        func.gen_code(&mut context)?;
        context.func_list[func_count] = func;
        func_count += 1;
    }

    let mut temp: Vec<usize> = vec![0; context.jump_target_count];
    for func in &mut context.func_list {
        let mut i = 0;
        while i < func.code_list.len() {
            match func.code_list[i] {
                OPCode::JumpTarget(JumpTarget(index)) => {
                    temp[index] = i;
                    func.code_list.remove(i);
                }
                _ => i += 1,
            }
        }
    }
    for func in &mut context.func_list {
        let mut i = 0;
        while i < func.code_list.len() {
            func.code_list[i] = match &func.code_list[i] {
                OPCode::For(JumpTarget(v)) => OPCode::For(JumpTarget(temp[*v])),
                OPCode::Jump(JumpTarget(v)) => OPCode::Jump(JumpTarget(temp[*v])),
                OPCode::JumpIfNull(JumpTarget(v)) => OPCode::JumpIfNull(JumpTarget(temp[*v])),
                OPCode::JumpPopIfFalse(JumpTarget(v)) => {
                    OPCode::JumpPopIfFalse(JumpTarget(temp[*v]))
                }
                OPCode::JumpIfTureOrPop(JumpTarget(v)) => {
                    OPCode::JumpIfTureOrPop(JumpTarget(temp[*v]))
                }
                OPCode::JumpIfFalseOrPop(JumpTarget(v)) => {
                    OPCode::JumpIfFalseOrPop(JumpTarget(temp[*v]))
                }
                v => v.clone(),
            };
            i += 1;
        }
    }

    for func in &mut context.func_list {
        func.stack_size = get_stack_size(&func.code_list, 0, 0);
    }

    Ok(Program::from(context))
}

/// Try estimate function stack size.
fn get_stack_size(code: &Vec<OPCode>, mut offset: usize, init_size: usize) -> usize {
    let mut stack_size = init_size;
    let mut t = init_size;
    while offset < code.len() {
        match code[offset] {
            OPCode::Pop => t += 1,
            OPCode::Dup => t += 1,
            OPCode::DupTwo => t += 2,
            OPCode::Rot => (),
            OPCode::LoadLocal(_)
            | OPCode::LoadGlobal(_)
            | OPCode::LoadUpvalue(_)
            | OPCode::LoadConst(_) => t += 1,
            OPCode::StoreLocal(_) | OPCode::StoreGlobal(_) | OPCode::StoreUpvalue(_) => t -= 1,
            OPCode::Import(_) => t += 1,
            OPCode::ImportFrom(_) => t += 1,
            OPCode::ImportGlob => (),
            OPCode::BuildTable(i) => t = t - i * 2 + 1,
            OPCode::GetAttr | OPCode::GetItem => t -= 1,
            OPCode::SetAttr | OPCode::SetItem => t -= 2,
            OPCode::Neg | OPCode::Not => (),
            OPCode::Add
            | OPCode::Sub
            | OPCode::Mul
            | OPCode::Div
            | OPCode::Mod
            | OPCode::Eq
            | OPCode::Ne
            | OPCode::Gt
            | OPCode::Ge
            | OPCode::Lt
            | OPCode::Le
            | OPCode::Is => t -= 1,
            OPCode::For(_) => t += 1,
            OPCode::Jump(JumpTarget(_)) => (),
            OPCode::JumpIfNull(JumpTarget(i)) => {
                stack_size = max(stack_size, get_stack_size(code, i, t));
            }
            OPCode::JumpPopIfFalse(JumpTarget(i)) => {
                t -= 1;
                stack_size = max(stack_size, get_stack_size(code, i, t));
            }
            OPCode::JumpIfTureOrPop(JumpTarget(i)) | OPCode::JumpIfFalseOrPop(JumpTarget(i)) => {
                stack_size = max(stack_size, get_stack_size(code, i, t));
                t -= 1;
            }
            OPCode::Call(i) => t = t - i + 1,
            OPCode::TryCall(i) => t = t - i + 1,
            OPCode::Return | OPCode::Throw => break,
            OPCode::ReturnCall(i) => t = t - i + 1,
            OPCode::JumpTarget(_) => panic!(),
        }
        stack_size = max(stack_size, t);
        offset += 1;
    }
    stack_size
}

/// A program.
#[derive(Debug, Clone)]
pub struct Program {
    pub func_list: Vec<Function>,
    pub const_list: Vec<ConstlValue>,
}

impl Program {
    pub fn new(func_list: Vec<Function>, const_list: Vec<ConstlValue>) -> Self {
        Program {
            func_list,
            const_list,
        }
    }
}

impl From<Context> for Program {
    fn from(value: Context) -> Self {
        Program {
            func_list: Vec::from_iter(value.func_list.iter().map(|x| Function::from(x.clone()))),
            const_list: value.const_list,
        }
    }
}

impl TryFrom<&str> for Program {
    type Error = Error;

    fn try_from(value: &str) -> Result<Self> {
        gen_code(Parser::new(&mut tokenize(value)).parse()?)
    }
}

impl TryFrom<&String> for Program {
    type Error = Error;

    fn try_from(value: &String) -> Result<Self> {
        gen_code(Parser::new(&mut tokenize(value)).parse()?)
    }
}

#[derive(Debug, Clone)]
struct Context {
    pub func_list: Vec<FunctionBuilder>,
    pub const_list: Vec<ConstlValue>,
    jump_target_count: usize,
}

impl Context {
    pub fn new() -> Self {
        Context {
            func_list: Vec::new(),
            const_list: Vec::new(),
            jump_target_count: 0,
        }
    }

    fn get_jump_target(&mut self) -> JumpTarget {
        self.jump_target_count += 1;
        JumpTarget(self.jump_target_count - 1)
    }

    fn add_const(&mut self, value: ConstlValue) -> usize {
        match self.const_list.iter().position(|x| *x == value) {
            Some(index) => index,
            None => {
                self.const_list.push(value);
                self.const_list.len() - 1
            }
        }
    }
}

enum LoadStore {
    Load,
    Store,
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
    pub function_id: usize,
    pub params: Vec<String>,
    pub variadic: Option<String>,
    pub code_list: Vec<OPCode>,
    pub kind: FunctionKind,
    pub base_function: Option<usize>,

    pub local_names: Vec<String>,
    pub global_names: Vec<String>,
    pub upvalue_names: Vec<(String, usize, usize)>,

    pub stack_size: usize,
}

impl From<FunctionBuilder> for Function {
    fn from(value: FunctionBuilder) -> Self {
        Function {
            function_id: value.function_id,
            params: value.params,
            variadic: value.variadic,
            code_list: value.code_list,
            kind: value.kind,
            base_function: value.base_function,
            local_names: value.local_names,
            global_names: Vec::from_iter(value.global_names.iter().map(|(x, _)| x.clone())),
            upvalue_names: value.upvalue_names,
            stack_size: value.stack_size,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionBuilder {
    code: Box<Block>,

    pub function_id: usize,
    pub params: Vec<String>,
    pub variadic: Option<String>,
    pub code_list: Vec<OPCode>,
    pub kind: FunctionKind,
    pub base_function: Option<usize>,

    pub local_names: Vec<String>,
    pub global_names: Vec<(String, bool)>,
    pub upvalue_names: Vec<(String, usize, usize)>,

    pub stack_size: usize,

    continue_stack: Vec<JumpTarget>,
    break_stack: Vec<JumpTarget>,
}

impl FunctionBuilder {
    fn new(
        code: Box<Block>,
        function_id: usize,
        base_function: Option<usize>,
        params: Vec<String>,
        variadic: Option<String>,
        kind: FunctionKind,
    ) -> Self {
        FunctionBuilder {
            code,
            function_id,
            params,
            variadic,
            code_list: Vec::new(),
            kind,
            base_function,
            local_names: Vec::new(),
            global_names: Vec::new(),
            upvalue_names: Vec::new(),
            stack_size: 0,
            continue_stack: Vec::new(),
            break_stack: Vec::new(),
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

    fn get_load(&mut self, name: &String, context: &mut Context) -> OPCode {
        self.get_load_store(name, LoadStore::Load, context)
    }

    fn get_store(&mut self, name: &String, context: &mut Context) -> OPCode {
        self.get_load_store(name, LoadStore::Store, context)
    }

    fn get_load_store(&mut self, name: &String, kind: LoadStore, context: &mut Context) -> OPCode {
        if let Some(index) = self.local_names.iter().position(|x| x == name) {
            match kind {
                LoadStore::Load => OPCode::LoadLocal(index),
                LoadStore::Store => OPCode::StoreLocal(index),
            }
        } else if let Some(index) = self.global_names.iter().position(|(x, _)| x == name) {
            match kind {
                LoadStore::Load => OPCode::LoadGlobal(index),
                LoadStore::Store => {
                    if self.global_names[index].1 {
                        OPCode::StoreGlobal(index)
                    } else {
                        OPCode::StoreLocal(self.add_local_name(name))
                    }
                }
            }
        } else if !(self.kind == FunctionKind::Closure) {
            match kind {
                LoadStore::Load => OPCode::LoadGlobal(self.add_global_name(name)),
                LoadStore::Store => OPCode::StoreLocal(self.add_local_name(name)),
            }
        } else {
            let mut base_func_count = 0;
            let mut base_func_id = self.base_function.unwrap();
            loop {
                let base_func = &context.func_list[base_func_id];
                if let Some(i) = base_func.local_names.iter().position(|x| x == name) {
                    self.upvalue_names.push((name.clone(), base_func_count, i));
                    break match kind {
                        LoadStore::Load => OPCode::LoadUpvalue(self.upvalue_names.len() - 1),
                        LoadStore::Store => OPCode::StoreUpvalue(self.upvalue_names.len() - 1),
                    };
                }
                if !(self.kind == FunctionKind::Closure) {
                    break match kind {
                        LoadStore::Load => OPCode::LoadGlobal(self.add_global_name(name)),
                        LoadStore::Store => OPCode::StoreLocal(self.add_local_name(name)),
                    };
                }
                base_func_id = base_func.base_function.unwrap();
                base_func_count += 1;
            }
        }
    }

    fn gen_code(&mut self, context: &mut Context) -> Result<()> {
        for param in self.params.clone() {
            self.add_local_name(&param);
        }
        if let Some(v) = self.variadic.clone() {
            self.add_local_name(&v);
        }
        let t = &mut self.gen_stmt(Stmt::from(*self.code.clone()), context)?;
        self.code_list.append(t);
        if self.kind == FunctionKind::Do {
            self.code_list.push(OPCode::Return);
        } else if *self.code_list.last().unwrap_or(&OPCode::Pop) != OPCode::Return {
            self.code_list
                .push(OPCode::LoadConst(context.add_const(ConstlValue::Null)));
            self.code_list.push(OPCode::Return);
        }
        Ok(())
    }

    fn gen_expr(&mut self, ast_node: Expr, context: &mut Context) -> Result<Vec<OPCode>> {
        let mut code_list = Vec::new();
        match ast_node.kind {
            ExprKind::Lit(lit) => code_list.push(OPCode::LoadConst(
                context.add_const(ConstlValue::from(lit.value)),
            )),
            ExprKind::Ident(ident) => code_list.push(self.get_load(&ident.name, context)),
            ExprKind::Do(body) => {
                let func = FunctionBuilder::new(
                    body,
                    context.func_list.len(),
                    Some(self.function_id),
                    Vec::new(),
                    None,
                    FunctionKind::Do,
                );
                code_list.push(OPCode::LoadConst(
                    context.add_const(ConstlValue::Func(func.function_id)),
                ));
                code_list.push(OPCode::Call(0));
                context.func_list.push(func);
            }
            ExprKind::Function {
                params,
                variadic,
                body,
                is_closure,
            } => {
                let func = FunctionBuilder::new(
                    body,
                    context.func_list.len(),
                    Some(self.function_id),
                    {
                        let mut temp = Vec::new();
                        for param in params {
                            temp.push(param.name);
                        }
                        temp
                    },
                    variadic.map(|v| v.name),
                    if is_closure {
                        FunctionKind::Closure
                    } else {
                        FunctionKind::Funciton
                    },
                );
                code_list.push(OPCode::LoadConst(
                    context.add_const(ConstlValue::Func(func.function_id)),
                ));
                context.func_list.push(func);
            }
            ExprKind::Table { properties } => {
                let temp = properties.len();
                for TableProperty { key, value, .. } in properties {
                    code_list.append(&mut self.gen_expr(*key, context)?);
                    code_list.append(&mut self.gen_expr(*value, context)?);
                }
                code_list.push(OPCode::BuildTable(temp));
            }
            ExprKind::Unary { operator, argument } => {
                code_list.append(&mut self.gen_expr(*argument, context)?);
                code_list.push(OPCode::from(operator));
            }
            ExprKind::Binary {
                operator,
                left,
                right,
            } => match operator {
                BinOp::And => {
                    let label = context.get_jump_target();
                    code_list.append(&mut self.gen_expr(*left, context)?);
                    code_list.push(OPCode::JumpIfFalseOrPop(label));
                    code_list.append(&mut self.gen_expr(*right, context)?);
                    code_list.push(OPCode::JumpTarget(label));
                }
                BinOp::Or => {
                    let label = context.get_jump_target();
                    code_list.append(&mut self.gen_expr(*left, context)?);
                    code_list.push(OPCode::JumpIfTureOrPop(label));
                    code_list.append(&mut self.gen_expr(*right, context)?);
                    code_list.push(OPCode::JumpTarget(label));
                }
                operator => {
                    code_list.append(&mut self.gen_expr(*left, context)?);
                    code_list.append(&mut self.gen_expr(*right, context)?);
                    code_list.push(OPCode::try_from(operator)?);
                }
            },
            ExprKind::Member {
                table,
                property,
                kind,
                safe,
            } => {
                code_list.append(&mut self.gen_expr(*table, context)?);
                let safe_label = context.get_jump_target();
                if safe {
                    code_list.push(OPCode::JumpIfNull(safe_label));
                }
                match kind {
                    MemberKind::Bracket => {
                        code_list.append(&mut self.gen_expr(*property, context)?);
                        code_list.push(OPCode::GetItem);
                    }
                    MemberKind::Dot | MemberKind::DoubleColon => {
                        match property.kind {
                            ExprKind::Ident(ident) => {
                                code_list.push(OPCode::LoadConst(
                                    context.add_const(ConstlValue::Str(ident.name)),
                                ));
                            }
                            _ => return Err(SyntaxError::IllegalAst.into()),
                        }
                        code_list.push(OPCode::GetAttr);
                    }
                }
                code_list.push(OPCode::JumpTarget(safe_label));
            }
            ExprKind::Call {
                callee,
                arguments,
                propagating_error,
            } => {
                let mut temp: usize = arguments.len();
                let safe_label = context.get_jump_target();
                match callee.kind.clone() {
                    ExprKind::Member {
                        table,
                        property,
                        kind,
                        safe,
                    } => {
                        code_list.append(&mut self.gen_expr(*table, context)?);
                        if safe {
                            code_list.push(OPCode::JumpIfNull(safe_label));
                        }
                        match kind {
                            MemberKind::Bracket => {
                                code_list.append(&mut self.gen_expr(*property, context)?);
                                code_list.push(OPCode::GetItem);
                            }
                            MemberKind::Dot => {
                                code_list.push(OPCode::Dup);
                                match property.kind {
                                    ExprKind::Ident(ident) => {
                                        code_list.push(OPCode::LoadConst(
                                            context.add_const(ConstlValue::Str(ident.name)),
                                        ));
                                    }
                                    _ => return Err(SyntaxError::IllegalAst.into()),
                                }
                                code_list.push(OPCode::GetAttr);
                                code_list.push(OPCode::Rot);
                                temp += 1;
                            }
                            MemberKind::DoubleColon => {
                                match property.kind {
                                    ExprKind::Ident(ident) => {
                                        code_list.push(OPCode::LoadConst(
                                            context.add_const(ConstlValue::Str(ident.name)),
                                        ));
                                    }
                                    _ => return Err(SyntaxError::IllegalAst.into()),
                                }
                                code_list.push(OPCode::GetAttr);
                            }
                        }
                    }
                    _ => {
                        code_list.append(&mut self.gen_expr(*callee, context)?);
                    }
                }
                for arg in arguments {
                    code_list.append(&mut self.gen_expr(arg, context)?);
                }
                code_list.push(if propagating_error {
                    OPCode::TryCall(temp)
                } else {
                    OPCode::Call(temp)
                });
                code_list.push(OPCode::JumpTarget(safe_label));
            }
        }
        Ok(code_list)
    }

    fn gen_stmt(&mut self, ast_node: Stmt, context: &mut Context) -> Result<Vec<OPCode>> {
        let mut code_list = Vec::new();
        macro_rules! gen_expr_member_without_get {
            ($table:expr, $property:expr, $kind:expr, $safe:expr) => {{
                if $safe {
                    return Err(SyntaxError::IllegalAst.into());
                }
                code_list.append(&mut self.gen_expr(*$table, context)?);
                match $kind {
                    MemberKind::Bracket => {
                        code_list.append(&mut self.gen_expr(*$property, context)?);
                        OPCode::GetItem
                    }
                    MemberKind::Dot | MemberKind::DoubleColon => {
                        match $property.kind {
                            ExprKind::Ident(ident) => {
                                code_list.push(OPCode::LoadConst(
                                    context.add_const(ConstlValue::Str(ident.name)),
                                ));
                            }
                            _ => return Err(SyntaxError::IllegalAst.into()),
                        }
                        OPCode::GetAttr
                    }
                }
            }};
        }
        match ast_node.kind {
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
                    let false_label = context.get_jump_target();
                    let end_label = context.get_jump_target();
                    code_list.append(&mut self.gen_expr(*test, context)?);
                    code_list.push(OPCode::JumpPopIfFalse(false_label));
                    code_list.append(&mut self.gen_stmt(Stmt::from(*consequent), context)?);
                    code_list.push(OPCode::Jump(end_label));
                    code_list.push(OPCode::JumpTarget(false_label));
                    code_list.append(&mut self.gen_stmt(*alternate, context)?);
                    code_list.push(OPCode::JumpTarget(end_label));
                } else {
                    let end_label = context.get_jump_target();
                    code_list.append(&mut self.gen_expr(*test, context)?);
                    code_list.push(OPCode::JumpPopIfFalse(end_label));
                    code_list.append(&mut self.gen_stmt(Stmt::from(*consequent), context)?);
                    code_list.push(OPCode::JumpTarget(end_label));
                }
            }
            StmtKind::Loop { body } => {
                let continue_label = context.get_jump_target();
                let break_label = context.get_jump_target();
                self.continue_stack.push(continue_label);
                self.break_stack.push(break_label);

                code_list.push(OPCode::JumpTarget(continue_label));
                code_list.append(&mut self.gen_stmt(Stmt::from(*body), context)?);
                code_list.push(OPCode::Jump(continue_label));
                code_list.push(OPCode::JumpTarget(break_label));

                self.continue_stack.pop();
                self.break_stack.pop();
            }
            StmtKind::While { test, body } => {
                let continue_label = context.get_jump_target();
                let break_label = context.get_jump_target();
                self.continue_stack.push(continue_label);
                self.break_stack.push(break_label);

                code_list.push(OPCode::JumpTarget(continue_label));
                code_list.append(&mut self.gen_expr(*test, context)?);
                code_list.push(OPCode::JumpPopIfFalse(break_label));
                code_list.append(&mut self.gen_stmt(Stmt::from(*body), context)?);
                code_list.push(OPCode::Jump(continue_label));
                code_list.push(OPCode::JumpTarget(break_label));

                self.continue_stack.pop();
                self.break_stack.pop();
            }
            StmtKind::For { left, right, body } => {
                let continue_label = context.get_jump_target();
                let break_label = context.get_jump_target();
                self.continue_stack.push(continue_label);
                self.break_stack.push(break_label);

                code_list.append(&mut self.gen_expr(*right, context)?);
                code_list.push(OPCode::JumpTarget(continue_label));
                code_list.push(OPCode::For(break_label));
                if left.len() == 1 {
                    code_list.push(OPCode::StoreLocal(self.add_local_name(&left[0].name)));
                } else {
                    for (i, l) in left.iter().enumerate() {
                        code_list.push(OPCode::Dup);
                        code_list.push(OPCode::LoadConst(
                            context.add_const(ConstlValue::Int(i.try_into().unwrap())),
                        ));
                        code_list.push(OPCode::GetItem);
                        code_list.push(OPCode::StoreLocal(self.add_local_name(&l.name)));
                    }
                    code_list.push(OPCode::Pop);
                }
                code_list.append(&mut self.gen_stmt(Stmt::from(*body), context)?);
                code_list.push(OPCode::Jump(continue_label));
                code_list.push(OPCode::JumpTarget(break_label));
                code_list.push(OPCode::Pop);

                self.continue_stack.pop();
                self.break_stack.pop();
            }
            StmtKind::Break => {
                code_list.push(OPCode::Jump(match self.break_stack.last() {
                    Some(v) => *v,
                    None => return Err(SyntaxError::BreakOutsideLoop.into()),
                }));
            }
            StmtKind::Continue => {
                code_list.push(OPCode::Jump(match self.continue_stack.last() {
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
                    code_list.append(&mut self.gen_expr(*argument, context)?);
                    if propagating_error {
                        code_list.push(OPCode::Return);
                    } else if let OPCode::Call(i) = code_list[code_list.len() - 2] {
                        let t = code_list.len();
                        code_list[t - 2] = OPCode::ReturnCall(i);
                    }
                } else {
                    code_list.append(&mut self.gen_expr(*argument, context)?);
                    code_list.push(OPCode::Return);
                }
            }
            StmtKind::Throw { argument } => {
                if self.kind == FunctionKind::Do {
                    return Err(SyntaxError::ThrowOutsideFunction.into());
                }
                code_list.append(&mut self.gen_expr(*argument, context)?);
                code_list.push(OPCode::Throw);
            }
            StmtKind::Global { arguments } => {
                if self.kind == FunctionKind::Do {
                    return Err(SyntaxError::GlobalOutsideFunction.into());
                }
                for arg in arguments {
                    self.global_names.push((arg.name, true));
                }
            }
            StmtKind::Import { path, kind } => {
                let path_str = path
                    .iter()
                    .map(|x| x.name.clone())
                    .collect::<Vec<String>>()
                    .join("::");
                code_list.push(OPCode::Import(
                    context.add_const(ConstlValue::Str(path_str)),
                ));
                match kind {
                    ImportKind::Simple(alias) => {
                        code_list.push(OPCode::StoreGlobal(self.add_global_name(&alias.name)));
                    }
                    ImportKind::Nested(items) => {
                        for (name, alias) in items {
                            code_list.push(OPCode::ImportFrom(
                                context.add_const(ConstlValue::Str(name.name)),
                            ));
                            code_list.push(OPCode::StoreGlobal(self.add_global_name(&alias.name)));
                        }
                        code_list.push(OPCode::Pop);
                    }
                    ImportKind::Glob => {
                        code_list.push(OPCode::ImportGlob);
                    }
                }
            }
            StmtKind::Assign { left, right } => {
                if left.len() == 1 {
                    match left[0].kind.clone() {
                        ExprKind::Ident(ident) => {
                            code_list.append(&mut self.gen_expr(*right, context)?);
                            code_list.push(self.get_store(&ident.name, context));
                        }
                        ExprKind::Member {
                            table,
                            property,
                            kind,
                            safe,
                        } => {
                            gen_expr_member_without_get!(table, property, kind, safe);
                            code_list.append(&mut self.gen_expr(*right, context)?);
                            code_list.push(match kind {
                                MemberKind::Bracket => OPCode::SetItem,
                                MemberKind::Dot | MemberKind::DoubleColon => OPCode::SetAttr,
                            });
                        }
                        _ => return Err(SyntaxError::IllegalAst.into()),
                    }
                } else {
                    let right_expr = self.gen_expr(*right, context)?;
                    for (i, l) in left.iter().enumerate() {
                        match &l.kind {
                            ExprKind::Ident(ident) => {
                                code_list.append(&mut right_expr.clone());
                                code_list.push(OPCode::LoadConst(
                                    context.add_const(ConstlValue::Int(i.try_into().unwrap())),
                                ));
                                code_list.push(OPCode::GetItem);
                                code_list.push(self.get_store(&ident.name, context));
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
                                code_list.append(&mut right_expr.clone());
                                code_list.push(OPCode::LoadConst(
                                    context.add_const(ConstlValue::Int(i.try_into().unwrap())),
                                ));
                                code_list.push(OPCode::GetItem);
                                code_list.push(match kind {
                                    MemberKind::Bracket => OPCode::SetItem,
                                    MemberKind::Dot | MemberKind::DoubleColon => OPCode::SetAttr,
                                });
                            }
                            _ => return Err(SyntaxError::IllegalAst.into()),
                        }
                    }
                }
            }
            StmtKind::AssignOp {
                operator,
                left,
                right,
            } => match left.kind.clone() {
                ExprKind::Ident(ident) => {
                    code_list.append(&mut self.gen_expr(*left, context)?);
                    code_list.append(&mut self.gen_expr(*right, context)?);
                    code_list.push(OPCode::try_from(operator)?);
                    code_list.push(self.get_store(&ident.name, context));
                }
                ExprKind::Member {
                    table,
                    property,
                    kind,
                    safe,
                } => {
                    let temp = gen_expr_member_without_get!(table, property, kind, safe);
                    code_list.push(OPCode::DupTwo);
                    code_list.push(temp);
                    code_list.append(&mut self.gen_expr(*right, context)?);
                    code_list.push(OPCode::try_from(operator)?);
                    code_list.push(match kind {
                        MemberKind::Bracket => OPCode::SetItem,
                        MemberKind::Dot | MemberKind::DoubleColon => OPCode::SetAttr,
                    });
                }
                _ => return Err(SyntaxError::IllegalAst.into()),
            },
            StmtKind::Block(block) => {
                for stmt in block.body {
                    code_list.append(&mut self.gen_stmt(stmt, context)?);
                }
            }
            StmtKind::Expr(expr) => {
                code_list.append(&mut self.gen_expr(*expr, context)?);
                code_list.push(OPCode::Pop);
            }
        }
        Ok(code_list)
    }
}
