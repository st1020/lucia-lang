//! The OpCodes for LVM.

use std::fmt;

use super::{
    index::{ConstId, GlobalNameId, LocalNameId, UpvalueNameId},
    value::ValueType,
};

/// The operation code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OpCode<JumpTarget> {
    /// Removes the top-of-stack item, `STACK.pop()`.
    Pop,
    /// Push the i-th item to the top of the stack without removing it from its original location.
    /// `STACK.push(STACK[-1])`.
    Copy(usize),
    /// Swap the top of the stack with the i-th element.
    /// `STACK[-i], STACK[-1] = STACK[-1], STACK[-i]`.
    Swap(usize),
    /// Pushes the value associated with `local_names[local_name_id]` onto the stack.
    LoadLocal(LocalNameId),
    /// Pushes the value associated with `global_names[global_name_id]` onto the stack.
    LoadGlobal(GlobalNameId),
    /// Pushes the value associated with `upvalue_names[upvalue_name_id]` onto the stack.
    LoadUpvalue(UpvalueNameId),
    /// Pushes `consts[const_id]` onto the stack.
    LoadConst(ConstId),
    /// Stores `STACK.pop()` into the `local_names[local_name_id]`.
    StoreLocal(LocalNameId),
    /// Stores `STACK.pop()` into the `global_names[global_name_id]`.
    StoreGlobal(GlobalNameId),

    /// Pushes a new table onto the stack. Pops `2 * count` items to build table.
    BuildTable(usize),
    /// Pushes a new list onto the stack. Pops `count` items to build list.
    BuildList(usize),
    /// Implements:
    /// ```lucia
    /// key = STACK.pop()
    /// table = STACK.pop()
    /// STACK.push(table::key)
    /// ```
    GetAttr,
    /// Implements:
    /// ```lucia
    /// key = STACK.pop()
    /// table = STACK.pop()
    /// STACK.push(table[key])
    /// ```
    GetItem,
    /// Implements:
    /// ```lucia
    /// table = STACK.pop()
    /// STACK.push(table[#])
    /// ```
    GetMeta,
    /// Implements:
    /// ```lucia
    /// key = STACK.pop()
    /// table = STACK.pop()
    /// value = STACK.pop()
    /// STACK.push(set_attr(table, key, value))
    /// ```
    SetAttr,
    /// Implements:
    /// ```lucia
    /// key = STACK.pop()
    /// table = STACK.pop()
    /// value = STACK.pop()
    /// STACK.push(set_item(table, key, value))
    /// ```
    SetItem,
    /// Implements:
    /// ```lucia
    /// table = STACK.pop()
    /// metatable = STACK.pop()
    /// STACK.push(set_metatable(table, metatable))
    /// ```
    SetMeta,

    /// Implements `STACK[-1] = -STACK[-1]`.
    Neg,
    /// Implements `STACK[-1] = not STACK[-1]`.
    Not,

    /// Implements the binary and in-place operators:
    /// ```lucia
    /// rhs = STACK.pop()
    /// lhs = STACK.pop()
    /// STACK.append(lhs + rhs)
    /// ```
    Add,
    /// Works as `Add`, but performs `-` operation.
    Sub,
    /// Works as `Add`, but performs `*` operation.
    Mul,
    /// Works as `Add`, but performs `/` operation.
    Div,
    /// Works as `Add`, but performs `%` operation.
    Rem,
    /// Works as `Add`, but performs `==` operation.
    Eq,
    /// Works as `Add`, but performs `!=` operation.
    Ne,
    /// Works as `Add`, but performs `>` operation.
    Gt,
    ///Works as `Add`, but performs `>=` operation.
    Ge,
    /// Works as `Add`, but performs `<` operation.
    Lt,
    /// Works as `Add`, but performs `>=` operation.
    Le,
    /// Implements `STACK[-1] = STACK[-1] is type`.
    TypeCheck(ValueType),

    /// Implements `STACK[-1] = len(STACK[-1])`.
    GetLen,

    /// Imports the module `consts[const_id]` and pushed it onto the stack.
    Import(ConstId),
    /// Loads the attribute `consts[const_id]` of the module in `STACK[-1]` and pushed it onto the
    /// stack.
    ImportFrom(ConstId),
    /// Loads all symbols from the module in `STACK[-1]` to the global namespace.
    ImportGlob,

    /// Get the __iter__ of `STACK[-1]` and pushed it onto the stack.
    Iter,
    /// Pops numbers of item for function arguments, then pop an callable value and call it.
    Call(usize),
    /// Returns with `STACK[-1]` to the caller of the function.
    Return,
    /// Works as `Call(usize); Return;`, this is for tail call optimization.
    ReturnCall(usize),
    /// Pushes a table of local names onto the stack.
    LoadLocals,

    /// Sets the bytecode counter to target.
    Jump(JumpTarget),
    /// As same as `Jump` in runtime, used in codegen.
    JumpBackEdge(JumpTarget),
    /// As same as `Jump` in runtime, used in codegen.
    Break(JumpTarget),
    /// As same as `Jump` in runtime, used in codegen.
    Continue(JumpTarget),

    /// If `STACK[-1]` is null, sets the bytecode counter to target and pop `STACK[-1]`.
    /// Otherwise, leaves `STACK[-1]` on the stack
    JumpPopIfNull(JumpTarget),
    /// If `STACK[-1]` is true, sets the bytecode counter to target. `STACK[-1]` is popped.
    PopJumpIfTrue(JumpTarget),
    /// If `STACK[-1]` is false, sets the bytecode counter to target. `STACK[-1]` is popped.
    PopJumpIfFalse(JumpTarget),
    /// If `STACK[-1]` is true, sets the bytecode counter to target and leaves it on the stack.
    /// Otherwise, `STACK[-1]` is popped.
    JumpIfTrueOrPop(JumpTarget),
    /// If `STACK[-1]` is false, sets the bytecode counter to target and leaves it on the stack.
    /// Otherwise, `STACK[-1]` is popped.
    JumpIfFalseOrPop(JumpTarget),

    /// Pops an effect from stack and and registers an effect handler for it.
    RegisterHandler(JumpTarget),
    /// Pops two effect from stack and checks if they match, if not, raises an error.
    /// The usize field is only used for stack size analysis during codegen.
    CheckEffect(usize),
    /// Noop in runtime, only used for stack size analysis during codegen.
    MarkAddStackSize(usize),
}

impl<JumpTarget> OpCode<JumpTarget> {
    pub fn is_load(self) -> bool {
        matches!(
            self,
            OpCode::LoadLocal(_)
                | OpCode::LoadGlobal(_)
                | OpCode::LoadUpvalue(_)
                | OpCode::LoadConst(_)
        )
    }

    pub fn is_store(self) -> bool {
        matches!(self, OpCode::StoreLocal(_) | OpCode::StoreGlobal(_))
    }

    pub fn is_arithmetic(self) -> bool {
        matches!(
            self,
            OpCode::Add | OpCode::Sub | OpCode::Mul | OpCode::Div | OpCode::Rem
        )
    }

    pub fn is_comparison(self) -> bool {
        matches!(
            self,
            OpCode::Eq | OpCode::Ne | OpCode::Gt | OpCode::Ge | OpCode::Lt | OpCode::Le
        )
    }

    pub fn is_jump(self) -> bool {
        matches!(
            self,
            OpCode::Jump(_)
                | OpCode::JumpBackEdge(_)
                | OpCode::Break(_)
                | OpCode::Continue(_)
                | OpCode::JumpPopIfNull(_)
                | OpCode::PopJumpIfTrue(_)
                | OpCode::PopJumpIfFalse(_)
                | OpCode::JumpIfTrueOrPop(_)
                | OpCode::JumpIfFalseOrPop(_)
        )
    }

    pub fn is_return(self) -> bool {
        matches!(self, OpCode::Return | OpCode::ReturnCall(_))
    }

    pub fn jump_target(&self) -> Option<&JumpTarget> {
        #[expect(clippy::wildcard_enum_match_arm)]
        match self {
            OpCode::Jump(target)
            | OpCode::JumpBackEdge(target)
            | OpCode::Break(target)
            | OpCode::Continue(target)
            | OpCode::JumpPopIfNull(target)
            | OpCode::PopJumpIfTrue(target)
            | OpCode::PopJumpIfFalse(target)
            | OpCode::JumpIfTrueOrPop(target)
            | OpCode::JumpIfFalseOrPop(target)
            | OpCode::RegisterHandler(target) => Some(target),
            _ => None,
        }
    }

    pub fn jump_target_mut(&mut self) -> Option<&mut JumpTarget> {
        #[expect(clippy::wildcard_enum_match_arm)]
        match self {
            OpCode::Jump(target)
            | OpCode::JumpBackEdge(target)
            | OpCode::Break(target)
            | OpCode::Continue(target)
            | OpCode::JumpPopIfNull(target)
            | OpCode::PopJumpIfTrue(target)
            | OpCode::PopJumpIfFalse(target)
            | OpCode::JumpIfTrueOrPop(target)
            | OpCode::JumpIfFalseOrPop(target)
            | OpCode::RegisterHandler(target) => Some(target),
            _ => None,
        }
    }

    #[inline]
    pub fn map_jump_target<U, F>(self, f: F) -> OpCode<U>
    where
        F: FnOnce(JumpTarget) -> U,
    {
        match self {
            OpCode::Pop => OpCode::Pop,
            OpCode::Copy(i) => OpCode::Copy(i),
            OpCode::Swap(i) => OpCode::Swap(i),
            OpCode::LoadLocal(i) => OpCode::LoadLocal(i),
            OpCode::LoadGlobal(i) => OpCode::LoadGlobal(i),
            OpCode::LoadUpvalue(i) => OpCode::LoadUpvalue(i),
            OpCode::LoadConst(i) => OpCode::LoadConst(i),
            OpCode::StoreLocal(i) => OpCode::StoreLocal(i),
            OpCode::StoreGlobal(i) => OpCode::StoreGlobal(i),
            OpCode::BuildTable(i) => OpCode::BuildTable(i),
            OpCode::BuildList(i) => OpCode::BuildList(i),
            OpCode::GetAttr => OpCode::GetAttr,
            OpCode::GetItem => OpCode::GetItem,
            OpCode::GetMeta => OpCode::GetMeta,
            OpCode::SetAttr => OpCode::SetAttr,
            OpCode::SetItem => OpCode::SetItem,
            OpCode::SetMeta => OpCode::SetMeta,
            OpCode::Neg => OpCode::Neg,
            OpCode::Not => OpCode::Not,
            OpCode::Add => OpCode::Add,
            OpCode::Sub => OpCode::Sub,
            OpCode::Mul => OpCode::Mul,
            OpCode::Div => OpCode::Div,
            OpCode::Rem => OpCode::Rem,
            OpCode::Eq => OpCode::Eq,
            OpCode::Ne => OpCode::Ne,
            OpCode::Gt => OpCode::Gt,
            OpCode::Ge => OpCode::Ge,
            OpCode::Lt => OpCode::Lt,
            OpCode::Le => OpCode::Le,
            OpCode::TypeCheck(i) => OpCode::TypeCheck(i),
            OpCode::GetLen => OpCode::GetLen,
            OpCode::Import(i) => OpCode::Import(i),
            OpCode::ImportFrom(i) => OpCode::ImportFrom(i),
            OpCode::ImportGlob => OpCode::ImportGlob,
            OpCode::Iter => OpCode::Iter,
            OpCode::Call(i) => OpCode::Call(i),
            OpCode::Return => OpCode::Return,
            OpCode::ReturnCall(i) => OpCode::ReturnCall(i),
            OpCode::LoadLocals => OpCode::LoadLocals,
            OpCode::Jump(i) => OpCode::Jump(f(i)),
            OpCode::JumpBackEdge(i) => OpCode::JumpBackEdge(f(i)),
            OpCode::Break(i) => OpCode::Break(f(i)),
            OpCode::Continue(i) => OpCode::Continue(f(i)),
            OpCode::JumpPopIfNull(i) => OpCode::JumpPopIfNull(f(i)),
            OpCode::PopJumpIfTrue(i) => OpCode::PopJumpIfTrue(f(i)),
            OpCode::PopJumpIfFalse(i) => OpCode::PopJumpIfFalse(f(i)),
            OpCode::JumpIfTrueOrPop(i) => OpCode::JumpIfTrueOrPop(f(i)),
            OpCode::JumpIfFalseOrPop(i) => OpCode::JumpIfFalseOrPop(f(i)),
            OpCode::RegisterHandler(i) => OpCode::RegisterHandler(f(i)),
            OpCode::CheckEffect(i) => OpCode::CheckEffect(i),
            OpCode::MarkAddStackSize(i) => OpCode::MarkAddStackSize(i),
        }
    }
}

impl<JumpTarget: fmt::Display> fmt::Display for OpCode<JumpTarget> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        const WIDTH: usize = 20;
        match self {
            OpCode::Pop => write!(f, "Pop"),
            OpCode::Copy(i) => write!(f, "{:WIDTH$}{}", "Copy", i),
            OpCode::Swap(i) => write!(f, "{:WIDTH$}{}", "Swap", i),
            OpCode::LoadLocal(i) => write!(f, "{:WIDTH$}{}", "LoadLocal", i),
            OpCode::LoadGlobal(i) => write!(f, "{:WIDTH$}{}", "LoadGlobal", i),
            OpCode::LoadUpvalue(i) => write!(f, "{:WIDTH$}{}", "LoadUpvalue", i),
            OpCode::LoadConst(i) => write!(f, "{:WIDTH$}{}", "LoadConst", i),
            OpCode::StoreLocal(i) => write!(f, "{:WIDTH$}{}", "StoreLocal", i),
            OpCode::StoreGlobal(i) => write!(f, "{:WIDTH$}{}", "StoreGlobal", i),
            OpCode::BuildTable(i) => write!(f, "{:WIDTH$}{}", "BuildTable", i),
            OpCode::BuildList(i) => write!(f, "{:WIDTH$}{}", "BuildList", i),
            OpCode::GetAttr => write!(f, "GetAttr"),
            OpCode::GetItem => write!(f, "GetItem"),
            OpCode::GetMeta => write!(f, "GetMeta"),
            OpCode::SetAttr => write!(f, "SetAttr"),
            OpCode::SetItem => write!(f, "SetItem"),
            OpCode::SetMeta => write!(f, "SetMeta"),
            OpCode::Neg => write!(f, "Neg"),
            OpCode::Not => write!(f, "Not"),
            OpCode::Add => write!(f, "Add"),
            OpCode::Sub => write!(f, "Sub"),
            OpCode::Mul => write!(f, "Mul"),
            OpCode::Div => write!(f, "Div"),
            OpCode::Rem => write!(f, "Rem"),
            OpCode::Eq => write!(f, "Eq"),
            OpCode::Ne => write!(f, "Ne"),
            OpCode::Gt => write!(f, "Gt"),
            OpCode::Ge => write!(f, "Ge"),
            OpCode::Lt => write!(f, "Lt"),
            OpCode::Le => write!(f, "Le"),
            OpCode::TypeCheck(ty) => write!(f, "{:WIDTH$}{}", "TypeCheck", ty),
            OpCode::GetLen => write!(f, "GetLen"),
            OpCode::Import(i) => write!(f, "{:WIDTH$}{}", "Import", i),
            OpCode::ImportFrom(i) => write!(f, "{:WIDTH$}{}", "ImportFrom", i),
            OpCode::ImportGlob => write!(f, "ImportGlob"),
            OpCode::Iter => write!(f, "Iter"),
            OpCode::Call(i) => write!(f, "{:WIDTH$}{}", "Call", i),
            OpCode::Return => write!(f, "Return"),
            OpCode::ReturnCall(i) => write!(f, "{:WIDTH$}{}", "ReturnCall", i),
            OpCode::LoadLocals => write!(f, "LoadLocals"),
            OpCode::Jump(i) => write!(f, "{:WIDTH$}{}", "Jump", i),
            OpCode::JumpBackEdge(i) => write!(f, "{:WIDTH$}{}", "JumpBackEdge", i),
            OpCode::Break(i) => write!(f, "{:WIDTH$}{}", "Break", i),
            OpCode::Continue(i) => write!(f, "{:WIDTH$}{}", "Continue", i),
            OpCode::JumpPopIfNull(i) => write!(f, "{:WIDTH$}{}", "JumpPopIfNull", i),
            OpCode::PopJumpIfTrue(i) => write!(f, "{:WIDTH$}{}", "PopJumpIfTrue", i),
            OpCode::PopJumpIfFalse(i) => write!(f, "{:WIDTH$}{}", "PopJumpIfFalse", i),
            OpCode::JumpIfTrueOrPop(i) => write!(f, "{:WIDTH$}{}", "JumpIfTrueOrPop", i),
            OpCode::JumpIfFalseOrPop(i) => {
                write!(f, "{:WIDTH$}{}", "JumpIfFalseOrPop", i)
            }
            OpCode::RegisterHandler(i) => write!(f, "{:WIDTH$}{}", "RegisterHandler", i),
            OpCode::CheckEffect(i) => write!(f, "{:WIDTH$}{}", "MatchEffect", i),
            OpCode::MarkAddStackSize(i) => write!(f, "{:WIDTH$}{}", "MarkStackSize", i),
        }
    }
}
