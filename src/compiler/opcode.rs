//! The OpCodes for LVM.

use std::fmt;

use super::value::ValueType;

/// The jump target.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct JumpTarget(pub usize);

/// The operation code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OpCode {
    /// Removes the top-of-stack item, `STACK.pop()`.
    Pop,
    /// Push the i-th item to the top of the stack without removing it from its original location, `STACK.push(STACK[-1])`.
    Copy(usize),
    /// Swap the top of the stack with the i-th element, `STACK[-i], STACK[-1] = STACK[-1], STACK[-i]`.
    Swap(usize),
    /// Pushes the value associated with `local_names[namei]` onto the stack.
    LoadLocal(usize),
    /// Pushes the value associated with `global_names[namei]` onto the stack.
    LoadGlobal(usize),
    /// Pushes the value associated with `upvalue_names[namei]` onto the stack.
    LoadUpvalue(usize),
    /// Pushes `consts[consti]` onto the stack.
    LoadConst(usize),
    /// Stores `STACK.pop()` into the `local_names[namei]`.
    StoreLocal(usize),
    /// Stores `STACK.pop()` into the `global_names[namei]`.
    StoreGlobal(usize),
    /// Stores `STACK.pop()` into the `upvalue_names[namei]`.
    StoreUpvalue(usize),

    /// Imports the module `consts[consti]` and pushed it onto the stack.
    Import(usize),
    /// Loads the attribute `consts[consti]` the module in `STACK[-1]` and pushed it onto the stack.
    ImportFrom(usize),
    /// Loads all symbols from the module in `STACK[-1]` to the global namespace.
    ImportGlob,

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
    /// key = STACK.pop()
    /// table = STACK.pop()
    /// STACK.push(table[#])
    /// ```
    GetMeta,
    /// Implements:
    /// ```lucia
    /// key = STACK.pop()
    /// table = STACK.pop()
    /// value = STACK.pop()
    /// table::key = value
    /// ```
    SetAttr,
    /// Implements:
    /// ```lucia
    /// key = STACK.pop()
    /// table = STACK.pop()
    /// value = STACK.pop()
    /// table[key] = value
    /// ```
    SetItem,
    /// Implements:
    /// ```lucia
    /// table = STACK.pop()
    /// metatable = STACK.pop()
    /// table[#] = metatable
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
    /// Works as `Add`, but performs `===` operation.
    Identical,
    /// Works as `Add`, but performs `!==` operation.
    NotIdentical,

    /// Implements `STACK[-1] = STACK[-1] is type`.
    TypeCheck(ValueType),

    /// Implements `STACK[-1] = len(STACK[-1])`.
    GetLen,

    /// Get the __iter__ of `STACK[-1]` and pushed it onto the stack.
    Iter,
    /// Sets the bytecode counter to target.
    Jump(JumpTarget),
    /// If `STACK[-1]` is null, sets the bytecode counter to target and pop `STACK[-1]`. Otherwise, leaves `STACK[-1]` on the stack
    JumpPopIfNull(JumpTarget),
    /// If `STACK[-1]` is true, sets the bytecode counter to target. `STACK[-1]` is popped.
    PopJumpIfTrue(JumpTarget),
    /// If `STACK[-1]` is false, sets the bytecode counter to target. `STACK[-1]` is popped.
    PopJumpIfFalse(JumpTarget),
    /// If `STACK[-1]` is true, sets the bytecode counter to target and leaves `STACK[-1]` on the stack. Otherwise, `STACK[-1]` is popped.
    JumpIfTrueOrPop(JumpTarget),
    /// If `STACK[-1]` is false, sets the bytecode counter to target and leaves `STACK[-1]` on the stack. Otherwise, `STACK[-1]` is popped.
    JumpIfFalseOrPop(JumpTarget),

    /// Pops numbers of item for function arguments, then pop an callable value and call it.
    Call(usize),
    /// Call with a shortcut for propagating errors. Implements `try`.
    TryCall(usize),
    /// Call with a shortcut for propagating errors. Implements `try?`.
    TryOptionCall(usize),
    /// Call with a shortcut for propagating errors. Implements `try!`.
    TryPanicCall(usize),
    /// Returns with `STACK[-1]` to the caller of the function.
    Return,
    /// Throws with `STACK[-1]` as an error.
    Throw,
    /// Works as `Call(usize); Return;`, this is for tail call optimization.
    ReturnCall(usize),
    /// Pushes a table of local names onto the stack.
    LoadLocals,

    /// A jump target, only used during code generation.
    JumpTarget(JumpTarget),
}

impl fmt::Display for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        const WIDTH: usize = 20;
        match self {
            Self::Pop => write!(f, "Pop"),
            Self::Copy(i) => write!(f, "{:WIDTH$}{}", "Copy", i),
            Self::Swap(i) => write!(f, "{:WIDTH$}{}", "Swap", i),
            Self::LoadLocal(i) => write!(f, "{:WIDTH$}{}", "LoadLocal", i),
            Self::LoadGlobal(i) => write!(f, "{:WIDTH$}{}", "LoadGlobal", i),
            Self::LoadUpvalue(i) => write!(f, "{:WIDTH$}{}", "LoadUpvalue", i),
            Self::LoadConst(i) => write!(f, "{:WIDTH$}{}", "LoadConst", i),
            Self::StoreLocal(i) => write!(f, "{:WIDTH$}{}", "StoreLocal", i),
            Self::StoreGlobal(i) => write!(f, "{:WIDTH$}{}", "StoreGlobal", i),
            Self::StoreUpvalue(i) => write!(f, "{:WIDTH$}{}", "StoreUpvalue", i),
            Self::Import(i) => write!(f, "{:WIDTH$}{}", "Import", i),
            Self::ImportFrom(i) => write!(f, "{:WIDTH$}{}", "ImportFrom", i),
            Self::ImportGlob => write!(f, "ImportGlob"),
            Self::BuildTable(i) => write!(f, "{:WIDTH$}{}", "BuildTable", i),
            Self::BuildList(i) => write!(f, "{:WIDTH$}{}", "BuildList", i),
            Self::GetAttr => write!(f, "GetAttr"),
            Self::GetItem => write!(f, "GetItem"),
            Self::GetMeta => write!(f, "GetMeta"),
            Self::SetAttr => write!(f, "SetAttr"),
            Self::SetItem => write!(f, "SetItem"),
            Self::SetMeta => write!(f, "SetMeta"),
            Self::Neg => write!(f, "Neg"),
            Self::Not => write!(f, "Not"),
            Self::Add => write!(f, "Add"),
            Self::Sub => write!(f, "Sub"),
            Self::Mul => write!(f, "Mul"),
            Self::Div => write!(f, "Div"),
            Self::Rem => write!(f, "Rem"),
            Self::Eq => write!(f, "Eq"),
            Self::Ne => write!(f, "Ne"),
            Self::Gt => write!(f, "Gt"),
            Self::Ge => write!(f, "Ge"),
            Self::Lt => write!(f, "Lt"),
            Self::Le => write!(f, "Le"),
            Self::Identical => write!(f, "Identical"),
            Self::NotIdentical => write!(f, "NotIdentical"),
            Self::TypeCheck(ty) => write!(f, "{:WIDTH$}{}", "TypeCheck", ty),
            Self::GetLen => write!(f, "GetLen"),
            Self::Iter => write!(f, "Iter"),
            Self::Jump(JumpTarget(i)) => write!(f, "{:WIDTH$}{}", "Jump", i),
            Self::JumpPopIfNull(JumpTarget(i)) => write!(f, "{:WIDTH$}{}", "JumpPopIfNull", i),
            Self::PopJumpIfTrue(JumpTarget(i)) => write!(f, "{:WIDTH$}{}", "PopJumpIfTrue", i),
            Self::PopJumpIfFalse(JumpTarget(i)) => write!(f, "{:WIDTH$}{}", "PopJumpIfFalse", i),
            Self::JumpIfTrueOrPop(JumpTarget(i)) => write!(f, "{:WIDTH$}{}", "JumpIfTrueOrPop", i),
            Self::JumpIfFalseOrPop(JumpTarget(i)) => {
                write!(f, "{:WIDTH$}{}", "JumpIfFalseOrPop", i)
            }
            Self::Call(i) => write!(f, "{:WIDTH$}{}", "Call", i),
            Self::TryCall(i) => write!(f, "{:WIDTH$}{}", "TryCall", i),
            Self::TryOptionCall(i) => write!(f, "{:WIDTH$}{}", "TryOptionCall", i),
            Self::TryPanicCall(i) => write!(f, "{:WIDTH$}{}", "TryPanicCall", i),
            Self::Return => write!(f, "Return"),
            Self::Throw => write!(f, "Throw"),
            Self::ReturnCall(i) => write!(f, "{:WIDTH$}{}", "ReturnCall", i),
            Self::LoadLocals => write!(f, "LoadLocals"),
            Self::JumpTarget(JumpTarget(i)) => write!(f, "{:WIDTH$}{}", "JumpTarget", i),
        }
    }
}
