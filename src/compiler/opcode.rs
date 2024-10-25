//! The OpCodes for LVM.

use std::fmt;

/// The jump target.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct JumpTarget(pub usize);

/// The operation code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OpCode {
    /// Removes the top-of-stack (TOS) item.
    Pop,
    /// Push the i-th item to the top of the stack.
    Copy(usize),
    /// Swap TOS with the item at position i.
    Swap(usize),
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
    /// Loads all symbols from the module TOS to the global namespace.
    ImportGlob,

    /// Pushes a new table onto the stack. Pops `2 * count` items to build table.
    BuildTable(usize),
    /// Pushes a new list onto the stack. Pops `count` items to build list.
    BuildList(usize),
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
    Rem,

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

    /// Get the __iter__ of TOS and pushed it onto the stack.
    Iter,
    /// Sets the bytecode counter to target.
    Jump(JumpTarget),
    /// If TOS is null, sets the bytecode counter to target.
    JumpIfNull(JumpTarget),
    /// If TOS is false, sets the bytecode counter to target. TOS is popped.
    JumpPopIfFalse(JumpTarget),
    /// If TOS is true, sets the bytecode counter to target and leaves TOS on the stack. Otherwise, TOS is popped.
    JumpIfTrueOrPop(JumpTarget),
    /// If TOS is false, sets the bytecode counter to target and leaves TOS on the stack. Otherwise, TOS is popped.
    JumpIfFalseOrPop(JumpTarget),

    /// Pops numbers of item for function arguments, then pop an callable value and call it.
    Call(usize),
    /// Call with a shortcut for propagating errors.
    TryCall(usize),
    /// Call with a shortcut for propagating errors.
    TryOptionCall(usize),
    /// Call with a shortcut for propagating errors.
    TryPanicCall(usize),
    /// Returns with TOS to the caller of the function.
    Return,
    /// Returns with TOS as a error.
    Throw,
    /// Same as "Call(usize); Return;", this is for tail call optimization.
    ReturnCall(usize),

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
            Self::Is => write!(f, "Is"),
            Self::Iter => write!(f, "Iter"),
            Self::Jump(JumpTarget(i)) => write!(f, "{:WIDTH$}{}", "Jump", i),
            Self::JumpIfNull(JumpTarget(i)) => write!(f, "{:WIDTH$}{}", "JumpIfNull", i),
            Self::JumpPopIfFalse(JumpTarget(i)) => write!(f, "{:WIDTH$}{}", "JumpPopIfFalse", i),
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
            Self::JumpTarget(JumpTarget(i)) => write!(f, "{:WIDTH$}{}", "JumpTarget", i),
        }
    }
}
