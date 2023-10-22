//! The OpCodes for LVM.

use std::fmt;

use gc_arena::Collect;

/// The jump target.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct JumpTarget(pub usize);

/// The operation code.
#[derive(Debug, Clone, Copy, Collect, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[collect(require_static)]
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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let width = 20;
        match self {
            Self::Pop => write!(f, "Pop"),
            Self::Copy(i) => write!(f, "{:width$}{}", "Copy", i),
            Self::Swap(i) => write!(f, "{:width$}{}", "Swap", i),
            Self::LoadLocal(i) => write!(f, "{:width$}{}", "LoadLocal", i),
            Self::LoadGlobal(i) => write!(f, "{:width$}{}", "LoadGlobal", i),
            Self::LoadUpvalue(i) => write!(f, "{:width$}{}", "LoadUpvalue", i),
            Self::LoadConst(i) => write!(f, "{:width$}{}", "LoadConst", i),
            Self::StoreLocal(i) => write!(f, "{:width$}{}", "StoreLocal", i),
            Self::StoreGlobal(i) => write!(f, "{:width$}{}", "StoreGlobal", i),
            Self::StoreUpvalue(i) => write!(f, "{:width$}{}", "StoreUpvalue", i),
            Self::Import(i) => write!(f, "{:width$}{}", "Import", i),
            Self::ImportFrom(i) => write!(f, "{:width$}{}", "ImportFrom", i),
            Self::ImportGlob => write!(f, "ImportGlob"),
            Self::BuildTable(i) => write!(f, "{:width$}{}", "BuildTable", i),
            Self::BuildList(i) => write!(f, "{:width$}{}", "BuildList", i),
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
            Self::Mod => write!(f, "Mod"),
            Self::Eq => write!(f, "Eq"),
            Self::Ne => write!(f, "Ne"),
            Self::Gt => write!(f, "Gt"),
            Self::Ge => write!(f, "Ge"),
            Self::Lt => write!(f, "Lt"),
            Self::Le => write!(f, "Le"),
            Self::Is => write!(f, "Is"),
            Self::Iter => write!(f, "Iter"),
            Self::Jump(JumpTarget(i)) => write!(f, "{:width$}{}", "Jump", i),
            Self::JumpIfNull(JumpTarget(i)) => write!(f, "{:width$}{}", "JumpIfNull", i),
            Self::JumpPopIfFalse(JumpTarget(i)) => write!(f, "{:width$}{}", "JumpPopIfFalse", i),
            Self::JumpIfTrueOrPop(JumpTarget(i)) => write!(f, "{:width$}{}", "JumpIfTrueOrPop", i),
            Self::JumpIfFalseOrPop(JumpTarget(i)) => {
                write!(f, "{:width$}{}", "JumpIfFalseOrPop", i)
            }
            Self::Call(i) => write!(f, "{:width$}{}", "Call", i),
            Self::TryCall(i) => write!(f, "{:width$}{}", "TryCall", i),
            Self::TryOptionCall(i) => write!(f, "{:width$}{}", "TryOptionCall", i),
            Self::TryPanicCall(i) => write!(f, "{:width$}{}", "TryPanicCall", i),
            Self::Return => write!(f, "Return"),
            Self::Throw => write!(f, "Throw"),
            Self::ReturnCall(i) => write!(f, "{:width$}{}", "ReturnCall", i),
            Self::JumpTarget(JumpTarget(i)) => write!(f, "{:width$}{}", "JumpTarget", i),
        }
    }
}
