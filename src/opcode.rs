use std::fmt::{Debug, Display};

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
        let width = 20;
        match self {
            Self::Pop => write!(f, "{:width$}", "Pop"),
            Self::Dup => write!(f, "{:width$}", "Dup"),
            Self::DupTwo => write!(f, "{:width$}", "DupTwo"),
            Self::RotTwo => write!(f, "{:width$}", "RotTwo"),
            Self::RotThree => write!(f, "{:width$}", "RotThree"),
            Self::LoadLocal(i) => write!(f, "{:width$}{}", "LoadLocal", i),
            Self::LoadGlobal(i) => write!(f, "{:width$}{}", "LoadGlobal", i),
            Self::LoadUpvalue(i) => write!(f, "{:width$}{}", "LoadUpvalue", i),
            Self::LoadConst(i) => write!(f, "{:width$}{}", "LoadConst", i),
            Self::StoreLocal(i) => write!(f, "{:width$}{}", "StoreLocal", i),
            Self::StoreGlobal(i) => write!(f, "{:width$}{}", "StoreGlobal", i),
            Self::StoreUpvalue(i) => write!(f, "{:width$}{}", "StoreUpvalue", i),
            Self::Import(i) => write!(f, "{:width$}{}", "Import", i),
            Self::ImportFrom(i) => write!(f, "{:width$}{}", "ImportFrom", i),
            Self::ImportGlob => write!(f, "{:width$}", "ImportGlob"),
            Self::BuildTable(i) => write!(f, "{:width$}{}", "BuildTable", i),
            Self::GetAttr => write!(f, "{:width$}", "GetAttr"),
            Self::GetItem => write!(f, "{:width$}", "GetItem"),
            Self::GetMeta => write!(f, "{:width$}", "GetMeta"),
            Self::SetAttr => write!(f, "{:width$}", "SetAttr"),
            Self::SetItem => write!(f, "{:width$}", "SetItem"),
            Self::SetMeta => write!(f, "{:width$}", "SetMeta"),
            Self::Neg => write!(f, "{:width$}", "Neg"),
            Self::Not => write!(f, "{:width$}", "Not"),
            Self::Add => write!(f, "{:width$}", "Add"),
            Self::Sub => write!(f, "{:width$}", "Sub"),
            Self::Mul => write!(f, "{:width$}", "Mul"),
            Self::Div => write!(f, "{:width$}", "Div"),
            Self::Mod => write!(f, "{:width$}", "Mod"),
            Self::Eq => write!(f, "{:width$}", "Eq"),
            Self::Ne => write!(f, "{:width$}", "Ne"),
            Self::Gt => write!(f, "{:width$}", "Gt"),
            Self::Ge => write!(f, "{:width$}", "Ge"),
            Self::Lt => write!(f, "{:width$}", "Lt"),
            Self::Le => write!(f, "{:width$}", "Le"),
            Self::Is => write!(f, "{:width$}", "Is"),
            Self::For(JumpTarget(i)) => write!(f, "{:width$}{}", "For", i),
            Self::Jump(JumpTarget(i)) => write!(f, "{:width$}{}", "Jump", i),
            Self::JumpIfNull(JumpTarget(i)) => write!(f, "{:width$}{}", "JumpIfNull", i),
            Self::JumpPopIfFalse(JumpTarget(i)) => write!(f, "{:width$}{}", "JumpPopIfFalse", i),
            Self::JumpIfTureOrPop(JumpTarget(i)) => write!(f, "{:width$}{}", "JumpIfTureOrPop", i),
            Self::JumpIfFalseOrPop(JumpTarget(i)) => {
                write!(f, "{:width$}{}", "JumpIfFalseOrPop", i)
            }
            Self::Call(i) => write!(f, "{:width$}{}", "Call", i),
            Self::TryCall(i) => write!(f, "{:width$}{}", "TryCall", i),
            Self::Return => write!(f, "{:width$}", "Return"),
            Self::Throw => write!(f, "{:width$}", "Throw"),
            Self::ReturnCall(i) => write!(f, "{:width$}{}", "ReturnCall", i),
            Self::JumpTarget(JumpTarget(i)) => write!(f, "{:width$}{}", "JumpTarget", i),
        }
    }
}
