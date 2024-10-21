//! The Code that runs in LVM.

use std::fmt;

use smol_str::SmolStr;

use crate::utils::{Float, Join};

pub use super::ast::FunctionKind;
use super::opcode::OpCode;

/// A Code, generated by codegen and executed in LVM.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Code {
    /// Name of parameters.
    pub params: Vec<SmolStr>,
    /// Name of variadic parameter.
    pub variadic: Option<SmolStr>,
    /// Function kind.
    pub kind: FunctionKind,
    /// Bytecode, a list of OpCodes.
    pub code: Vec<OpCode>,

    /// List of constants used in the bytecode.
    pub consts: Vec<ConstValue>,
    /// List of local names.
    pub local_names: Vec<SmolStr>,
    /// List of global names.
    pub global_names: Vec<SmolStr>,
    /// List of Upvalue information.
    pub upvalue_names: Vec<(SmolStr, Option<usize>)>,

    /// The required virtual machine stack space.
    pub stack_size: usize,
}

impl fmt::Display for Code {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(v) = &self.variadic {
            writeln!(f, "params: ({}), *{}", self.params.join(", "), v)?;
        } else {
            writeln!(f, "params: ({})", self.params.join(", "))?;
        }
        writeln!(f, "kind: {}", self.kind)?;
        writeln!(f, "stack_size: {}", self.stack_size)?;
        let code_str = self
            .code
            .iter()
            .enumerate()
            .map(|(i, code)| {
                format!(
                    "{i:>12} {code}{}",
                    match code {
                        OpCode::LoadLocal(i) | OpCode::StoreLocal(i) =>
                            format!(" ({})", self.local_names[*i]),
                        OpCode::LoadGlobal(i) | OpCode::StoreGlobal(i) =>
                            format!(" ({})", self.global_names[*i]),
                        OpCode::LoadUpvalue(i) | OpCode::StoreUpvalue(i) => {
                            let (name, base_closure_upvalue_id) = &self.upvalue_names[*i];
                            format!(" ({}, {:?})", name, base_closure_upvalue_id)
                        }
                        OpCode::LoadConst(i) | OpCode::Import(i) | OpCode::ImportFrom(i) =>
                            format!(" ({})", self.consts[*i]),
                        _ => "".to_string(),
                    }
                )
            })
            .join("\n");
        write!(f, "code:\n{}", code_str)
    }
}

/// The const value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstValue {
    /// "null"
    Null,
    /// "true", "false"
    Bool(bool),
    /// "12", "0o100", "0b110"
    Int(i64),
    /// "12.34", "0b100.100"
    Float(Float),
    /// ""abc"", ""abc"
    Str(SmolStr),
    /// A function.
    Func(Box<Code>),
}

impl fmt::Display for ConstValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Null => write!(f, "null"),
            Self::Bool(v) => write!(f, "{}", v),
            Self::Int(v) => write!(f, "{}", v),
            Self::Float(v) => write!(f, "{}", v),
            Self::Str(v) => write!(f, "{}", v),
            Self::Func(_) => write!(f, "<code>"),
        }
    }
}
