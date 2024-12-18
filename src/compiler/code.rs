//! The Code that runs in LVM.

use std::{fmt, hash, mem};

use crate::utils::{Float, Join};

pub use super::ast::FunctionKind;
use super::opcode::OpCode;

/// A Code, generated by codegen and executed in LVM.
#[derive(Debug, Clone)]
pub struct Code<S> {
    /// Function name.
    pub name: Option<S>,
    /// Name of parameters.
    pub params: Vec<S>,
    /// Name of variadic parameter.
    pub variadic: Option<S>,
    /// Function kind.
    pub kind: FunctionKind,
    /// Bytecode, a list of OpCodes.
    pub code: Vec<OpCode>,

    /// List of constants used in the bytecode.
    pub consts: Vec<ConstValue<S>>,
    /// List of local names.
    pub local_names: Vec<S>,
    /// List of global names.
    pub global_names: Vec<S>,
    /// List of Upvalue information.
    pub upvalue_names: Vec<(S, Option<usize>)>,

    /// The required virtual machine stack space.
    pub stack_size: usize,
}

impl<S: AsRef<str>> fmt::Display for Code<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(name) = &self.name {
            writeln!(f, "name: {}", name.as_ref())?;
        }
        if let Some(v) = &self.variadic {
            writeln!(
                f,
                "params: ({}, ...{})",
                self.params.iter().map(AsRef::as_ref).join(", "),
                v.as_ref()
            )?;
        } else {
            writeln!(
                f,
                "params: ({})",
                self.params.iter().map(AsRef::as_ref).join(", ")
            )?;
        }
        writeln!(f, "kind: {}", self.kind)?;
        writeln!(f, "stack_size: {}", self.stack_size)?;
        writeln!(f, "consts: {}", self.consts.iter().join(", "))?;
        writeln!(
            f,
            "local_names: {}",
            self.local_names.iter().map(AsRef::as_ref).join(", ")
        )?;
        writeln!(
            f,
            "global_names: {}",
            self.global_names.iter().map(AsRef::as_ref).join(", ")
        )?;
        writeln!(
            f,
            "upvalue_names: {}",
            self.upvalue_names
                .iter()
                .map(|(name, base_closure_upvalue_id)| {
                    format!("({}, {:?})", name.as_ref(), base_closure_upvalue_id)
                })
                .join(", ")
        )?;
        let code_str = self
            .code
            .iter()
            .enumerate()
            .map(|(i, code)| {
                format!(
                    "{i:>12} {code}{}",
                    match code {
                        OpCode::LoadLocal(i) | OpCode::StoreLocal(i) =>
                            format!(" ({})", self.local_names[*i].as_ref()),
                        OpCode::LoadGlobal(i) | OpCode::StoreGlobal(i) =>
                            format!(" ({})", self.global_names[*i].as_ref()),
                        OpCode::LoadUpvalue(i) | OpCode::StoreUpvalue(i) => {
                            let (name, base_closure_upvalue_id) = &self.upvalue_names[*i];
                            format!(" ({}, {:?})", name.as_ref(), base_closure_upvalue_id)
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
#[derive(Debug, Clone)]
pub enum ConstValue<S> {
    /// "null"
    Null,
    /// "true", "false"
    Bool(bool),
    /// "12", "0o100", "0b110"
    Int(i64),
    /// "12.34", "0b100.100"
    Float(Float),
    /// ""abc"", ""abc"
    Str(S),
    /// A function code.
    Code(Box<Code<S>>),
}

impl<S: AsRef<str>> fmt::Display for ConstValue<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Null => write!(f, "null"),
            Self::Bool(v) => write!(f, "{}", v),
            Self::Int(v) => write!(f, "{}", v),
            Self::Float(v) => write!(f, "{}", v),
            Self::Str(v) => write!(f, "{}", v.as_ref()),
            Self::Code(_) => write!(f, "<code>"),
        }
    }
}

impl<S: AsRef<str>> PartialEq for ConstValue<S> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Null, Self::Null) => true,
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::Int(l0), Self::Int(r0)) => l0 == r0,
            (Self::Float(l0), Self::Float(r0)) => l0 == r0,
            (Self::Str(l0), Self::Str(r0)) => l0.as_ref() == r0.as_ref(),
            (Self::Code(_), Self::Code(_)) => false,
            _ => false,
        }
    }
}

impl<S: AsRef<str>> Eq for ConstValue<S> {}

impl<S: AsRef<str>> hash::Hash for ConstValue<S> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        mem::discriminant(self).hash(state);
        match self {
            ConstValue::Null => (),
            ConstValue::Bool(v) => v.hash(state),
            ConstValue::Int(v) => v.hash(state),
            ConstValue::Float(v) => v.hash(state),
            ConstValue::Str(v) => v.as_ref().hash(state),
            ConstValue::Code(_) => (),
        }
    }
}
