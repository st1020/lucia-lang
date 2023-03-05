use std::fmt::Display;

pub use crate::ast::FunctionKind;
use crate::opcode::OpCode;

/// A function.
#[derive(Debug, Clone)]
pub struct Code {
    pub params: Vec<String>,
    pub variadic: Option<String>,
    pub kind: FunctionKind,
    pub code: Vec<OpCode>,

    pub consts: Vec<ConstlValue>,
    pub local_names: Vec<String>,
    pub global_names: Vec<String>,
    pub upvalue_names: Vec<(String, usize, usize)>,

    pub def_upvalue_count: usize,
    pub stack_size: usize,
}

impl Display for Code {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(v) = &self.variadic {
            writeln!(f, "params: ({}), *{}", self.params.join(", "), v)?;
        } else {
            writeln!(f, "params: ({})", self.params.join(", "))?;
        }
        writeln!(f, "kind: {}", self.kind)?;
        writeln!(f, "stack_size: {}", self.stack_size)?;
        let mut code_str = String::new();
        for (i, code) in self.code.iter().enumerate() {
            code_str.push_str(&format!(
                "{:>12} {}{}\n",
                i,
                code,
                match code {
                    OpCode::LoadLocal(i) | OpCode::StoreLocal(i) =>
                        format!(" ({})", self.local_names[*i]),
                    OpCode::LoadGlobal(i) | OpCode::StoreGlobal(i) =>
                        format!(" ({})", self.global_names[*i]),
                    OpCode::LoadUpvalue(i) | OpCode::StoreUpvalue(i) => {
                        let t = &self.upvalue_names[*i];
                        format!(" ({}, {}, {})", t.0, t.1, t.2)
                    }
                    OpCode::LoadConst(i) | OpCode::Import(i) | OpCode::ImportFrom(i) =>
                        format!(" ({})", self.consts[*i]),
                    _ => "".to_string(),
                }
            ));
        }
        write!(f, "code:\n{}", code_str)
    }
}

impl PartialEq for Code {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

impl Display for FunctionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionKind::Funciton => write!(f, "Funciton"),
            FunctionKind::Closure => write!(f, "Closure"),
            FunctionKind::Do => write!(f, "Do"),
        }
    }
}

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
    Func(Code),
}

impl Display for ConstlValue {
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
