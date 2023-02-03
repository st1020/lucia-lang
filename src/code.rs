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

    pub stack_size: usize,
}

impl PartialEq for Code {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

impl Code {
    pub fn dummy() -> Self {
        Code {
            params: Vec::new(),
            variadic: None,
            kind: FunctionKind::Funciton,
            code: Vec::new(),
            consts: Vec::new(),
            local_names: Vec::new(),
            global_names: Vec::new(),
            upvalue_names: Vec::new(),
            stack_size: 0,
        }
    }
}

/// Kind of function.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionKind {
    Funciton,
    Closure,
    Do,
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
