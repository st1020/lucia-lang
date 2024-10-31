use std::fmt;

/// The type of Value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ValueType {
    Null,
    Bool,
    Int,
    Float,
    Str,
    Table,
    Function,
    UserData,
}

impl ValueType {
    pub const fn name(self) -> &'static str {
        match self {
            ValueType::Null => "null",
            ValueType::Bool => "bool",
            ValueType::Int => "int",
            ValueType::Float => "float",
            ValueType::Str => "str",
            ValueType::Table => "table",
            ValueType::Function => "function",
            ValueType::UserData => "userdata",
        }
    }
}

impl fmt::Display for ValueType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}
