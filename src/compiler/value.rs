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

/// The name of a metamethod.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MetaName {
    Call,
    Iter,
    GetAttr,
    GetItem,
    SetAttr,
    SetItem,
    Neg,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Eq,
    Ne,
    Gt,
    Ge,
    Lt,
    Le,
    Len,
    Bool,
    Int,
    Float,
    Str,
    Repr,
}

impl MetaName {
    pub const fn name(self) -> &'static str {
        match self {
            MetaName::Call => "__call__",
            MetaName::Iter => "__iter__",
            MetaName::GetAttr => "__getattr__",
            MetaName::GetItem => "__getitem__",
            MetaName::SetAttr => "__setattr__",
            MetaName::SetItem => "__setitem__",
            MetaName::Neg => "__neg__",
            MetaName::Add => "__add__",
            MetaName::Sub => "__sub__",
            MetaName::Mul => "__mul__",
            MetaName::Div => "__div__",
            MetaName::Rem => "__rem__",
            MetaName::Eq => "__eq__",
            MetaName::Ne => "__ne__",
            MetaName::Gt => "__gt__",
            MetaName::Ge => "__ge__",
            MetaName::Lt => "__lt__",
            MetaName::Le => "__le__",
            MetaName::Len => "__len__",
            MetaName::Bool => "__bool__",
            MetaName::Int => "__int__",
            MetaName::Float => "__float__",
            MetaName::Str => "__str__",
            MetaName::Repr => "__repr__",
        }
    }
}

impl AsRef<str> for MetaName {
    fn as_ref(&self) -> &str {
        self.name()
    }
}

impl fmt::Display for MetaName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

pub trait MetaMethod<Value> {
    type ResultCall;
    type ResultIter;
    type Result1;
    type Result2;
    type Result3;

    fn call(&self, value: Value) -> Self::ResultCall;
    fn iter(&self, value: Value) -> Self::ResultIter;
    fn get_attr(&self, table: Value, key: Value) -> Self::Result2;
    fn get_item(&self, table: Value, key: Value) -> Self::Result2;
    fn set_attr(&self, table: Value, key: Value, value: Value) -> Self::Result3;
    fn set_item(&self, table: Value, key: Value, value: Value) -> Self::Result3;
    fn neg(&self, value: Value) -> Self::Result1;
    fn add(&self, lhs: Value, rhs: Value) -> Self::Result2;
    fn sub(&self, lhs: Value, rhs: Value) -> Self::Result2;
    fn mul(&self, lhs: Value, rhs: Value) -> Self::Result2;
    fn div(&self, lhs: Value, rhs: Value) -> Self::Result2;
    fn rem(&self, lhs: Value, rhs: Value) -> Self::Result2;
    fn eq(&self, lhs: Value, rhs: Value) -> Self::Result2;
    fn ne(&self, lhs: Value, rhs: Value) -> Self::Result2;
    fn gt(&self, lhs: Value, rhs: Value) -> Self::Result2;
    fn ge(&self, lhs: Value, rhs: Value) -> Self::Result2;
    fn lt(&self, lhs: Value, rhs: Value) -> Self::Result2;
    fn le(&self, lhs: Value, rhs: Value) -> Self::Result2;
    fn len(&self, value: Value) -> Self::Result1;
    fn bool(&self, value: Value) -> Self::Result1;
    fn int(&self, value: Value) -> Self::Result1;
    fn float(&self, value: Value) -> Self::Result1;
    fn str(&self, value: Value) -> Self::Result1;
    fn repr(&self, value: Value) -> Self::Result1;
}
