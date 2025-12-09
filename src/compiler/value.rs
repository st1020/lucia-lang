use derive_more::Display;

/// The type of Value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Display)]
#[display("{}", self.name())]
pub enum ValueType {
    Null,
    Bool,
    Int,
    Float,
    Str,
    Bytes,
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
            ValueType::Bytes => "bytes",
            ValueType::Table => "table",
            ValueType::Function => "function",
            ValueType::UserData => "userdata",
        }
    }
}

/// The name of a metamethod.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Display)]
#[display("{}", self.name())]
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

macro_rules! metamethod_default {
    (1, $name:ident, $meta_name:ident) => {
        fn $name(self, ctx: Context) -> Result<Self::Result1, Self::Error> {
            Err(self.meta_error(ctx, MetaName::$meta_name, vec![]))
        }
    };
    (2, $name:ident, $meta_name:ident) => {
        fn $name(self, ctx: Context, other: Self::Value) -> Result<Self::Result2, Self::Error> {
            Err(self.meta_error(ctx, MetaName::$meta_name, vec![other]))
        }
    };
}

pub trait MetaMethod<Context>
where
    Self: Sized,
{
    type Value;
    type Error;
    type ResultCall;
    type ResultIter;
    type Result1;
    type Result2;
    type Result3;

    fn meta_call(self, ctx: Context) -> Result<Self::ResultCall, Self::Error> {
        Err(self.meta_error(ctx, MetaName::Call, vec![]))
    }
    fn meta_iter(self, ctx: Context) -> Result<Self::ResultIter, Self::Error> {
        Err(self.meta_error(ctx, MetaName::Iter, vec![]))
    }
    metamethod_default!(1, meta_len, Len);

    metamethod_default!(1, meta_bool, Bool);
    metamethod_default!(1, meta_int, Int);
    metamethod_default!(1, meta_float, Float);
    metamethod_default!(1, meta_str, Str);
    metamethod_default!(1, meta_repr, Repr);

    metamethod_default!(1, meta_neg, Neg);
    metamethod_default!(2, meta_add, Add);
    metamethod_default!(2, meta_sub, Sub);
    metamethod_default!(2, meta_mul, Mul);
    metamethod_default!(2, meta_div, Div);
    metamethod_default!(2, meta_rem, Rem);

    metamethod_default!(2, meta_eq, Eq);
    metamethod_default!(2, meta_ne, Ne);
    metamethod_default!(2, meta_gt, Gt);
    metamethod_default!(2, meta_ge, Ge);
    metamethod_default!(2, meta_lt, Lt);
    metamethod_default!(2, meta_le, Le);

    fn meta_get_attr(self, ctx: Context, key: Self::Value) -> Result<Self::Result2, Self::Error> {
        Err(self.meta_error(ctx, MetaName::GetAttr, vec![key]))
    }
    fn meta_get_item(self, ctx: Context, key: Self::Value) -> Result<Self::Result2, Self::Error> {
        Err(self.meta_error(ctx, MetaName::GetItem, vec![key]))
    }
    fn meta_set_attr(
        self,
        ctx: Context,
        key: Self::Value,
        value: Self::Value,
    ) -> Result<Self::Result3, Self::Error> {
        Err(self.meta_error(ctx, MetaName::SetAttr, vec![key, value]))
    }
    fn meta_set_item(
        self,
        ctx: Context,
        key: Self::Value,
        value: Self::Value,
    ) -> Result<Self::Result3, Self::Error> {
        Err(self.meta_error(ctx, MetaName::SetItem, vec![key, value]))
    }

    fn meta_error(self, ctx: Context, operator: MetaName, args: Vec<Self::Value>) -> Self::Error;
}
