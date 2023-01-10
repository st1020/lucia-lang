pub mod closure;
pub mod table;

use std::fmt::{Debug, Display};
use std::hash::Hash;

use crate::errors::Result;
use crate::lvm::Lvm;

pub use self::closure::Closure;
pub use self::table::Table;

// canonical raw float bit
const CANONICAL_NAN_BITS: u64 = 0x7ff8000000000000u64;
const CANONICAL_ZERO_BITS: u64 = 0x0u64;

pub type ExtFunction = fn(Vec<Value>, &mut Lvm) -> Result<Value>;

/// Enum of all lucia values.
#[derive(Clone, Copy)]
pub enum Value {
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    ExtFunction(ExtFunction),
    GCObject(*mut GCObject),
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Null => write!(f, "Null"),
            Self::Bool(arg0) => f.debug_tuple("Bool").field(arg0).finish(),
            Self::Int(arg0) => f.debug_tuple("Int").field(arg0).finish(),
            Self::Float(arg0) => f.debug_tuple("Float").field(arg0).finish(),
            Self::ExtFunction(_) => f.debug_tuple("ExtFunction").finish(),
            Self::GCObject(arg0) => unsafe {
                f.debug_tuple("GCObject").field(&(**arg0).kind).finish()
            },
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Null, Self::Null) => true,
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::Int(l0), Self::Int(r0)) => l0 == r0,
            (Self::Float(l0), Self::Float(r0)) => {
                if l0.is_nan() {
                    r0.is_nan()
                } else {
                    l0 == r0
                }
            }
            (Self::ExtFunction(_), Self::ExtFunction(_)) => false,
            (Self::GCObject(l0), Self::GCObject(r0)) => unsafe { **l0 == **r0 },
            _ => false,
        }
    }
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Value::Null => 0.hash(state),
            Value::Bool(v) => v.hash(state),
            Value::Int(v) => v.hash(state),
            Value::Float(v) => {
                if v.is_nan() {
                    CANONICAL_NAN_BITS.hash(state)
                } else if *v == 0.0f64 {
                    CANONICAL_ZERO_BITS.hash(state)
                } else {
                    (*v).to_bits().hash(state)
                }
            }
            Value::ExtFunction(_) => 0.hash(state),
            Value::GCObject(ptr) => unsafe {
                match &(**ptr).kind {
                    GCObjectKind::Str(v) => v.hash(state),
                    GCObjectKind::Table(_) => ptr.hash(state),
                    GCObjectKind::Closure(_) => ptr.hash(state),
                    GCObjectKind::ExtClosure(_) => ptr.hash(state),
                }
            },
        }
        // core::mem::discriminant(self).hash(state);
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Bool(v) => write!(f, "{}", if *v { "true" } else { "false" }),
            Value::Int(v) => write!(f, "{}", v),
            Value::Float(v) => write!(f, "{}", v),
            Value::ExtFunction(_) => write!(f, "function: ext_function"),
            Value::GCObject(v) => unsafe {
                match &(**v).kind {
                    GCObjectKind::Str(v) => write!(f, "{}", v),
                    GCObjectKind::Table(v) => write!(f, "{}", v),
                    GCObjectKind::Closure(v) => write!(f, "{}", v),
                    GCObjectKind::ExtClosure(_) => write!(f, "function: ext_closure"),
                }
            },
        }
    }
}

macro_rules! impl_value {
    ((), Null, $is_ident:ident, $as_ident:ident) => {
        impl Value {
            pub fn $is_ident(&self) -> bool {
                self.$as_ident().is_some()
            }

            pub fn $as_ident(&self) -> Option<()> {
                match *self {
                    Value::Null => Some(()),
                    _ => None,
                }
            }
        }
    };

    (str, Str, $is_ident:ident, $as_ident:ident) => {
        impl Value {
            pub fn $is_ident(&self) -> bool {
                self.$as_ident().is_some()
            }

            pub fn $as_ident(&self) -> Option<&str> {
                if let Value::GCObject(v) = self {
                    unsafe {
                        match &(**v).kind {
                            GCObjectKind::Str(v) => Some(v),
                            _ => None,
                        }
                    }
                } else {
                    None
                }
            }
        }
    };

    ($ty:ty, $kind:tt, $is_ident:ident, $as_ident:ident) => {
        impl Value {
            pub fn $is_ident(&self) -> bool {
                self.$as_ident().is_some()
            }

            pub fn $as_ident(&self) -> Option<$ty> {
                match *self {
                    Value::$kind(v) => Some(v),
                    _ => None,
                }
            }
        }
    };

    ($ty:ty, $kind:tt, $is_ident:ident, $as_ident:ident, $as_mut_ident:ident) => {
        impl Value {
            pub fn $is_ident(&self) -> bool {
                self.$as_ident().is_some()
            }

            pub fn $as_ident(&self) -> Option<&$ty> {
                if let Value::GCObject(v) = self {
                    unsafe {
                        match &(**v).kind {
                            GCObjectKind::$kind(v) => Some(v),
                            _ => None,
                        }
                    }
                } else {
                    None
                }
            }

            pub fn $as_mut_ident(&mut self) -> Option<&mut $ty> {
                if let Value::GCObject(v) = self {
                    unsafe {
                        match &mut (**v).kind {
                            GCObjectKind::$kind(v) => Some(v),
                            _ => None,
                        }
                    }
                } else {
                    None
                }
            }
        }
    };
}

impl_value!((), Null, is_null, as_null);
impl_value!(bool, Bool, is_bool, as_bool);
impl_value!(i64, Int, is_int, as_int);
impl_value!(f64, Float, is_float, as_float);
impl_value!(ExtFunction, ExtFunction, is_ext_function, as_ext_function);
impl_value!(str, Str, is_str, as_str);
impl_value!(Table, Table, is_table, as_table, as_table_mut);
impl_value!(Closure, Closure, is_closure, as_closure, as_closure_mut);
impl_value!(
    ExtClosure,
    ExtClosure,
    is_ext_closure,
    as_ext_closure,
    as_ext_closure_mut
);

impl Value {
    pub fn value_type(&self) -> ValueType {
        match self {
            Value::Null => ValueType::Null,
            Value::Bool(_) => ValueType::Bool,
            Value::Int(_) => ValueType::Int,
            Value::Float(_) => ValueType::Float,
            Value::ExtFunction(_) => ValueType::ExtFunction,
            Value::GCObject(v) => match unsafe { v.as_ref() } {
                Some(v) => match v.kind {
                    GCObjectKind::Str(_) => ValueType::Str,
                    GCObjectKind::Table(_) => ValueType::Table,
                    GCObjectKind::Closure(_) => ValueType::Closure,
                    GCObjectKind::ExtClosure(_) => ValueType::ExtClosure,
                },
                None => ValueType::UnknownGCObject,
            },
        }
    }

    pub fn is_error(&self) -> bool {
        match self {
            Value::GCObject(v) => unsafe { (**v).is_error },
            _ => false,
        }
    }
}

/// The type of LuciaValue.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueType {
    Null,
    Bool,
    Int,
    Float,
    ExtFunction,
    UnknownGCObject,
    Str,
    Table,
    Closure,
    ExtClosure,
}

impl Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ValueType::Null => "null",
                ValueType::Bool => "bool",
                ValueType::Int => "int",
                ValueType::Float => "float",
                ValueType::ExtFunction => "function",
                ValueType::UnknownGCObject => "unknown_object",
                ValueType::Str => "str",
                ValueType::Table => "table",
                ValueType::Closure => "function",
                ValueType::ExtClosure => "function",
            }
        )
    }
}

/// All collectable objects.
#[derive(Debug)]
pub struct GCObject {
    pub kind: GCObjectKind,
    pub gc_state: bool,
    pub is_error: bool,
}

impl GCObject {
    pub fn new(kind: GCObjectKind) -> Self {
        Self {
            kind,
            gc_state: false,
            is_error: false,
        }
    }
}

impl PartialEq for GCObject {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

pub type ExtClosure = dyn FnMut(Vec<Value>, &mut Lvm) -> Result<Value>;

/// Enum of all collectable objects.
pub enum GCObjectKind {
    Str(String),
    Table(Table),
    Closure(Closure),
    ExtClosure(Box<ExtClosure>),
}

impl Debug for GCObjectKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Str(arg0) => f.debug_tuple("Str").field(arg0).finish(),
            Self::Table(arg0) => f.debug_tuple("Table").field(arg0).finish(),
            Self::Closure(arg0) => f.debug_tuple("Closure").field(arg0).finish(),
            Self::ExtClosure(_) => f.debug_tuple("ExtClosure").finish(),
        }
    }
}

impl PartialEq for GCObjectKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Str(l0), Self::Str(r0)) => l0 == r0,
            (Self::Table(l0), Self::Table(r0)) => l0 == r0,
            (Self::Closure(l0), Self::Closure(r0)) => l0 == r0,
            (Self::ExtClosure(_), Self::ExtClosure(_)) => false,
            _ => false,
        }
    }
}
