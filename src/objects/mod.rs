pub mod closure;
pub mod table;

use std::convert::TryFrom;
use std::fmt::{Debug, Display};
use std::hash::Hash;

use crate::errors::{BuiltinError, LResult};
use crate::lvm::Lvm;
use crate::type_convert_error;

pub use self::closure::Closure;
pub use self::table::LuciaTable;

// canonical raw float bit
const CANONICAL_NAN_BITS: u64 = 0x7ff8000000000000u64;
const CANONICAL_ZERO_BITS: u64 = 0x0u64;

/// Enum of all lucia values.
#[derive(Clone, Copy)]
pub enum LuciaValue {
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    ExtFunction(fn(Vec<LuciaValue>, &mut Lvm) -> LResult<LuciaValue>),
    GCObject(*mut GCObject),
}

impl Debug for LuciaValue {
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

impl PartialEq for LuciaValue {
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

impl Eq for LuciaValue {}

impl Hash for LuciaValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            LuciaValue::Null => 0.hash(state),
            LuciaValue::Bool(v) => v.hash(state),
            LuciaValue::Int(v) => v.hash(state),
            LuciaValue::Float(v) => {
                if v.is_nan() {
                    CANONICAL_NAN_BITS.hash(state)
                } else if *v == 0.0f64 {
                    CANONICAL_ZERO_BITS.hash(state)
                } else {
                    (*v).to_bits().hash(state)
                }
            }
            LuciaValue::ExtFunction(_) => 0.hash(state),
            LuciaValue::GCObject(ptr) => unsafe {
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

impl Display for LuciaValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LuciaValue::Null => write!(f, "null"),
            LuciaValue::Bool(v) => write!(f, "{}", if *v { "true" } else { "false" }),
            LuciaValue::Int(v) => write!(f, "{}", v),
            LuciaValue::Float(v) => write!(f, "{}", v),
            LuciaValue::ExtFunction(_) => write!(f, "function: ext_function"),
            LuciaValue::GCObject(v) => unsafe {
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

macro_rules! impl_from_for_value {
    ($ty:ty, $kind:tt, $type_name:expr) => {
        impl From<$ty> for LuciaValue {
            fn from(value: $ty) -> Self {
                LuciaValue::$kind(value)
            }
        }

        impl TryFrom<LuciaValue> for $ty {
            type Error = BuiltinError;

            fn try_from(value: LuciaValue) -> Result<Self, Self::Error> {
                match value {
                    LuciaValue::$kind(v) => Ok(v),
                    _ => Err(type_convert_error!(value.value_type(), $type_name)),
                }
            }
        }
    };
}

impl_from_for_value!(bool, Bool, LuciaValueType::Bool);
impl_from_for_value!(i64, Int, LuciaValueType::Int);
impl_from_for_value!(f64, Float, LuciaValueType::Float);

impl TryFrom<LuciaValue> for String {
    type Error = BuiltinError;

    fn try_from(value: LuciaValue) -> Result<Self, Self::Error> {
        match value {
            LuciaValue::GCObject(v) => unsafe {
                match &(*v).kind {
                    GCObjectKind::Str(v) => Ok(v.clone()),
                    _ => Err(type_convert_error!(value.value_type(), LuciaValueType::Str)),
                }
            },
            _ => Err(type_convert_error!(value.value_type(), LuciaValueType::Str)),
        }
    }
}

macro_rules! impl_try_from_value {
    ($ty:ty, $kind:tt, $type_name:expr) => {
        impl TryFrom<LuciaValue> for $ty {
            type Error = BuiltinError;

            fn try_from(value: LuciaValue) -> Result<Self, Self::Error> {
                match value {
                    LuciaValue::GCObject(v) => unsafe {
                        match &mut (*v).kind {
                            GCObjectKind::$kind(v) => Ok(v),
                            _ => Err(type_convert_error!(value.value_type(), $type_name)),
                        }
                    },
                    _ => Err(type_convert_error!(value.value_type(), $type_name)),
                }
            }
        }
    };
}

impl_try_from_value!(&LuciaTable, Table, LuciaValueType::Table);
impl_try_from_value!(&Closure, Closure, LuciaValueType::Closure);
impl_try_from_value!(&mut LuciaTable, Table, LuciaValueType::Table);
impl_try_from_value!(&mut Closure, Closure, LuciaValueType::Closure);

impl LuciaValue {
    pub fn value_type(&self) -> LuciaValueType {
        match self {
            LuciaValue::Null => LuciaValueType::Null,
            LuciaValue::Bool(_) => LuciaValueType::Bool,
            LuciaValue::Int(_) => LuciaValueType::Int,
            LuciaValue::Float(_) => LuciaValueType::Float,
            LuciaValue::ExtFunction(_) => LuciaValueType::ExtFunction,
            LuciaValue::GCObject(v) => match unsafe { v.as_ref() } {
                Some(v) => match v.kind {
                    GCObjectKind::Str(_) => LuciaValueType::Str,
                    GCObjectKind::Table(_) => LuciaValueType::Table,
                    GCObjectKind::Closure(_) => LuciaValueType::Closure,
                    GCObjectKind::ExtClosure(_) => LuciaValueType::ExtClosure,
                },
                None => LuciaValueType::UnknownGCObject,
            },
        }
    }

    pub fn is_error(&self) -> bool {
        match self {
            LuciaValue::GCObject(v) => unsafe { (**v).is_error },
            _ => false,
        }
    }
}

/// The type of LuciaValue.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LuciaValueType {
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

impl Display for LuciaValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                LuciaValueType::Null => "null",
                LuciaValueType::Bool => "bool",
                LuciaValueType::Int => "int",
                LuciaValueType::Float => "float",
                LuciaValueType::ExtFunction => "function",
                LuciaValueType::UnknownGCObject => "unknown_object",
                LuciaValueType::Str => "str",
                LuciaValueType::Table => "table",
                LuciaValueType::Closure => "function",
                LuciaValueType::ExtClosure => "function",
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

pub type ExtClosure = dyn FnMut(Vec<LuciaValue>, &mut Lvm) -> LResult<LuciaValue>;

/// Enum of all collectable objects.
pub enum GCObjectKind {
    Str(String),
    Table(LuciaTable),
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
