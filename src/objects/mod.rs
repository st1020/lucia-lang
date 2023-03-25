//! The Lucai Objects.

mod closure;
mod ext_closure;
mod table;
mod userdata;

use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::num::NonZeroUsize;

use crate::errors::{BuiltinError, Result};
use crate::gc::{Gc, Ref, RefCell, RefMut, Trace};
use crate::lvm::Lvm;
use crate::type_convert_error;
use crate::utils::escape_str;

pub use self::closure::Closure;
pub use self::ext_closure::{ExtClosure, ExtClosureFunc};
pub use self::table::Table;
pub use self::userdata::UserData;

// canonical raw float bit
const CANONICAL_NAN_BITS: u64 = 0x7ff8000000000000u64;
const CANONICAL_ZERO_BITS: u64 = 0x0u64;

/// The ext function type.
pub type ExtFunction = fn(Vec<Value>, &mut Lvm) -> Result<Value>;

/// Enum of all lucia values.
#[derive(Clone, Copy)]
pub enum Value {
    /// `null` - A null value.
    Null,
    /// `bool` - A `true` / `false` value.
    Bool(bool),
    /// `int` - A 64-bit integer.
    Int(i64),
    /// `float` - A 64-bit floating point number.
    Float(f64),
    /// `ext_function` - A Rust function pointer.
    ExtFunction(ExtFunction),
    /// `light_user_data` -  A raw pointer.
    LightUserData(*mut u8),
    /// `str` - A UTF-8 string.
    Str(Gc<String>),
    /// `table` - A table.
    Table(Gc<RefCell<Table>>),
    /// `userdata` - A UserData.
    UserData(Gc<RefCell<UserData>>),
    /// `closure` - A closure.
    Closure(Gc<RefCell<Closure>>),
    /// `ext_closure` - A closure defined in Rust.
    ExtClosure(Gc<RefCell<ExtClosure>>),
}

unsafe impl Trace for Value {
    #[inline]
    unsafe fn trace(&self) {
        match self {
            Value::Str(v) => v.trace(),
            Value::Table(v) => v.trace(),
            Value::UserData(v) => v.trace(),
            Value::Closure(v) => v.trace(),
            Value::ExtClosure(v) => v.trace(),
            _ => (),
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Null => write!(f, "Null"),
            Self::Bool(arg0) => f.debug_tuple("Bool").field(arg0).finish(),
            Self::Int(arg0) => f.debug_tuple("Int").field(arg0).finish(),
            Self::Float(arg0) => f.debug_tuple("Float").field(arg0).finish(),
            Self::ExtFunction(_) => f.debug_tuple("ExtFunction").finish(),
            Self::LightUserData(_) => f.debug_tuple("LightUserData").finish(),
            Self::Str(arg0) => f.debug_tuple("Str").field(arg0).finish(),
            Self::Table(arg0) => f.debug_tuple("Table").field(&arg0.borrow()).finish(),
            Self::UserData(arg0) => f.debug_tuple("UserData").field(&arg0.borrow()).finish(),
            Self::Closure(arg0) => f.debug_tuple("Closure").field(&arg0.borrow()).finish(),
            Self::ExtClosure(arg0) => f.debug_tuple("ExtClosure").field(&arg0.borrow()).finish(),
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
            (Self::Str(l0), Self::Str(r0)) => l0 == r0,
            (Self::Table(l0), Self::Table(r0)) => l0 == r0,
            (Self::UserData(l0), Self::UserData(r0)) => l0 == r0,
            (Self::Closure(l0), Self::Closure(r0)) => l0 == r0,
            (Self::ExtClosure(l0), Self::ExtClosure(r0)) => l0 == r0,
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
            Value::LightUserData(ptr) => ptr.hash(state),
            Value::Str(v) => (**v).hash(state),
            Value::Table(_v) => 0.hash(state),
            Value::UserData(_v) => 0.hash(state),
            Value::Closure(_v) => 0.hash(state),
            Value::ExtClosure(_v) => 0.hash(state),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Bool(v) => write!(f, "{}", if *v { "true" } else { "false" }),
            Value::Int(v) => write!(f, "{}", v),
            Value::Float(v) => write!(f, "{}", v),
            Value::ExtFunction(_) => write!(f, "<function(ext_function)>"),
            Value::LightUserData(_) => write!(f, "<userdata>"),
            Value::Str(v) => write!(f, "{}", v),
            Value::Table(v) => write!(f, "{}", v.borrow()),
            Value::UserData(v) => write!(f, "{}", v.borrow()),
            Value::Closure(v) => write!(f, "{}", v.borrow()),
            Value::ExtClosure(v) => write!(f, "{}", v.borrow()),
        }
    }
}

macro_rules! impl_value {
    ((), Null, $is_ident:ident, $as_ident:ident) => {
        impl Value {
            #[inline]
            pub fn $is_ident(&self) -> bool {
                self.$as_ident().is_some()
            }

            #[inline]
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
            #[inline]
            pub fn $is_ident(&self) -> bool {
                self.$as_ident().is_some()
            }

            #[inline]
            pub fn $as_ident(&self) -> Option<&str> {
                match self {
                    Value::Str(v) => Some((**v).as_str()),
                    _ => None,
                }
            }
        }
    };

    ($ty:ty, $kind:tt, $is_ident:ident, $as_ident:ident) => {
        impl Value {
            #[inline]
            pub fn $is_ident(&self) -> bool {
                self.$as_ident().is_some()
            }

            #[inline]
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
            #[inline]
            pub fn $is_ident(&self) -> bool {
                self.$as_ident().is_some()
            }

            #[inline]
            pub fn $as_ident(&self) -> Option<Ref<$ty>> {
                match self {
                    Value::$kind(v) => Some(v.borrow()),
                    _ => None,
                }
            }

            #[inline]
            pub fn $as_mut_ident(&self) -> Option<RefMut<$ty>> {
                match self {
                    Value::$kind(v) => Some(v.borrow_mut()),
                    _ => None,
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
impl_value!(*mut u8, LightUserData, is_light_userdata, as_light_userdata);
impl_value!(str, Str, is_str, as_str);
impl_value!(Table, Table, is_table, as_table, as_table_mut);
impl_value!(
    UserData,
    UserData,
    is_userdata,
    as_userdata,
    as_userdata_mut
);
impl_value!(Closure, Closure, is_closure, as_closure, as_closure_mut);
impl_value!(
    ExtClosure,
    ExtClosure,
    is_ext_closure,
    as_ext_closure,
    as_ext_closure_mut
);

impl From<Value> for String {
    fn from(value: Value) -> Self {
        value.to_string()
    }
}

impl From<Value> for bool {
    fn from(value: Value) -> Self {
        match value {
            Value::Null => false,
            Value::Bool(v) => v,
            Value::Int(v) => v != 0,
            Value::Float(v) => v != 0.0,
            _ => true,
        }
    }
}

impl TryFrom<Value> for i64 {
    type Error = BuiltinError;

    fn try_from(value: Value) -> std::result::Result<Self, Self::Error> {
        match value {
            Value::Null => Ok(0),
            Value::Bool(v) => Ok(i64::from(v)),
            Value::Int(v) => Ok(v),
            Value::Float(v) => Ok(v as i64),
            Value::Str(v) => v
                .parse()
                .map_err(|_| type_convert_error!(ValueType::Str, ValueType::Int)),
            _ => Err(type_convert_error!(value.value_type(), ValueType::Int)),
        }
    }
}

impl TryFrom<Value> for f64 {
    type Error = BuiltinError;

    fn try_from(value: Value) -> std::result::Result<Self, Self::Error> {
        match value {
            Value::Null => Ok(0.0),
            Value::Bool(v) => Ok(if v { 1.0 } else { 0.0 }),
            Value::Int(v) => Ok(v as f64),
            Value::Float(v) => Ok(v),
            Value::Str(v) => v
                .parse()
                .map_err(|_| type_convert_error!(ValueType::Str, ValueType::Int)),
            _ => Err(type_convert_error!(value.value_type(), ValueType::Float)),
        }
    }
}

impl Value {
    #[inline]
    pub fn value_type(&self) -> ValueType {
        match self {
            Value::Null => ValueType::Null,
            Value::Bool(_) => ValueType::Bool,
            Value::Int(_) => ValueType::Int,
            Value::Float(_) => ValueType::Float,
            Value::ExtFunction(_) => ValueType::ExtFunction,
            Value::LightUserData(_) => ValueType::LightUserData,
            Value::Str(_) => ValueType::Str,
            Value::Table(_) => ValueType::Table,
            Value::UserData(_) => ValueType::UserData,
            Value::Closure(_) => ValueType::Closure,
            Value::ExtClosure(_) => ValueType::ExtClosure,
        }
    }

    #[inline]
    pub fn id(&self) -> Option<NonZeroUsize> {
        match self {
            Value::Str(v) => Some(v.addr()),
            Value::Table(v) => Some(v.addr()),
            Value::UserData(v) => Some(v.addr()),
            Value::Closure(v) => Some(v.addr()),
            Value::ExtClosure(v) => Some(v.addr()),
            _ => None,
        }
    }

    #[inline]
    pub fn set_error(&mut self) -> bool {
        match self {
            Value::Str(v) => v.set_error(),
            Value::Table(v) => v.set_error(),
            Value::UserData(v) => v.set_error(),
            Value::Closure(v) => v.set_error(),
            Value::ExtClosure(v) => v.set_error(),
            _ => return false,
        }
        true
    }

    #[inline]
    pub fn is_error(&self) -> bool {
        match self {
            Value::Str(v) => v.is_error(),
            Value::Table(v) => v.is_error(),
            Value::UserData(v) => v.is_error(),
            Value::Closure(v) => v.is_error(),
            Value::ExtClosure(v) => v.is_error(),
            _ => false,
        }
    }

    #[inline]
    pub fn is(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Str(v1), Value::Str(v2)) => v1.addr() == v2.addr(),
            (Value::Table(v1), Value::Table(v2)) => v1.addr() == v2.addr(),
            (Value::UserData(v1), Value::UserData(v2)) => v1.addr() == v2.addr(),
            (Value::Closure(v1), Value::Closure(v2)) => v1.addr() == v2.addr(),
            (Value::ExtClosure(v1), Value::ExtClosure(v2)) => v1.addr() == v2.addr(),
            _ => self == other,
        }
    }

    #[inline]
    pub fn repr(&self) -> String {
        if let Some(s) = self.as_str() {
            format!("\"{}\"", escape_str(s, false))
        } else if let Some(t) = self.as_table() {
            t.repr_table(self)
        } else {
            self.to_string()
        }
    }
}

/// The type of Value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueType {
    Null,
    Bool,
    Int,
    Float,
    ExtFunction,
    LightUserData,
    Str,
    Table,
    UserData,
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
                ValueType::LightUserData => "userdata",
                ValueType::Str => "str",
                ValueType::Table => "table",
                ValueType::UserData => "userdata",
                ValueType::Closure => "function",
                ValueType::ExtClosure => "function",
            }
        )
    }
}

#[macro_export]
macro_rules! as_value_type {
    ($value:expr, Null) => {
        $value.as_null()
    };
    ($value:expr, Int) => {
        $value.as_int()
    };
    ($value:expr, Float) => {
        $value.as_float()
    };
    ($value:expr, ExtFunction) => {
        $value.as_ext_function()
    };
    ($value:expr, LightUserData) => {
        $value.as_light_userdata()
    };
    ($value:expr, Str) => {
        $value.as_str()
    };
    ($value:expr, Table) => {
        $value.as_table()
    };
    ($value:expr, Table, mut) => {
        $value.as_table_mut()
    };
    ($value:expr, UserData) => {
        $value.as_userdata()
    };
    ($value:expr, UserData, mut) => {
        $value.as_userdata_mut()
    };
    ($value:expr, Closure) => {
        $value.as_closure()
    };
    ($value:expr, Closure, mut) => {
        $value.as_closure_mut()
    };
    ($value:expr, ExtClosure) => {
        $value.as_ext_closure()
    };
    ($value:expr, ExtClosure, mut) => {
        $value.as_ext_closure_mut()
    };
}

#[macro_export]
macro_rules! try_as_value_type {
    ($lvm:ident, $value:ident, $ty:tt, mut) => {{
        let t = $value.clone();
        match $crate::as_value_type!($value, $ty, mut) {
            Some(val) => val,
            None => $crate::return_error!(
                $lvm,
                $crate::unexpect_type_error!(t.value_type(), vec![$crate::objects::ValueType::$ty])
            ),
        }
    }};

    ($lvm:ident, $value:ident, $ty:tt) => {{
        match $crate::as_value_type!($value, $ty) {
            Some(val) => val,
            None => $crate::return_error!(
                $lvm,
                $crate::unexpect_type_error!(
                    $value.value_type(),
                    vec![$crate::objects::ValueType::$ty]
                )
            ),
        }
    }};
}
