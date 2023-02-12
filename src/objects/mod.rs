pub mod closure;
pub mod ext_closure;
pub mod table;
pub mod userdata;

use std::fmt::{Debug, Display};
use std::hash::Hash;

use crate::errors::{BuiltinError, Result};
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

pub type ExtFunction = fn(Vec<Value>, &mut Lvm) -> Result<Value>;

/// Enum of all lucia values.
#[derive(Clone, Copy)]
pub enum Value {
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    ExtFunction(ExtFunction),
    LightUserData(*mut u8),
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
            Self::LightUserData(_) => f.debug_tuple("LightUserData").finish(),
            Self::GCObject(arg0) => unsafe { Debug::fmt(&(**arg0).kind, f) },
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
            Value::LightUserData(ptr) => ptr.hash(state),
            Value::GCObject(ptr) => unsafe {
                match &(**ptr).kind {
                    GCObjectKind::Str(v) => v.hash(state),
                    GCObjectKind::Table(_) => ptr.hash(state),
                    GCObjectKind::UserData(_) => ptr.hash(state),
                    GCObjectKind::Closure(_) => ptr.hash(state),
                    GCObjectKind::ExtClosure(_) => ptr.hash(state),
                }
            },
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
            Value::GCObject(v) => unsafe { write!(f, "{}", **v) },
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
            Value::ExtFunction(_) => true,
            Value::LightUserData(_) => true,
            Value::GCObject(_) => true,
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
            Value::ExtFunction(_) => {
                Err(type_convert_error!(ValueType::ExtFunction, ValueType::Int))
            }
            Value::LightUserData(_) => Err(type_convert_error!(
                ValueType::LightUserData,
                ValueType::Int
            )),
            Value::GCObject(_) => {
                if let Some(v) = value.as_str() {
                    v.parse()
                        .map_err(|_| type_convert_error!(ValueType::Str, ValueType::Int))
                } else {
                    Err(type_convert_error!(value.value_type(), ValueType::Int))
                }
            }
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
            Value::ExtFunction(_) => Err(type_convert_error!(
                ValueType::ExtFunction,
                ValueType::Float
            )),
            Value::LightUserData(_) => Err(type_convert_error!(
                ValueType::LightUserData,
                ValueType::Float
            )),
            Value::GCObject(_) => {
                if let Some(v) = value.as_str() {
                    v.parse()
                        .map_err(|_| type_convert_error!(ValueType::Str, ValueType::Float))
                } else {
                    Err(type_convert_error!(value.value_type(), ValueType::Float))
                }
            }
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
            Value::GCObject(v) => match unsafe { v.as_ref() } {
                Some(v) => match v.kind {
                    GCObjectKind::Str(_) => ValueType::Str,
                    GCObjectKind::Table(_) => ValueType::Table,
                    GCObjectKind::UserData(_) => ValueType::UserData,
                    GCObjectKind::Closure(_) => ValueType::Closure,
                    GCObjectKind::ExtClosure(_) => ValueType::ExtClosure,
                },
                None => ValueType::UnknownGCObject,
            },
        }
    }

    #[inline]
    pub fn metatable(&self) -> Option<&Table> {
        if let Some(t) = self.as_table() {
            t.metatable.as_table()
        } else if let Some(t) = self.as_userdata() {
            Some(&t.metatable)
        } else {
            None
        }
    }

    #[inline]
    pub fn is_error(&self) -> bool {
        match self {
            Value::GCObject(v) => unsafe { (**v).is_error },
            _ => false,
        }
    }

    #[inline]
    pub fn is(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::GCObject(v1), Value::GCObject(v2)) => v1 == v2,
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

/// The type of LuciaValue.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueType {
    Null,
    Bool,
    Int,
    Float,
    ExtFunction,
    LightUserData,
    UnknownGCObject,
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
                ValueType::UnknownGCObject => "unknown_object",
                ValueType::Str => "str",
                ValueType::Table => "table",
                ValueType::UserData => "userdata",
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

impl PartialEq for GCObject {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl Display for GCObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
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

/// Enum of all collectable objects.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GCObjectKind {
    Str(String),
    Table(Table),
    UserData(UserData),
    Closure(Closure),
    ExtClosure(ExtClosure),
}

impl Display for GCObjectKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GCObjectKind::Str(v) => write!(f, "{}", v),
            GCObjectKind::Table(v) => write!(f, "{}", v),
            GCObjectKind::Closure(v) => write!(f, "{}", v),
            GCObjectKind::ExtClosure(v) => write!(f, "{}", v),
            GCObjectKind::UserData(v) => write!(f, "{}", v),
        }
    }
}

#[macro_export]
macro_rules! iter_to_value {
    ($lvm:expr, $iter:expr, $ty:ty) => {{
        let mut userdata_table = $crate::objects::Table::new();
        userdata_table.set(
            &$lvm.get_builtin_str("__call__"),
            $crate::objects::Value::ExtFunction(|mut args, lvm| {
                $crate::check_arguments_num!(lvm, args, None, Eq(1));
                let t = try_convert!(lvm, args[0], as_userdata_mut, UserData);
                let iter = unsafe { (t.ptr as *mut $ty).as_mut().unwrap() };
                Ok(iter.next().copied().unwrap_or($crate::objects::Value::Null))
            }),
        );
        $lvm.new_userdata_value($crate::objects::UserData::new(
            Box::into_raw(Box::new($iter)) as *mut u8,
            userdata_table,
            |userdata| unsafe {
                userdata.ptr.drop_in_place();
                std::alloc::dealloc(userdata.ptr as *mut u8, std::alloc::Layout::new::<$ty>());
            },
        ))
    }};
    ($lvm:expr, $iter:expr, $ty:ty, $marker_value:expr) => {{
        let mut userdata_table = $crate::table![$marker_value];
        userdata_table.set(
            &$lvm.get_builtin_str("__call__"),
            $crate::objects::Value::ExtFunction(|mut args, lvm| {
                $crate::check_arguments_num!(lvm, args, None, Eq(1));
                let t = try_convert!(lvm, args[0], as_userdata_mut, UserData);
                let iter = unsafe { (t.ptr as *mut $ty).as_mut().unwrap() };
                Ok(iter.next().copied().unwrap_or($crate::objects::Value::Null))
            }),
        );
        $lvm.new_userdata_value($crate::objects::UserData::new(
            Box::into_raw(Box::new($iter)) as *mut u8,
            userdata_table,
            |userdata| unsafe {
                userdata.ptr.drop_in_place();
                std::alloc::dealloc(userdata.ptr as *mut u8, std::alloc::Layout::new::<$ty>());
            },
        ))
    }};
    ($lvm:expr, $iter:expr, $ty:ty, $marker_value:expr, $map_fn:expr) => {{
        let mut userdata_table = $crate::table![$marker_value];
        userdata_table.set(
            &$lvm.get_builtin_str("__call__"),
            $crate::objects::Value::ExtFunction(|mut args, lvm| {
                $crate::check_arguments_num!(lvm, args, None, Eq(1));
                let t = try_convert!(lvm, args[0], as_userdata_mut, UserData);
                let iter = unsafe { (t.ptr as *mut $ty).as_mut().unwrap() };
                Ok(iter
                    .next()
                    .map(|x| $map_fn(x, lvm))
                    .unwrap_or($crate::objects::Value::Null))
            }),
        );
        $lvm.new_userdata_value($crate::objects::UserData::new(
            Box::into_raw(Box::new($iter)) as *mut u8,
            userdata_table,
            |userdata| unsafe {
                userdata.ptr.drop_in_place();
                std::alloc::dealloc(userdata.ptr as *mut u8, std::alloc::Layout::new::<$ty>());
            },
        ))
    }};
}
