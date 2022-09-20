use core::ptr::NonNull;
use std::convert::{TryFrom, TryInto};
use std::fmt::{Debug, Display};

use crate::codegen::Function;
use crate::errors::{LResult, LuciaError};
use crate::lvm::Lvm;
use crate::type_convert_error;

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
            (Self::Float(l0), Self::Float(r0)) => l0 == r0,
            (Self::ExtFunction(_), Self::ExtFunction(_)) => false,
            (Self::GCObject(l0), Self::GCObject(r0)) => unsafe { **l0 == **r0 },
            _ => false,
        }
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
                    GCObjectKind::Closuer(v) => write!(f, "{}", v),
                    GCObjectKind::ExtClosuer(_) => write!(f, "function: ext_closuer"),
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
            type Error = LuciaError;

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
    type Error = LuciaError;

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
            type Error = LuciaError;

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
impl_try_from_value!(&Closuer, Closuer, LuciaValueType::Closuer);
impl_try_from_value!(&mut LuciaTable, Table, LuciaValueType::Table);
impl_try_from_value!(&mut Closuer, Closuer, LuciaValueType::Closuer);

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
                    GCObjectKind::Closuer(_) => LuciaValueType::Closuer,
                    GCObjectKind::ExtClosuer(_) => LuciaValueType::ExtClosuer,
                },
                None => LuciaValueType::UnknownGCObject,
            },
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
    Closuer,
    ExtClosuer,
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
                LuciaValueType::Closuer => "function",
                LuciaValueType::ExtClosuer => "function",
            }
        )
    }
}

/// All collectable objects.
#[derive(Debug)]
pub struct GCObject {
    pub kind: GCObjectKind,
    pub gc_state: bool,
}

impl GCObject {
    pub fn new(kind: GCObjectKind) -> Self {
        Self {
            kind,
            gc_state: false,
        }
    }
}

impl PartialEq for GCObject {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

/// Enum of all collectable objects.
pub enum GCObjectKind {
    Str(String),
    Table(LuciaTable),
    Closuer(Closuer),
    ExtClosuer(Box<dyn FnMut(Vec<LuciaValue>, &mut Lvm) -> LResult<LuciaValue>>),
}

impl Debug for GCObjectKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Str(arg0) => f.debug_tuple("Str").field(arg0).finish(),
            Self::Table(arg0) => f.debug_tuple("Table").field(arg0).finish(),
            Self::Closuer(arg0) => f.debug_tuple("Closuer").field(arg0).finish(),
            Self::ExtClosuer(_) => f.debug_tuple("ExtClosuer").finish(),
        }
    }
}

impl PartialEq for GCObjectKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Str(l0), Self::Str(r0)) => l0 == r0,
            (Self::Table(l0), Self::Table(r0)) => l0 == r0,
            (Self::Closuer(l0), Self::Closuer(r0)) => l0 == r0,
            (Self::ExtClosuer(_), Self::ExtClosuer(_)) => false,
            _ => false,
        }
    }
}

/// The table implement.
#[derive(Debug, Clone, PartialEq)]
pub struct LuciaTable(pub Vec<(LuciaValue, LuciaValue)>);

impl LuciaTable {
    pub fn new() -> Self {
        LuciaTable(Vec::new())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn get_by_index(&self, index: usize) -> Option<&(LuciaValue, LuciaValue)> {
        self.0.get(index)
    }

    pub fn raw_get(&self, key: &LuciaValue) -> Option<LuciaValue> {
        for (k, v) in &self.0 {
            if k == key {
                return Some(*v);
            }
        }
        None
    }

    pub fn raw_get_by_str(&self, key: &str) -> Option<LuciaValue> {
        for (k, v) in &self.0 {
            match String::try_from(*k) {
                Ok(k) => {
                    if k == key {
                        return Some(*v);
                    }
                }
                Err(_) => (),
            }
        }
        None
    }

    pub fn get(&self, key: &LuciaValue) -> Option<LuciaValue> {
        match self.raw_get(key) {
            Some(v) => Some(v),
            None => match self.raw_get_by_str("__base__") {
                Some(v) => {
                    let t: &LuciaTable = v.try_into().unwrap();
                    t.get(key)
                }
                None => None,
            },
        }
    }

    pub fn get_by_str(&self, key: &str) -> Option<LuciaValue> {
        match self.raw_get_by_str(key) {
            Some(v) => Some(v),
            None => match self.raw_get_by_str("__base__") {
                Some(v) => {
                    let t: &LuciaTable = v.try_into().unwrap();
                    t.raw_get_by_str(key)
                }
                None => None,
            },
        }
    }

    pub fn set(&mut self, key: &LuciaValue, value: LuciaValue) {
        for i in 0..self.0.len() {
            let (k, _) = &self.0[i];
            if k == key {
                if value == LuciaValue::Null {
                    self.0.remove(i);
                } else {
                    self.0[i] = (*key, value);
                }
                return;
            }
        }
        if value != LuciaValue::Null {
            self.0.push((*key, value));
        }
    }
}

impl Display for LuciaTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{{}}}",
            self.clone()
                .into_iter()
                .map(|(k, v)| format!("{}: {}", k, v))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

impl IntoIterator for LuciaTable {
    type Item = (LuciaValue, LuciaValue);
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl From<Vec<LuciaValue>> for LuciaTable {
    fn from(value: Vec<LuciaValue>) -> Self {
        let mut temp: Vec<(LuciaValue, LuciaValue)> = Vec::new();
        for (i, v) in value.iter().enumerate() {
            temp.push((LuciaValue::Int(i.try_into().unwrap()), *v))
        }
        LuciaTable(temp)
    }
}

/// The closuer object. Any function is a closuer.
#[derive(Debug, Clone)]
pub struct Closuer {
    pub module_id: usize,
    pub function: Function,
    pub base_closuer: Option<NonNull<GCObject>>,
    pub variables: Vec<LuciaValue>,
}

impl PartialEq for Closuer {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

impl Display for Closuer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "function")
    }
}
