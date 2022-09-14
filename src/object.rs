use core::ptr::NonNull;
use std::convert::{TryFrom, TryInto};
use std::fmt::Debug;

use crate::codegen::Function;
use crate::errors::LucyError;
use crate::lvm::Lvm;
use crate::type_convert_error;

#[derive(Clone, Copy)]
pub enum LucyValue {
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    ExtFunction(fn(Vec<LucyValue>, &mut Lvm) -> LucyValue),
    GCObject(*mut GCObject),
}

impl Debug for LucyValue {
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

impl PartialEq for LucyValue {
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

impl ToString for LucyValue {
    fn to_string(&self) -> String {
        match self {
            LucyValue::Null => String::from("null"),
            LucyValue::Bool(v) => String::from(if *v { "true" } else { "false" }),
            LucyValue::Int(v) => String::from(v.to_string()),
            LucyValue::Float(v) => String::from(v.to_string()),
            LucyValue::ExtFunction(_) => String::from("function: ext_function"),
            LucyValue::GCObject(v) => unsafe {
                match &(**v).kind {
                    GCObjectKind::Str(v) => v.clone(),
                    GCObjectKind::Table(v) => v.to_string(),
                    GCObjectKind::Closuer(v) => v.to_string(),
                    GCObjectKind::ExtClosuer(_) => String::from("function: ext_closuer"),
                }
            },
        }
    }
}

macro_rules! impl_from_for_value {
    ($ty:ty, $kind:tt, $type_name:expr) => {
        impl From<$ty> for LucyValue {
            fn from(value: $ty) -> Self {
                LucyValue::$kind(value)
            }
        }

        impl TryFrom<LucyValue> for $ty {
            type Error = LucyError;

            fn try_from(value: LucyValue) -> Result<Self, Self::Error> {
                match value {
                    LucyValue::$kind(v) => Ok(v),
                    _ => Err(type_convert_error!(value.value_type(), $type_name)),
                }
            }
        }
    };
}

impl_from_for_value!(bool, Bool, LucyValueType::Bool);
impl_from_for_value!(i64, Int, LucyValueType::Int);
impl_from_for_value!(f64, Float, LucyValueType::Float);

impl TryFrom<LucyValue> for String {
    type Error = LucyError;

    fn try_from(value: LucyValue) -> Result<Self, Self::Error> {
        match value {
            LucyValue::GCObject(v) => unsafe {
                match &(*v).kind {
                    GCObjectKind::Str(v) => Ok(v.clone()),
                    _ => Err(type_convert_error!(value.value_type(), LucyValueType::Str)),
                }
            },
            _ => Err(type_convert_error!(value.value_type(), LucyValueType::Str)),
        }
    }
}

macro_rules! impl_try_from_value {
    ($ty:ty, $kind:tt, $type_name:expr) => {
        impl TryFrom<LucyValue> for $ty {
            type Error = LucyError;

            fn try_from(value: LucyValue) -> Result<Self, Self::Error> {
                match value {
                    LucyValue::GCObject(v) => unsafe {
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

impl_try_from_value!(&LucyTable, Table, LucyValueType::Table);
impl_try_from_value!(&Closuer, Closuer, LucyValueType::Closuer);
impl_try_from_value!(&mut LucyTable, Table, LucyValueType::Table);
impl_try_from_value!(&mut Closuer, Closuer, LucyValueType::Closuer);

impl LucyValue {
    pub fn value_type(&self) -> LucyValueType {
        match self {
            LucyValue::Null => LucyValueType::Null,
            LucyValue::Bool(_) => LucyValueType::Bool,
            LucyValue::Int(_) => LucyValueType::Int,
            LucyValue::Float(_) => LucyValueType::Float,
            LucyValue::ExtFunction(_) => LucyValueType::ExtFunction,
            LucyValue::GCObject(v) => match unsafe { v.as_ref() } {
                Some(v) => match v.kind {
                    GCObjectKind::Str(_) => LucyValueType::Str,
                    GCObjectKind::Table(_) => LucyValueType::Table,
                    GCObjectKind::Closuer(_) => LucyValueType::Closuer,
                    GCObjectKind::ExtClosuer(_) => LucyValueType::ExtClosuer,
                },
                None => LucyValueType::UnknownGCObject,
            },
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LucyValueType {
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

impl ToString for LucyValueType {
    fn to_string(&self) -> String {
        String::from(match self {
            LucyValueType::Null => "null",
            LucyValueType::Bool => "bool",
            LucyValueType::Int => "int",
            LucyValueType::Float => "float",
            LucyValueType::ExtFunction => "function",
            LucyValueType::UnknownGCObject => "unknown_object",
            LucyValueType::Str => "str",
            LucyValueType::Table => "table",
            LucyValueType::Closuer => "function",
            LucyValueType::ExtClosuer => "function",
        })
    }
}

pub enum GCObjectKind {
    Str(String),
    Table(LucyTable),
    Closuer(Closuer),
    ExtClosuer(Box<dyn FnMut(Vec<LucyValue>, &mut Lvm) -> LucyValue>),
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

#[derive(Debug, Clone, PartialEq)]
pub struct LucyTable(pub Vec<(LucyValue, LucyValue)>);

impl LucyTable {
    pub fn new() -> Self {
        LucyTable(Vec::new())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn get_by_index(&self, index: usize) -> Option<&(LucyValue, LucyValue)> {
        self.0.get(index)
    }

    pub fn raw_get(&self, key: &LucyValue) -> Option<LucyValue> {
        for (k, v) in &self.0 {
            if k == key {
                return Some(*v);
            }
        }
        None
    }

    pub fn raw_get_by_str(&self, key: &str) -> Option<LucyValue> {
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

    pub fn get(&self, key: &LucyValue) -> Option<LucyValue> {
        match self.raw_get(key) {
            Some(v) => Some(v),
            None => match self.raw_get_by_str("__base__") {
                Some(v) => {
                    let t: &LucyTable = v.try_into().unwrap();
                    t.get(key)
                }
                None => None,
            },
        }
    }

    pub fn get_by_str(&self, key: &str) -> Option<LucyValue> {
        match self.raw_get_by_str(key) {
            Some(v) => Some(v),
            None => match self.raw_get_by_str("__base__") {
                Some(v) => {
                    let t: &LucyTable = v.try_into().unwrap();
                    t.raw_get_by_str(key)
                }
                None => None,
            },
        }
    }

    pub fn set(&mut self, key: &LucyValue, value: LucyValue) {
        for i in 0..self.0.len() {
            let (k, _) = &self.0[i];
            if k == key {
                if value == LucyValue::Null {
                    self.0.remove(i);
                } else {
                    self.0[i] = (*key, value);
                }
                return;
            }
        }
        if value != LucyValue::Null {
            self.0.push((*key, value));
        }
    }
}

impl ToString for LucyTable {
    fn to_string(&self) -> String {
        format!(
            "{{{}}}",
            self.clone()
                .into_iter()
                .map(|(k, v)| format!("{}: {}", k.to_string(), v.to_string()))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

impl IntoIterator for LucyTable {
    type Item = (LucyValue, LucyValue);
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

#[derive(Debug, Clone)]
pub struct Closuer {
    pub module_id: usize,
    pub function: Function,
    pub base_closuer: Option<NonNull<GCObject>>,
    pub variables: Vec<LucyValue>,
}

impl PartialEq for Closuer {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

impl ToString for Closuer {
    fn to_string(&self) -> String {
        format!("function")
    }
}
