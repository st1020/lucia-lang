use core::ptr::NonNull;
use std::fmt::Debug;

use crate::codegen::Function;
use crate::lvm::Lvm;

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
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::Int(l0), Self::Int(r0)) => l0 == r0,
            (Self::Float(l0), Self::Float(r0)) => l0 == r0,
            (Self::ExtFunction(_), Self::ExtFunction(_)) => false,
            (Self::GCObject(l0), Self::GCObject(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
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
                }
            },
        }
    }
}

impl From<bool> for LucyValue {
    fn from(value: bool) -> Self {
        LucyValue::Bool(value)
    }
}

impl From<i64> for LucyValue {
    fn from(value: i64) -> Self {
        LucyValue::Int(value)
    }
}

impl From<f64> for LucyValue {
    fn from(value: f64) -> Self {
        LucyValue::Float(value)
    }
}

impl TryFrom<LucyValue> for String {
    type Error = &'static str;

    fn try_from(value: LucyValue) -> Result<Self, Self::Error> {
        match value {
            LucyValue::GCObject(v) => unsafe {
                match &(*v).kind {
                    GCObjectKind::Str(v) => Ok(v.clone()),
                    _ => Err("LucyValue is not string type!"),
                }
            },
            _ => Err("LucyValue is not string type!"),
        }
    }
}

impl TryFrom<LucyValue> for &LucyTable {
    type Error = &'static str;

    fn try_from(value: LucyValue) -> Result<Self, Self::Error> {
        match value {
            LucyValue::GCObject(v) => unsafe {
                match &(*v).kind {
                    GCObjectKind::Table(v) => Ok(v),
                    _ => Err("LucyValue is not String type!"),
                }
            },
            _ => Err("LucyValue is not String type!"),
        }
    }
}

impl TryFrom<LucyValue> for &mut LucyTable {
    type Error = &'static str;

    fn try_from(value: LucyValue) -> Result<Self, Self::Error> {
        match value {
            LucyValue::GCObject(v) => unsafe {
                match &mut (*v).kind {
                    GCObjectKind::Table(v) => Ok(v),
                    _ => Err("LucyValue is not String type!"),
                }
            },
            _ => Err("LucyValue is not String type!"),
        }
    }
}

impl TryFrom<LucyValue> for &Closuer {
    type Error = &'static str;

    fn try_from(value: LucyValue) -> Result<Self, Self::Error> {
        match value {
            LucyValue::GCObject(v) => unsafe {
                match &(*v).kind {
                    GCObjectKind::Closuer(v) => Ok(v),
                    _ => Err("LucyValue is not Closuer type!"),
                }
            },
            _ => Err("LucyValue is not Closuer type!"),
        }
    }
}

impl TryFrom<LucyValue> for &mut Closuer {
    type Error = &'static str;

    fn try_from(value: LucyValue) -> Result<Self, Self::Error> {
        match value {
            LucyValue::GCObject(v) => unsafe {
                match &mut (*v).kind {
                    GCObjectKind::Closuer(v) => Ok(v),
                    _ => Err("LucyValue is not Closuer type!"),
                }
            },
            _ => Err("LucyValue is not Closuer type!"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum GCObjectKind {
    Str(String),
    Table(LucyTable),
    Closuer(Closuer),
}

#[derive(Debug, Clone)]
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

    pub fn raw_get(&self, key: &LucyValue) -> Option<LucyValue> {
        for (k, v) in &self.0 {
            if k == key {
                return Some(*v);
            }
            match (k, key) {
                (LucyValue::GCObject(k), LucyValue::GCObject(key)) => unsafe {
                    match (&(**k).kind, &(**key).kind) {
                        (GCObjectKind::Str(k), GCObjectKind::Str(key)) => {
                            if k == key {
                                return Some(*v);
                            }
                        }
                        _ => (),
                    }
                },
                _ => (),
            }
        }
        None
    }

    pub fn raw_get_by_str(&self, key: &str) -> Option<LucyValue> {
        for (k, v) in &self.0 {
            if let LucyValue::GCObject(k) = k {
                unsafe {
                    if let GCObjectKind::Str(k) = &(**k).kind {
                        if k == key {
                            return Some(*v);
                        }
                    }
                }
            }
        }
        None
    }

    pub fn get(&self, key: &LucyValue) -> Option<LucyValue> {
        let mut t = self;
        loop {
            match t.raw_get(key) {
                Some(v) => return Some(v),
                None => match t.raw_get_by_str("__base__") {
                    Some(v) => {
                        if let LucyValue::GCObject(v) = v {
                            unsafe {
                                if let GCObjectKind::Table(v) = &(*v).kind {
                                    t = v;
                                } else {
                                    break;
                                }
                            }
                        } else {
                            break;
                        }
                    }
                    None => break,
                },
            }
        }
        None
    }

    pub fn get_by_str(&self, key: &str) -> Option<LucyValue> {
        let mut t = self;
        loop {
            match t.raw_get_by_str(key) {
                Some(v) => return Some(v),
                None => match t.raw_get_by_str("__base__") {
                    Some(v) => {
                        if let LucyValue::GCObject(v) = v {
                            unsafe {
                                if let GCObjectKind::Table(v) = &(*v).kind {
                                    t = v;
                                } else {
                                    break;
                                }
                            }
                        } else {
                            break;
                        }
                    }
                    None => break,
                },
            }
        }
        None
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
    pub module_id: u32,
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
