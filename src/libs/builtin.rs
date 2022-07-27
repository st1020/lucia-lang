use crate::lvm::Lvm;
use crate::object::{GCObject, GCObjectKind, LucyValue};

pub const TYPE: fn(Vec<LucyValue>, &mut Lvm) -> LucyValue = |args, lvm| {
    if args.len() != 1 {
        panic!()
    }
    LucyValue::GCObject(
        lvm.new_gc_object(GCObject::new(GCObjectKind::Str(String::from(
            match args.first().unwrap() {
                LucyValue::Null => "null",
                LucyValue::Bool(_) => "bool",
                LucyValue::Int(_) => "int",
                LucyValue::Float(_) => "float",
                LucyValue::ExtFunction(_) => "function",
                LucyValue::GCObject(v) => unsafe {
                    match &(**v).kind {
                        GCObjectKind::Str(_) => "str",
                        GCObjectKind::Table(_) => "table",
                        GCObjectKind::Closuer(_) => "function",
                    }
                },
            },
        )))),
    )
};

pub const ID: fn(Vec<LucyValue>, &mut Lvm) -> LucyValue = |args, _| {
    if args.len() != 1 {
        panic!()
    }
    match args.first().unwrap() {
        LucyValue::GCObject(v) => LucyValue::Int((*v as usize).try_into().unwrap()),
        _ => LucyValue::Null,
    }
};
