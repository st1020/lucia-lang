use std::convert::TryFrom;

use crate::errors::{LucyError, TypeErrorKind};
use crate::lvm::Lvm;
use crate::object::{GCObjectKind, LucyTable, LucyValue, LucyValueType};
use crate::{call_arguments_error, str_to_value, type_convert_error};

pub fn libs(lvm: &mut Lvm) -> LucyTable {
    let mut t = LucyTable::new();
    t.set(
        &str_to_value!(lvm, "bool"),
        LucyValue::ExtFunction(|args, _| {
            if args.len() != 1 {
                return Err(call_arguments_error!(None, 1, args.len()));
            }
            Ok(LucyValue::Bool(match args.first().unwrap() {
                LucyValue::Null => false,
                LucyValue::Bool(v) => *v,
                LucyValue::Int(v) => {
                    if *v == 0 {
                        false
                    } else {
                        true
                    }
                }
                LucyValue::Float(v) => {
                    if *v == 0.0 {
                        false
                    } else {
                        true
                    }
                }
                LucyValue::ExtFunction(_) => true,
                LucyValue::GCObject(_) => true,
            }))
        }),
    );
    t.set(
        &str_to_value!(lvm, "int"),
        LucyValue::ExtFunction(|args, _| {
            if args.len() != 1 {
                return Err(call_arguments_error!(None, 1, args.len()));
            }
            let arg1 = args.first().unwrap();
            Ok(LucyValue::Int(match arg1 {
                LucyValue::Null => 0,
                LucyValue::Bool(v) => {
                    if *v {
                        1
                    } else {
                        0
                    }
                }
                LucyValue::Int(v) => *v,
                LucyValue::Float(v) => *v as i64,
                LucyValue::ExtFunction(_) => {
                    return Err(type_convert_error!(
                        LucyValueType::ExtFunction,
                        LucyValueType::Int
                    ))
                }
                LucyValue::GCObject(_) => String::try_from(*arg1)
                    .or_else(|_| Err(type_convert_error!(arg1.value_type(), LucyValueType::Int)))?
                    .parse()
                    .or_else(|_| {
                        Err(type_convert_error!(LucyValueType::Str, LucyValueType::Int))
                    })?,
            }))
        }),
    );
    t.set(
        &str_to_value!(lvm, "float"),
        LucyValue::ExtFunction(|args, _| {
            if args.len() != 1 {
                return Err(call_arguments_error!(None, 1, args.len()));
            }
            let arg1 = args.first().unwrap();
            Ok(LucyValue::Float(match arg1 {
                LucyValue::Null => 0.0,
                LucyValue::Bool(v) => {
                    if *v {
                        1.0
                    } else {
                        0.0
                    }
                }
                LucyValue::Int(v) => *v as f64,
                LucyValue::Float(v) => *v,
                LucyValue::ExtFunction(_) => {
                    return Err(type_convert_error!(
                        LucyValueType::ExtFunction,
                        LucyValueType::Float
                    ))
                }
                LucyValue::GCObject(_) => String::try_from(*arg1)
                    .or_else(|_| Err(type_convert_error!(arg1.value_type(), LucyValueType::Float)))?
                    .parse()
                    .or_else(|_| {
                        Err(type_convert_error!(
                            LucyValueType::Str,
                            LucyValueType::Float
                        ))
                    })?,
            }))
        }),
    );
    t.set(
        &str_to_value!(lvm, "str"),
        LucyValue::ExtFunction(|args, lvm| {
            if args.len() != 1 {
                return Err(call_arguments_error!(None, 1, args.len()));
            }
            Ok(lvm.new_gc_value(GCObjectKind::Str(args.first().unwrap().to_string())))
        }),
    );
    t
}
