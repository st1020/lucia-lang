use std::convert::TryFrom;

use crate::lvm::Lvm;
use crate::objects::{GCObjectKind, LuciaTable, LuciaValue, LuciaValueType};
use crate::{check_arguments_num, str_to_value, type_convert_error};

pub fn libs(lvm: &mut Lvm) -> LuciaTable {
    let mut t = LuciaTable::new();
    t.set(
        &str_to_value!(lvm, "bool"),
        LuciaValue::ExtFunction(|args, _| {
            check_arguments_num!(args, None, 1);
            Ok(LuciaValue::Bool(match args.first().unwrap() {
                LuciaValue::Null => false,
                LuciaValue::Bool(v) => *v,
                LuciaValue::Int(v) => *v != 0,
                LuciaValue::Float(v) => *v != 0.0,
                LuciaValue::ExtFunction(_) => true,
                LuciaValue::GCObject(_) => true,
            }))
        }),
    );
    t.set(
        &str_to_value!(lvm, "int"),
        LuciaValue::ExtFunction(|args, _| {
            check_arguments_num!(args, None, 1);
            let arg1 = args.first().unwrap();
            Ok(LuciaValue::Int(match arg1 {
                LuciaValue::Null => 0,
                LuciaValue::Bool(v) => i64::from(*v),
                LuciaValue::Int(v) => *v,
                LuciaValue::Float(v) => *v as i64,
                LuciaValue::ExtFunction(_) => {
                    return Err(type_convert_error!(
                        LuciaValueType::ExtFunction,
                        LuciaValueType::Int
                    ))
                }
                LuciaValue::GCObject(_) => String::try_from(*arg1)
                    .map_err(|_| type_convert_error!(arg1.value_type(), LuciaValueType::Int))?
                    .parse()
                    .map_err(|_| type_convert_error!(LuciaValueType::Str, LuciaValueType::Int))?,
            }))
        }),
    );
    t.set(
        &str_to_value!(lvm, "float"),
        LuciaValue::ExtFunction(|args, _| {
            check_arguments_num!(args, None, 1);
            let arg1 = args.first().unwrap();
            Ok(LuciaValue::Float(match arg1 {
                LuciaValue::Null => 0.0,
                LuciaValue::Bool(v) => {
                    if *v {
                        1.0
                    } else {
                        0.0
                    }
                }
                LuciaValue::Int(v) => *v as f64,
                LuciaValue::Float(v) => *v,
                LuciaValue::ExtFunction(_) => {
                    return Err(type_convert_error!(
                        LuciaValueType::ExtFunction,
                        LuciaValueType::Float
                    ))
                }
                LuciaValue::GCObject(_) => String::try_from(*arg1)
                    .map_err(|_| type_convert_error!(arg1.value_type(), LuciaValueType::Float))?
                    .parse()
                    .map_err(|_| type_convert_error!(LuciaValueType::Str, LuciaValueType::Float))?,
            }))
        }),
    );
    t.set(
        &str_to_value!(lvm, "str"),
        LuciaValue::ExtFunction(|args, lvm| {
            check_arguments_num!(args, None, 1);
            Ok(lvm.new_gc_value(GCObjectKind::Str(args.first().unwrap().to_string())))
        }),
    );
    t
}
