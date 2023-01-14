use crate::lvm::Lvm;
use crate::objects::{Table, Value, ValueType};
use crate::{builtin_error_to_table, check_arguments_num, type_convert_error};

pub fn libs(lvm: &mut Lvm) -> Table {
    let mut t = Table::new();
    t.set(
        &lvm.new_str_value("bool".to_string()),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, 1);
            Ok(Value::Bool(match args.first().unwrap() {
                Value::Null => false,
                Value::Bool(v) => *v,
                Value::Int(v) => *v != 0,
                Value::Float(v) => *v != 0.0,
                Value::ExtFunction(_) => true,
                Value::LightUserData(_) => true,
                Value::GCObject(_) => true,
            }))
        }),
    );
    t.set(
        &lvm.new_str_value("int".to_string()),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, 1);
            let arg1 = args.first().unwrap();
            Ok(Value::Int(match arg1 {
                Value::Null => 0,
                Value::Bool(v) => i64::from(*v),
                Value::Int(v) => *v,
                Value::Float(v) => *v as i64,
                Value::ExtFunction(_) => {
                    return Ok(builtin_error_to_table!(
                        lvm,
                        type_convert_error!(ValueType::ExtFunction, ValueType::Int)
                    ));
                }
                Value::LightUserData(_) => {
                    return Ok(builtin_error_to_table!(
                        lvm,
                        type_convert_error!(ValueType::LightUserData, ValueType::Int)
                    ));
                }
                Value::GCObject(_) => {
                    if let Some(v) = arg1.as_str() {
                        if let Ok(v) = v.parse() {
                            v
                        } else {
                            return Ok(builtin_error_to_table!(
                                lvm,
                                type_convert_error!(ValueType::Str, ValueType::Int)
                            ));
                        }
                    } else {
                        return Ok(builtin_error_to_table!(
                            lvm,
                            type_convert_error!(arg1.value_type(), ValueType::Int)
                        ));
                    }
                }
            }))
        }),
    );
    t.set(
        &lvm.new_str_value("float".to_string()),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, 1);
            let arg1 = args.first().unwrap();
            Ok(Value::Float(match arg1 {
                Value::Null => 0.0,
                Value::Bool(v) => {
                    if *v {
                        1.0
                    } else {
                        0.0
                    }
                }
                Value::Int(v) => *v as f64,
                Value::Float(v) => *v,
                Value::ExtFunction(_) => {
                    return Ok(builtin_error_to_table!(
                        lvm,
                        type_convert_error!(ValueType::ExtFunction, ValueType::Float)
                    ));
                }
                Value::LightUserData(_) => {
                    return Ok(builtin_error_to_table!(
                        lvm,
                        type_convert_error!(ValueType::LightUserData, ValueType::Float)
                    ));
                }
                Value::GCObject(_) => {
                    if let Some(v) = arg1.as_str() {
                        if let Ok(v) = v.parse() {
                            v
                        } else {
                            return Ok(builtin_error_to_table!(
                                lvm,
                                type_convert_error!(ValueType::Str, ValueType::Float)
                            ));
                        }
                    } else {
                        return Ok(builtin_error_to_table!(
                            lvm,
                            type_convert_error!(arg1.value_type(), ValueType::Float)
                        ));
                    }
                }
            }))
        }),
    );
    t.set(
        &lvm.new_str_value("str".to_string()),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, 1);
            Ok(lvm.new_str_value(args.first().unwrap().to_string()))
        }),
    );
    t
}
