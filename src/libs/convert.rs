use crate::lvm::Lvm;
use crate::objects::{Table, Value, ValueType};
use crate::{check_arguments_num, type_convert_error};

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
            macro_rules! return_convert_error {
                () => {
                    $crate::return_error!(
                        type_convert_error!(arg1.value_type(), ValueType::Int),
                        lvm
                    )
                };
            }
            Ok(Value::Int(match arg1 {
                Value::Null => 0,
                Value::Bool(v) => i64::from(*v),
                Value::Int(v) => *v,
                Value::Float(v) => *v as i64,
                Value::ExtFunction(_) => return_convert_error!(),
                Value::LightUserData(_) => return_convert_error!(),
                Value::GCObject(_) => {
                    if let Some(v) = arg1.as_str() {
                        match v.parse() {
                            Ok(v) => v,
                            Err(_) => return_convert_error!(),
                        }
                    } else {
                        return_convert_error!();
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
            macro_rules! return_convert_error {
                () => {
                    $crate::return_error!(
                        type_convert_error!(arg1.value_type(), ValueType::Float),
                        lvm
                    )
                };
            }
            Ok(Value::Float(match arg1 {
                Value::Null => 0.0,
                Value::Bool(v) => f64::from(u8::from(*v)),
                Value::Int(v) => *v as f64,
                Value::Float(v) => *v,
                Value::ExtFunction(_) => return_convert_error!(),
                Value::LightUserData(_) => return_convert_error!(),
                Value::GCObject(_) => {
                    if let Some(v) = arg1.as_str() {
                        match v.parse() {
                            Ok(v) => v,
                            Err(_) => return_convert_error!(),
                        }
                    } else {
                        return_convert_error!();
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
