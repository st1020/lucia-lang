use crate::check_arguments_num;
use crate::lvm::Lvm;
use crate::objects::{Table, Value};

pub fn libs(lvm: &mut Lvm) -> Table {
    let mut t = Table::new();
    t.set(
        &lvm.new_str_value("keys".to_string()),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, 1);
            let table: &mut Table = (*args.first().unwrap()).try_into().unwrap();
            let mut keys = table.keys();
            Ok(lvm.new_ext_closure_value(Box::new(move |_, _| {
                Ok(if let Some(v) = keys.next() {
                    *v
                } else {
                    Value::Null
                })
            })))
        }),
    );
    t.set(
        &lvm.new_str_value("values".to_string()),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, 1);
            let table: &mut Table = (*args.first().unwrap()).try_into().unwrap();
            let mut values = table.values();
            Ok(lvm.new_ext_closure_value(Box::new(move |_, _| {
                Ok(if let Some(v) = values.next() {
                    *v
                } else {
                    Value::Null
                })
            })))
        }),
    );
    t.set(
        &lvm.new_str_value("raw_len".to_string()),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, 1);
            let table: &mut Table = (*args.first().unwrap()).try_into().unwrap();
            Ok(Value::Int(table.len().try_into().unwrap()))
        }),
    );
    t.set(
        &lvm.new_str_value("raw_get".to_string()),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, 2);
            let table: &mut Table = (*args.first().unwrap()).try_into().unwrap();
            Ok(match table.raw_get(args.last().unwrap()) {
                Some(v) => v,
                None => Value::Null,
            })
        }),
    );
    t.set(
        &lvm.new_str_value("raw_set".to_string()),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, 3);
            let table: &mut Table = (*args.first().unwrap()).try_into().unwrap();
            let key = args.get(1).unwrap();
            let value = args.get(2).unwrap();
            table.set(key, *value);
            Ok(Value::Null)
        }),
    );
    t
}
