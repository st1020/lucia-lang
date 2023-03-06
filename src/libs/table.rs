use crate::check_args;
use crate::lvm::Lvm;
use crate::objects::{ExtClosure, Table, Value};

pub fn libs(lvm: &mut Lvm) -> Table {
    let mut t = Table::new();
    t.set(
        &lvm.new_str_value("keys".to_string()),
        Value::ExtFunction(|args, lvm| {
            let (table_value,) = check_args!(lvm, args, Value);
            Ok(lvm.new_ext_closure_value(ExtClosure::new(
                |args, upvalues, lvm| {
                    check_args!(lvm, args);
                    let (table, index) = check_args!(lvm, upvalues, Table, Int);
                    let t = table
                        .get_index(index.try_into().unwrap())
                        .map_or_else(|| Value::Null, |(k, _)| *k);
                    drop(table);
                    upvalues[1] = Value::Int(index + 1);
                    Ok(t)
                },
                vec![table_value, Value::Int(0)],
            )))
        }),
    );
    t.set(
        &lvm.new_str_value("values".to_string()),
        Value::ExtFunction(|args, lvm| {
            let (table_value,) = check_args!(lvm, args, Value);
            Ok(lvm.new_ext_closure_value(ExtClosure::new(
                |args, upvalues, lvm| {
                    check_args!(lvm, args);
                    let (table, index) = check_args!(lvm, upvalues, Table, Int);
                    let t = table
                        .get_index(index.try_into().unwrap())
                        .map_or_else(|| Value::Null, |(_, v)| *v);
                    drop(table);
                    upvalues[1] = Value::Int(index + 1);
                    Ok(t)
                },
                vec![table_value, Value::Int(0)],
            )))
        }),
    );
    t.set(
        &lvm.new_str_value("raw_len".to_string()),
        Value::ExtFunction(|args, lvm| {
            let (table,) = check_args!(lvm, args, Table);
            Ok(Value::Int(table.len().try_into().unwrap()))
        }),
    );
    t.set(
        &lvm.new_str_value("raw_get".to_string()),
        Value::ExtFunction(|args, lvm| {
            let (table, k) = check_args!(lvm, args, Table, Value);
            Ok(table.get(&k).copied().unwrap_or(Value::Null))
        }),
    );
    t.set(
        &lvm.new_str_value("raw_set".to_string()),
        Value::ExtFunction(|mut args, lvm| {
            let (mut table, k, v) = check_args!(lvm, args, mut Table, Value, Value);
            table.set(&k, v);
            Ok(Value::Null)
        }),
    );
    t.set(
        &lvm.new_str_value("raw_iter".to_string()),
        Value::ExtFunction(|args, lvm| {
            let (table_value,) = check_args!(lvm, args, Value);
            Ok(lvm.iter_table(table_value))
        }),
    );
    t
}
