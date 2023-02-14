use crate::lvm::Lvm;
use crate::objects::table::{Keys, Values};
use crate::objects::{Table, Value};
use crate::{check_args, iter_to_value};

pub fn libs(lvm: &mut Lvm) -> Table {
    let mut t = Table::new();
    t.set(
        &lvm.new_str_value("keys".to_string()),
        Value::ExtFunction(|args, lvm| {
            let (table,) = check_args!(lvm, args, Table);
            Ok(iter_to_value!(lvm, table.keys(), Keys, args[0]))
        }),
    );
    t.set(
        &lvm.new_str_value("values".to_string()),
        Value::ExtFunction(|args, lvm| {
            let (table,) = check_args!(lvm, args, Table);
            Ok(iter_to_value!(lvm, table.values(), Values, args[0]))
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
            let (table, k, v) = check_args!(lvm, args, mut Table, Value, Value);
            table.set(&k, v);
            Ok(Value::Null)
        }),
    );
    t.set(
        &lvm.new_str_value("raw_iter".to_string()),
        Value::ExtFunction(|args, lvm| {
            let (table_value,) = check_args!(lvm, args, Value);
            lvm.iter_table(table_value)
        }),
    );
    t
}
