use crate::lvm::Lvm;
use crate::objects::table::{Keys, Values};
use crate::objects::{Table, Value};
use crate::{check_arguments_num, iter_to_value, try_convert};

pub fn libs(lvm: &mut Lvm) -> Table {
    let mut t = Table::new();
    t.set(
        &lvm.new_str_value("keys".to_string()),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, Eq(1));
            let table = try_convert!(lvm, args[0], as_table, Table);
            Ok(iter_to_value!(lvm, table.keys(), Keys, args[0]))
        }),
    );
    t.set(
        &lvm.new_str_value("values".to_string()),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, Eq(1));
            let table = try_convert!(lvm, args[0], as_table, Table);
            Ok(iter_to_value!(lvm, table.values(), Values, args[0]))
        }),
    );
    t.set(
        &lvm.new_str_value("raw_len".to_string()),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, Eq(1));
            let table = try_convert!(lvm, args[0], as_table, Table);
            Ok(Value::Int(table.len().try_into().unwrap()))
        }),
    );
    t.set(
        &lvm.new_str_value("raw_get".to_string()),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, Eq(2));
            let table = try_convert!(lvm, args[0], as_table, Table);
            Ok(table.get(&args[1]).copied().unwrap_or(Value::Null))
        }),
    );
    t.set(
        &lvm.new_str_value("raw_set".to_string()),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, Eq(3));
            let mut t = args[0];
            let table = try_convert!(lvm, t, as_table_mut, Table);
            table.set(&args[1], args[2]);
            Ok(Value::Null)
        }),
    );
    t.set(
        &lvm.new_str_value("raw_iter".to_string()),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, Eq(1));
            lvm.iter_table(args[0])
        }),
    );
    t
}
