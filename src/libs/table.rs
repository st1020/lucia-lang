use crate::lvm::Lvm;
use crate::objects::{GCObjectKind, LuciaTable, LuciaValue};
use crate::{check_arguments_num, str_to_value};

pub fn libs(lvm: &mut Lvm) -> LuciaTable {
    let mut t = LuciaTable::new();
    t.set(
        &str_to_value!(lvm, "keys"),
        LuciaValue::ExtFunction(|args, lvm| {
            check_arguments_num!(args, None, 1);
            let table: &mut LuciaTable = (*args.first().unwrap()).try_into().unwrap();
            let mut keys = table.keys();
            Ok(
                lvm.new_gc_value(GCObjectKind::ExtClosure(Box::new(move |_, _| {
                    Ok(if let Some(v) = keys.next() {
                        *v
                    } else {
                        LuciaValue::Null
                    })
                }))),
            )
        }),
    );
    t.set(
        &str_to_value!(lvm, "values"),
        LuciaValue::ExtFunction(|args, lvm| {
            check_arguments_num!(args, None, 1);
            let table: &mut LuciaTable = (*args.first().unwrap()).try_into().unwrap();
            let mut values = table.values();
            Ok(
                lvm.new_gc_value(GCObjectKind::ExtClosure(Box::new(move |_, _| {
                    Ok(if let Some(v) = values.next() {
                        *v
                    } else {
                        LuciaValue::Null
                    })
                }))),
            )
        }),
    );
    t.set(
        &str_to_value!(lvm, "raw_len"),
        LuciaValue::ExtFunction(|args, _| {
            check_arguments_num!(args, None, 1);
            let table: &mut LuciaTable = (*args.first().unwrap()).try_into().unwrap();
            Ok(LuciaValue::Int(table.len().try_into().unwrap()))
        }),
    );
    t.set(
        &str_to_value!(lvm, "raw_get"),
        LuciaValue::ExtFunction(|args, _| {
            check_arguments_num!(args, None, 2);
            let table: &mut LuciaTable = (*args.first().unwrap()).try_into().unwrap();
            Ok(match table.raw_get(args.last().unwrap()) {
                Some(v) => v,
                None => LuciaValue::Null,
            })
        }),
    );
    t.set(
        &str_to_value!(lvm, "raw_set"),
        LuciaValue::ExtFunction(|args, _| {
            check_arguments_num!(args, None, 3);
            let table: &mut LuciaTable = (*args.first().unwrap()).try_into().unwrap();
            let key = args.get(1).unwrap();
            let value = args.get(2).unwrap();
            table.set(key, *value);
            Ok(LuciaValue::Null)
        }),
    );
    t
}
