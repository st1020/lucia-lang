use crate::lvm::Lvm;
use crate::object::{GCObjectKind, LucyTable, LucyValue};
use crate::str_to_value;

pub fn libs(lvm: &mut Lvm) -> LucyTable {
    let mut t = LucyTable::new();
    t.set(
        &str_to_value!(lvm, "keys"),
        LucyValue::ExtFunction(|args, lvm| {
            if args.len() != 1 {
                panic!()
            }
            let table: &mut LucyTable = (*args.first().unwrap()).try_into().unwrap();
            let mut i = 0;
            Ok(
                lvm.new_gc_value(GCObjectKind::ExtClosuer(Box::new(move |_, _| {
                    Ok(if i >= table.len().try_into().unwrap() {
                        LucyValue::Null
                    } else {
                        let (k, _) = table.get_by_index(i.try_into().unwrap()).unwrap();
                        i += 1;
                        *k
                    })
                }))),
            )
        }),
    );
    t.set(
        &str_to_value!(lvm, "values"),
        LucyValue::ExtFunction(|args, lvm| {
            if args.len() != 1 {
                panic!()
            }
            let table: &mut LucyTable = (*args.first().unwrap()).try_into().unwrap();
            let mut i = 0;
            Ok(
                lvm.new_gc_value(GCObjectKind::ExtClosuer(Box::new(move |_, _| {
                    Ok(if i >= table.len().try_into().unwrap() {
                        LucyValue::Null
                    } else {
                        let (_, v) = table.get_by_index(i.try_into().unwrap()).unwrap();
                        i += 1;
                        *v
                    })
                }))),
            )
        }),
    );
    t.set(
        &str_to_value!(lvm, "raw_len"),
        LucyValue::ExtFunction(|args, _| {
            if args.len() != 1 {
                panic!()
            }
            let table: &mut LucyTable = (*args.first().unwrap()).try_into().unwrap();
            Ok(LucyValue::Int(table.len().try_into().unwrap()))
        }),
    );
    t.set(
        &str_to_value!(lvm, "raw_get"),
        LucyValue::ExtFunction(|args, _| {
            if args.len() != 2 {
                panic!()
            }
            let table: &mut LucyTable = (*args.first().unwrap()).try_into().unwrap();
            Ok(match table.raw_get(args.last().unwrap()) {
                Some(v) => v,
                None => LucyValue::Null,
            })
        }),
    );
    t.set(
        &str_to_value!(lvm, "raw_set"),
        LucyValue::ExtFunction(|args, _| {
            if args.len() != 3 {
                panic!()
            }
            let table: &mut LucyTable = (*args.first().unwrap()).try_into().unwrap();
            let key = args.get(1).unwrap();
            let value = args.get(2).unwrap();
            table.set(key, *value);
            Ok(LucyValue::Null)
        }),
    );
    t
}
