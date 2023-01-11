use std::alloc::{dealloc, Layout};

use crate::lvm::Lvm;
use crate::objects::table::{Keys, Values};
use crate::objects::{ExtClosure, Table, Value};
use crate::{check_arguments_num, try_convert};

pub fn libs(lvm: &mut Lvm) -> Table {
    macro_rules! keys_or_values {
        ($keys_or_values:ident, $ty:ty) => {
            |args, lvm| {
                check_arguments_num!(lvm, args, None, 1);
                let table = try_convert!(lvm, args[0], as_table, Table);
                Ok(lvm.new_ext_closure_value(ExtClosure {
                    upvalues: vec![
                        Value::LightUserData(
                            Box::into_raw(Box::new(table.$keys_or_values())) as *mut u8
                        ),
                        args[0], // prevent table being dealloc during GC
                    ],
                    func: |args, upvalues, lvm| {
                        check_arguments_num!(lvm, args, None, 0);
                        let iter_userdata = upvalues[0].as_userdata().unwrap() as *mut $ty;
                        let iter = unsafe { iter_userdata.as_mut().unwrap() };
                        if let Some(v) = iter.next() {
                            Ok(*v)
                        } else {
                            unsafe {
                                iter_userdata.drop_in_place();
                                dealloc(iter_userdata as *mut u8, Layout::new::<$ty>());
                            }
                            Ok(Value::Null)
                        }
                    },
                }))
            }
        };
    }
    let mut t = Table::new();
    t.set(
        &lvm.new_str_value("keys".to_string()),
        Value::ExtFunction(keys_or_values!(keys, Keys)),
    );
    t.set(
        &lvm.new_str_value("values".to_string()),
        Value::ExtFunction(keys_or_values!(values, Values)),
    );
    t.set(
        &lvm.new_str_value("raw_len".to_string()),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, 1);
            let table = args.first().unwrap().as_table().unwrap();
            Ok(Value::Int(table.len().try_into().unwrap()))
        }),
    );
    t.set(
        &lvm.new_str_value("raw_get".to_string()),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, 2);
            let table = args.first().unwrap().as_table().unwrap();
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
            let mut arg1 = *args.first().unwrap();
            let table = arg1.as_table_mut().unwrap();
            let key = args.get(1).unwrap();
            let value = args.get(2).unwrap();
            table.set(key, *value);
            Ok(Value::Null)
        }),
    );
    t
}
