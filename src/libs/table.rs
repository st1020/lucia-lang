use std::alloc::{dealloc, Layout};

use crate::lvm::Lvm;
use crate::objects::table::{Keys, Values};
use crate::objects::{Table, UserData, Value};
use crate::{check_arguments_num, try_convert};

pub fn libs(lvm: &mut Lvm) -> Table {
    macro_rules! keys_or_values {
        ($keys_or_values:ident, $ty:ty) => {
            |args, lvm| {
                check_arguments_num!(lvm, args, None, Eq(1));
                let table = try_convert!(lvm, args[0], as_table, Table);
                let mut userdata_table = Table::new();
                userdata_table.set(&lvm.get_builtin_str("_marker"), args[0]);
                userdata_table.set(
                    &lvm.get_builtin_str("__call__"),
                    Value::ExtFunction(|mut args, lvm| {
                        check_arguments_num!(lvm, args, None, Eq(1));
                        let t = try_convert!(lvm, args[0], as_userdata_mut, UserData);
                        let iter = unsafe { (t.ptr as *mut $ty).as_mut().unwrap() };
                        Ok(*iter.next().unwrap_or(&Value::Null))
                    }),
                );
                Ok(lvm.new_userdata_value(UserData::new(
                    Box::into_raw(Box::new(table.$keys_or_values())) as *mut u8,
                    userdata_table,
                    |userdata| unsafe {
                        userdata.ptr.drop_in_place();
                        dealloc(userdata.ptr as *mut u8, Layout::new::<$ty>());
                    },
                )))
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
            check_arguments_num!(lvm, args, None, Eq(1));
            let table = args.first().unwrap().as_table().unwrap();
            Ok(Value::Int(table.len().try_into().unwrap()))
        }),
    );
    t.set(
        &lvm.new_str_value("raw_get".to_string()),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, Eq(1));
            let table = args.first().unwrap().as_table().unwrap();
            Ok(match table.raw_get(args.last().unwrap()) {
                Some(v) => *v,
                None => Value::Null,
            })
        }),
    );
    t.set(
        &lvm.new_str_value("raw_set".to_string()),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, Eq(3));
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
