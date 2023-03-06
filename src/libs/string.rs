use std::str::{Chars, Lines};

use crate::check_args;
use crate::lvm::Lvm;
use crate::objects::{Table, Value};

#[macro_export]
macro_rules! iter_to_value {
    ($lvm:expr, $marker_value:expr, $iter:expr, $ty:ty) => {{
        let mut userdata_table = $crate::table![$marker_value];
        userdata_table.set(
            &$lvm.get_builtin_str("__call__"),
            $crate::objects::Value::ExtFunction(|mut args, lvm| {
                let (t,) = $crate::check_args!(lvm, args, mut UserData);
                let iter = unsafe { (t.ptr as *mut $ty).as_mut().unwrap() };
                Ok(iter
                    .next()
                    .map(|x| lvm.new_str_value(x.to_string()))
                    .unwrap_or($crate::objects::Value::Null))
            }),
        );
        $lvm.new_userdata_value($crate::objects::UserData::new(
            Box::into_raw(Box::new($iter)) as *mut u8,
            userdata_table,
            |userdata| unsafe { drop(Box::from_raw(userdata.ptr as *mut $ty)) },
        ))
    }};
}

pub fn libs(lvm: &mut Lvm) -> Table {
    let mut t = Table::new();
    t.set(
        &lvm.new_str_value("get".to_string()),
        Value::ExtFunction(|args, lvm| {
            let (s, i) = check_args!(lvm, args, Str, Int);
            Ok(s.chars()
                .nth(i.try_into().unwrap())
                .map_or(Value::Null, |x| lvm.new_str_value(x.to_string())))
        }),
    );
    t.set(
        &lvm.new_str_value("chars".to_string()),
        Value::ExtFunction(|args, lvm| {
            let (s,) = check_args!(lvm, args, Str);
            let t = Table::from_iter(s.chars().map(|x| lvm.new_str_value(x.to_string())));
            Ok(lvm.new_table_value(t))
        }),
    );
    t.set(
        &lvm.new_str_value("chars_iter".to_string()),
        Value::ExtFunction(|args, lvm| {
            let (s,) = check_args!(lvm, args, Str);
            Ok(iter_to_value!(lvm, args[0], s.chars(), Chars))
        }),
    );
    t.set(
        &lvm.new_str_value("lines".to_string()),
        Value::ExtFunction(|args, lvm| {
            let (s,) = check_args!(lvm, args, Str);
            let t = Table::from_iter(s.lines().map(|x| lvm.new_str_value(x.to_string())));
            Ok(lvm.new_table_value(t))
        }),
    );
    t.set(
        &lvm.new_str_value("lines_iter".to_string()),
        Value::ExtFunction(|args, lvm| {
            let (s,) = check_args!(lvm, args, Str);
            Ok(iter_to_value!(lvm, args[0], s.lines(), Lines))
        }),
    );
    t.set(
        &lvm.new_str_value("contains".to_string()),
        Value::ExtFunction(|args, lvm| {
            let (s1, s2) = check_args!(lvm, args, Str, Str);
            Ok(Value::Bool(s1.contains(s2)))
        }),
    );
    t.set(
        &lvm.new_str_value("starts_with".to_string()),
        Value::ExtFunction(|args, lvm| {
            let (s1, s2) = check_args!(lvm, args, Str, Str);
            Ok(Value::Bool(s1.starts_with(s2)))
        }),
    );
    t.set(
        &lvm.new_str_value("ends_with".to_string()),
        Value::ExtFunction(|args, lvm| {
            let (s1, s2) = check_args!(lvm, args, Str, Str);
            Ok(Value::Bool(s1.ends_with(s2)))
        }),
    );
    t.set(
        &lvm.new_str_value("find".to_string()),
        Value::ExtFunction(|args, lvm| {
            let (s1, s2) = check_args!(lvm, args, Str, Str);
            Ok(s1
                .find(s2)
                .map_or(Value::Null, |x| Value::Int(x.try_into().unwrap())))
        }),
    );
    t.set(
        &lvm.new_str_value("split".to_string()),
        Value::ExtFunction(|args, lvm| {
            let (s, pat, count) = check_args!(lvm, args, Str, Str | Int);
            if let Some(count) = count {
                let t = s
                    .splitn(count.try_into().unwrap(), pat)
                    .map(|x| lvm.new_str_value(x.to_string()))
                    .collect::<Vec<_>>();
                Ok(lvm.new_table_value(Table::from_iter(t)))
            } else {
                let t = s
                    .split(pat)
                    .map(|x| lvm.new_str_value(x.to_string()))
                    .collect::<Vec<_>>();
                Ok(lvm.new_table_value(Table::from_iter(t)))
            }
        }),
    );
    t.set(
        &lvm.new_str_value("trim".to_string()),
        Value::ExtFunction(|args, lvm| {
            let (s,) = check_args!(lvm, args, Str);
            Ok(lvm.new_str_value(s.trim().to_string()))
        }),
    );
    t.set(
        &lvm.new_str_value("trim_start".to_string()),
        Value::ExtFunction(|args, lvm| {
            let (s,) = check_args!(lvm, args, Str);
            Ok(lvm.new_str_value(s.trim_start().to_string()))
        }),
    );
    t.set(
        &lvm.new_str_value("trim_end".to_string()),
        Value::ExtFunction(|args, lvm| {
            let (s,) = check_args!(lvm, args, Str);
            Ok(lvm.new_str_value(s.trim_end().to_string()))
        }),
    );
    t.set(
        &lvm.new_str_value("strip_prefix".to_string()),
        Value::ExtFunction(|args, lvm| {
            let (s1, s2) = check_args!(lvm, args, Str, Str);
            Ok(s1
                .strip_prefix(s2)
                .map_or(args[0], |x| lvm.new_str_value(x.to_string())))
        }),
    );
    t.set(
        &lvm.new_str_value("strip_suffix".to_string()),
        Value::ExtFunction(|args, lvm| {
            let (s1, s2) = check_args!(lvm, args, Str, Str);
            Ok(s1
                .strip_suffix(s2)
                .map_or(args[0], |x| lvm.new_str_value(x.to_string())))
        }),
    );
    t.set(
        &lvm.new_str_value("is_ascii".to_string()),
        Value::ExtFunction(|args, lvm| {
            let (s,) = check_args!(lvm, args, Str);
            Ok(Value::Bool(s.is_ascii()))
        }),
    );
    t.set(
        &lvm.new_str_value("replace".to_string()),
        Value::ExtFunction(|args, lvm| {
            let (s, from, to, count) = check_args!(lvm, args, Str, Str, Str | Int);
            if let Some(count) = count {
                Ok(lvm.new_str_value(s.replacen(from, to, count.try_into().unwrap())))
            } else {
                Ok(lvm.new_str_value(s.replace(from, to)))
            }
        }),
    );
    t.set(
        &lvm.new_str_value("to_lowercase".to_string()),
        Value::ExtFunction(|args, lvm| {
            let (s,) = check_args!(lvm, args, Str);
            Ok(lvm.new_str_value(s.to_lowercase()))
        }),
    );
    t.set(
        &lvm.new_str_value("to_uppercase".to_string()),
        Value::ExtFunction(|args, lvm| {
            let (s,) = check_args!(lvm, args, Str);
            Ok(lvm.new_str_value(s.to_uppercase()))
        }),
    );
    t.set(
        &lvm.new_str_value("to_ascii_lowercase".to_string()),
        Value::ExtFunction(|args, lvm| {
            let (s,) = check_args!(lvm, args, Str);
            Ok(lvm.new_str_value(s.to_ascii_lowercase()))
        }),
    );
    t.set(
        &lvm.new_str_value("to_ascii_uppercase".to_string()),
        Value::ExtFunction(|args, lvm| {
            let (s,) = check_args!(lvm, args, Str);
            Ok(lvm.new_str_value(s.to_ascii_uppercase()))
        }),
    );
    t.set(
        &lvm.new_str_value("repeat".to_string()),
        Value::ExtFunction(|args, lvm| {
            let (s, i) = check_args!(lvm, args, Str, Int);
            Ok(lvm.new_str_value(s.repeat(i.try_into().unwrap())))
        }),
    );
    t
}
