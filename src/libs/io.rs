use std::io;

use crate::{
    check_args,
    objects::{Callback, CallbackReturn, IntoValue, Table, Value},
    utils::Join,
    Context,
};

pub fn io_lib(ctx: Context<'_>) -> Table<'_> {
    let t = Table::new(&ctx);
    t.set(
        ctx,
        "print",
        Callback::from_fn(&ctx, |_ctx, args| {
            match args.len() {
                0 => (),
                1 => print!("{}", args.first().unwrap()),
                _ => print!("{}", args.iter().join(" ")),
            };
            Ok(CallbackReturn::Return(Value::Null))
        }),
    );
    t.set(
        ctx,
        "println",
        Callback::from_fn(&ctx, |_ctx, args| {
            match args.len() {
                0 => println!(),
                1 => println!("{}", args.first().unwrap()),
                _ => println!("{}", args.iter().join(" ")),
            }
            Ok(CallbackReturn::Return(Value::Null))
        }),
    );
    t.set(
        ctx,
        "input",
        Callback::from_fn(&ctx, |ctx, args| {
            check_args!(args);
            let mut t = String::new();
            io::stdin().read_line(&mut t).unwrap();
            Ok(CallbackReturn::Return(
                t.strip_suffix('\n')
                    .unwrap_or(&t)
                    .to_string()
                    .into_value(ctx),
            ))
        }),
    );
    t
}
