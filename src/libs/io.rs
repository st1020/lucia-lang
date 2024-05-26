use std::io;

use smol_str::ToSmolStr;

use crate::{
    objects::{Callback, Table, Varargs},
    utils::Join,
    Context,
};

pub fn io_lib<'gc>(ctx: Context<'gc>) -> Table<'gc> {
    let t = Table::new(&ctx);
    t.set(
        ctx,
        "print",
        Callback::from_fn(&ctx, |args: Varargs<'gc>| match args.len() {
            0 => (),
            1 => print!("{}", args.first().unwrap()),
            _ => print!("{}", args.iter().join(" ")),
        }),
    );
    t.set(
        ctx,
        "println",
        Callback::from_fn(&ctx, |args: Varargs<'gc>| match args.len() {
            0 => println!(),
            1 => println!("{}", args.first().unwrap()),
            _ => println!("{}", args.iter().join(" ")),
        }),
    );
    t.set(
        ctx,
        "input",
        Callback::from_fn(&ctx, || {
            let mut t = String::new();
            io::stdin().read_line(&mut t).unwrap();
            t.strip_suffix('\n').unwrap_or(&t).to_smolstr()
        }),
    );
    t
}
