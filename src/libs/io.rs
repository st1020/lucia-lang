use std::io;

use compact_str::ToCompactString;
use itertools::Itertools;

use crate::{
    Context,
    objects::{Callback, Table, Value},
};

pub fn io_lib<'gc>(ctx: Context<'gc>) -> Table<'gc> {
    let t = Table::new(&ctx);
    t.set(
        ctx,
        "print",
        #[expect(clippy::print_stdout)]
        Callback::from(&ctx, |args: &[Value<'gc>]| match args.len() {
            0 => (),
            1 => print!("{}", args.first().unwrap()),
            _ => print!("{}", args.iter().join(" ")),
        }),
    );
    t.set(
        ctx,
        "println",
        #[expect(clippy::print_stdout)]
        Callback::from(&ctx, |args: &[Value<'gc>]| match args.len() {
            0 => println!(),
            1 => println!("{}", args.first().unwrap()),
            _ => println!("{}", args.iter().join(" ")),
        }),
    );
    t.set(
        ctx,
        "input",
        Callback::from(&ctx, || {
            let mut buf = String::new();
            io::stdin().read_line(&mut buf).unwrap();
            buf.strip_suffix('\n').unwrap_or(&buf).to_compact_string()
        }),
    );
    t
}
