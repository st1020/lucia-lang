use lucia_lang::{compiler::code::Code, Lucia};

fn main() {
    let path = std::env::args().nth(1).expect("no lucia script path given");
    let input = std::fs::read_to_string(path).expect("could not read file contents");
    let code = Code::try_from(&input).expect("error encountered compiling");
    let mut lucia = Lucia::new();
    if let Err(err) = lucia.run_code(code) {
        lucia.run(|ctx| {
            let err = ctx.state.registry.fetch(&err);
            println!("{}", err);
        });
    }
}
