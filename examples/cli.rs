use lucia_lang::Lucia;

fn main() {
    let path = std::env::args().nth(1).expect("no lucia script path given");
    let input = std::fs::read_to_string(path).expect("could not read file contents");
    let mut lucia = Lucia::new();
    if let Err(err) = lucia.run_code(&input) {
        lucia.run(|ctx| {
            let err = ctx.state.registry.fetch(&err);
            println!("{}", err);
        });
    }
}
