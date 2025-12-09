use lucia_lang::Context;

fn main() {
    let path = std::env::args().nth(1).expect("no lucia script path given");
    let input = std::fs::read_to_string(path).expect("could not read file contents");
    let mut lucia = Context::new();
    let code = lucia.compile(&input).unwrap();
    if let Err(err) = lucia.execute(code) {
        println!("{err}");
    }
}
