use lucia_lang::Lucia;

fn main() {
    let path = std::env::args().nth(1).expect("no lucia script path given");
    let input = std::fs::read_to_string(path).expect("could not read file contents");
    let mut lucia = Lucia::new();
    let code = lucia.compile(&input).unwrap();
    if let Err(err) = lucia.execute(&code) {
        println!("{err}");
    }
}
