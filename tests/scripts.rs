use std::{
    fs,
    io::{stdout, Write},
};

use lucia_lang::Lucia;

#[test]
fn test_scripts() {
    const DIR: &str = "./tests/scripts";
    let _ = writeln!(stdout(), "running all test scripts in {:?}", DIR);
    for dir in fs::read_dir(DIR).expect("could not list dir") {
        let path = dir.expect("could not read dir entry").path();
        if path.extension().is_some_and(|ext| ext == "lucia") {
            let input = fs::read_to_string(&path).expect("could not read file contents");
            let _ = writeln!(stdout(), "running {:?}", path.file_name().unwrap());
            let mut lucia = Lucia::new();
            if let Err(err) = lucia.run_code(&input) {
                panic!("error encountered running: {}", err);
            }
        } else {
            let _ = writeln!(stdout(), "skipping file {:?}", path);
        }
    }
}
