use std::{
    fs,
    io::{stdout, Write},
};

use lucia_lang::{compiler::code::Code, Lucia};

#[test]
fn test_scripts() {
    const DIR: &str = "./tests/scripts";
    let _ = writeln!(stdout(), "running all test scripts in {:?}", DIR);
    for dir in fs::read_dir(DIR).expect("could not list dir") {
        let path = dir.expect("could not read dir entry").path();
        if path.extension().is_some_and(|ext| ext == "lucia") {
            let input = fs::read_to_string(&path).expect("could not read file contents");
            let _ = writeln!(stdout(), "running {:?}", path.file_name().unwrap());
            let code = Code::try_from(&input).expect("error encountered compiling");
            let mut lucia = Lucia::new();
            if let Err(err) = lucia.run_code(code) {
                lucia.run(|ctx| {
                    let err = ctx.state.registry.fetch(&err);
                    panic!("error encountered running: {}", err);
                });
            }
        } else {
            let _ = writeln!(stdout(), "skipping file {:?}", path);
        }
    }
}
