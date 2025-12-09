use criterion::{Criterion, criterion_group, criterion_main};

use std::fs;

use lucia_lang::Context;

pub fn benchmark_scripts(c: &mut Criterion) {
    const DIR: &str = "./benches/scripts";
    for dir in fs::read_dir(DIR).expect("could not list dir") {
        let path = dir.expect("could not read dir entry").path();
        if path.extension().is_some_and(|ext| ext == "lucia") {
            let input = fs::read_to_string(&path).expect("could not read file contents");
            let mut lucia = Context::new();
            let code = lucia.compile(&input).unwrap();
            c.bench_function(
                &format!("run {:?}", path.file_name().unwrap().to_str().unwrap()),
                |b| b.iter(|| lucia.execute(code.clone()).unwrap()),
            );
        }
    }
}

criterion_group!(scripts, benchmark_scripts);
criterion_main!(scripts);
