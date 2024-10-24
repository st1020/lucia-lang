use criterion::{black_box, criterion_group, criterion_main, Criterion};

use std::fs;

use lucia_lang::Lucia;

pub fn benchmark_scripts(c: &mut Criterion) {
    const DIR: &str = "./benches/scripts";
    for dir in fs::read_dir(DIR).expect("could not list dir") {
        let path = dir.expect("could not read dir entry").path();
        if path.extension().is_some_and(|ext| ext == "lucia") {
            let input = fs::read_to_string(&path).expect("could not read file contents");
            c.bench_function(
                &format!("run {:?}", path.file_name().unwrap().to_str().unwrap()),
                |b| b.iter(|| Lucia::new().run_code(black_box(&input)).unwrap()),
            );
        }
    }
}

criterion_group!(scripts, benchmark_scripts);
criterion_main!(scripts);
