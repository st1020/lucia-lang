use bumpalo::Bump;
use criterion::{criterion_group, criterion_main, Criterion};

use std::fs;

use lucia_lang::compiler::{compile, interning::BumpInterner};

pub fn benchmark_compiler(c: &mut Criterion) {
    const DIR: &str = "./benches/scripts";
    for dir in fs::read_dir(DIR).expect("could not list dir") {
        let path = dir.expect("could not read dir entry").path();
        if path.extension().is_some_and(|ext| ext == "lucia") {
            let input = &fs::read_to_string(&path).expect("could not read file contents");
            c.bench_function(
                &format!("compile {}", path.file_name().unwrap().to_str().unwrap()),
                |b| {
                    b.iter(|| {
                        let allocator = &Bump::new();
                        let interner = BumpInterner::new(allocator);
                        compile(allocator, interner, input).unwrap();
                    })
                },
            );
        }
    }
}

criterion_group!(compiler, benchmark_compiler);
criterion_main!(compiler);
