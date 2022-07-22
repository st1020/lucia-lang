mod ast;
mod codegen;
mod lexer;
mod lvm;
mod parser;
use std::time::Instant;

use std::collections::BTreeMap;

fn main() {
    // let input = "
    // i = 0;
    // ans = 0;
    // while i < 100000 {
    //     i += 1;
    //     ans += 1;
    // }
    // ";
    let input = "
    global gcd;
    gcd = func (x, y) {
        if y == 0 {
            return x;
        } else {
            return gcd(y, x % y);
        }
    };
    i = 0;
    j = 0;
    ans = 0;
    while i < 10000 {
        while j < 10000 {
            ans += gcd(i, j);
            j += 1;
        }
        i += 1;
    }
    ";

    let mut a = BTreeMap::new();
    a.insert(0, 1);
    a.insert(4, 2);
    a.insert(2, 0);
    println!("{:?}", a);

    let a = parser::Parser::new(Box::new(lexer::tokenize(input))).parse();
    let b = codegen::gen_code(a);
    // println!("{:?}", b);
    // println!("{:?}", std::mem::size_of::<lvm::LucyData>());
    let mut c = lvm::Lvm::new(b);

    let start = Instant::now();
    c.run();

    let duration = start.elapsed();
    println!("Time: {:?}", duration);
}
