mod ast;
mod codegen;
mod lexer;
mod lvm;
mod parser;
use std::time::Instant;

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
    let start = Instant::now();
    let a = parser::Parser::new(Box::new(lexer::tokenize(input))).parse();
    let b = codegen::gen_code(a);
    // println!("{:?}", b);
    // println!("{:?}", std::mem::size_of::<lvm::LucyData>());
    let mut c = lvm::Lvm::new(b);

    c.run();

    let duration = start.elapsed();
    println!("Time: {:?}", duration);
}
