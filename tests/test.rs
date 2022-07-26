use lucy_lang::*;
use std::time::Instant;

#[test]
fn temp() {
    let input = "
    l = {
        '__base__': {
            'lll': 1,
            '__setitem__': func (self, key, value) {
                self.a = key;
            },
        },
    };
    l[1] = 2;
    return l['a'];
    ";
    let start = Instant::now();
    let a = parser::Parser::new(&mut lexer::tokenize(input)).parse();
    let b = codegen::gen_code(a);
    // println!("{:#?}", b);
    // println!("{:?}", std::mem::size_of::<lvm::LucyValue>());
    let mut c = lvm::Lvm::new(b);
    c.run();
    let duration = start.elapsed();
    println!("Time: {:?}", duration);
}

#[test]
fn add_pref() {
    let input = "
    i = 0;
    ans = 0;
    while i < 100000 {
        i += 1;
        ans += 1;
    }
    ";
    let start = Instant::now();
    // let a = parser::Parser::new(&mut lexer::tokenize(input)).parse();
    // let b = codegen::gen_code(a);
    // println!("{:#?}", b);
    // println!("{:?}", std::mem::size_of::<lvm::LucyData>());
    let mut c = lvm::Lvm::from_str(input);
    c.run();
    let duration = start.elapsed();
    println!("Time: {:?}", duration);
}

#[test]
fn gcd_pref() {
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
    // let a = parser::Parser::new(&mut lexer::tokenize(input)).parse();
    // let b = codegen::gen_code(a);
    // println!("{:#?}", b);
    // println!("{:?}", std::mem::size_of::<lvm::LucyData>());
    let mut c = lvm::Lvm::from_str(input);
    c.run();
    let duration = start.elapsed();
    println!("Time: {:?}", duration);
}

#[test]
fn for_test() {
    let input = "
    a = func () {
        t = 0;
        return || {
            t += 1;
            if t > 10 {
                return null;
            }
            return t * 2;
        };
    };
    l = {};
    for i in a() {
        l[i] = i;
    }
    return l;
    ";
    let start = Instant::now();
    // let a = parser::Parser::new(&mut lexer::tokenize(input)).parse();
    // let b = codegen::gen_code(a);
    // println!("{:#?}", b);
    // println!("{:?}", std::mem::size_of::<lvm::LucyData>());
    let mut c = lvm::Lvm::from_str(input);
    c.run();
    let duration = start.elapsed();
    println!("Time: {:?}", duration);
}
