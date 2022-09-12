use lucy_lang::*;
use std::time::Instant;

#[test]
fn temp() {
    let input = "
    import std::io::{println};
    import std::table::{raw_get};
    l = {
        '__base__': {
            'lll': 1,
            '__setitem__': func (self, key, value) {
                self.a = key;
            },
            '__getitem__': func (self, key) {
                return 0;
            },
        },
    };
    l[1] = 2;
    return l['a'];
    ";
    let start = Instant::now();
    let a = parser::Parser::new(&mut lexer::tokenize(input))
        .parse()
        .unwrap();
    let b = codegen::gen_code(a).unwrap();
    // println!("{:#?}", b);
    // println!("{:?}", std::mem::size_of::<object::LucyValue>());
    let mut c = lvm::Lvm::new(b);
    println!("{:?}", c.run());
    let duration = start.elapsed();
    println!("Time: {:?}", duration);
}

#[test]
fn import() {
    let input = "
    import std::io::{input, println};
    import std::convert::{bool, int};
    println(bool(int(input())));
    ";
    lvm::Lvm::from_str(input).run();
}

#[test]
fn for_table() {
    let input = "
    import std::io::{input, println};
    import std::convert::{bool, int};
    // println(bool(int(input())));
    import std::table::{keys};
    t = {1: 0, 2: 0};
    for i in keys(t) {
        println(i);
    }
    ";
    lvm::Lvm::from_str(input).run();
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
    lvm::Lvm::from_str(input).run();
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
    lvm::Lvm::from_str(input).run();
    let duration = start.elapsed();
    println!("Time: {:?}", duration);
}

#[test]
fn for_test() {
    let input = "
    import std::io::{println};
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
    println(l);
    return l;
    ";
    let start = Instant::now();
    lvm::Lvm::from_str(input).run();
    let duration = start.elapsed();
    println!("Time: {:?}", duration);
}
