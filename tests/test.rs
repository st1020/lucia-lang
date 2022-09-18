use lucy_lang::*;
use std::time::Instant;

#[test]
fn temp() -> errors::LResult<()> {
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
    let a = parser::Parser::new(&mut lexer::tokenize(input)).parse()?;
    let b = codegen::gen_code(a)?;
    // println!("{:#?}", b);
    // println!("{:?}", std::mem::size_of::<errors::LucyError>());
    let mut c = lvm::Lvm::new(b);
    println!("{:?}", c.run());
    let duration = start.elapsed();
    println!("Time: {:?}", duration);
    Ok(())
}

#[test]
fn variadic() -> errors::LResult<()> {
    let input = "
    import std::io::{println};
    test = func (a, *b) {
        println(a);
        println(b);
    };
    test(1, 2, 3);
    ";
    lvm::Lvm::new(codegen::Program::try_from(input)?).run()?;
    Ok(())
}

#[test]
fn import() -> errors::LResult<()> {
    let input = "
    import std::io::{input, println};
    import std::convert::{bool, int};
    println(bool(int(input())));
    ";
    lvm::Lvm::new(codegen::Program::try_from(input)?).run()?;
    Ok(())
}

#[test]
fn for_table() -> errors::LResult<()> {
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
    lvm::Lvm::new(codegen::Program::try_from(input)?).run()?;
    Ok(())
}

#[test]
fn add_pref() -> errors::LResult<()> {
    let input = "
    i = 0;
    ans = 0;
    while i < 100000 {
        i += 1;
        ans += 1;
    }
    ";
    let start = Instant::now();
    for _ in 0..100 {
        lvm::Lvm::new(codegen::Program::try_from(input)?).run()?;
    }
    let duration = start.elapsed();
    println!("Time: {:?}", duration / 100);
    Ok(())
}

#[test]
fn gcd_pref() -> errors::LResult<()> {
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
    for _ in 0..100 {
        lvm::Lvm::new(codegen::Program::try_from(input)?).run()?;
    }
    let duration = start.elapsed();
    println!("Time: {:?}", duration / 100);
    Ok(())
}

#[test]
fn for_test() -> errors::LResult<()> {
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
    lvm::Lvm::new(codegen::Program::try_from(input)?).run()?;
    let duration = start.elapsed();
    println!("Time: {:?}", duration);
    Ok(())
}
