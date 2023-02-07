use lucia_lang::*;
use std::time::Instant;

#[test]
fn temp() -> errors::Result<()> {
    let input = "
    import std::io::{println}
    l = {}
    l[#] = {
        '__setitem__': fn (self, key, value) {
            println('__setitem__', self, key, value)
            self.k = key
            self.v = value
        },
        '__getitem__': fn (self, key) {
            println('__setitem__', self, key)
            return 0
        },
    }
    println(repr(l[#]))
    l[1] = 2
    println(repr(l))
    return l['a']
    ";
    let ast = parser::Parser::new(&mut lexer::tokenize(input)).parse()?;
    // println!("{:#?}", ast);
    let code = codegen::CodeGen::from(ast).gen_code()?;
    // println!("{:#}", code);
    let mut lvm = lvm::Lvm::new();
    println!("{:?}", lvm.run(code));
    Ok(())
}

#[test]
fn tail_call() -> errors::Result<()> {
    let input = "
    global f
    fn f(n, total) {
        if n == 1 {
            return total
        }
        return f(n - 1, n + total)
    }
    return f(1_000_000, 1)
    ";
    lvm::Lvm::new().run(code::Code::try_from(input)?)?;
    Ok(())
}

#[test]
fn variadic() -> errors::Result<()> {
    let input = "
    import std::io::{println}
    fn test(a, *b) {
        println(a)
        println(repr(b))
    }
    test(1, 2, 3)
    ";
    lvm::Lvm::new().run(code::Code::try_from(input)?)?;
    Ok(())
}

#[test]
fn import() -> errors::Result<()> {
    let input = "
    import std::io::{input, println}
    println(bool(int(input())))
    ";
    lvm::Lvm::new().run(code::Code::try_from(input)?)?;
    Ok(())
}

#[test]
fn for_table() -> errors::Result<()> {
    let input = "
    import std::io::{println}
    import std::table::{keys}
    t = {1: 0, 2: 0}
    for i in keys(t) {
        println(i)
    }
    ";
    lvm::Lvm::new().run(code::Code::try_from(input)?)?;
    Ok(())
}

#[test]
fn add_pref() -> errors::Result<()> {
    let input = "
    i = 0
    ans = 0
    while i < 100000 {
        i += 1
        ans += 1
    }
    ";
    let start = Instant::now();
    for _ in 0..100 {
        lvm::Lvm::new().run(code::Code::try_from(input)?)?;
    }
    let duration = start.elapsed();
    println!("Time: {:?}", duration / 100);
    Ok(())
}

#[test]
fn gcd_pref() -> errors::Result<()> {
    let input = "
    global gcd
    fn gcd(x, y) {
        if y == 0 {
            return x
        } else {
            return gcd(y, x % y)
        }
    }
    i = 0
    j = 0
    ans = 0
    while i < 10000 {
        while j < 10000 {
            ans += gcd(i, j)
            j += 1
        }
        i += 1
    }
    ";
    let start = Instant::now();
    for _ in 0..100 {
        lvm::Lvm::new().run(code::Code::try_from(input)?)?;
    }
    let duration = start.elapsed();
    println!("Time: {:?}", duration / 100);
    Ok(())
}

#[test]
fn for_test() -> errors::Result<()> {
    let input = "
    import std::io::{println}
    fn a() {
        t = 0
        return || {
            t += 1
            if t > 10 {
                return null
            }
            return t * 2
        }
    }
    l = {}
    for i in a() {
        l[i] = i
    }
    println(repr(l))
    return l
    ";
    let start = Instant::now();
    lvm::Lvm::new().run(code::Code::try_from(input)?)?;
    let duration = start.elapsed();
    println!("Time: {:?}", duration);
    Ok(())
}

#[test]
fn do_test() -> errors::Result<()> {
    let input = "
    import std::io::{println}
    l = do {
        a = 1
        b = 2
    }
    println(repr(l))
    ";
    let start = Instant::now();
    lvm::Lvm::new().run(code::Code::try_from(input)?)?;
    let duration = start.elapsed();
    println!("Time: {:?}", duration);
    Ok(())
}
