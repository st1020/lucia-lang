use lucia_lang::{objects::IntoValue, *};
use std::time::Instant;

macro_rules! run {
    ($input:expr $(,)?) => {{
        let mut lucia = context::Lucia::new();
        lucia
            .run_code(compiler::code::Code::try_from($input).unwrap())
            .unwrap()
    }};
}

macro_rules! run_check_value {
    ($input:expr, $value:expr $(,)?) => {{
        let mut lucia = context::Lucia::new();
        let r = lucia.run_code(compiler::code::Code::try_from($input).unwrap());
        lucia.run(|ctx| {
            assert_eq!(
                ctx.state.registry.fetch(&r.unwrap()),
                $value.into_value(ctx)
            )
        });
    }};
}

#[test]
fn test_return() {
    run_check_value!(r#"return 0"#, 0);
    run_check_value!(r#"return 0"#, 0);
}

#[test]
fn test_string_escape() {
    run_check_value!(r#"return "\""  "#, "\"");
    run_check_value!(r#"return "\n"  "#, "\n");
    run_check_value!(r#"return "\r"  "#, "\r");
    run_check_value!(r#"return "\t"  "#, "\t");
    run_check_value!(r#"return "\\"  "#, "\\");
    run_check_value!(r#"return "\'"  "#, "\'");
    run_check_value!(r#"return "\0"  "#, "\0");
    run_check_value!(r#"return "\x21"  "#, "!"); // ASCII 0x21 is "!"
    run_check_value!(r#"return "\u{4F60}\u{597D}\u{2764}"  "#, "你好❤");
}

#[test]
fn test_number_int() {
    run_check_value!(r#"return 0"#, 0);
    run_check_value!(r#"return 1"#, 1);
    run_check_value!(r#"return -1"#, -1);
    run_check_value!(r#"return 100_000_000"#, 100_000_000);
    run_check_value!(r#"return 0b1010"#, 10);
    run_check_value!(r#"return 0o1010"#, 520);
    run_check_value!(r#"return 0xABCD"#, 43981);
}

#[test]
fn test_number_float() {
    run_check_value!(r#"return 0.0"#, 0.);
    run_check_value!(r#"return 1.0"#, 1.);
    run_check_value!(r#"return -1.0"#, -1.);
    run_check_value!(r#"return 1.0001"#, 1.0001);
    run_check_value!(r#"return 1.2e2"#, 120.);
    run_check_value!(r#"return 1.2E2"#, 120.);
    run_check_value!(r#"return 12e1"#, 120.);
}

#[test]
fn test_arithmetic_expr_int() {
    run_check_value!(r#"return 1 + 1"#, 2);
    run_check_value!(r#"return 1 - 1"#, 0);
    run_check_value!(r#"return 2 * 3"#, 6);
    run_check_value!(r#"return 6 / 2"#, 3);
    run_check_value!(r#"return 5 / 2"#, 2);
    run_check_value!(r#"return 5 % 2"#, 1);
}

#[test]
fn test_arithmetic_expr_float() {
    run_check_value!(r#"return 1.0 + 1.0"#, 2.);
    run_check_value!(r#"return 1.0 - 1.0"#, 0.);
    run_check_value!(r#"return 2.0 * 3.0"#, 6.);
    run_check_value!(r#"return 6.0 / 2.0"#, 3.);
    run_check_value!(r#"return 5.0 / 2.0"#, 2.5);
    run_check_value!(r#"return 5.0 % 2.0"#, 1.);
}

#[test]
fn test_complex_arithmetic_expr() {
    run_check_value!(r#"return 1 + 1 * 2"#, 3);
    run_check_value!(r#"return (1 + 1) * 2"#, 4);
    run_check_value!(r#"return (1 + 1) * 2 + 1 * 2"#, 6);
    run_check_value!(r#"return ((1 + 1) * 2 + 1) * 2"#, 10);
}

#[test]
fn test_logic_operation() {
    run_check_value!(r#"return true"#, true);
    run_check_value!(r#"return false"#, false);
    run_check_value!(r#"return true and false"#, false);
    run_check_value!(r#"return true or false"#, true);
    run_check_value!(r#"return not true"#, false);
    run_check_value!(r#"return true and false or true"#, true);
    run_check_value!(r#"return true or true and not true"#, true);
}

#[test]
fn test_compare_operation() {
    run_check_value!(r#"return 1 == 1"#, true);
    run_check_value!(r#"return 1 != 2"#, true);
    run_check_value!(r#"return 1 < 2"#, true);
    run_check_value!(r#"return 2 > 1"#, true);
    run_check_value!(r#"return 1 >= 1"#, true);
    run_check_value!(r#"return 2 >= 1"#, true);
    run_check_value!(r#"return 1 <= 1"#, true);
    run_check_value!(r#"return 1 <= 2"#, true);
}

#[test]
fn test_import_and_print() {
    run!(
        r#"
import std::io::{println}
println("Hello World!")
"#,
    );
}

#[test]
fn test_comment() {
    run!(
        r#"
import std::io::{println}
println("01")
// println("02")
/*
println("03")
println("04")
*/
"#,
    );
}

#[test]
fn test_assert() {
    if let objects::StaticValue::Null = run!(r#"assert(1 == 1)"#) {
    } else {
        panic!("unexpected return")
    }
}

#[test]
#[should_panic]
fn test_assert_error() {
    run!(r#"assert(1 != 1)"#);
}

#[test]
fn test_assign() {
    run!(
        r#"
a = 1
assert(a == 1)
"#,
    );
    run!(
        r#"
a = 1
a = 2
assert(a == 2)
"#,
    );
    run!(
        r#"
a = 1
a = "1"
assert(a == "1")
"#,
    );
    run!(
        r#"
a = 1
a += 1
assert(a == 2)
"#,
    );
    run!(
        r#"
a = 1
a -= 1
assert(a == 0)
"#,
    );
    run!(
        r#"
a = 2
a *= 3
assert(a == 6)
"#,
    );
    run!(
        r#"
a = 6
a /= 2
assert(a == 3)
"#,
    );
    run!(
        r#"
a = 5
a %= 2
assert(a == 1)
"#,
    );
}

#[test]
fn test_if_stmt() {
    run!(
        r#"
if 1 == 1 {
    assert(true)
} else {
    assert(false)  // never reach
}
"#,
    );
    run!(
        r#"
if 1 != 1 {
    assert(false)  // never reach
} else {
    assert(true)
}
"#,
    );
    run!(
        r#"
a = 1
if a == 1 {
    b = 1
} else if a == 2 {
    b = 2
} else {
    b = 3
}
assert(b == 1)
"#,
    );
    run!(
        r#"
a = 2
if a == 1 {
    b = 1
} else if a == 2 {
    b = 2
} else {
    b = 3
}
assert(b == 2)
"#,
    );
    run!(
        r#"
a = 3
if a == 1 {
    b = 1
} else if a == 2 {
    b = 2
} else {
    b = 3
}
assert(b == 3)
"#,
    );
}

#[test]
fn test_while_stmt() {
    run!(
        r#"
a = 0
while a < 10 {
    a += 1
}
assert(a == 10)
"#,
    );
    run!(
        r#"
a = 0
while a < 10 {
    a += 1
    if a == 5 {
        break
    }
}
assert(a == 5)
"#,
    );
    run!(
        r#"
a = 0
b = 0
while a < 10 {
    a += 1
    if a == 10 {
        continue
    }
    b += 1
}
assert(b == 9)
"#,
    );
}

#[test]
fn test_fn_call() {
    run!(
        r#"
fn add(a, b) {
    return a + b
}
assert(add(1, 2) == 3)
"#,
    );
    run!(
        r#"
add = fn(a, b) {
    return a + b
}
assert(add(1, 2) == 3)
"#,
    );
    run!(
        r#"
global gcd
fn gcd(x, y) {
    if y == 0 {
        return x
    } else {
        return gcd(y, x % y)
    }
}
assert(gcd(54, 24) == 6)
"#,
    );
}

#[test]
fn test_table_expr() {
    run!(
        r#"
a = {
    "a": 1,
    "b": 2,
}
assert(a["a"] == 1)
assert(a.a == 1)
assert(a::a == 1)
"#,
    );
    run!(
        r#"
a = {
    "a": 1,
    "get_a": fn (self) {
        return self.a
    }
}
other_a = {"a": 2}
assert(a.get_a() == 1)
assert(a::get_a(other_a) == 2)
"#,
    );
}

#[test]
fn test_variadic_fn() {
    run!(
        r#"
fn f(a, *b) {
    assert(a == 1)
    assert(b == [2, 3])
}
f(1, 2, 3)
"#,
    );
}

#[test]
fn test_type_convert_bool() {
    run!(
        r#"
assert(bool(null) == false)
assert(bool(0) == false)
assert(bool(1) == true)
assert(bool(0.0) == false)
assert(bool("") == true)
assert(bool({}) == true)
"#,
    );
    run!(
        r#"
assert(int(null) == 0)
assert(int(true) == 1)
assert(int(flase) == 0)
assert(int(0.0) == 0)
assert(int(0.5) == 0)
assert(int(0.9) == 0)
assert(int(1.0) == 1)
assert(int(1.5) == 1)
assert(int("1") == 1)
assert(int("123") == 123)
"#,
    );
    run!(
        r#"
assert(float(null) == 0.0)
assert(float(true) == 1.0)
assert(float(flase) == 0.0)
assert(float(0) == 0.0)
assert(float(1) == 1.0)
assert(float("1.0") == 1.0)
assert(float("1.23") == 1.23)
"#,
    );
    run!(
        r#"
assert(str(null) == "null")
assert(str(true) == "true")
assert(str(false) == "false")
assert(str(123) == "123")
assert(str(0.123) == "0.123")
"#,
    );
}

#[test]
fn test_for_table() {
    run!(
        r#"
import std::table::{keys, values}
t = {1: 123, 2: 456}
flag = 0
for k in keys(t) {
    if flag == 0 {
        assert(k == 1)
    } else if flag == 1 {
        assert(k == 2)
    } else {
        assert(false)
    }
    flag += 1
}
flag = 0
for v in values(t) {
    if flag == 0 {
        assert(v == 123)
    } else if flag == 1 {
        assert(v == 456)
    } else {
        assert(false)
    }
    flag += 1
}
flag = 0
for k, v in t {
    if flag == 0 {
        assert(k == 1)
        assert(v == 123)
    } else if flag == 1 {
        assert(k == 2)
        assert(v == 456)
    } else {
        assert(false)
    }
    flag += 1
}
"#,
    );
}

#[test]
fn test_for() {
    run!(
        r#"
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
assert(l, {2: 2, 4: 4, 6: 6, 8: 8, 10: 10, 12: 12, 14: 14, 16: 16, 18: 18, 20: 20})
println(repr(l))
"#,
    );
}

#[test]
fn test_do() {
    run!(
        r#"
import std::io::{println}
l = do {
    a = 1
    b = 2
}
println(repr(l))
"#
    );
}

#[test]
fn test_try() {
    run!(
        r#"
fn a() {
    return 1
}
fn b () {
    throw 1
}
res, err = try a()
assert(res == 1)
assert(err == null)

res, err = try b()
assert(res == null)
assert(err == 1)
"#
    );
}

#[test]
fn test_try_option() {
    run!(
        r#"
fn a() {
    return 1
}
fn b () {
    throw 1
}
res = try? a()
assert(res == 1)

res = try? b()
assert(res == null)
"#
    );
}

#[test]
fn test_try_panic() {
    run!(
        r#"
fn a() {
    return 1
}
res = try! a()
assert(res == 1)
    "#
    );
}

#[test]
#[should_panic]
fn test_try_panic_error() {
    run!(
        r#"
fn b () {
    throw 1
}
res = try! b()
    "#
    );
}

#[test]
fn tail_call() {
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
    context::Lucia::new()
        .run_code(compiler::code::Code::try_from(input).unwrap())
        .unwrap();
}

#[test]
fn add_pref() {
    let input = "
    i = 0
    ans = 0
    while i < 100000 {
        i += 1
        ans += 1
    }
    ";
    let start = Instant::now();
    let mut lucia = context::Lucia::new();
    let code = compiler::code::Code::try_from(input).unwrap();
    for _ in 0..100 {
        lucia.run_code(code.clone()).unwrap();
    }
    let duration = start.elapsed();
    println!("Time: {:?}", duration / 100);
}

#[test]
fn gcd_pref() {
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
    let mut lucia = context::Lucia::new();
    let code = compiler::code::Code::try_from(input).unwrap();
    for _ in 0..100 {
        lucia.run_code(code.clone()).unwrap();
    }
    let duration = start.elapsed();
    println!("Time: {:?}", duration / 100);
}

#[test]
fn temp() {
    let input = "
    // import std::io::{println}
    // l = {}
    // l[#] = {
    //     '__setitem__': fn (self, key, value) {
    //         println('__setitem__', self, key, value)
    //         self.k = key
    //         self.v = value
    //     },
    //     '__getitem__': fn (self, key) {
    //         println('__setitem__', self, key)
    //         return 0
    //     },
    // }
    // println(repr(l[#]))
    // l[1] = 2
    // println(repr(l))
    // return l['a']


    import std::io::*

    // t = {}
    // t[#] = {
    //     '__sub__': fn(self, other) {
    //         other + 114
    //     }
    // }
    // t - 100

    // import std::table::*
    // t = {'i': 10}
    // t[#] = {
    //     '__iter__': fn(self) {
    //         self.i -= 1
    //         if self.i == 0 {
    //             return null
    //         }
    //         return self.i
    //     }
    // }
    // for i in t {
    //     println(i)
    // }

    // fn a() {
    //     t = 0
    //     return || {
    //         t += 1
    //         if t > 10 {
    //             return null
    //         }
    //         return t * 2
    //     }
    // }
    // for i in a() {
    //     println(i)
    // }

    // import std::io::*
    // println(bool(int(input())))
    // println(42)

    // import std::table::*
    // t = [1, 2, 3]
    // for i in values(t) {
    //     if i == 1 {
    //         t[3] = 4
    //     }
    //     println(i)
    // }

    fn t() {
        throw 42
    }
    println(try t())
    ";
    let code = compiler::code::Code::try_from(input).unwrap();
    let mut lucia = context::Lucia::new();
    println!("{:?}", lucia.run_code(code));
}

#[test]
fn temp_pref() {
    for _ in 0..10 {
        add_pref()
    }
}
