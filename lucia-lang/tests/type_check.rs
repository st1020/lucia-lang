use lucia_lang::compiler::{analyzer::analyze, lexer::tokenize, parser::parse};

#[test]
fn test_type_hint() {
    let input = r#"
// type-check: on
t1: int? = 1
t2: int | str = 1
t3: any | int = 1
t4: fn() -> null = fn(){}
t5: fn(int, str) -> bool = fn(a, b){return true}
t6: fn(int, str, *bool) -> bool = fn(a, b, *args){return true}
t7: {} = {}
t8: {a: int, b: str} = {'a': 1, 'b': '1'}
t9: {[int]: str} = ['test', 'test']
t10: {a: int, b: str, [int]: str} = {'a': 1, 'b': '1', 0: 'test'}
"#;
    let ast = parse(&mut tokenize(input)).unwrap();
    println!("{}", ast);

    let functions = analyze(ast).unwrap();
    for (name, t) in &functions[0].local_names {
        if let Some(t) = t {
            println!("{name}: {t}");
        }
    }
}

#[test]
fn test_type_hint_error() {
    let input = r#"
// type-check: on
t1: int = ""
"#;
    let parse_error = analyze(parse(&mut tokenize(input)).unwrap()).err().unwrap();
    assert_eq!(parse_error.len(), 1);
}

#[test]
fn test_type_check_error() {
    let input = r#"
// type-check: on
t1: int = "" // error
t2: int = 1 // ok
t3: int = 0.1 // error
"#;
    let parse_error = analyze(parse(&mut tokenize(input)).unwrap()).err().unwrap();
    assert_eq!(parse_error.len(), 2);
}

#[test]
fn test_parse_error() {
    let input = r#"
1 +

println(1 + 1)

1 -
"#;
    let parse_error = parse(&mut tokenize(input)).err().unwrap();
    assert_eq!(parse_error.len(), 2);
}
