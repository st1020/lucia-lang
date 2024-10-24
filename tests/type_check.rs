use bumpalo::Bump;
use lucia_lang::compiler::{check_type, compile, interning::BumpInterner};

#[test]
fn test_type_hint() {
    let input = r#"
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
    let allocator = &Bump::new();
    let interner = BumpInterner::new(allocator);
    let (parse_error, type_error) = check_type(allocator, interner, input);
    assert_eq!(parse_error.len(), 0);
    assert_eq!(type_error.len(), 0);
}

#[test]
fn test_type_hint_error() {
    let input = r#"
t1: int = ""
"#;
    let allocator = &Bump::new();
    let interner = BumpInterner::new(allocator);
    let (parse_error, type_error) = check_type(allocator, interner, input);
    assert_eq!(parse_error.len(), 0);
    assert_eq!(type_error.len(), 1);
}

#[test]
fn test_type_check_error() {
    let input = r#"
t1: int = "" // error
t2: int = 1 // ok
t3: int = 0.1 // error
"#;
    let allocator = &Bump::new();
    let interner = BumpInterner::new(allocator);
    let (parse_error, type_error) = check_type(allocator, interner, input);
    assert_eq!(parse_error.len(), 0);
    assert_eq!(type_error.len(), 2);
}

#[test]
fn test_parse_error() {
    let input = r#"
1 +

println(1 + 1)

1 -
"#;
    let allocator = &Bump::new();
    let interner = BumpInterner::new(allocator);
    let parse_error = compile(allocator, interner, input).unwrap_err();
    assert_eq!(parse_error.len(), 2);
}
