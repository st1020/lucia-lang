use lucia_lang::compiler::interning::BasicInterner;
use lucia_typing::check_type_source;

#[test]
fn test_type_hint() {
    let input = r#"
t1: int? = 1
t2: int | str = 1
t3: any | int = 1
t4: fn() -> null = fn(){}
t5: fn(int, str) -> bool = fn(a, b){return true}
t6: fn(int, str, ...bool) -> bool = fn(a, b, ...args){return true}
t7: {} = {}
t8: {a: int, b: str} = {'a': 1, 'b': '1'}
t9: {[int]: str} = ['test', 'test']
t10: {a: int, b: str, [int]: str} = {'a': 1, 'b': '1', 0: 'test'}
"#;
    let interner = BasicInterner::default();
    let (parse_error, type_error) = check_type_source(interner, input);
    assert_eq!(parse_error.len(), 0);
    assert_eq!(type_error.len(), 0);
}

#[test]
fn test_type_hint_error() {
    let input = r#"
t1: int = ""
"#;
    let interner = BasicInterner::default();
    let (parse_error, type_error) = check_type_source(interner, input);
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
    let interner = BasicInterner::default();
    let (parse_error, type_error) = check_type_source(interner, input);
    assert_eq!(parse_error.len(), 0);
    assert_eq!(type_error.len(), 2);
}
