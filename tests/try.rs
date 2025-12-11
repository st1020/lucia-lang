use lucia_lang::compiler::{compile, interning::BasicInterner};

#[test]
fn test_parse_error() {
    let input = r#"
1 +

println(1 + 1)

1 -
"#;
    let interner = BasicInterner::default();
    let parse_error = compile(interner, input).unwrap_err();
    assert_eq!(parse_error.len(), 2);
}
