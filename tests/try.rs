use lucia_lang::{
    Context,
    compiler::{compile, interning::BasicInterner},
};

#[test]
#[should_panic]
fn test_try_panic_error() {
    let input = r#"
fn b () {
    throw 1
}
res = try! b()
    "#;
    let mut lucia = Context::new();
    let code = lucia.compile(input).unwrap();
    lucia.execute(code).unwrap();
}

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
