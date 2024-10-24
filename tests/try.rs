use lucia_lang::Lucia;

#[test]
#[should_panic]
fn test_try_panic_error() {
    let input = r#"
fn b () {
    throw 1
}
res = try! b()
    "#;
    Lucia::new().run_code(input).unwrap();
}
