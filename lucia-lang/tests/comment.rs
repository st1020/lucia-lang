use lucia_lang::{
    compiler::{code::Code, lexer::tokenize, parser::parse},
    Lucia,
};

#[test]
fn test_comment() {
    let input = r#"
assert(true)
// assert(false)
/*
assert(false)
assert(false)
*/
"#;
    Lucia::new()
        .run_code(Code::try_from(input).unwrap())
        .unwrap();

    let input = r#"
import std::io::{println}
println("Hello World!")
"#;
    let ast = parse(&mut tokenize(input)).unwrap();
    assert!(ast.first_comment.is_empty());

    let input = r#"
//
"#;

    let ast = parse(&mut tokenize(input)).unwrap();
    assert_eq!(ast.first_comment, "\n");

    let input = r#"
// first_comment
"#;

    let ast = parse(&mut tokenize(input)).unwrap();
    assert_eq!(ast.first_comment, " first_comment\n");

    let input = r#"
/* first_comment */
"#;
    let ast = parse(&mut tokenize(input)).unwrap();
    assert_eq!(ast.first_comment, " first_comment \n");

    let input = r#"
// first_comment
// first_comment
"#;
    let ast = parse(&mut tokenize(input)).unwrap();
    assert_eq!(ast.first_comment, " first_comment\n first_comment\n");
}
