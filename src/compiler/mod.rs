//! The Lucia lang compiler.

pub mod analyzer;
pub mod ast;
pub mod code;
pub mod codegen;
pub mod error;
pub mod index;
pub mod lexer;
pub mod opcode;
pub mod parser;
pub mod token;
pub mod typing;

/// Compile the input source code into Lucia code.
pub fn compile(input: &str) -> Result<code::Code, Vec<error::CompilerError>> {
    let allocator = &bumpalo::Bump::new();
    let (ast, mut errors) = parser::parse(allocator, input);
    let semantic = analyzer::analyze(&ast);
    let (code, mut code_gen_errors) = codegen::gen_code(&ast, &semantic);
    errors.append(&mut code_gen_errors);
    if errors.is_empty() {
        Ok(code)
    } else {
        Err(errors)
    }
}

/// Check the type of the input source code.
pub fn check_type(input: &str) -> (Vec<error::CompilerError>, Vec<typing::TypeError>) {
    let allocator = &bumpalo::Bump::new();
    let (ast, errors) = parser::parse(allocator, input);
    let semantic = analyzer::analyze(&ast);
    (errors, typing::check_type(&ast, &semantic))
}
