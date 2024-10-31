//! The Lucia lang compiler.

pub mod analyzer;
pub mod ast;
pub mod code;
pub mod codegen;
pub mod error;
pub mod index;
pub mod interning;
pub mod lexer;
pub mod opcode;
pub mod parser;
pub mod token;
pub mod typing;
pub mod value;

/// Compile the input source code into Lucia code.
pub fn compile<S: interning::StringInterner>(
    allocator: &bumpalo::Bump,
    interner: S,
    input: &str,
) -> Result<code::Code<S::String>, Vec<error::CompilerError>> {
    let (ast, mut errors) = parser::parse(allocator, interner, input);
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
pub fn check_type<S: interning::StringInterner<String: Eq + Ord>>(
    allocator: &bumpalo::Bump,
    interner: S,
    input: &str,
) -> (Vec<error::CompilerError>, Vec<typing::TypeError<S::String>>) {
    let (ast, errors) = parser::parse(allocator, interner, input);
    let semantic = analyzer::analyze(&ast);
    (errors, typing::check_type(&ast, &semantic))
}
