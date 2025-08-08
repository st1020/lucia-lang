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
pub mod semantic;
pub mod token;
pub mod value;

/// Compile the input source code into Lucia code.
pub fn compile<S: interning::StringInterner>(
    interner: S,
    input: &str,
) -> Result<code::Code<S::String>, Vec<error::CompilerError>> {
    let (ast, parser_errors) = parser::parse(interner, input);
    let (semantic, analyzer_errors) = analyzer::analyze(&ast);
    let (code, codegen_errors) = codegen::gen_code(&ast, &semantic);
    let errors = parser_errors
        .into_iter()
        .chain(analyzer_errors)
        .chain(codegen_errors)
        .collect::<Vec<_>>();
    if errors.is_empty() {
        Ok(code)
    } else {
        Err(errors)
    }
}
