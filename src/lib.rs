pub mod ast;
pub mod code;
pub mod codegen;
pub mod errors;
pub mod gc;
pub mod lexer;
pub mod libs;
pub mod lvm;
pub mod objects;
pub mod opcode;
pub mod parser;
pub mod token;
pub mod utils;

use std::convert::TryFrom;

use code::Code;
use codegen::CodeGen;
use errors::{Error, Result};
use lexer::tokenize;
use parser::Parser;

impl TryFrom<&str> for Code {
    type Error = Error;

    fn try_from(value: &str) -> Result<Self> {
        CodeGen::from(Parser::new(&mut tokenize(value)).parse()?).gen_code()
    }
}

impl TryFrom<&String> for Code {
    type Error = Error;

    fn try_from(value: &String) -> Result<Self> {
        CodeGen::from(Parser::new(&mut tokenize(value)).parse()?).gen_code()
    }
}
