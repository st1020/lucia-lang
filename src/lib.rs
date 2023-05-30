//! Lucia Language Compiler and VM.
//!
//! ```txt
//!        +-------+             +--------+          +----------+
//! str -> | lexer | - Tokens -> | parser | - AST -> | analyzer |
//!        +-------+             +--------+          +----------+
//!                                                        |
//!             +----- AST with semantic information ------+
//!             |
//!             v
//!        +---------+           +-----+
//!        | codegen | - Code -> | LVM |
//!        +---------+           +-----+
//! ```
//!
//! # Examples
//!
//! ```
//! use lucia_lang::{lvm::Lvm, code:: Code};
//! let input = r#"
//! import std::io::{println}
//! println("Hello World!")
//! "#;
//! let code = Code::try_from(input).unwrap();
//! let mut lvm = Lvm::new();
//! lvm.run(code).unwrap();
//! ```

pub mod analyzer;
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

use analyzer::analyze;
use code::Code;
use codegen::gen_code;
use errors::{Error, Result};
use lexer::tokenize;
use parser::Parser;

impl TryFrom<&str> for Code {
    type Error = Error;

    fn try_from(value: &str) -> Result<Self> {
        gen_code(analyze(Parser::new(&mut tokenize(value)).parse()?))
    }
}

impl TryFrom<&String> for Code {
    type Error = Error;

    fn try_from(value: &String) -> Result<Self> {
        gen_code(analyze(Parser::new(&mut tokenize(value)).parse()?))
    }
}
