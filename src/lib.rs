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
//!        +---------+           +-----------------------+
//!        | codegen | - Code -> | Lucia Virtual Machine |
//!        +---------+           +-----------------------+
//! ```
//!
//! # Examples
//!
//! ```rust
//! use lucia_lang::Lucia;
//! let input = r#"
//! import std::io::{println}
//! println("Hello World!")
//! "#;
//! let mut lucia = Lucia::new();
//! let code = lucia.compile(input).unwrap();
//! lucia.execute(&code).unwrap();
//! ```

pub mod compiler;
pub mod context;
pub mod errors;
pub mod frame;
pub mod fuel;
pub mod libs;
pub mod meta_ops;
pub mod objects;
pub mod thread;
pub mod utils;
mod vm;

pub use context::*;
