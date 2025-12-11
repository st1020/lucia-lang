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
//! use lucia_lang::Context;
//! let input = r#"
//! import std::io::{println}
//! println("Hello World!")
//! "#;
//! let mut lucia = Context::new();
//! let code = lucia.compile(input).unwrap();
//! lucia.execute(code).unwrap();
//! ```

// Pedantic warnings
#![warn(clippy::pedantic)]
#![allow(clippy::too_many_lines, clippy::must_use_candidate)]
// TODO: Improve documentation
#![allow(
    clippy::doc_markdown,
    clippy::missing_errors_doc,
    clippy::missing_panics_doc
)]
// Restriction warnings
#![warn(clippy::restriction)]
#![allow(
    clippy::blanket_clippy_restriction_lints,
    clippy::cognitive_complexity,
    clippy::exhaustive_enums,
    clippy::exhaustive_structs,
    clippy::implicit_return,
    clippy::min_ident_chars,
    clippy::missing_inline_in_public_items,
    clippy::missing_trait_methods,
    clippy::mod_module_files,
    clippy::module_name_repetitions,
    clippy::pattern_type_mismatch,
    clippy::pointer_format,
    clippy::pub_use,
    clippy::pub_with_shorthand,
    clippy::question_mark_used,
    clippy::redundant_test_prefix,
    clippy::semicolon_outside_block,
    clippy::separated_literal_suffix,
    clippy::shadow_reuse,
    clippy::single_call_fn,
    clippy::single_char_lifetime_names,
    clippy::std_instead_of_alloc,
    clippy::std_instead_of_core,
    clippy::unneeded_field_pattern,
    clippy::unused_trait_names
)]
// TODO: Improve documentation
#![allow(
    clippy::allow_attributes_without_reason,
    clippy::missing_assert_message,
    clippy::missing_docs_in_private_items
)]
// TODO: Prevent panic
#![allow(
    clippy::arithmetic_side_effects,
    clippy::expect_used,
    clippy::indexing_slicing,
    clippy::panic,
    clippy::panic_in_result_fn,
    clippy::unreachable,
    clippy::unwrap_used
)]

pub mod compiler;
pub mod context;
pub mod errors;
pub mod executor;
pub mod frame;
pub mod fuel;
pub mod libs;
pub mod objects;
pub mod utils;
mod vm;

pub use context::*;
