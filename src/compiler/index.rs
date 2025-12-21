//! The index types used in the compiler.

#![expect(clippy::integer_division_remainder_used, clippy::same_name_method)]

use oxc_index::define_nonmax_u32_index_type;

define_nonmax_u32_index_type! {
    pub struct FunctionId;
}

define_nonmax_u32_index_type! {
    pub struct ScopeId;
}

define_nonmax_u32_index_type! {
    pub struct SymbolId;
}

define_nonmax_u32_index_type! {
    pub struct ReferenceId;
}

define_nonmax_u32_index_type! {
    #[derive(Default)]
    pub struct BasicBlockId;
}
