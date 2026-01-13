//! The index types used in the compiler.

#![expect(clippy::integer_division_remainder_used, clippy::same_name_method)]

macro_rules! define_index {
    ($($type:ident),* $(,)?) => {
        $(
            oxc_index::define_nonmax_u32_index_type! {
                #[derive(Default, derive_more::Display)]
                #[display("{self:?}")]
                pub struct $type;
            }
        )*
    };
}

define_index!(
    FunctionId,
    ScopeId,
    SymbolId,
    ReferenceId,
    LabelId,
    BasicBlockId,
    CodeId,
    LocalNameId,
    GlobalNameId,
    UpvalueNameId,
    ConstId,
    ConstCodeId,
);

impl From<LabelId> for BasicBlockId {
    #[inline]
    fn from(value: LabelId) -> Self {
        BasicBlockId::from_raw(value.raw())
    }
}

impl From<BasicBlockId> for LabelId {
    #[inline]
    fn from(value: BasicBlockId) -> Self {
        LabelId::from_raw(value.raw())
    }
}

impl From<CodeId> for BasicBlockId {
    #[inline]
    fn from(value: CodeId) -> Self {
        BasicBlockId::from_raw(value.raw())
    }
}

impl From<BasicBlockId> for CodeId {
    #[inline]
    fn from(value: BasicBlockId) -> Self {
        CodeId::from_raw(value.raw())
    }
}
