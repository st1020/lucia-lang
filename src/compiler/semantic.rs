//! The semantic information of Lucia program.

use ordermap::{OrderMap, OrderSet};
use oxc_index::IndexVec;
use rustc_hash::FxBuildHasher;
use text_size::TextRange;

use super::{
    ast::FunctionKind,
    index::{FunctionId, ReferenceId, ScopeId, SymbolId},
};

/// Semantic information of a Lucia program.
///
/// [`Semantic`] contains the results of analyzing a program, including the
/// [function] table, [scope] tree and [symbol] table.
///
/// [function]: FunctionSemantic
/// [scope]: Scope
/// [symbol]: Symbol
#[derive(Debug, Clone)]
pub struct Semantic<S> {
    pub functions: IndexVec<FunctionId, FunctionSemantic>,
    pub scopes: IndexVec<ScopeId, Scope<S>>,
    pub symbols: IndexVec<SymbolId, Symbol<S>>,
    pub references: IndexVec<ReferenceId, Reference>,
}

/// The semantic information of a function.
#[derive(Debug, Clone)]
pub struct FunctionSemantic {
    /// The kind of function.
    pub kind: FunctionKind,
    /// The parent function it belongs in.
    pub parent_id: Option<FunctionId>,
    /// Symbols in a function.
    pub symbols: OrderSet<SymbolId, FxBuildHasher>,
}

impl FunctionSemantic {
    pub fn new(kind: FunctionKind, parent_id: Option<FunctionId>) -> Self {
        Self {
            kind,
            parent_id,
            symbols: OrderSet::with_hasher(FxBuildHasher),
        }
    }
}

/// The semantic information of a scope.
#[derive(Debug, Clone)]
pub struct Scope<S> {
    /// The kind of scope.
    pub kind: ScopeKind,
    /// The parent scope it belongs in.
    pub parent_id: Option<ScopeId>,
    /// The function it belongs in.
    pub function_id: FunctionId,
    /// Symbol bindings in a scope.
    ///
    /// A binding is a mapping from an identifier name to its [`SymbolId`]
    pub bindings: OrderMap<S, SymbolId, FxBuildHasher>,
}

impl<S> Scope<S> {
    pub fn new(kind: ScopeKind, parent_id: Option<ScopeId>, function_id: FunctionId) -> Self {
        Self {
            kind,
            parent_id,
            function_id,
            bindings: OrderMap::with_hasher(FxBuildHasher),
        }
    }
}

/// The kind of scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScopeKind {
    Function,
    Closure,
    Block,
}

/// The semantic information of a symbol.
#[derive(Debug, Clone)]
pub struct Symbol<S> {
    /// The name of the symbol.
    pub name: S,
    /// The range of the symbol definition.
    pub range: TextRange,
    /// The scope it belongs in.
    pub scope_id: ScopeId,
    /// The kind of symbol.
    pub kind: SymbolKind,
    /// The references of the symbol.
    pub references: Vec<ReferenceId>,
}

/// The kind of symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SymbolKind {
    Local,
    Upvalue,
    Global,
}

/// The semantic information of a reference of a symbol.
#[derive(Debug, Clone)]
pub struct Reference {
    /// The range of the reference.
    pub range: TextRange,
    /// The symbol being referenced.
    pub symbol_id: SymbolId,
    /// The kind of reference.
    pub kind: ReferenceKind,
}

/// The kind of reference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ReferenceKind {
    Read,
    Write,
}
