use archery::RcK;
use lucia_lang::compiler::index::SymbolId;
use rpds::HashTrieMap;
use rustc_hash::FxBuildHasher;

use crate::typing::Type;

#[derive(Clone)]
pub struct Env<S: Clone + Eq + Ord>(HashTrieMap<SymbolId, Type<S>, RcK, FxBuildHasher>);

impl<S: Clone + Eq + Ord> Env<S> {
    pub fn new() -> Self {
        Self(HashTrieMap::new_with_hasher_and_ptr_kind(FxBuildHasher))
    }

    pub fn get(&self, key: &SymbolId) -> Option<&Type<S>> {
        self.0.get(key)
    }

    pub fn insert_mut(&mut self, key: SymbolId, value: Type<S>) {
        self.0.insert_mut(key, value);
    }
}

impl<S: Clone + Eq + Ord> Default for Env<S> {
    fn default() -> Self {
        Self::new()
    }
}
