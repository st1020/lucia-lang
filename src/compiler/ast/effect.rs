use core::fmt;

use text_size::TextRange;

use super::*;

/// An effect.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Effect<S> {
    pub name: S,
    pub params: Box<Params<S>>,
    pub returns: Option<Box<Ty<S>>>,
    pub range: TextRange,
}

impl_locatable!(Effect);

impl<S: fmt::Display> fmt::Display for Effect<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let returns_str = self
            .returns
            .as_ref()
            .map(|returns| format!(" -> {returns}"))
            .unwrap_or_default();
        write!(f, "({}){}", self.params, returns_str)
    }
}

/// An effect handler.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectHandler<S> {
    pub effect: Box<Ident<S>>,
    pub params: Box<Params<S>>,
    pub body: Box<Block<S>>,
    pub range: TextRange,
}

impl_locatable!(EffectHandler);

impl<S: fmt::Display> fmt::Display for EffectHandler<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "with {}({}) {}", self.effect, self.params, self.body)
    }
}
