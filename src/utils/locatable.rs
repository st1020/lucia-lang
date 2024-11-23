use text_size::TextRange;

/// A type that can be located in source text.
pub trait Locatable {
    fn range(&self) -> TextRange;
}
