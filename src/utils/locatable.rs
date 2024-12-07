use text_size::TextRange;

/// A type that can be located in source text.
pub trait Locatable {
    fn range(&self) -> TextRange;
}

impl Locatable for TextRange {
    fn range(&self) -> TextRange {
        *self
    }
}

impl<T: Locatable> Locatable for Box<T> {
    fn range(&self) -> TextRange {
        self.as_ref().range()
    }
}
