use std::sync::Arc;

use dashmap::DashSet;
use lucia_lang::compiler::interning::StringInterner;

#[derive(Debug, Default)]
pub struct ThreadSafeInterner(DashSet<Arc<str>>);

impl StringInterner for ThreadSafeInterner {
    type String = Arc<str>;

    fn intern(&mut self, s: &str) -> Self::String {
        if let Some(s) = self.0.get(s) {
            s.clone()
        } else {
            let s = Arc::from(Box::from(s));
            self.0.insert(Arc::clone(&s));
            s
        }
    }
}
