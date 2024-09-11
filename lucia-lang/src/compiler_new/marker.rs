use core::panic;

use super::{event::Event, parser::Parser, syntax::SyntaxKind, token::Token};

pub(crate) struct Marker {
    pos: usize,
    completed: bool,
}

impl Marker {
    pub(crate) fn new(pos: usize) -> Marker {
        Marker {
            pos,
            completed: false,
        }
    }

    /// Finishes the syntax tree node and assigns `kind` to it,
    /// and mark the create a `CompletedMarker` for possible future
    /// operation like `.precede()` to deal with forward_parent.
    pub(crate) fn complete<'token, 'input: 'token, T: Iterator<Item = &'token Token<'input>>>(
        mut self,
        p: &mut Parser<'_, 'token, 'input, T>,
        kind: SyntaxKind,
    ) -> CompletedMarker {
        self.completed = true;
        let event_at_pos = &mut p.events[self.pos];
        match event_at_pos {
            Event::Placeholder => {
                *event_at_pos = Event::Start {
                    kind,
                    forward_parent: None,
                };
            }
            _ => unreachable!(),
        }
        p.events.push(Event::Finish);
        CompletedMarker::new(self.pos, kind)
    }
}

impl Drop for Marker {
    fn drop(&mut self) {
        if !self.completed {
            panic!("Marker must be completed")
        }
    }
}

pub(crate) struct CompletedMarker {
    pos: usize,
    kind: SyntaxKind,
}

impl CompletedMarker {
    fn new(pos: usize, kind: SyntaxKind) -> Self {
        CompletedMarker { pos, kind }
    }

    /// This method allows to create a new node which starts
    /// *before* the current one. That is, parser could start
    /// node `A`, then complete it, and then after parsing the
    /// whole `A`, decide that it should have started some node
    /// `B` before starting `A`. `precede` allows to do exactly
    /// that. See also docs about
    /// [`Event::Start::forward_parent`](crate::event::Event::Start::forward_parent).
    ///
    /// Given completed events `[START, FINISH]` and its corresponding
    /// `CompletedMarker(pos: 0, _)`.
    /// Append a new `START` events as `[START, FINISH, NEWSTART]`,
    /// then mark `NEWSTART` as `START`'s parent with saving its relative
    /// distance to `NEWSTART` into forward_parent(=2 in this case);
    pub(crate) fn precede<'token, 'input, T: Iterator<Item = &'token Token<'input>>>(
        self,
        p: &mut Parser<'_, 'token, 'input, T>,
    ) -> Marker {
        let new_pos = p.start();
        match &mut p.events[self.pos] {
            Event::Start { forward_parent, .. } => {
                *forward_parent = Some(new_pos.pos - self.pos);
            }
            _ => unreachable!(),
        }
        new_pos
    }

    pub(crate) fn kind(&self) -> SyntaxKind {
        self.kind
    }
}
