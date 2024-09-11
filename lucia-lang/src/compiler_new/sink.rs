use std::{iter::Peekable, mem};

use rowan::{GreenNodeBuilder, Language};

use super::{event::Event, parse::Parse, parser::ParseError, syntax::LuciaLanguage, token::Token};

pub(crate) struct Sink<'iter, 'token, 'input: 'token, T: Iterator<Item = &'token Token<'input>>> {
    builder: GreenNodeBuilder<'static>,
    token_iter: Peekable<&'iter mut T>,
    events: Vec<Event>,
    errors: Vec<ParseError>,
}

impl<'iter, 'token, 'input: 'token, T: Iterator<Item = &'token Token<'input>>>
    Sink<'iter, 'token, 'input, T>
{
    pub(crate) fn new(token_iter: &'iter mut T, events: Vec<Event>) -> Self {
        Self {
            builder: GreenNodeBuilder::new(),
            token_iter: token_iter.peekable(),
            events,
            errors: Vec::new(),
        }
    }

    pub(crate) fn finish(mut self) -> Parse {
        for idx in 0..self.events.len() {
            match mem::replace(&mut self.events[idx], Event::Placeholder) {
                Event::Start {
                    kind,
                    forward_parent,
                } => {
                    let mut kinds = vec![kind];

                    let mut idx = idx;
                    let mut forward_parent = forward_parent;

                    // Walk through the forward parent of the forward parent and the forward parent
                    // of that, and of that, etc. until we reach a Start event without a forward
                    // parent.
                    while let Some(fp) = forward_parent {
                        idx += fp;

                        forward_parent = if let Event::Start {
                            kind,
                            forward_parent,
                        } =
                            mem::replace(&mut self.events[idx], Event::Placeholder)
                        {
                            kinds.push(kind);
                            forward_parent
                        } else {
                            unreachable!()
                        };
                    }

                    for kind in kinds.into_iter().rev() {
                        self.builder.start_node(LuciaLanguage::kind_to_raw(kind));
                    }
                }
                Event::Token { kind } => {
                    assert!(self.token_iter.peek().unwrap().kind == kind);
                    self.token();
                }
                Event::Finish => self.builder.finish_node(),
                Event::Error { error } => self.errors.push(error),
                Event::Placeholder => (),
            }

            self.eat_trivia();
        }

        Parse {
            green_node: self.builder.finish(),
            errors: self.errors,
        }
    }

    fn eat_trivia(&mut self) {
        while let Some(token) = self.token_iter.peek() {
            if !token.kind.is_trivia() {
                break;
            }

            self.token();
        }
    }

    fn token(&mut self) {
        let Token { kind, text, .. } = self.token_iter.next().unwrap();
        self.builder
            .token(LuciaLanguage::kind_to_raw((*kind).into()), text);
    }
}
