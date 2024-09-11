use super::{parser::ParseError, syntax::SyntaxKind, token::TokenKind};

#[derive(Debug)]
pub(crate) enum Event {
    Placeholder,

    Start {
        kind: SyntaxKind,
        forward_parent: Option<usize>,
    },

    Finish,

    Token {
        kind: TokenKind,
    },

    Error {
        error: ParseError,
    },
}
