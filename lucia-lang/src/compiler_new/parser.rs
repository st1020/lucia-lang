use std::{iter::Peekable, mem};

use rowan::{TextRange, TextSize};
use thiserror::Error;

use crate::utils::Join;

use super::{
    event::Event,
    marker::{CompletedMarker, Marker},
    syntax::SyntaxKind,
    token::{Token, TokenKind},
};

pub(crate) struct Parser<'iter, 'token, 'input: 'token, T: Iterator<Item = &'token Token<'input>>> {
    pub(crate) events: Vec<Event>,
    token_iter: Peekable<&'iter mut T>,
    expected_kinds: Vec<TokenKind>,
}

impl<'iter, 'token, 'input: 'token, T: Iterator<Item = &'token Token<'input>>>
    Parser<'iter, 'token, 'input, T>
{
    /// Constructs a new `Parser` with a token iter.
    pub(crate) fn new(token_iter: &'iter mut T) -> Self {
        Self {
            token_iter: token_iter.peekable(),
            events: Vec::new(),
            expected_kinds: Vec::new(),
        }
    }

    /// Starts a new node in the syntax tree. All nodes and tokens
    /// consumed between the `start` and the corresponding `Marker::complete`
    /// belong to the same node.
    pub(crate) fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.events.push(Event::Placeholder);
        Marker::new(pos)
    }

    /// Moves to the next token.
    fn bump(&mut self) {
        self.expected_kinds.clear();
        self.eat_trivia();
        let token = self.token_iter.next();
        self.events.push(Event::Token {
            kind: token.unwrap().kind,
        });
    }

    fn peek(&mut self) -> Option<TokenKind> {
        self.eat_trivia();
        self.peek_raw()
    }

    fn peek_token(&mut self) -> Option<&Token<'input>> {
        self.eat_trivia();
        self.peek_token_raw()
    }

    fn eat_trivia(&mut self) {
        while self.peek_raw().map_or(false, TokenKind::is_trivia) {
            self.token_iter.next();
        }
    }

    fn peek_raw(&mut self) -> Option<TokenKind> {
        self.peek_token_raw().map(|Token { kind, .. }| *kind)
    }

    fn peek_token_raw(&mut self) -> Option<&Token<'input>> {
        self.token_iter.peek().cloned()
    }

    /// Expects and consumes the token `t`. Signals an error if the next token is not `t`.
    fn expect(&mut self, t: TokenKind) {
        if self.check(t) {
            self.bump();
        } else {
            self.error();
        }
    }

    /// Checks if the current token is `t`, and returns `true` if so.
    /// This method will automatically add `t` to `expected_tokens` if `t` is not encountered.
    fn check(&mut self, t: TokenKind) -> bool {
        let is_present = self.peek() == Some(t);
        if !is_present {
            self.expected_kinds.push(t);
        }
        is_present
    }

    /// Consumes a token 't' if it exists. Returns whether the given token was present.
    fn eat(&mut self, t: TokenKind) -> bool {
        let is_present = self.check(t);
        if is_present {
            self.bump();
        }
        is_present
    }

    fn eat_start(&mut self, t: TokenKind) -> Option<Marker> {
        if self.check(t) {
            let m = self.start();
            self.bump();
            Some(m)
        } else {
            None
        }
    }

    pub(crate) fn error(&mut self) {
        let current_token = self.peek_token();

        let (found, range) = if let Some(Token { kind, range, .. }) = current_token {
            (Some(*kind), *range)
        } else {
            (None, TextRange::empty(TextSize::new(0)))
        };

        self.events.push(Event::Error {
            error: ParseError {
                found,
                range,
                expected: mem::take(&mut self.expected_kinds),
            },
        });

        if !self.at_end() {
            let m = self.start();
            self.bump();
            m.complete(self, SyntaxKind::Error);
        }
    }

    pub(crate) fn at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    pub(crate) fn parse(mut self) -> Vec<Event> {
        let m = self.start();
        while !self.at_end() {
            self.parse_stmt();
        }
        m.complete(&mut self, SyntaxKind::Root);
        self.events
    }

    fn parse_block(&mut self) {
        let m = self.start();
        while self.eat(TokenKind::CloseBrace) && !self.at_end() {
            self.parse_stmt();
        }
        m.complete(self, SyntaxKind::Block);
    }

    fn parse_stmt(&mut self) {
        if self.check(TokenKind::OpenBrace) {
            self.parse_block();
        } else if self.check(TokenKind::If) {
            self.parse_stmt_if();
        } else if let Some(m) = self.eat_start(TokenKind::Loop) {
            self.parse_block();
            m.complete(self, SyntaxKind::LoopStmt);
        } else if let Some(m) = self.eat_start(TokenKind::While) {
            self.parse_expr();
            self.parse_block();
            m.complete(self, SyntaxKind::WhileStmt);
        } else if let Some(m) = self.eat_start(TokenKind::For) {
            while !self.eat(TokenKind::In) && !self.at_end() {
                self.parse_name();
                if self.check(TokenKind::In) {
                    break;
                }
                self.expect(TokenKind::Comma);
            }
            self.parse_expr();
            self.parse_block();
            m.complete(self, SyntaxKind::ForStmt);
        } else if let Some(m) = self.eat_start(TokenKind::Break) {
            m.complete(self, SyntaxKind::BreakStmt);
        } else if let Some(m) = self.eat_start(TokenKind::Continue) {
            m.complete(self, SyntaxKind::ContinueStmt);
        } else if let Some(m) = self.eat_start(TokenKind::Return) {
            self.parse_expr();
            m.complete(self, SyntaxKind::ReturnStmt);
        } else if let Some(m) = self.eat_start(TokenKind::Throw) {
            self.parse_expr();
            m.complete(self, SyntaxKind::ThrowStmt);
        } else if let Some(m) = self.eat_start(TokenKind::Global) {
            while !self.at_end() {
                self.parse_typed_name(false);
                if !self.check(TokenKind::Comma) {
                    break;
                }
            }
            m.complete(self, SyntaxKind::GlobalStmt);
        } else if let Some(m) = self.eat_start(TokenKind::Import) {
            self.parse_stmt_package_path();
            if let Some(m) = self.eat_start(TokenKind::DoubleColon) {
                if self.eat(TokenKind::Mul) {
                    m.complete(self, SyntaxKind::GlobImport);
                } else if self.eat(TokenKind::OpenBrace) {
                    self.parse_stmt_import_items();
                    self.expect(TokenKind::CloseBrace);
                    m.complete(self, SyntaxKind::NestedImport);
                }
            } else if let Some(m) = self.eat_start(TokenKind::As) {
                self.parse_name();
                m.complete(self, SyntaxKind::AliasImport);
            }
            m.complete(self, SyntaxKind::ImportStmt);
        } else if let Some(m) = self.eat_start(TokenKind::Fn) {
            self.parse_name();
            self.expect(TokenKind::OpenParen);
            self.parse_params(false);
            self.expect(TokenKind::CloseParen);
            self.parse_ret_type_option();
            self.parse_block();
            m.complete(self, SyntaxKind::FnStmt);
        } else if let Some(lhs) = self.parse_expr_option() {
            if lhs.kind() == SyntaxKind::Name && self.check(TokenKind::Colon) {
                let m = lhs.precede(self);
                self.bump();
                self.parse_type();
                self.expect(TokenKind::Assign);
                self.parse_expr();
                m.complete(self, SyntaxKind::Assign);
            } else if matches!(
                lhs.kind(),
                SyntaxKind::Name | SyntaxKind::MemberItemExpr | SyntaxKind::MemberAttrExpr
            ) {
                if self.check(TokenKind::Comma) {
                    let m = lhs.precede(self);
                    loop {
                        self.expect(TokenKind::Comma);
                        self.parse_expr();
                        if self.eat(TokenKind::Assign) {
                            break;
                        }
                    }
                    let mut multi = false;
                    self.parse_expr();
                    while self.eat(TokenKind::Comma) {
                        multi = true;
                        self.parse_expr();
                    }
                    m.complete(
                        self,
                        if multi {
                            SyntaxKind::AssignMultiStmt
                        } else {
                            SyntaxKind::AssignUnpackStmt
                        },
                    );
                } else if self.check(TokenKind::Assign) {
                    let m = lhs.precede(self);
                    self.bump();
                    self.parse_expr();
                    m.complete(self, SyntaxKind::AssignStmt);
                } else if self.check(TokenKind::AddAssign)
                    || self.check(TokenKind::SubAssign)
                    || self.check(TokenKind::MulAssign)
                    || self.check(TokenKind::DivAssign)
                    || self.check(TokenKind::RemAssign)
                {
                    let m = lhs.precede(self);
                    self.bump();
                    self.parse_expr();
                    m.complete(self, SyntaxKind::AssignOpStmt);
                }
            }
        }

        self.expect(TokenKind::Eol);
    }

    fn parse_stmt_if(&mut self) {
        let m = self.start();
        self.expect(TokenKind::If);
        self.parse_expr();
        self.parse_block();
        if self.eat(TokenKind::Else) {
            if self.check(TokenKind::If) {
                self.parse_stmt_if();
            } else {
                self.parse_block();
            }
        }
        m.complete(self, SyntaxKind::IfStmt);
    }

    fn parse_stmt_package_path(&mut self) {
        let m = self.start();
        self.parse_name();
        while self.eat(TokenKind::DoubleColon) && !self.at_end() {
            self.parse_name();
        }
        m.complete(self, SyntaxKind::PackagePath);
    }

    fn parse_stmt_import_items(&mut self) {
        while !self.at_end() {
            let m = self.start();
            self.parse_name();
            if self.eat(TokenKind::As) {
                self.parse_name();
            }
            m.complete(self, SyntaxKind::ImportItem);
            if !self.eat(TokenKind::Comma) {
                break;
            }
        }
    }

    fn parse_params(&mut self, atom_type: bool) {
        while !self.at_end() {
            if let Some(m) = self.eat_start(TokenKind::Mul) {
                self.parse_typed_name(atom_type);
                self.eat(TokenKind::Comma);
                m.complete(self, SyntaxKind::Param);
                break;
            } else if self.check(TokenKind::Ident) {
                let mut end = false;
                let m = self.start();
                self.parse_typed_name(atom_type);
                if !self.eat(TokenKind::Comma) {
                    end = true;
                }
                m.complete(self, SyntaxKind::Param);
                if end {
                    break;
                }
            } else {
                break;
            }
        }
    }

    fn parse_expr(&mut self) {
        if self.parse_expr_bp(0).is_none() {
            self.error();
        }
    }

    fn parse_expr_option(&mut self) -> Option<CompletedMarker> {
        self.parse_expr_bp(0)
    }

    fn parse_expr_bp(&mut self, min_bp: u8) -> Option<CompletedMarker> {
        let mut lhs = self.parse_expr_lhs()?;

        while let Some(t) = self.peek() {
            if let Some((l_bp, ())) = t.postfix_binding_power() {
                if l_bp < min_bp {
                    break;
                }
                self.bump();
                let m = lhs.precede(self);
                match t {
                    TokenKind::OpenParen => {
                        let parsed_rhs = self.parse_expr_bp(0).is_some();
                        self.expect(TokenKind::CloseParen);
                        lhs = m.complete(self, SyntaxKind::CallExpr);
                        if !parsed_rhs {
                            break;
                        }
                    }
                    TokenKind::OpenBracket => {
                        let parsed_rhs =
                            self.eat(TokenKind::Pound) || self.parse_expr_bp(0).is_some();
                        self.expect(TokenKind::CloseBracket);
                        lhs = m.complete(self, SyntaxKind::MemberItemExpr);
                        if !parsed_rhs {
                            break;
                        }
                    }
                    _ => unreachable!(),
                }
            } else if let Some((l_bp, r_bp)) = t.infix_binding_power() {
                if l_bp < min_bp {
                    break;
                }
                self.bump();
                let m = lhs.precede(self);
                match t {
                    TokenKind::Dot | TokenKind::SafeDot | TokenKind::DoubleColon => {
                        let parsed_rhs =
                            self.eat(TokenKind::Pound) || self.parse_expr_bp(r_bp).is_some();
                        lhs = m.complete(self, SyntaxKind::MemberAttrExpr);
                        if !parsed_rhs {
                            break;
                        }
                    }
                    _ => {
                        let parsed_rhs = self.parse_expr_bp(r_bp).is_some();
                        lhs = m.complete(self, SyntaxKind::BinaryExpr);
                        if !parsed_rhs {
                            break;
                        }
                    }
                }
            } else {
                break;
            }
        }

        Some(lhs)
    }

    fn parse_expr_lhs(&mut self) -> Option<CompletedMarker> {
        self.parse_expr_atom().or_else(|| self.parse_expr_prefix())
    }

    fn parse_expr_prefix(&mut self) -> Option<CompletedMarker> {
        let ((), r_bp) = self.peek()?.prefix_binding_power()?;
        let m = self.start();
        self.bump();
        self.parse_expr_bp(r_bp);
        Some(m.complete(self, SyntaxKind::UnaryExpr))
    }

    fn parse_expr_atom(&mut self) -> Option<CompletedMarker> {
        let res = if self.check(TokenKind::Ident) {
            self.parse_name()
        } else if let Some(m) = self.parse_literal_option() {
            m
        } else if let Some(m) = self.eat_start(TokenKind::OpenParen) {
            self.parse_expr();
            self.expect(TokenKind::CloseParen);
            m.complete(self, SyntaxKind::ParenExpr)
        } else if let Some(m) = self.eat_start(TokenKind::OpenBrace) {
            while !self.check(TokenKind::CloseBrace) {
                let m = self.start();
                self.parse_expr();
                self.expect(TokenKind::Colon);
                self.parse_expr();
                let mut end = false;
                if !self.eat(TokenKind::Comma) {
                    end = true;
                }
                m.complete(self, SyntaxKind::TableItem);
                if end {
                    break;
                }
            }
            self.expect(TokenKind::CloseBrace);
            m.complete(self, SyntaxKind::TableExpr)
        } else if let Some(m) = self.eat_start(TokenKind::OpenBracket) {
            while !self.check(TokenKind::CloseBracket) {
                let m = self.start();
                self.parse_expr();
                let mut end = false;
                if !self.eat(TokenKind::Comma) {
                    end = true;
                }
                m.complete(self, SyntaxKind::ListItem);
                if end {
                    break;
                }
            }
            self.expect(TokenKind::CloseBracket);
            m.complete(self, SyntaxKind::ListExpr)
        } else if let Some(m) = self.eat_start(TokenKind::Fn) {
            self.expect(TokenKind::OpenParen);
            self.parse_params(false);
            self.expect(TokenKind::CloseParen);
            self.parse_ret_type_option();
            self.parse_block();
            m.complete(self, SyntaxKind::FnExpr)
        } else if let Some(m) = self.eat_start(TokenKind::VBar) {
            self.parse_params(true);
            self.expect(TokenKind::VBar);
            self.parse_ret_type_option();
            self.parse_block();
            m.complete(self, SyntaxKind::ClosureExpr)
        } else if let Some(m) = self.eat_start(TokenKind::Do) {
            self.parse_block();
            m.complete(self, SyntaxKind::DoExpr)
        } else if let Some(m) = self.eat_start(TokenKind::Try) {
            if self.check(TokenKind::Question) || self.check(TokenKind::Exclamation) {
                self.bump();
            }
            self.parse_expr();
            m.complete(self, SyntaxKind::TryExpr)
        } else {
            return None;
        };
        Some(res)
    }

    fn parse_literal_option(&mut self) -> Option<CompletedMarker> {
        if self.check(TokenKind::Null)
            || self.check(TokenKind::True)
            || self.check(TokenKind::False)
            || self.check(TokenKind::Int)
            || self.check(TokenKind::Float)
            || self.check(TokenKind::Str)
            || self.check(TokenKind::RawStr)
        {
            let m = self.start();
            self.bump();
            Some(m.complete(self, SyntaxKind::LiteralExpr))
        } else {
            None
        }
    }

    fn parse_name(&mut self) -> CompletedMarker {
        let m = self.start();
        self.expect(TokenKind::Ident);
        m.complete(self, SyntaxKind::Name)
    }

    fn parse_typed_name(&mut self, atom_type: bool) -> CompletedMarker {
        let m = self.start();
        self.parse_name();
        self.expect(TokenKind::Colon);
        if atom_type {
            self.parse_type_atom();
        } else {
            self.parse_type();
        }

        m.complete(self, SyntaxKind::TypedName)
    }

    fn parse_type(&mut self) {
        self.parse_type_atom();
        while self.eat(TokenKind::VBar) {
            self.parse_expr_atom();
        }
    }

    fn parse_type_atom(&mut self) {
        if self.check(TokenKind::Ident) {
            self.parse_name();
        } else if self.parse_literal_option().is_some() {
        } else if let Some(m) = self.eat_start(TokenKind::OpenBrace) {
            while !self.check(TokenKind::CloseBrace) {
                if let Some(m) = self.eat_start(TokenKind::OpenBracket) {
                    self.parse_type();
                    self.expect(TokenKind::CloseBracket);
                    self.expect(TokenKind::Colon);
                    self.parse_type();
                    self.eat(TokenKind::Comma);
                    m.complete(self, SyntaxKind::TableOtherTypeItem);
                    break;
                } else {
                    let m = self.start();
                    self.parse_type();
                    self.expect(TokenKind::Colon);
                    self.parse_type();
                    let mut end = false;
                    if !self.eat(TokenKind::Comma) {
                        end = true;
                    }
                    m.complete(self, SyntaxKind::TableTypeItem);
                    if end {
                        break;
                    }
                }
            }
            self.expect(TokenKind::CloseBrace);
            m.complete(self, SyntaxKind::TableType);
        } else if let Some(m) = self.eat_start(TokenKind::Fn) {
            self.expect(TokenKind::OpenParen);
            loop {
                if let Some(m) = self.eat_start(TokenKind::Mul) {
                    self.parse_type();
                    self.eat(TokenKind::Comma);
                    m.complete(self, SyntaxKind::FnTypeParam);
                    break;
                } else if self.check(TokenKind::Ident) {
                    let mut end = false;
                    let m = self.start();
                    self.parse_type();
                    if !self.eat(TokenKind::Comma) {
                        end = true;
                    }
                    m.complete(self, SyntaxKind::FnTypeParam);
                    if end {
                        break;
                    }
                } else {
                    break;
                }
            }
            self.expect(TokenKind::CloseParen);
            self.parse_ret_type_option();
            m.complete(self, SyntaxKind::FnType);
        } else if let Some(m) = self.eat_start(TokenKind::OpenParen) {
            self.parse_type();
            self.expect(TokenKind::CloseParen);
            m.complete(self, SyntaxKind::ParenType);
        }
    }

    fn parse_ret_type_option(&mut self) -> Option<CompletedMarker> {
        if self.check(TokenKind::Arrow) || self.check(TokenKind::Throw) {
            let m = self.start();
            if let Some(m) = self.eat_start(TokenKind::Arrow) {
                self.parse_type();
                m.complete(self, SyntaxKind::ReturnType);
            }
            if let Some(m) = self.eat_start(TokenKind::Throw) {
                self.parse_type();
                m.complete(self, SyntaxKind::ThrowStmt);
            }
            Some(m.complete(self, SyntaxKind::RetType))
        } else {
            None
        }
    }
}

impl TokenKind {
    fn infix_binding_power(&self) -> Option<(u8, u8)> {
        let res = match self {
            Self::Or => (1, 2),
            Self::And => (3, 4),
            Self::Eq | Self::Lt | Self::LtEq | Self::NotEq | Self::GtEq | Self::Gt | Self::Is => {
                (5, 6)
            }
            Self::Add | Self::Sub => (7, 8),
            Self::Mul | Self::Div | Self::Rem => (9, 10),
            Self::Dot | Self::SafeDot | Self::DoubleColon => (13, 14),
            _ => return None,
        };
        Some(res)
    }

    fn prefix_binding_power(&self) -> Option<((), u8)> {
        let res = match self {
            Self::Not | Self::Sub => ((), 11),
            _ => return None,
        };
        Some(res)
    }

    fn postfix_binding_power(&self) -> Option<(u8, ())> {
        let res = match self {
            Self::OpenBracket | Self::SafeOpenBracket | Self::OpenParen => (13, ()),
            _ => return None,
        };
        Some(res)
    }
}

/// ParseError.
#[derive(Error, Debug, Clone)]
#[error(
        "unexpected token (expected {}, found {})",
        .expected.iter().join(", "),
        .found.map(|t| t.to_string()).unwrap_or("None".to_string()),
    )]
pub struct ParseError {
    expected: Vec<TokenKind>,
    found: Option<TokenKind>,
    range: TextRange,
}
