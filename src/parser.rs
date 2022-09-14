use crate::ast::*;
use crate::errors::{LResult, LucyError, ParserErrorKind, SyntaxErrorKind};
use crate::lexer::{LiteralValue, Token, TokenKind};

#[macro_export]
macro_rules! parser_error {
    ($error_kind:expr) => {
        LucyError::SyntaxError(SyntaxErrorKind::ParserError($error_kind))
    };
}

pub struct Parser<'a> {
    /// The current token.
    pub token: Token,
    /// The previous token.
    pub prev_token: Token,
    /// The token iter
    pub token_iter: &'a mut dyn Iterator<Item = Token>,
    /// Is token iter end
    pub is_eof: bool,
}

impl<'a> Parser<'a> {
    pub fn new(token_iter: &'a mut dyn Iterator<Item = Token>) -> Self {
        let mut parser = Parser {
            token: Token::dummy(),
            prev_token: Token::dummy(),
            token_iter,
            is_eof: false,
        };
        parser.bump();
        parser
    }

    pub fn bump(&mut self) {
        self.prev_token = self.token.clone();
        self.token = match self.token_iter.next() {
            Some(t) => t,
            None => {
                self.is_eof = true;
                Token::dummy()
            }
        };
    }

    pub fn expect(&self, t: TokenKind) -> LResult<()> {
        if self.token.kind != t {
            if self.is_eof {
                Err(parser_error!(ParserErrorKind::UnexpectEOF))
            } else {
                Err(parser_error!(ParserErrorKind::UnexpectToken {
                    token: Box::new(self.token.clone()),
                    expect_token_kind: Some(Box::new(t)),
                }))
            }
        } else {
            Ok(())
        }
    }

    pub fn parse(&mut self) -> LResult<Box<Block>> {
        Ok(Box::new(Block {
            start: self.token.start,
            body: {
                let mut temp = Vec::new();
                while !self.is_eof {
                    temp.push(*self.parse_stmt()?);
                }
                temp
            },
            end: self.prev_token.end,
        }))
    }

    fn parse_stmt(&mut self) -> LResult<Box<Stmt>> {
        Ok(Box::new(match self.token.kind {
            TokenKind::If => Stmt {
                start: self.token.start,
                kind: StmtKind::If {
                    test: {
                        self.bump();
                        self.parse_expr(1)?
                    },
                    consequent: self.parse_block()?,
                    alternate: match self.token.kind {
                        TokenKind::Else => {
                            self.bump();
                            Some(self.parse_block()?)
                        }
                        _ => None,
                    },
                },
                end: self.prev_token.end,
            },
            TokenKind::Loop => Stmt {
                start: self.token.start,
                kind: StmtKind::Loop {
                    body: {
                        self.bump();
                        self.parse_block()?
                    },
                },
                end: self.prev_token.end,
            },
            TokenKind::While => Stmt {
                start: self.token.start,
                kind: StmtKind::While {
                    test: {
                        self.bump();
                        self.parse_expr(1)?
                    },
                    body: self.parse_block()?,
                },
                end: self.prev_token.end,
            },
            TokenKind::For => Stmt {
                start: self.token.start,
                kind: StmtKind::For {
                    left: {
                        self.bump();
                        self.parse_ident()?
                    },
                    right: {
                        self.expect(TokenKind::In)?;
                        self.bump();
                        self.parse_expr(1)?
                    },
                    body: self.parse_block()?,
                },
                end: self.prev_token.end,
            },
            TokenKind::Break => Stmt {
                start: self.token.start,
                kind: {
                    self.bump();
                    self.expect(TokenKind::Semi)?;
                    self.bump();
                    StmtKind::Break
                },
                end: self.prev_token.end,
            },
            TokenKind::Continue => Stmt {
                start: self.token.start,
                kind: {
                    self.bump();
                    self.expect(TokenKind::Semi)?;
                    self.bump();
                    StmtKind::Continue
                },
                end: self.prev_token.end,
            },
            TokenKind::Return => Stmt {
                start: self.token.start,
                kind: StmtKind::Return {
                    argument: {
                        self.bump();
                        let temp = self.parse_expr(1)?;
                        self.expect(TokenKind::Semi)?;
                        self.bump();
                        temp
                    },
                },
                end: self.prev_token.end,
            },
            TokenKind::Global => Stmt {
                start: self.token.start,
                kind: StmtKind::Global {
                    arguments: {
                        let mut temp = Vec::new();
                        while self.token.kind != TokenKind::Semi {
                            self.bump();
                            temp.push(*self.parse_ident()?);
                            match self.token.kind {
                                TokenKind::Semi => break,
                                _ => self.expect(TokenKind::Comma)?,
                            }
                        }
                        self.bump();
                        temp
                    },
                },
                end: self.prev_token.end,
            },
            TokenKind::Import => Stmt {
                start: self.token.start,
                kind: {
                    let mut path = Vec::new();
                    let kind: ImportKind;
                    let mut glob = false;
                    self.bump();
                    loop {
                        match self.token.kind {
                            TokenKind::Semi | TokenKind::As | TokenKind::OpenBrace => break,
                            TokenKind::Mul => {
                                glob = true;
                                self.bump();
                                self.expect(TokenKind::Semi)?;
                                break;
                            }
                            _ => {
                                path.push(*self.parse_ident()?);
                                match self.token.kind {
                                    TokenKind::Semi | TokenKind::As | TokenKind::OpenBrace => break,
                                    _ => {
                                        self.expect(TokenKind::DoubleColon)?;
                                        self.bump();
                                    }
                                };
                            }
                        }
                    }
                    match self.token.kind {
                        TokenKind::Semi => {
                            kind = if glob {
                                ImportKind::Glob
                            } else {
                                match path.last() {
                                    Some(v) => ImportKind::Simple(v.clone()),
                                    None => {
                                        return Err(parser_error!(
                                            ParserErrorKind::ParserImportStmtError
                                        ))
                                    }
                                }
                            };
                        }
                        TokenKind::As => {
                            self.bump();
                            kind = ImportKind::Simple(*self.parse_ident()?);
                            self.expect(TokenKind::Semi)?;
                        }
                        TokenKind::OpenBrace => {
                            self.bump();
                            let mut temp = Vec::new();
                            loop {
                                match self.token.kind {
                                    TokenKind::CloseBrace => break,
                                    TokenKind::Ident(_) => {
                                        let t = *self.parse_ident()?;
                                        temp.push((
                                            t.clone(),
                                            match self.token.kind {
                                                TokenKind::As => {
                                                    self.bump();
                                                    let t = *self.parse_ident()?;
                                                    t
                                                }
                                                _ => t,
                                            },
                                        ));
                                        if self.token.kind == TokenKind::CloseBrace {
                                            break;
                                        }
                                        self.expect(TokenKind::Comma)?;
                                        self.bump()
                                    }
                                    _ => self.expect(TokenKind::CloseBrace)?,
                                }
                            }
                            kind = ImportKind::Nested(temp);
                            self.bump();
                            self.expect(TokenKind::Semi)?;
                        }
                        _ => {
                            self.expect(TokenKind::Semi)?;
                            return Err(parser_error!(ParserErrorKind::ParserImportStmtError));
                        }
                    }
                    self.bump();
                    StmtKind::Import { path, kind }
                },
                end: self.prev_token.end,
            },
            TokenKind::OpenBrace => {
                let temp = self.parse_block()?;
                Stmt {
                    start: temp.start,
                    end: temp.end,
                    kind: StmtKind::Block(temp),
                }
            }
            _ => {
                let ast_node = self.parse_expr(1)?;
                let temp = match self.token.kind.clone() {
                    TokenKind::Assign => {
                        match ast_node.kind {
                            ExprKind::Ident(_)
                            | ExprKind::Member {
                                table: _,
                                property: _,
                                kind: _,
                            } => (),
                            _ => return Err(parser_error!(ParserErrorKind::ParserAssignStmtError)),
                        }
                        self.bump();
                        let right = self.parse_expr(1)?;
                        Stmt {
                            start: ast_node.start,
                            end: right.end,
                            kind: StmtKind::Assign {
                                left: ast_node,
                                right,
                            },
                        }
                    }
                    token_kind @ (TokenKind::AddAssign
                    | TokenKind::SubAssign
                    | TokenKind::MulAssign
                    | TokenKind::DivAssign
                    | TokenKind::ModAssign) => {
                        match ast_node.kind {
                            ExprKind::Ident(_)
                            | ExprKind::Member {
                                table: _,
                                property: _,
                                kind: _,
                            } => (),
                            _ => return Err(parser_error!(ParserErrorKind::ParserAssignStmtError)),
                        }
                        self.bump();
                        let right = self.parse_expr(1)?;
                        Stmt {
                            start: ast_node.start,
                            end: right.end,
                            kind: StmtKind::AssignOp {
                                operator: BinOp::from_assign_token(&token_kind)?,
                                left: ast_node,
                                right,
                            },
                        }
                    }
                    _ => Stmt {
                        start: ast_node.start,
                        end: ast_node.end,
                        kind: StmtKind::Expr(ast_node),
                    },
                };
                self.expect(TokenKind::Semi)?;
                self.bump();
                temp
            }
        }))
    }

    fn parse_block(&mut self) -> LResult<Box<Block>> {
        self.expect(TokenKind::OpenBrace)?;
        Ok(Box::new(Block {
            start: self.token.start,
            body: {
                let mut temp = Vec::new();
                self.bump();
                while self.token.kind != TokenKind::CloseBrace {
                    temp.push(*self.parse_stmt()?);
                }
                self.bump();
                temp
            },
            end: self.prev_token.end,
        }))
    }

    fn parse_expr(&mut self, min_precedence: u32) -> LResult<Box<Expr>> {
        let start = self.token.start;
        let mut left = self.parse_expr_unary()?;
        loop {
            let op_info = BinOp::from_token(&self.token.kind);
            match op_info {
                Ok(operator) => {
                    let precedence = operator.get_precedence();
                    if precedence < min_precedence {
                        break;
                    }
                    self.bump();
                    let right = self.parse_expr(precedence + 1)?;
                    left = Box::new(Expr {
                        kind: ExprKind::Binary {
                            operator,
                            left,
                            right,
                        },
                        start,
                        end: self.prev_token.end,
                    })
                }
                Err(_) => break,
            }
        }
        Ok(left)
    }

    fn parse_expr_unary(&mut self) -> LResult<Box<Expr>> {
        match self.token.kind {
            TokenKind::Literal(_)
            | TokenKind::Ident(_)
            | TokenKind::Func
            | TokenKind::VBar
            | TokenKind::OpenBrace
            | TokenKind::OpenParen => self.parse_expr_primary(),
            TokenKind::Not | TokenKind::Sub => {
                let start = self.token.start;
                let operator = UnOp::from_token(&self.token.kind)?;
                self.bump();
                Ok(Box::new(Expr {
                    start,
                    kind: ExprKind::Unary {
                        operator,
                        argument: self.parse_expr_primary()?,
                    },
                    end: self.prev_token.end,
                }))
            }
            _ => Err(parser_error!(ParserErrorKind::UnexpectToken {
                token: Box::new(self.token.clone()),
                expect_token_kind: None,
            })),
        }
    }

    fn parse_expr_primary(&mut self) -> LResult<Box<Expr>> {
        let start = self.token.start;
        let mut ast_node = self.parse_expr_atom()?;
        loop {
            match &self.token.kind {
                TokenKind::OpenParen => {
                    ast_node = Box::new(Expr {
                        kind: ExprKind::Call {
                            callee: ast_node,
                            arguments: {
                                let mut temp = Vec::new();
                                self.bump();
                                while self.token.kind != TokenKind::CloseParen {
                                    temp.push(*self.parse_expr(1)?);
                                    if self.token.kind == TokenKind::CloseParen {
                                        break;
                                    }
                                    self.expect(TokenKind::Comma)?;
                                    self.bump()
                                }
                                temp
                            },
                        },
                        start,
                        end: self.prev_token.end,
                    });
                    self.bump();
                }
                TokenKind::OpenBracket => {
                    ast_node = Box::new(Expr {
                        kind: ExprKind::Member {
                            table: ast_node,
                            kind: MemberExprKind::OpenBracket,
                            property: {
                                self.bump();
                                self.parse_expr(1)?
                            },
                        },
                        start,
                        end: self.prev_token.end,
                    });
                    self.expect(TokenKind::CloseBracket)?;
                    self.bump()
                }
                TokenKind::Dot | TokenKind::DoubleColon => {
                    ast_node = Box::new(Expr {
                        kind: ExprKind::Member {
                            table: ast_node,
                            kind: MemberExprKind::from_token(&self.token.kind)?,
                            property: {
                                self.bump();
                                self.parse_expr_ident()?
                            },
                        },
                        start,
                        end: self.prev_token.end,
                    });
                }
                _ => break,
            }
        }
        Ok(ast_node)
    }

    fn parse_expr_atom(&mut self) -> LResult<Box<Expr>> {
        match &self.token.kind {
            TokenKind::OpenParen => {
                self.bump();
                let temp = self.parse_expr(1)?;
                self.expect(TokenKind::CloseParen)?;
                self.bump();
                Ok(temp)
            }
            TokenKind::Literal(v) => {
                let temp = Box::new(Expr {
                    kind: ExprKind::Lit(Box::new(Lit {
                        value: v.clone(),
                        start: self.token.start,
                        end: self.token.end,
                    })),
                    start: self.token.start,
                    end: self.token.end,
                });
                self.bump();
                Ok(temp)
            }
            TokenKind::Ident(_) => self.parse_expr_ident(),
            TokenKind::OpenBrace => self.parse_expr_table(),
            TokenKind::Func | TokenKind::VBar => self.parse_expr_func(),
            _ => Err(parser_error!(ParserErrorKind::UnexpectToken {
                token: Box::new(self.token.clone()),
                expect_token_kind: None,
            })),
        }
    }

    fn parse_expr_ident(&mut self) -> LResult<Box<Expr>> {
        let temp = self.parse_ident()?;
        Ok(Box::new(Expr {
            start: temp.start,
            end: temp.end,
            kind: ExprKind::Ident(temp),
        }))
    }

    fn parse_expr_table(&mut self) -> LResult<Box<Expr>> {
        Ok(Box::new(Expr {
            start: self.token.start,
            kind: ExprKind::Table {
                properties: {
                    let mut temp = Vec::new();
                    let mut c = 0;
                    self.bump();
                    while self.token.kind != TokenKind::CloseBrace {
                        let start = self.token.start;
                        let temp_expr = self.parse_expr(1)?;
                        let key: Box<Expr>;
                        let value: Box<Expr>;
                        if self.token.kind == TokenKind::Colon {
                            key = temp_expr;
                            self.bump();
                            value = self.parse_expr(1)?;
                        } else {
                            key = Box::new(Expr {
                                kind: ExprKind::Lit(Box::new(Lit {
                                    value: LiteralValue::Int(c),
                                    start: self.token.start,
                                    end: self.token.end,
                                })),
                                start: self.token.start,
                                end: self.token.end,
                            });
                            value = temp_expr;
                        }
                        c += 1;
                        temp.push(TableProperty {
                            key,
                            value,
                            start,
                            end: self.prev_token.end,
                        });
                        if self.token.kind == TokenKind::CloseBrace {
                            break;
                        }
                        self.expect(TokenKind::Comma)?;
                        self.bump();
                    }
                    self.bump();
                    temp
                },
            },
            end: self.prev_token.end,
        }))
    }

    fn parse_expr_func(&mut self) -> LResult<Box<Expr>> {
        let end_token;
        Ok(Box::new(Expr {
            start: self.token.start,
            kind: ExprKind::Function {
                is_closure: {
                    match self.token.kind {
                        TokenKind::Func => {
                            self.bump();
                            self.expect(TokenKind::OpenParen)?;
                            end_token = TokenKind::CloseParen;
                            false
                        }
                        TokenKind::VBar => {
                            end_token = TokenKind::VBar;
                            true
                        }
                        _ => {
                            return Err(parser_error!(ParserErrorKind::UnexpectToken {
                                token: Box::new(self.token.clone()),
                                expect_token_kind: None,
                            }))
                        }
                    }
                },
                params: {
                    let mut temp = Vec::new();
                    self.bump();
                    while self.token.kind != end_token {
                        temp.push(*self.parse_ident()?);
                        if self.token.kind == end_token {
                            break;
                        }
                        self.expect(TokenKind::Comma)?;
                        self.bump()
                    }
                    self.bump();
                    temp
                },
                body: self.parse_block()?,
            },
            end: self.prev_token.end,
        }))
    }

    fn parse_ident(&mut self) -> LResult<Box<Ident>> {
        match &self.token.kind {
            TokenKind::Ident(v) => {
                let temp = Ident {
                    start: self.token.start,
                    name: v.clone(),
                    end: self.token.end,
                };
                self.bump();
                Ok(Box::new(temp))
            }
            _ => Err(parser_error!(ParserErrorKind::UnexpectToken {
                token: Box::new(self.token.clone()),
                expect_token_kind: None,
            })),
        }
    }
}
