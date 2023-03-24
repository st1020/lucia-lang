use crate::ast::*;
use crate::errors::{Error, Result, SyntaxError};
use crate::token::{LiteralKind, Token, TokenKind, TokenType};

macro_rules! unexpected_token_error {
    ($self:expr) => {
        return Err(Error::SyntaxError(SyntaxError::UnexpectToken {
            token: Box::new($self.token.clone()),
            expected: $self.expected_tokens.clone(),
        }))
    };
}

/// The parser. Turns token iter into AST.
pub struct Parser<'a> {
    /// The current token.
    pub token: Token,
    /// The previous token.
    pub prev_token: Token,
    /// The token iter.
    pub token_iter: &'a mut dyn Iterator<Item = Token>,
    /// Is token iter end.
    pub is_eof: bool,

    expected_tokens: Vec<TokenType>,
}

impl<'a> Parser<'a> {
    /// Constructs a new `Paser` with a token iter.
    pub fn new(token_iter: &'a mut dyn Iterator<Item = Token>) -> Self {
        let mut parser = Parser {
            token: Token::dummy(),
            prev_token: Token::dummy(),
            token_iter,
            is_eof: false,
            expected_tokens: Vec::new(),
        };
        parser.bump();
        parser
    }

    /// Moves to the next token.
    fn bump(&mut self) {
        self.prev_token = self.token.clone();
        self.token = self.token_iter.next().unwrap_or_else(|| {
            self.is_eof = true;
            Token::dummy()
        });
        self.expected_tokens.clear();
    }

    /// Eats EOL.
    fn eat_eol(&mut self) {
        while self.token.kind == TokenKind::EOL {
            self.bump();
        }
    }

    /// Expects and consumes the token `t`. Signals an error if the next token is not `t`.
    fn expect(&mut self, t: &TokenKind) -> Result<()> {
        if self.token.kind == *t {
            self.bump();
            Ok(())
        } else if self.is_eof {
            Err(Error::SyntaxError(SyntaxError::UnexpectEOF))
        } else {
            self.expected_tokens.push(TokenType::Token(t.clone()));
            unexpected_token_error!(self)
        }
    }

    /// Checks if the current token is `t`, and returns `true` if so.
    /// This method will automatically add `t` to `expected_tokens` if `t` is not encountered.
    fn check(&mut self, t: &TokenKind) -> bool {
        let is_present = self.token.kind == *t;
        if !is_present {
            self.expected_tokens.push(TokenType::Token(t.clone()));
        }
        is_present
    }

    fn check_noexpect(&self, t: &TokenKind) -> bool {
        self.token.kind == *t
    }

    /// Consumes a token 't' if it exists. Returns whether the given token was present.
    fn eat(&mut self, t: &TokenKind) -> bool {
        let is_present = self.check(t);
        if is_present {
            self.bump()
        }
        is_present
    }

    fn eat_noexpect(&mut self, t: &TokenKind) -> bool {
        let is_present = self.check_noexpect(t);
        if is_present {
            self.bump()
        }
        is_present
    }

    fn eat_ident(&mut self) -> Option<String> {
        if let TokenKind::Ident(v) = self.token.kind.clone() {
            self.bump();
            Some(v)
        } else {
            self.expected_tokens.push(TokenType::Literal);
            None
        }
    }

    fn eat_literal(&mut self) -> Option<LiteralKind> {
        if let TokenKind::Literal(v) = self.token.kind.clone() {
            self.bump();
            Some(v)
        } else {
            self.expected_tokens.push(TokenType::Literal);
            None
        }
    }

    /// Parse token iter into AST `Box<Block>`.
    pub fn parse(&mut self) -> Result<Box<Block>> {
        Ok(Box::new(Block {
            start: self.token.start,
            body: {
                let mut temp = Vec::new();
                while !self.is_eof {
                    temp.push(self.parse_stmt()?);
                    self.expect(&TokenKind::EOL)?;
                    self.eat_eol();
                }
                temp
            },
            end: self.prev_token.end,
        }))
    }

    /// Parse statement.
    fn parse_stmt(&mut self) -> Result<Stmt> {
        macro_rules! stmt {
            ($kind:expr) => {
                Stmt {
                    start: self.prev_token.start,
                    kind: $kind,
                    end: self.prev_token.end,
                }
            };
        }
        macro_rules! check_assign_left {
            ($ast_node:expr) => {
                match $ast_node.kind {
                    ExprKind::Ident(_) => (),
                    ExprKind::Member { safe, .. } | ExprKind::MetaMember { safe, .. } => {
                        if safe {
                            return Err(SyntaxError::ParseAssignStmtError.into());
                        }
                    }
                    _ => return Err(SyntaxError::ParseAssignStmtError.into()),
                }
            };
        }
        macro_rules! assign_op_stmt {
            ($ast_node:expr, $bin_op:expr) => {{
                check_assign_left!($ast_node);
                let right = self.parse_expr(1)?;
                Stmt {
                    start: $ast_node.start,
                    end: right.end,
                    kind: StmtKind::AssignOp {
                        operator: $bin_op,
                        left: $ast_node,
                        right,
                    },
                }
            }};
        }
        self.eat_eol();
        let ast_node = if self.eat(&TokenKind::If) {
            self.parse_stmt_if()?
        } else if self.eat(&TokenKind::Loop) {
            stmt!(StmtKind::Loop {
                body: self.parse_block()?,
            })
        } else if self.eat(&TokenKind::While) {
            stmt!(StmtKind::While {
                test: self.parse_expr(1)?,
                body: self.parse_block()?,
            })
        } else if self.eat(&TokenKind::For) {
            stmt!(StmtKind::For {
                left: {
                    let mut temp = Vec::new();
                    while !self.eat_noexpect(&TokenKind::In) {
                        temp.push(self.parse_ident()?);
                        if self.eat(&TokenKind::In) {
                            break;
                        }
                        self.expect(&TokenKind::Comma)?;
                    }
                    temp
                },
                right: self.parse_expr(1)?,
                body: self.parse_block()?,
            })
        } else if self.eat(&TokenKind::Break) {
            stmt!(StmtKind::Break)
        } else if self.eat(&TokenKind::Continue) {
            stmt!(StmtKind::Continue)
        } else if self.eat(&TokenKind::Return) {
            stmt!(StmtKind::Return {
                argument: self.parse_expr(1)?,
            })
        } else if self.eat(&TokenKind::Throw) {
            stmt!(StmtKind::Throw {
                argument: self.parse_expr(1)?,
            })
        } else if self.eat(&TokenKind::Global) {
            stmt!(StmtKind::Global {
                arguments: {
                    let mut temp = Vec::new();
                    while !self.check_noexpect(&TokenKind::EOL) {
                        temp.push(self.parse_ident()?);
                        if self.check(&TokenKind::EOL) {
                            break;
                        }
                        self.expect(&TokenKind::Comma)?;
                    }
                    temp
                },
            })
        } else if self.eat(&TokenKind::Import) {
            stmt!({
                let mut path = Vec::new();
                let mut glob = false;
                loop {
                    if self.check_noexpect(&TokenKind::EOL)
                        | self.check_noexpect(&TokenKind::As)
                        | self.check_noexpect(&TokenKind::OpenBrace)
                    {
                        break;
                    } else if self.eat_noexpect(&TokenKind::Mul) {
                        glob = true;
                        break;
                    }
                    path.push(self.parse_ident()?);
                    if self.check(&TokenKind::EOL)
                        | self.check(&TokenKind::As)
                        | self.check(&TokenKind::OpenBrace)
                    {
                        break;
                    }
                    self.expect(&TokenKind::DoubleColon)?;
                }
                if path.is_empty() {
                    self.expected_tokens = vec![TokenType::Ident];
                    unexpected_token_error!(self);
                }
                let kind = if glob {
                    ImportKind::Glob
                } else if self.check(&TokenKind::EOL) {
                    ImportKind::Simple(Box::new(path.last().unwrap().clone()))
                } else if self.eat(&TokenKind::As) {
                    ImportKind::Simple(Box::new(self.parse_ident()?))
                } else if self.eat(&TokenKind::OpenBrace) {
                    let mut temp = Vec::new();
                    while !self.eat_noexpect(&TokenKind::CloseBrace) {
                        let t = self.parse_ident()?;
                        if self.eat(&TokenKind::As) {
                            temp.push((t, self.parse_ident()?));
                        } else {
                            temp.push((t.clone(), t));
                        }
                        self.eat_eol();
                        if self.eat(&TokenKind::CloseBrace) {
                            break;
                        }
                        self.expect(&TokenKind::Comma)?;
                        self.eat_eol();
                    }
                    ImportKind::Nested(temp)
                } else {
                    unexpected_token_error!(self);
                };
                StmtKind::Import { path, kind }
            })
        } else if self.check(&TokenKind::OpenBrace) {
            (*self.parse_block()?).into()
        } else if self.eat(&TokenKind::Fn) {
            stmt!(StmtKind::Assign {
                left: Box::new(self.parse_ident()?.into()),
                right: self.parse_expr_func(false)?
            })
        } else {
            let ast_node = self.parse_expr(1)?;
            if self.check(&TokenKind::Comma) {
                check_assign_left!(ast_node);
                let start = ast_node.start;
                let mut left = vec![*ast_node];
                loop {
                    if self.eat(&TokenKind::Comma) {
                        let ast_node = self.parse_expr(1)?;
                        check_assign_left!(ast_node);
                        left.push(*ast_node);
                    } else {
                        self.expect(&TokenKind::Assign)?;
                        break;
                    }
                }
                let right = self.parse_expr(1)?;
                if self.check_noexpect(&TokenKind::Comma) {
                    let mut right = vec![*right];
                    loop {
                        if self.eat(&TokenKind::Comma) {
                            right.push(*self.parse_expr(1)?);
                        } else {
                            break;
                        }
                    }
                    if left.len() != right.len() {
                        return Err(SyntaxError::ParseAssignStmtError.into());
                    }
                    Stmt {
                        start,
                        end: self.prev_token.end,
                        kind: StmtKind::AssignMulti { left, right },
                    }
                } else {
                    Stmt {
                        start,
                        end: right.end,
                        kind: StmtKind::AssignUnpack { left, right },
                    }
                }
            } else if self.eat(&TokenKind::Assign) {
                check_assign_left!(ast_node);
                let right = self.parse_expr(1)?;
                Stmt {
                    start: ast_node.start,
                    end: right.end,
                    kind: StmtKind::Assign {
                        left: ast_node,
                        right,
                    },
                }
            } else if self.eat(&TokenKind::AddAssign) {
                assign_op_stmt!(ast_node, BinOp::Add)
            } else if self.eat(&TokenKind::SubAssign) {
                assign_op_stmt!(ast_node, BinOp::Sub)
            } else if self.eat(&TokenKind::MulAssign) {
                assign_op_stmt!(ast_node, BinOp::Mul)
            } else if self.eat(&TokenKind::DivAssign) {
                assign_op_stmt!(ast_node, BinOp::Div)
            } else if self.eat(&TokenKind::ModAssign) {
                assign_op_stmt!(ast_node, BinOp::Mod)
            } else {
                Stmt {
                    start: ast_node.start,
                    end: ast_node.end,
                    kind: StmtKind::Expr(ast_node),
                }
            }
        };
        Ok(ast_node)
    }

    /// Parse block.
    fn parse_block(&mut self) -> Result<Box<Block>> {
        self.expect(&TokenKind::OpenBrace)?;
        self.eat_eol();
        Ok(Box::new(Block {
            start: self.prev_token.start,
            body: {
                let mut temp = Vec::new();
                while !self.eat_noexpect(&TokenKind::CloseBrace) {
                    temp.push(self.parse_stmt()?);
                    if self.eat(&TokenKind::CloseBrace) {
                        break;
                    }
                    self.expect(&TokenKind::EOL)?;
                    self.eat_eol();
                }
                temp
            },
            end: self.prev_token.end,
        }))
    }

    /// Parse if statement.
    fn parse_stmt_if(&mut self) -> Result<Stmt> {
        Ok(Stmt {
            start: self.prev_token.start,
            kind: StmtKind::If {
                test: self.parse_expr(1)?,
                consequent: self.parse_block()?,
                alternate: {
                    if self.eat(&TokenKind::Else) {
                        Some(Box::new(if self.eat(&TokenKind::If) {
                            self.parse_stmt_if()?
                        } else {
                            (*self.parse_block()?).into()
                        }))
                    } else {
                        None
                    }
                },
            },
            end: self.prev_token.end,
        })
    }

    /// Parse expression.
    fn parse_expr(&mut self, min_precedence: u32) -> Result<Box<Expr>> {
        let start = self.token.start;
        let mut left = self.parse_expr_unary()?;
        loop {
            let operator = match self.token.kind {
                TokenKind::Is => BinOp::Is,
                TokenKind::And => BinOp::And,
                TokenKind::Or => BinOp::Or,
                TokenKind::Eq => BinOp::Eq,
                TokenKind::NotEq => BinOp::Ne,
                TokenKind::LtEq => BinOp::Le,
                TokenKind::GtEq => BinOp::Ge,
                TokenKind::Lt => BinOp::Lt,
                TokenKind::Gt => BinOp::Gt,
                TokenKind::Add => BinOp::Add,
                TokenKind::Sub => BinOp::Sub,
                TokenKind::Mul => BinOp::Mul,
                TokenKind::Div => BinOp::Div,
                TokenKind::Mod => BinOp::Mod,
                _ => break,
            };
            if operator.precedence() < min_precedence {
                break;
            }
            self.bump();
            let right = self.parse_expr(operator.precedence() + 1)?;
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
        Ok(left)
    }

    /// Parse unary expression.
    fn parse_expr_unary(&mut self) -> Result<Box<Expr>> {
        macro_rules! unary_expr {
            ($un_op:expr) => {{
                Ok(Box::new(Expr {
                    start: self.token.start,
                    kind: ExprKind::Unary {
                        operator: $un_op,
                        argument: self.parse_expr_primary()?,
                    },
                    end: self.prev_token.end,
                }))
            }};
        }
        if self.eat_noexpect(&TokenKind::Not) {
            unary_expr!(UnOp::Not)
        } else if self.eat_noexpect(&TokenKind::Sub) {
            unary_expr!(UnOp::Neg)
        } else {
            self.parse_expr_primary()
        }
    }

    /// Parse primary expression.
    fn parse_expr_primary(&mut self) -> Result<Box<Expr>> {
        let start = self.token.start;
        let mut ast_node = self.parse_expr_atom()?;
        macro_rules! member_attr_expr {
            ($member_expr_kind:expr, $safe:expr) => {
                if self.eat_noexpect(&TokenKind::Pound) {
                    Box::new(Expr {
                        kind: ExprKind::MetaMember {
                            table: ast_node,
                            safe: $safe,
                        },
                        start,
                        end: self.prev_token.end,
                    })
                } else {
                    Box::new(Expr {
                        kind: ExprKind::Member {
                            table: ast_node,
                            kind: $member_expr_kind,
                            safe: $safe,
                            property: Box::new(self.parse_ident()?.into()),
                        },
                        start,
                        end: self.prev_token.end,
                    })
                }
            };
        }
        macro_rules! member_item_expr {
            ($safe:expr) => {
                if self.eat_noexpect(&TokenKind::Pound) {
                    Box::new(Expr {
                        kind: ExprKind::MetaMember {
                            table: ast_node,
                            safe: $safe,
                        },
                        start: {
                            self.expect(&TokenKind::CloseBracket)?;
                            start
                        },
                        end: self.prev_token.end,
                    })
                } else {
                    Box::new(Expr {
                        kind: ExprKind::Member {
                            table: ast_node,
                            kind: MemberKind::Bracket,
                            safe: $safe,
                            property: self.parse_expr(1)?,
                        },
                        start: {
                            self.expect(&TokenKind::CloseBracket)?;
                            start
                        },
                        end: self.prev_token.end,
                    })
                }
            };
        }
        loop {
            ast_node = if self.eat_noexpect(&TokenKind::OpenParen) {
                Box::new(Expr {
                    kind: ExprKind::Call {
                        callee: ast_node,
                        arguments: {
                            let mut temp = Vec::new();
                            while !self.eat_noexpect(&TokenKind::CloseParen) {
                                temp.push(*self.parse_expr(1)?);
                                self.eat_eol();
                                if self.eat(&TokenKind::CloseParen) {
                                    break;
                                }
                                self.expect(&TokenKind::Comma)?;
                                self.eat_eol();
                            }
                            temp
                        },
                        propagating_error: false,
                    },
                    start,
                    end: self.prev_token.end,
                })
            } else if self.eat_noexpect(&TokenKind::OpenBracket) {
                member_item_expr!(false)
            } else if self.eat_noexpect(&TokenKind::Dot) {
                member_attr_expr!(MemberKind::Dot, false)
            } else if self.eat_noexpect(&TokenKind::DoubleColon) {
                member_attr_expr!(MemberKind::DoubleColon, false)
            } else if self.eat_noexpect(&TokenKind::Question) {
                if self.eat(&TokenKind::OpenBracket) {
                    member_item_expr!(true)
                } else if self.eat(&TokenKind::Dot) {
                    member_attr_expr!(MemberKind::Dot, true)
                } else if self.eat(&TokenKind::DoubleColon) {
                    member_attr_expr!(MemberKind::DoubleColon, true)
                } else {
                    unexpected_token_error!(self);
                }
            } else {
                break;
            };
        }
        Ok(ast_node)
    }

    /// Parse atom expression.
    fn parse_expr_atom(&mut self) -> Result<Box<Expr>> {
        macro_rules! lit_expr {
            ($lit_kind:expr) => {{
                Ok(Box::new(Expr::from(Lit {
                    value: $lit_kind,
                    start: self.token.start,
                    end: self.token.end,
                })))
            }};
        }
        if self.eat(&TokenKind::OpenParen) {
            let temp = self.parse_expr(1)?;
            self.expect(&TokenKind::CloseParen)?;
            Ok(temp)
        } else if self.eat(&TokenKind::Null) {
            lit_expr!(LitKind::Null)
        } else if self.eat(&TokenKind::True) {
            lit_expr!(LitKind::Bool(true))
        } else if self.eat(&TokenKind::False) {
            lit_expr!(LitKind::Bool(false))
        } else if let Some(v) = self.eat_literal() {
            lit_expr!(match v {
                LiteralKind::Int(v) => LitKind::Int(v?),
                LiteralKind::Float(v) => LitKind::Float(v?),
                LiteralKind::Str(v) => LitKind::Str(v?),
            })
        } else if self.eat(&TokenKind::OpenBrace) {
            self.parse_expr_table()
        } else if self.eat(&TokenKind::OpenBracket) {
            self.parse_expr_list()
        } else if self.eat(&TokenKind::Fn) {
            self.parse_expr_func(false)
        } else if self.eat(&TokenKind::VBar) {
            self.parse_expr_func(true)
        } else if self.eat(&TokenKind::Do) {
            Ok(Box::new(Expr {
                start: self.prev_token.start,
                kind: ExprKind::Function {
                    kind: FunctionKind::Do,
                    params: Vec::new(),
                    variadic: None,
                    body: self.parse_block()?,
                },
                end: self.prev_token.end,
            }))
        } else if self.eat(&TokenKind::Try) {
            let mut temp = self.parse_expr_primary()?;
            if let ExprKind::Call {
                propagating_error, ..
            } = &mut temp.kind
            {
                *propagating_error = true;
            } else {
                return Err(SyntaxError::ParseTryExprError.into());
            }
            Ok(temp)
        } else {
            Ok(Box::new(self.parse_ident()?.into()))
        }
    }

    /// Parse table expression.
    fn parse_expr_table(&mut self) -> Result<Box<Expr>> {
        Ok(Box::new(Expr {
            start: self.prev_token.start,
            kind: ExprKind::Table {
                properties: {
                    let mut temp = Vec::new();
                    self.eat_eol();
                    while !self.eat_noexpect(&TokenKind::CloseBrace) {
                        let start = self.token.start;
                        let key = self.parse_expr(1)?;
                        self.expect(&TokenKind::Colon)?;
                        let value = self.parse_expr(1)?;
                        temp.push(TableProperty {
                            key,
                            value,
                            start,
                            end: self.prev_token.end,
                        });
                        self.eat_eol();
                        if self.eat(&TokenKind::CloseBrace) {
                            break;
                        }
                        self.expect(&TokenKind::Comma)?;
                        self.eat_eol();
                    }
                    temp
                },
            },
            end: self.prev_token.end,
        }))
    }

    /// Parse table expression.
    fn parse_expr_list(&mut self) -> Result<Box<Expr>> {
        Ok(Box::new(Expr {
            start: self.prev_token.start,
            kind: ExprKind::List {
                items: {
                    let mut temp = Vec::new();
                    self.eat_eol();
                    while !self.eat_noexpect(&TokenKind::CloseBracket) {
                        temp.push(*self.parse_expr(1)?);
                        self.eat_eol();
                        if self.eat(&TokenKind::CloseBracket) {
                            break;
                        }
                        self.expect(&TokenKind::Comma)?;
                        self.eat_eol();
                    }
                    temp
                },
            },
            end: self.prev_token.end,
        }))
    }

    /// Parse function expression.
    fn parse_expr_func(&mut self, is_closure: bool) -> Result<Box<Expr>> {
        let start = self.prev_token.start;
        let end_token = if is_closure {
            TokenKind::VBar
        } else {
            self.expect(&TokenKind::OpenParen)?;
            TokenKind::CloseParen
        };
        let mut variadic = None;
        Ok(Box::new(Expr {
            start,
            kind: ExprKind::Function {
                kind: if is_closure {
                    FunctionKind::Closure
                } else {
                    FunctionKind::Funciton
                },
                params: {
                    let mut temp = Vec::new();
                    while !self.eat_noexpect(&end_token) {
                        if self.eat_noexpect(&TokenKind::Mul) {
                            variadic = Some(Box::new(self.parse_ident()?));
                            self.expect(&end_token)?;
                            break;
                        }
                        temp.push(self.parse_ident()?);
                        self.eat_eol();
                        if self.eat(&end_token) {
                            break;
                        }
                        self.expect(&TokenKind::Comma)?;
                        self.eat_eol();
                    }
                    temp
                },
                variadic,
                body: self.parse_block()?,
            },
            end: self.prev_token.end,
        }))
    }

    /// Parse ident.
    fn parse_ident(&mut self) -> Result<Ident> {
        if let Some(v) = self.eat_ident() {
            Ok(Ident {
                start: self.token.start,
                name: v,
                end: self.token.end,
            })
        } else {
            unexpected_token_error!(self)
        }
    }
}
