//! The parser.

use smol_str::SmolStr;
use thiserror::Error;

use crate::utils::{Float, Join, Locatable};

use super::{
    ast::*,
    lexer::LexerError,
    token::{LiteralKind, Token, TokenKind, TokenType},
    typing::{LiteralType, Type},
};

/// Parse the token iter into AST.
pub fn parse<T: Iterator<Item = Token>>(token_iter: &mut T) -> Result<AST, Vec<ParserError>> {
    Parser::new(token_iter).parse()
}

macro_rules! unexpected_token_error {
    ($self:expr) => {
        return Err(ParserError::UnexpectedToken {
            token: Box::new($self.token.clone()),
            expected: $self.expected_tokens.clone(),
        })
    };
}

/// The parser. Turns token iter into AST.
struct Parser<'a, T: Iterator<Item = Token>> {
    /// The current token.
    token: Token,
    /// The previous token.
    prev_token: Token,
    /// The token iter.
    token_iter: &'a mut T,

    expected_tokens: Vec<TokenType>,
    errors: Vec<ParserError>,
}

impl<'a, T: Iterator<Item = Token>> Parser<'a, T> {
    /// Constructs a new `Parser` with a token iter.
    fn new(token_iter: &'a mut T) -> Self {
        let mut parser = Parser {
            token: Token::dummy(),
            prev_token: Token::dummy(),
            token_iter,
            expected_tokens: Vec::new(),
            errors: Vec::new(),
        };
        parser.bump_no_skip_comment();
        parser
    }

    /// Moves to the next token, not skip comment token.
    fn bump_no_skip_comment(&mut self) {
        self.token = self.token_iter.next().unwrap();
    }

    /// Moves to the next token.
    fn bump(&mut self) {
        self.prev_token = self.token.clone();
        self.bump_no_skip_comment();
        while let TokenKind::LineComment(_) | TokenKind::BlockComment(_) = self.token.kind {
            self.bump_no_skip_comment();
            if self.token.kind == TokenKind::EOL {
                break;
            }
        }
        self.expected_tokens.clear();
    }

    /// Eats EOL.
    fn eat_eol(&mut self) {
        while self.token.kind == TokenKind::EOL {
            self.bump();
        }
    }

    /// Expects and consumes the token `t`. Signals an error if the next token is not `t`.
    fn expect(&mut self, t: &TokenKind) -> Result<(), ParserError> {
        if self.token.kind == *t {
            self.bump();
            Ok(())
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

    fn eat_ident(&mut self) -> Option<SmolStr> {
        if let TokenKind::Ident(v) = self.token.kind.clone() {
            self.bump();
            Some(v)
        } else {
            self.expected_tokens.push(TokenType::Literal);
            None
        }
    }

    fn eat_literal(&mut self) -> Option<Result<LiteralKind, ParserError>> {
        let token = self.token.clone();
        if let TokenKind::Literal(v) = token.kind.clone() {
            self.bump();
            Some(v.map_err(|e| ParserError::LexerError {
                token: Box::new(token),
                e,
            }))
        } else {
            self.expected_tokens.push(TokenType::Literal);
            None
        }
    }

    /// Parse token iter into AST.
    fn parse(mut self) -> Result<AST, Vec<ParserError>> {
        let ast = AST {
            first_comment: {
                let mut first_comment = String::new();
                loop {
                    if let TokenKind::LineComment(v) | TokenKind::BlockComment(v) = &self.token.kind
                    {
                        first_comment.push_str(v);
                        first_comment.push('\n');
                        self.bump_no_skip_comment();
                    } else if self.token.kind == TokenKind::EOL {
                        self.bump_no_skip_comment();
                    } else {
                        break;
                    }
                }
                first_comment.into()
            },
            body: Box::new(Block {
                start: self.token.start,
                body: self.parse_stmts_handle_error(&TokenKind::EOF),
                end: self.prev_token.end,
            }),
        };
        if self.errors.is_empty() {
            Ok(ast)
        } else {
            Err(self.errors)
        }
    }

    /// Parse statements with error handle.
    fn parse_stmts_handle_error(&mut self, end_token: &TokenKind) -> Vec<Stmt> {
        let mut temp = Vec::new();
        self.eat_eol();
        while !self.eat_noexpect(end_token) {
            match self.parse_stmt() {
                Ok(stmt) => temp.push(stmt),
                Err(e) => {
                    self.errors.push(e);
                    while !matches!(self.token.kind, TokenKind::EOL | TokenKind::EOF)
                        && &self.token.kind != end_token
                    {
                        self.bump();
                    }
                }
            }
            if self.eat(end_token) {
                break;
            }
            if let Err(e) = self.expect(&TokenKind::EOL) {
                self.errors.push(e);
                if self.check_noexpect(&TokenKind::EOF) {
                    break;
                }
            }
            self.eat_eol();
        }
        temp
    }

    /// Parse block.
    fn parse_block(&mut self) -> Result<Box<Block>, ParserError> {
        self.expect(&TokenKind::OpenBrace)?;
        Ok(Box::new(Block {
            start: self.prev_token.start,
            body: self.parse_stmts_handle_error(&TokenKind::CloseBrace),
            end: self.prev_token.end,
        }))
    }

    /// Parse statement.
    fn parse_stmt(&mut self) -> Result<Stmt, ParserError> {
        macro_rules! stmt {
            ($kind:expr) => {
                Stmt {
                    start: self.prev_token.start,
                    kind: $kind,
                    end: self.prev_token.end,
                }
            };
        }
        macro_rules! assign_op_stmt {
            ($ast_node:expr, $bin_op:expr) => {{
                let right = self.parse_expr(1)?;
                Stmt {
                    start: $ast_node.start,
                    end: right.end,
                    kind: StmtKind::AssignOp {
                        operator: $bin_op,
                        left: expr_to_assign_left(*$ast_node)?,
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
                        temp.push(self.parse_typed_ident()?);
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
                left: self.parse_ident()?.into(),
                right: self.parse_expr_func(false)?
            })
        } else {
            let ast_node = self.parse_expr(1)?;
            if self.check(&TokenKind::Comma) {
                let start = ast_node.start;
                let mut left = vec![expr_to_assign_left(*ast_node)?];
                loop {
                    if self.eat(&TokenKind::Comma) {
                        let ast_node = self.parse_expr(1)?;
                        left.push(expr_to_assign_left(*ast_node)?);
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
                    let left_len = left.len();
                    let right_len = right.len();
                    let stmt = Stmt {
                        start,
                        end: self.prev_token.end,
                        kind: StmtKind::AssignMulti { left, right },
                    };
                    if left_len == right_len {
                        stmt
                    } else {
                        return Err(ParserError::ParseAssignMultiStmtError(Box::new(stmt)));
                    }
                } else {
                    Stmt {
                        start,
                        end: right.end,
                        kind: StmtKind::AssignUnpack { left, right },
                    }
                }
            } else if self.check(&TokenKind::Colon) {
                if let ExprKind::Ident(ident) = ast_node.kind {
                    self.bump();
                    let types = self.parse_type()?;
                    self.expect(&TokenKind::Assign)?;
                    let right = self.parse_expr(1)?;
                    Stmt {
                        start: ast_node.start,
                        end: right.end,
                        kind: StmtKind::Assign {
                            left: AssignLeft::Ident(Box::new(TypedIdent {
                                ident: *ident,
                                t: Some(types),
                            })),
                            right,
                        },
                    }
                } else {
                    unexpected_token_error!(self);
                }
            } else if self.eat(&TokenKind::Assign) {
                let right = self.parse_expr(1)?;
                Stmt {
                    start: ast_node.start,
                    end: right.end,
                    kind: StmtKind::Assign {
                        left: expr_to_assign_left(*ast_node)?,
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
            } else if self.eat(&TokenKind::RemAssign) {
                assign_op_stmt!(ast_node, BinOp::Rem)
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

    /// Parse if statement.
    fn parse_stmt_if(&mut self) -> Result<Stmt, ParserError> {
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
    fn parse_expr(&mut self, min_precedence: u32) -> Result<Box<Expr>, ParserError> {
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
                TokenKind::Rem => BinOp::Rem,
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
    fn parse_expr_unary(&mut self) -> Result<Box<Expr>, ParserError> {
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
    fn parse_expr_primary(&mut self) -> Result<Box<Expr>, ParserError> {
        let start = self.token.start;
        let mut ast_node = self.parse_expr_atom()?;
        macro_rules! member_attr_expr {
            ($member_expr_kind:path, $safe:expr) => {
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
                            property: $member_expr_kind(Box::new(self.parse_ident()?.into())),
                            safe: $safe,
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
                            property: MemberKind::Bracket(self.parse_expr(1)?),
                            safe: $safe,
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
                        kind: CallKind::None,
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
    fn parse_expr_atom(&mut self) -> Result<Box<Expr>, ParserError> {
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
            lit_expr!(match v? {
                LiteralKind::Int(v) => LitKind::Int(v),
                LiteralKind::Float(v) => LitKind::Float(Float(v)),
                LiteralKind::Str(v) => LitKind::Str(v),
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
                    returns: None,
                    throws: None,
                },
                end: self.prev_token.end,
            }))
        } else if self.eat(&TokenKind::Try) {
            let call_kind = if self.eat(&TokenKind::Question) {
                CallKind::TryOption
            } else if self.eat(&TokenKind::Exclamation) {
                CallKind::TryPanic
            } else {
                CallKind::Try
            };
            let mut temp = self.parse_expr_primary()?;
            if let ExprKind::Call { kind, .. } = &mut temp.kind {
                *kind = call_kind;
            } else {
                return Err(ParserError::ParseTryExprError(temp));
            }
            Ok(temp)
        } else {
            Ok(Box::new(self.parse_ident()?.into()))
        }
    }

    /// Parse table expression.
    fn parse_expr_table(&mut self) -> Result<Box<Expr>, ParserError> {
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
    fn parse_expr_list(&mut self) -> Result<Box<Expr>, ParserError> {
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
    fn parse_expr_func(&mut self, is_closure: bool) -> Result<Box<Expr>, ParserError> {
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
                    FunctionKind::Function
                },
                params: {
                    let mut temp = Vec::new();
                    while !self.eat_noexpect(&end_token) {
                        if self.eat_noexpect(&TokenKind::Mul) {
                            variadic = Some(Box::new(if is_closure {
                                self.parse_atom_typed_ident()?
                            } else {
                                self.parse_typed_ident()?
                            }));
                            self.expect(&end_token)?;
                            break;
                        }
                        temp.push(if is_closure {
                            self.parse_atom_typed_ident()?
                        } else {
                            self.parse_typed_ident()?
                        });
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
                returns: if self.eat(&TokenKind::Arrow) {
                    Some(Box::new(self.parse_type()?))
                } else {
                    None
                },
                throws: if self.eat(&TokenKind::Throw) {
                    Some(Box::new(self.parse_type()?))
                } else {
                    None
                },
                body: self.parse_block()?,
            },
            end: self.prev_token.end,
        }))
    }

    /// Parse ident.
    fn parse_ident(&mut self) -> Result<Ident, ParserError> {
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

    // Parse typed ident.
    fn parse_typed_ident(&mut self) -> Result<TypedIdent, ParserError> {
        let ident = self.parse_ident()?;
        let mut t = None;
        if self.eat_noexpect(&TokenKind::Colon) {
            t = Some(self.parse_type()?);
        }
        Ok(TypedIdent { ident, t })
    }

    // Parse typed ident with atom type.
    fn parse_atom_typed_ident(&mut self) -> Result<TypedIdent, ParserError> {
        let ident = self.parse_ident()?;
        let mut t = None;
        if self.eat_noexpect(&TokenKind::Colon) {
            t = Some(self.parse_type_atom()?);
        }
        Ok(TypedIdent { ident, t })
    }

    /// Parse type.
    fn parse_type(&mut self) -> Result<Type, ParserError> {
        let mut t = self.parse_type_atom()?;
        while self.eat(&TokenKind::VBar) {
            t = t.union(&self.parse_type_atom()?);
        }
        Ok(t)
    }

    /// Parse atom type.
    fn parse_type_atom(&mut self) -> Result<Type, ParserError> {
        let t = if self.eat(&TokenKind::Null) {
            Type::Null
        } else if self.eat(&TokenKind::True) {
            Type::Literal(LiteralType::Bool(true))
        } else if self.eat(&TokenKind::False) {
            Type::Literal(LiteralType::Bool(false))
        } else if let Some(v) = self.eat_literal() {
            Type::Literal(match v? {
                LiteralKind::Int(v) => LiteralType::Int(v),
                LiteralKind::Float(v) => LiteralType::Float(Float(v)),
                LiteralKind::Str(v) => LiteralType::Str(v),
            })
        } else if let Some(v) = self.eat_ident() {
            match v.as_str() {
                "never" => Type::Never,
                "any" => Type::Any,
                "bool" => Type::Bool,
                "int" => Type::Int,
                "float" => Type::Float,
                "str" => Type::Str,
                _ => Type::UserData(v),
            }
        } else if self.eat(&TokenKind::OpenBrace) {
            self.parse_type_table()?
        } else if self.eat(&TokenKind::Fn) {
            self.parse_type_func()?
        } else if self.eat(&TokenKind::OpenParen) {
            let t = self.parse_type()?;
            self.expect(&TokenKind::CloseParen)?;
            t
        } else {
            unexpected_token_error!(self)
        };
        if self.eat(&TokenKind::Question) {
            Ok(t.union(&Type::Null))
        } else {
            Ok(t)
        }
    }

    /// Parse table type.
    fn parse_type_table(&mut self) -> Result<Type, ParserError> {
        let mut pairs = Vec::new();
        let mut others = None;
        self.eat_eol();
        while !self.eat_noexpect(&TokenKind::CloseBrace) {
            if self.eat(&TokenKind::OpenBracket) {
                if others.is_some() {
                    unexpected_token_error!(self)
                }
                let key = self.parse_type()?;
                self.expect(&TokenKind::CloseBracket)?;
                self.expect(&TokenKind::Colon)?;
                let value = self.parse_type()?;
                others = Some((Box::new(key), Box::new(value)))
            } else {
                let key = self.parse_ident()?;
                self.expect(&TokenKind::Colon)?;
                let value = self.parse_type()?;
                pairs.push((LiteralType::Str(key.name), value));
            }
            self.eat_eol();
            if self.eat(&TokenKind::CloseBrace) {
                break;
            }
            self.expect(&TokenKind::Comma)?;
            self.eat_eol();
        }
        Ok(Type::Table { pairs, others })
    }

    /// Parse function type.
    fn parse_type_func(&mut self) -> Result<Type, ParserError> {
        self.expect(&TokenKind::OpenParen)?;
        let mut variadic = Type::Null;
        Ok(Type::Function {
            params: {
                let mut temp = Vec::new();
                while !self.eat_noexpect(&TokenKind::CloseParen) {
                    if self.eat_noexpect(&TokenKind::Mul) {
                        variadic = self.parse_type()?;
                        self.expect(&TokenKind::CloseParen)?;
                        break;
                    }
                    temp.push(self.parse_type()?);
                    self.eat_eol();
                    if self.eat(&TokenKind::CloseParen) {
                        break;
                    }
                    self.expect(&TokenKind::Comma)?;
                    self.eat_eol();
                }
                temp
            },
            variadic: Box::new(variadic),
            returns: if self.eat(&TokenKind::Arrow) {
                Box::new(self.parse_type()?)
            } else {
                Box::new(Type::Null)
            },
            throws: if self.eat(&TokenKind::Throw) {
                Box::new(self.parse_type()?)
            } else {
                Box::new(Type::Any)
            },
        })
    }
}

fn expr_to_assign_left(expr: Expr) -> Result<AssignLeft, ParserError> {
    match expr.kind.clone() {
        ExprKind::Ident(ident) => Ok(AssignLeft::Ident(Box::new(TypedIdent {
            ident: *ident,
            t: None,
        }))),
        ExprKind::Member {
            table,
            property,
            safe,
        } => {
            if safe {
                Err(ParserError::ParseAssignStmtError(Box::new(expr)))
            } else {
                Ok(AssignLeft::Member { table, property })
            }
        }
        ExprKind::MetaMember { table, safe } => {
            if safe {
                Err(ParserError::ParseAssignStmtError(Box::new(expr)))
            } else {
                Ok(AssignLeft::MetaMember { table })
            }
        }
        _ => Err(ParserError::ParseAssignStmtError(Box::new(expr))),
    }
}

/// Kind of ParserError.
#[derive(Error, Debug, Clone, PartialEq)]
pub enum ParserError {
    #[error("unexpected token (expected {}, found {token})", .expected.iter().join(", "))]
    UnexpectedToken {
        token: Box<Token>,
        expected: Vec<TokenType>,
    },
    #[error("parse assign statement error")]
    ParseAssignStmtError(Box<Expr>),
    #[error("parse assign multi statement error: {}", .0)]
    ParseAssignMultiStmtError(Box<Stmt>),
    #[error("parse try expression error")]
    ParseTryExprError(Box<Expr>),
    #[error("parse try expression error")]
    LexerError { token: Box<Token>, e: LexerError },
}

impl Locatable for ParserError {
    fn start(&self) -> crate::utils::Location {
        match self {
            ParserError::UnexpectedToken { token, expected: _ } => token.start,
            ParserError::ParseAssignStmtError(expr) => expr.start,
            ParserError::ParseAssignMultiStmtError(stmt) => stmt.start,
            ParserError::ParseTryExprError(expr) => expr.start,
            ParserError::LexerError { token, e: _ } => token.start,
        }
    }

    fn end(&self) -> crate::utils::Location {
        match self {
            ParserError::UnexpectedToken { token, expected: _ } => token.end,
            ParserError::ParseAssignStmtError(expr) => expr.end,
            ParserError::ParseAssignMultiStmtError(stmt) => stmt.end,
            ParserError::ParseTryExprError(expr) => expr.end,
            ParserError::LexerError { token, e: _ } => token.end,
        }
    }
}
