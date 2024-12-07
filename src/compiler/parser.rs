//! The parser.

use std::{iter::Peekable, sync::OnceLock};

use text_size::{TextRange, TextSize};

use crate::utils::{unescape_str, Float, Join};

use super::{
    ast::*,
    error::CompilerError,
    interning::StringInterner,
    lexer::tokenize,
    token::{Token, TokenKind},
    value::ValueType,
};

type ParseParamsResult<S> = Result<(Vec<TypedIdent<S>>, Option<Box<TypedIdent<S>>>), CompilerError>;

/// Parse the token iter into AST.
pub fn parse<S: StringInterner>(
    interner: S,
    input: &str,
) -> (Program<S::String>, Vec<CompilerError>) {
    Parser::new(interner, input, tokenize(input)).parse()
}

struct Parser<'input, S: StringInterner, I: Iterator<Item = Token>> {
    interner: S,
    input: &'input str,
    token_iter: Peekable<I>,
    prev_token_end: TextSize,
    expected_kinds: Vec<TokenKind>,
    errors: Vec<CompilerError>,
}

impl<'input, S: StringInterner, I: Iterator<Item = Token>> Parser<'input, S, I> {
    /// Constructs a new `Parser` with a token iter.
    fn new(interner: S, input: &'input str, token_iter: I) -> Self {
        Self {
            interner,
            input,
            token_iter: token_iter.peekable(),
            prev_token_end: TextSize::default(),
            expected_kinds: Vec::new(),
            errors: Vec::new(),
        }
    }

    /// Returns the current token.
    fn current_token(&mut self) -> Option<Token> {
        self.token_iter.peek().copied()
    }

    /// Returns the kind of the current token.
    fn current_kind(&mut self) -> Option<TokenKind> {
        self.current_token().map(|token| token.kind)
    }

    /// Returns the range of the current token.
    fn current_range(&mut self) -> Option<TextRange> {
        self.current_token().map(|token| token.range)
    }

    /// Moves to the next token.
    fn bump(&mut self) {
        self.expected_kinds.clear();
        if let Some(range) = self.current_range() {
            self.prev_token_end = range.end();
        }
        self.token_iter.next();
        self.eat_trivia();
    }

    /// Skips all trivia tokens.
    fn eat_trivia(&mut self) {
        while self
            .current_token()
            .is_some_and(|token| token.kind.is_trivia())
        {
            self.token_iter.next();
        }
    }

    /// Checks if the current token is `t`, and returns `true` if so.
    /// This method will automatically add `t` to `expected_kinds` if `t` is not encountered.
    fn check(&mut self, t: TokenKind) -> bool {
        let is_present = self.current_kind().unwrap_or(TokenKind::Eof) == t;
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

    /// Expects and consumes the token `t`. Signals an error if the next token is not `t`.
    fn expect(&mut self, t: TokenKind) -> Result<(), CompilerError> {
        let is_present = self.eat(t);
        if !is_present {
            return Err(self.unexpected());
        }
        Ok(())
    }

    /// Eats all Eol tokens.
    fn eat_eol(&mut self) {
        self.eat_trivia();
        while self.current_kind().is_some_and(|t| t == TokenKind::Eol) {
            self.bump();
        }
    }

    /// Consumes a token 't' if it exists. Returns the range of the given token.
    fn eat_range(&mut self, t: TokenKind) -> Option<TextRange> {
        let is_present = self.check(t);
        if is_present {
            let range = self.current_range().unwrap();
            self.bump();
            Some(range)
        } else {
            None
        }
    }

    /// Returns an error for an unexpected token.
    fn unexpected(&mut self) -> CompilerError {
        let (found, range) = self
            .current_token()
            .map(|token| (token.kind, token.range))
            .unwrap_or((TokenKind::Eof, TextRange::empty(self.prev_token_end)));

        CompilerError::UnexpectedToken {
            expected: self.expected_kinds.clone(),
            found,
            range,
        }
    }

    /// Start a new range.
    fn start_range(&mut self) -> TextSize {
        self.current_range()
            .map(TextRange::start)
            .unwrap_or_default()
    }

    /// End a new range.
    fn end_range(&mut self, start: TextSize) -> TextRange {
        TextRange::new(start, self.prev_token_end)
    }

    /// Parse token iter into AST.
    fn parse(mut self) -> (Program<S::String>, Vec<CompilerError>) {
        let start = self.start_range();
        let body = self.parse_stmts(TokenKind::Eof);
        let range = self.end_range(start);
        let body = Box::new(Block {
            body,
            range,
            scope_id: OnceLock::new(),
        });
        let function = Box::new(Function {
            name: None,
            kind: FunctionKind::Function,
            params: Vec::new(),
            variadic: None,
            returns: None,
            throws: None,
            body,
            function_id: OnceLock::new(),
        });
        (Program { function }, self.errors)
    }

    /// Parses one or more items separated by `sep`.
    fn parse_items<T, F: Fn(&mut Self) -> Result<T, CompilerError>>(
        &mut self,
        parse_func: F,
    ) -> Result<Vec<T>, CompilerError> {
        let mut items = Vec::new();
        loop {
            items.push(parse_func(self)?);
            if !self.eat(TokenKind::Comma) {
                break;
            }
        }
        Ok(items)
    }

    /// Parses zero or more items separated by `sep` between `start` and `end`.
    /// Allowing trailing `seq` and eol before `sep`. The `end` will be consumed.
    fn parse_items_between<T, F: Fn(&mut Self) -> Result<T, CompilerError>>(
        &mut self,
        start: TokenKind,
        parse_func: F,
        end: TokenKind,
    ) -> Result<Vec<T>, CompilerError> {
        self.expect(start)?;
        let mut items = Vec::new();
        self.eat_eol();
        while !self.eat(end) {
            items.push(parse_func(self)?);
            self.eat_eol();
            if self.eat(end) {
                break;
            }
            self.expect(TokenKind::Comma)?;
            self.eat_eol();
        }
        Ok(items)
    }

    /// Parses zero or more items separated by `sep` between `start` and `end`.
    /// Allowing trailing `seq` and eol before `sep`. The `end` will be consumed.
    /// The last element is specified by `parse_last_func`.
    fn parse_items_between_with_last<Item, Last, ItemFunc, LastFunc>(
        &mut self,
        start: TokenKind,
        parse_item_func: ItemFunc,
        last: TokenKind,
        parse_last_func: LastFunc,
        end: TokenKind,
    ) -> Result<(Vec<Item>, Option<Last>), CompilerError>
    where
        ItemFunc: Fn(&mut Self) -> Result<Item, CompilerError>,
        LastFunc: Fn(&mut Self) -> Result<Last, CompilerError>,
    {
        self.expect(start)?;
        let mut items = Vec::new();
        self.eat_eol();
        while !self.eat(end) {
            if self.check(last) {
                let last_item = parse_last_func(self)?;
                self.eat_eol();
                self.expect(end)?;
                return Ok((items, Some(last_item)));
            }
            items.push(parse_item_func(self)?);
            self.eat_eol();
            if self.eat(end) {
                break;
            }
            self.expect(TokenKind::Comma)?;
            self.eat_eol();
        }
        Ok((items, None))
    }

    fn parse_stmts(&mut self, end_token: TokenKind) -> Vec<Stmt<S::String>> {
        let mut stmts = Vec::new();
        self.eat_eol();
        while !self.eat(end_token) {
            match self.parse_stmt() {
                Ok(stmt) => stmts.push(stmt),
                Err(e) => {
                    self.errors.push(e);
                    while !(self.check(TokenKind::Eol)
                        || self.check(TokenKind::Eof)
                        || self.check(end_token))
                    {
                        self.bump();
                    }
                }
            }
            if self.eat(end_token) {
                break;
            } else if self.check(TokenKind::Eof) {
                let e = self.unexpected();
                self.errors.push(e);
                break;
            } else if let Err(e) = self.expect(TokenKind::Eol) {
                self.errors.push(e);
            }
            self.eat_eol();
        }
        stmts
    }

    fn parse_block(&mut self) -> Result<Box<Block<S::String>>, CompilerError> {
        self.expect(TokenKind::OpenBrace)?;
        let start = self.start_range();
        let body = self.parse_stmts(TokenKind::CloseBrace);
        let range = self.end_range(start);
        Ok(Box::new(Block {
            body,
            range,
            scope_id: OnceLock::new(),
        }))
    }

    fn parse_stmt(&mut self) -> Result<Stmt<S::String>, CompilerError> {
        self.eat_eol();
        let start = self.start_range();
        let kind = if self.eat(TokenKind::If) {
            let test = Box::new(self.parse_expr()?);
            let consequent = self.parse_block()?;
            let alternate = if self.eat(TokenKind::Else) {
                let expr = if self.check(TokenKind::If) {
                    self.parse_stmt()?
                } else {
                    self.parse_block()?.into()
                };
                Some(Box::new(expr))
            } else {
                None
            };
            StmtKind::If {
                test,
                consequent,
                alternate,
            }
        } else if self.eat(TokenKind::Match) {
            let expr = Box::new(self.parse_expr()?);
            self.expect(TokenKind::OpenBrace)?;
            let mut cases = Vec::new();
            while !self.eat(TokenKind::CloseBrace) {
                self.eat_eol();
                cases.push(self.parse_match_case()?);
                self.eat_eol();
            }
            StmtKind::Match { expr, cases }
        } else if self.eat(TokenKind::Loop) {
            let body = self.parse_block()?;
            StmtKind::Loop { body }
        } else if self.eat(TokenKind::While) {
            let test = Box::new(self.parse_expr()?);
            let body = self.parse_block()?;
            StmtKind::While { test, body }
        } else if self.eat(TokenKind::For) {
            let left = self.parse_items(Parser::parse_ident)?;
            self.expect(TokenKind::In)?;
            let right = Box::new(self.parse_expr()?);
            let body = self.parse_block()?;
            StmtKind::For { left, right, body }
        } else if self.eat(TokenKind::Break) {
            StmtKind::Break
        } else if self.eat(TokenKind::Continue) {
            StmtKind::Continue
        } else if self.eat(TokenKind::Return) {
            let argument = Box::new(self.parse_expr()?);
            StmtKind::Return { argument }
        } else if self.eat(TokenKind::Throw) {
            let argument = Box::new(self.parse_expr()?);
            StmtKind::Throw { argument }
        } else if self.eat(TokenKind::Import) {
            let mut path = Vec::new();
            let kind = loop {
                path.push(self.parse_ident()?);
                if !self.eat(TokenKind::DoubleColon) {
                    break if self.check(TokenKind::Eol) {
                        ImportKind::Simple(None)
                    } else if self.eat(TokenKind::As) {
                        ImportKind::Simple(Some(Box::new(self.parse_ident()?)))
                    } else {
                        return Err(self.unexpected());
                    };
                }
                if !self.check(TokenKind::Ident) {
                    break if self.check(TokenKind::OpenBrace) {
                        let items = self.parse_items_between(
                            TokenKind::OpenBrace,
                            |p| {
                                let start = p.start_range();
                                let name = p.parse_ident()?;
                                let alias = if p.eat(TokenKind::As) {
                                    Some(p.parse_ident()?)
                                } else {
                                    None
                                };
                                let range = p.end_range(start);
                                Ok(ImportItem { name, alias, range })
                            },
                            TokenKind::CloseBrace,
                        )?;
                        ImportKind::Nested(items)
                    } else if self.eat(TokenKind::Mul) {
                        ImportKind::Glob
                    } else {
                        return Err(self.unexpected());
                    };
                }
            };
            let path_str = self
                .interner
                .intern(&path.iter().map(|ident| ident.name.as_ref()).join("::"));
            StmtKind::Import {
                path,
                path_str,
                kind,
            }
        } else if self.eat(TokenKind::Fn) {
            let name = Box::new(self.parse_ident()?);
            let function = Box::new(self.parse_function(Some(name.name.clone()))?);
            StmtKind::Fn {
                glo: None,
                name,
                function,
            }
        } else if let Some(glo_range) = self.eat_range(TokenKind::Glo) {
            if self.eat(TokenKind::Fn) {
                let name = Box::new(self.parse_ident()?);
                let function = Box::new(self.parse_function(Some(name.name.clone()))?);
                StmtKind::Fn {
                    glo: Some(glo_range),
                    name,
                    function,
                }
            } else {
                let left = Box::new(self.parse_typed_ident()?);
                self.expect(TokenKind::Assign)?;
                let right = Box::new(self.parse_expr()?);
                StmtKind::GloAssign { left, right }
            }
        } else if self.check(TokenKind::OpenBrace) {
            StmtKind::Block(self.parse_block()?)
        } else {
            let ast_node = self.parse_expr()?;
            if self.check(TokenKind::Comma) {
                let mut left = Vec::new();
                left.push(
                    self.expr_to_assign_left(ast_node)
                        .ok_or_else(|| self.unexpected())?,
                );
                while let Some(comma_range) = self.eat_range(TokenKind::Comma) {
                    let ast_node = self.parse_expr()?;
                    left.push(self.expr_to_assign_left(ast_node).ok_or_else(|| {
                        CompilerError::UnexpectedToken {
                            expected: Vec::new(),
                            found: TokenKind::Comma,
                            range: comma_range,
                        }
                    })?);
                }
                self.expect(TokenKind::Assign)?;
                let right_expr = self.parse_expr()?;
                if self.check(TokenKind::Comma) {
                    let mut right = Vec::new();
                    right.push(right_expr);
                    for _ in 0..(left.len() - 1) {
                        self.expect(TokenKind::Comma)?;
                        right.push(self.parse_expr()?);
                    }
                    StmtKind::AssignMulti { left, right }
                } else {
                    let right = Box::new(right_expr);
                    StmtKind::AssignUnpack { left, right }
                }
            } else if self.check(TokenKind::Colon) {
                let ExprKind::Ident(ident) = ast_node.kind else {
                    return Err(self.unexpected());
                };
                self.bump();
                let ty = Some(self.parse_type()?);
                let range = self.end_range(start);
                let left = AssignLeft {
                    kind: AssignLeftKind::Ident(Box::new(TypedIdent { ident, ty, range })),
                    range,
                };
                self.expect(TokenKind::Assign)?;
                let right = Box::new(self.parse_expr()?);
                StmtKind::Assign { left, right }
            } else if self.check(TokenKind::Assign) {
                let left = self
                    .expr_to_assign_left(ast_node)
                    .ok_or_else(|| self.unexpected())?;
                self.bump();
                let right = Box::new(self.parse_expr()?);
                StmtKind::Assign { left, right }
            } else if self.check(TokenKind::AddAssign)
                || self.check(TokenKind::SubAssign)
                || self.check(TokenKind::MulAssign)
                || self.check(TokenKind::DivAssign)
                || self.check(TokenKind::RemAssign)
            {
                let left = self
                    .expr_to_assign_left(ast_node)
                    .ok_or_else(|| self.unexpected())?;
                let operator = match self.current_kind().unwrap() {
                    TokenKind::AddAssign => BinOp::Add,
                    TokenKind::SubAssign => BinOp::Sub,
                    TokenKind::MulAssign => BinOp::Mul,
                    TokenKind::DivAssign => BinOp::Div,
                    TokenKind::RemAssign => BinOp::Rem,
                    _ => unreachable!(),
                };
                self.bump();
                let right = Box::new(self.parse_expr()?);
                StmtKind::AssignOp {
                    operator,
                    left,
                    right,
                }
            } else {
                StmtKind::Expr(Box::new(ast_node))
            }
        };
        let range = self.end_range(start);
        Ok(Stmt { kind, range })
    }

    fn expr_to_assign_left(&self, expr: Expr<S::String>) -> Option<AssignLeft<S::String>> {
        let kind = match expr.kind {
            ExprKind::Ident(ident) => AssignLeftKind::Ident(Box::new(ident.into())),
            ExprKind::Member {
                table,
                property,
                safe,
            } if safe.is_none() => AssignLeftKind::Member { table, property },
            ExprKind::MetaMember {
                table,
                property,
                safe,
            } if safe.is_none() => AssignLeftKind::MetaMember { table, property },
            _ => return None,
        };
        Some(AssignLeft {
            kind,
            range: expr.range,
        })
    }

    fn parse_function(
        &mut self,
        name: Option<S::String>,
    ) -> Result<Function<S::String>, CompilerError> {
        let (params, variadic) = self.parse_params()?;
        Ok(Function {
            name,
            kind: FunctionKind::Function,
            params,
            variadic,
            returns: self.parse_returns()?,
            throws: self.parse_throws()?,
            body: self.parse_block()?,
            function_id: OnceLock::new(),
        })
    }

    fn parse_expr(&mut self) -> Result<Expr<S::String>, CompilerError> {
        self.parse_expr_precedence(1)
    }

    fn parse_expr_precedence(
        &mut self,
        min_precedence: u8,
    ) -> Result<Expr<S::String>, CompilerError> {
        let start = self.start_range();
        let mut left = self.parse_expr_unary()?;
        loop {
            let operator = match self.current_kind().unwrap_or(TokenKind::Eof) {
                TokenKind::Add => BinOp::Add,
                TokenKind::Sub => BinOp::Sub,
                TokenKind::Mul => BinOp::Mul,
                TokenKind::Div => BinOp::Div,
                TokenKind::Rem => BinOp::Rem,
                TokenKind::And => BinOp::And,
                TokenKind::Or => BinOp::Or,
                TokenKind::Eq => BinOp::Eq,
                TokenKind::NotEq => BinOp::Ne,
                TokenKind::Lt => BinOp::Lt,
                TokenKind::LtEq => BinOp::Le,
                TokenKind::Gt => BinOp::Gt,
                TokenKind::GtEq => BinOp::Ge,
                TokenKind::Identical => BinOp::Identical,
                TokenKind::NotIdentical => BinOp::NotIdentical,
                TokenKind::Is => BinOp::Is,
                _ => break,
            };
            if operator.precedence() < min_precedence {
                break;
            }
            self.bump();
            left = match operator {
                BinOp::Is => {
                    let token = self.current_token().ok_or_else(|| self.unexpected())?;
                    let right = match (token.kind, &self.input[token.range]) {
                        (TokenKind::Null, _) => ValueType::Null,
                        (TokenKind::Ident, "bool") => ValueType::Bool,
                        (TokenKind::Ident, "int") => ValueType::Int,
                        (TokenKind::Ident, "float") => ValueType::Float,
                        (TokenKind::Ident, "str") => ValueType::Str,
                        (TokenKind::Ident, "table") => ValueType::Table,
                        (TokenKind::Ident, "function") => ValueType::Function,
                        (TokenKind::Ident, "userdata") => ValueType::UserData,
                        _ => return Err(self.unexpected()),
                    };
                    self.bump();
                    let left = Box::new(left);
                    let kind = ExprKind::TypeCheck { left, right };
                    let range = self.end_range(start);
                    Expr { kind, range }
                }
                _ => {
                    let right = self.parse_expr_precedence(operator.precedence() + 1)?;
                    let kind = ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    };
                    let range = self.end_range(start);
                    Expr { kind, range }
                }
            };
        }
        Ok(left)
    }

    fn parse_expr_unary(&mut self) -> Result<Expr<S::String>, CompilerError> {
        let start = self.start_range();
        let kind = if self.eat(TokenKind::Not) {
            let argument = self.parse_expr_primary()?;
            ExprKind::Unary {
                operator: UnOp::Not,
                argument: Box::new(argument),
            }
        } else if self.eat(TokenKind::Sub) {
            let argument = self.parse_expr_primary()?;
            ExprKind::Unary {
                operator: UnOp::Neg,
                argument: Box::new(argument),
            }
        } else {
            return self.parse_expr_primary();
        };
        let range = self.end_range(start);
        Ok(Expr { kind, range })
    }

    fn parse_expr_primary(&mut self) -> Result<Expr<S::String>, CompilerError> {
        let start = self.start_range();
        let mut ast_node = self.parse_expr_atom()?;
        macro_rules! member_expr {
            (Bracket, $safe:expr) => {
                if self.eat(TokenKind::Pound) {
                    self.expect(TokenKind::CloseBracket)?;
                    ExprKind::MetaMember {
                        table: Box::new(ast_node),
                        property: MetaMemberKind::Bracket,
                        safe: $safe,
                    }
                } else {
                    let expr = self.parse_expr()?;
                    self.expect(TokenKind::CloseBracket)?;
                    ExprKind::Member {
                        table: Box::new(ast_node),
                        property: MemberKind::Bracket(Box::new(expr)),
                        safe: $safe,
                    }
                }
            };

            ($kind:ident, $safe:expr) => {
                if self.eat(TokenKind::Pound) {
                    ExprKind::MetaMember {
                        table: Box::new(ast_node),
                        property: MetaMemberKind::$kind,
                        safe: $safe,
                    }
                } else {
                    let ident = self.parse_ident()?.into();
                    ExprKind::Member {
                        table: Box::new(ast_node),
                        property: MemberKind::$kind(Box::new(ident)),
                        safe: $safe,
                    }
                }
            };
        }
        loop {
            let kind = if self.check(TokenKind::OpenParen) {
                let arguments = self.parse_items_between(
                    TokenKind::OpenParen,
                    Parser::parse_expr,
                    TokenKind::CloseParen,
                )?;
                ExprKind::Call {
                    callee: Box::new(ast_node),
                    arguments,
                    kind: CallKind::None,
                }
            } else if self.eat(TokenKind::OpenBracket) {
                member_expr!(Bracket, None)
            } else if self.eat(TokenKind::Dot) {
                member_expr!(Dot, None)
            } else if self.eat(TokenKind::DoubleColon) {
                member_expr!(DoubleColon, None)
            } else if let Some(range) = self.eat_range(TokenKind::Question) {
                if self.eat(TokenKind::OpenBracket) {
                    member_expr!(Bracket, Some(range))
                } else if self.eat(TokenKind::Dot) {
                    member_expr!(Dot, Some(range))
                } else if self.eat(TokenKind::DoubleColon) {
                    member_expr!(DoubleColon, Some(range))
                } else {
                    return Err(self.unexpected());
                }
            } else {
                break;
            };
            let range = self.end_range(start);
            ast_node = Expr { kind, range }
        }
        Ok(ast_node)
    }

    fn parse_expr_atom(&mut self) -> Result<Expr<S::String>, CompilerError> {
        let start = self.start_range();
        let kind = if self.eat(TokenKind::OpenParen) {
            let expr = self.parse_expr()?;
            self.expect(TokenKind::CloseParen)?;
            ExprKind::Paren(Box::new(expr))
        } else if self.check(TokenKind::Ident) {
            ExprKind::Ident(Box::new(self.parse_ident()?))
        } else if self.check(TokenKind::OpenBrace) {
            let properties = self.parse_items_between(
                TokenKind::OpenBrace,
                Parser::parse_table_property,
                TokenKind::CloseBrace,
            )?;
            ExprKind::Table { properties }
        } else if self.check(TokenKind::OpenBracket) {
            let items = self.parse_items_between(
                TokenKind::OpenBracket,
                Parser::parse_expr,
                TokenKind::CloseBracket,
            )?;
            ExprKind::List { items }
        } else if self.eat(TokenKind::Fn) {
            let function = self.parse_function(None)?;
            ExprKind::Function(Box::new(function))
        } else if self.check(TokenKind::VBar) {
            let (params, variadic) = self.parse_closure_params()?;
            let function = Function {
                name: None,
                kind: FunctionKind::Closure,
                params,
                variadic,
                returns: self.parse_returns()?,
                throws: self.parse_throws()?,
                body: self.parse_block()?,
                function_id: OnceLock::new(),
            };
            ExprKind::Function(Box::new(function))
        } else if self.eat(TokenKind::Do) {
            let function = Function {
                name: None,
                kind: FunctionKind::Do,
                params: Vec::new(),
                variadic: None,
                body: self.parse_block()?,
                returns: None,
                throws: None,
                function_id: OnceLock::new(),
            };
            ExprKind::Function(Box::new(function))
        } else if let Some(try_range) = self.eat_range(TokenKind::Try) {
            let call_kind = if self.eat(TokenKind::Question) {
                CallKind::TryOption
            } else if self.eat(TokenKind::Exclamation) {
                CallKind::TryPanic
            } else {
                CallKind::Try
            };
            let mut expr = self.parse_expr_primary()?;
            return if let ExprKind::Call { kind, .. } = &mut expr.kind {
                *kind = call_kind;
                Ok(expr)
            } else {
                Err(CompilerError::UnexpectedToken {
                    expected: Vec::new(),
                    found: TokenKind::Try,
                    range: try_range,
                })
            };
        } else {
            ExprKind::Lit(Box::new(self.parse_lit()?))
        };
        let range = self.end_range(start);
        Ok(Expr { kind, range })
    }

    fn parse_table_property(&mut self) -> Result<TableProperty<S::String>, CompilerError> {
        let start = self.start_range();
        let key = self.parse_expr()?;
        self.expect(TokenKind::Colon)?;
        let value = self.parse_expr()?;
        let range = self.end_range(start);
        Ok(TableProperty { key, value, range })
    }

    fn parse_params(&mut self) -> ParseParamsResult<S::String> {
        self.parse_items_between_with_last(
            TokenKind::OpenParen,
            Parser::parse_typed_ident,
            TokenKind::Ellipsis,
            |p| {
                p.expect(TokenKind::Ellipsis)?;
                Ok(Box::new(p.parse_typed_ident()?))
            },
            TokenKind::CloseParen,
        )
    }

    fn parse_closure_params(&mut self) -> ParseParamsResult<S::String> {
        self.parse_items_between_with_last(
            TokenKind::VBar,
            Parser::parse_atom_typed_ident,
            TokenKind::Ellipsis,
            |p| {
                p.expect(TokenKind::Ellipsis)?;
                Ok(Box::new(p.parse_atom_typed_ident()?))
            },
            TokenKind::VBar,
        )
    }

    fn parse_returns(&mut self) -> Result<Option<Box<Ty<S::String>>>, CompilerError> {
        Ok(if self.eat(TokenKind::Arrow) {
            Some(Box::new(self.parse_type()?))
        } else {
            None
        })
    }

    fn parse_throws(&mut self) -> Result<Option<Box<Ty<S::String>>>, CompilerError> {
        Ok(if self.eat(TokenKind::Throw) {
            Some(Box::new(self.parse_type()?))
        } else {
            None
        })
    }

    fn parse_lit(&mut self) -> Result<Lit<S::String>, CompilerError> {
        let token = self.current_token().ok_or_else(|| self.unexpected())?;
        let range = token.range;
        let text = &self.input[range];
        let kind = match token.kind {
            TokenKind::Null => LitKind::Null,
            TokenKind::True => LitKind::Bool(true),
            TokenKind::False => LitKind::Bool(false),
            TokenKind::Int => {
                let mut s = text.to_string();
                s.retain(|c| c != '_');
                let base = match s.as_bytes() {
                    [b'0', b'x', ..] => 16,
                    [b'0', b'o', ..] => 8,
                    [b'0', b'b', ..] => 2,
                    _ => 10,
                };
                let s = &s[if base != 10 { 2 } else { 0 }..];
                i64::from_str_radix(s, base)
                    .map(LitKind::Int)
                    .map_err(|error| CompilerError::ParseIntError { error, range })?
            }
            TokenKind::Float => {
                let mut s = text.to_string();
                s.retain(|c| c != '_');
                s.parse::<f64>()
                    .map(|f| LitKind::Float(Float(f)))
                    .map_err(|error| CompilerError::ParseFloatError { error, range })?
            }
            TokenKind::Str => {
                debug_assert!(
                    text.starts_with('"') && text.ends_with('"')
                        || text.starts_with('\'') && text.ends_with('\'')
                );
                let text = &text[1..text.len() - 1];
                let mut s = String::new();
                unescape_str(text, &mut |c| s.push(c))
                    .map(|_| LitKind::Str(self.interner.intern(&s)))
                    .map_err(|error| CompilerError::EscapeError { error, range })?
            }
            TokenKind::RawStr => {
                debug_assert!(
                    text.starts_with("r\"") && text.ends_with('"')
                        || text.starts_with("r'") && text.ends_with('\'')
                );
                let text = &text[2..text.len() - 1];
                LitKind::Str(self.interner.intern(text))
            }
            _ => return Err(self.unexpected()),
        };
        self.bump();
        Ok(Lit { kind, range })
    }

    fn parse_ident(&mut self) -> Result<Ident<S::String>, CompilerError> {
        let token = self.current_token().ok_or_else(|| self.unexpected())?;
        if !self.check(TokenKind::Ident) {
            return Err(self.unexpected());
        }
        let ident = Ident {
            range: token.range,
            name: self.interner.intern(&self.input[token.range]),
            reference_id: OnceLock::new(),
        };
        self.bump();
        Ok(ident)
    }

    fn parse_typed_ident(&mut self) -> Result<TypedIdent<S::String>, CompilerError> {
        let start = self.start_range();
        let ident = Box::new(self.parse_ident()?);
        let ty = if self.eat(TokenKind::Colon) {
            Some(self.parse_type()?)
        } else {
            None
        };
        let range = self.end_range(start);
        Ok(TypedIdent { ident, ty, range })
    }

    fn parse_atom_typed_ident(&mut self) -> Result<TypedIdent<S::String>, CompilerError> {
        let start = self.start_range();
        let ident = Box::new(self.parse_ident()?);
        let ty = if self.eat(TokenKind::Colon) {
            Some(self.parse_type_atom()?)
        } else {
            None
        };
        let range = self.end_range(start);
        Ok(TypedIdent { ident, ty, range })
    }

    fn parse_match_case(&mut self) -> Result<MatchCase<S::String>, CompilerError> {
        let start = self.start_range();
        let mut patterns = Vec::new();
        loop {
            patterns.push(self.parse_pattern()?);
            if !self.eat(TokenKind::VBar) {
                break;
            }
        }
        self.expect(TokenKind::FatArrow)?;
        let body = self.parse_block()?;
        let range = self.end_range(start);
        Ok(MatchCase {
            patterns,
            body,
            range,
        })
    }

    fn parse_pattern(&mut self) -> Result<Pattern<S::String>, CompilerError> {
        let start = self.start_range();
        let kind = if self.check(TokenKind::Ident) {
            PatternKind::Ident(Box::new(self.parse_ident()?))
        } else if self.check(TokenKind::OpenBrace) {
            let (pairs, others) = self.parse_items_between_with_last(
                TokenKind::OpenBrace,
                |p| {
                    let start = p.start_range();
                    let key = p.parse_lit()?;
                    p.expect(TokenKind::Colon)?;
                    let value = p.parse_pattern()?;
                    let range = p.end_range(start);
                    Ok(TablePatternPair { key, value, range })
                },
                TokenKind::Ellipsis,
                |p| {
                    let range = p.current_range().unwrap_or_default();
                    p.bump();
                    Ok(range)
                },
                TokenKind::CloseBrace,
            )?;
            PatternKind::Table { pairs, others }
        } else if self.check(TokenKind::OpenBracket) {
            let (items, others) = self.parse_items_between_with_last(
                TokenKind::OpenBracket,
                Parser::parse_pattern,
                TokenKind::Ellipsis,
                |p| {
                    let range = p.current_range().unwrap_or_default();
                    p.bump();
                    Ok(range)
                },
                TokenKind::CloseBracket,
            )?;
            PatternKind::List { items, others }
        } else {
            let lit = self.parse_lit()?;
            PatternKind::Lit(Box::new(lit))
        };
        let range = self.end_range(start);
        Ok(Pattern { kind, range })
    }

    fn parse_type(&mut self) -> Result<Ty<S::String>, CompilerError> {
        let start = self.start_range();
        let ty = self.parse_type_atom()?;
        if self.check(TokenKind::VBar) {
            let mut types = Vec::new();
            types.push(ty);
            while self.eat(TokenKind::VBar) {
                types.push(self.parse_type_atom()?);
            }
            let kind = TyKind::Union(types);
            let range = self.end_range(start);
            Ok(Ty { kind, range })
        } else {
            Ok(ty)
        }
    }

    fn parse_type_atom(&mut self) -> Result<Ty<S::String>, CompilerError> {
        let start = self.start_range();
        let kind = if self.eat(TokenKind::OpenParen) {
            let ty = self.parse_type()?;
            self.expect(TokenKind::CloseParen)?;
            TyKind::Paren(Box::new(ty))
        } else if self.check(TokenKind::Ident) {
            TyKind::Ident(Box::new(self.parse_ident()?))
        } else if self.check(TokenKind::OpenBrace) {
            let (pairs, others) = self.parse_items_between_with_last(
                TokenKind::OpenBrace,
                |p| {
                    let start = p.start_range();
                    let key = p.parse_ident()?;
                    p.expect(TokenKind::Colon)?;
                    let value = p.parse_type()?;
                    let range = p.end_range(start);
                    Ok(TableTyPair { key, value, range })
                },
                TokenKind::OpenBracket,
                |p| {
                    let start = p.start_range();
                    p.expect(TokenKind::OpenBracket)?;
                    let key = p.parse_type()?;
                    p.expect(TokenKind::CloseBracket)?;
                    p.expect(TokenKind::Colon)?;
                    let value = p.parse_type()?;
                    let range = p.end_range(start);
                    Ok(Box::new(TableTyOther { key, value, range }))
                },
                TokenKind::CloseBrace,
            )?;
            TyKind::Table { pairs, others }
        } else if self.eat(TokenKind::Fn) {
            let (params, variadic) = self.parse_items_between_with_last(
                TokenKind::OpenParen,
                Parser::parse_type,
                TokenKind::Ellipsis,
                |p| {
                    p.expect(TokenKind::Ellipsis)?;
                    Ok(Box::new(p.parse_type()?))
                },
                TokenKind::CloseParen,
            )?;
            let returns = if self.eat(TokenKind::Arrow) {
                Some(Box::new(self.parse_type()?))
            } else {
                None
            };
            let throws = if self.eat(TokenKind::Throw) {
                Some(Box::new(self.parse_type()?))
            } else {
                None
            };
            TyKind::Function {
                params,
                variadic,
                returns,
                throws,
            }
        } else {
            TyKind::Lit(Box::new(self.parse_lit()?))
        };
        let range = self.end_range(start);
        let ty = Ty { kind, range };
        if self.eat(TokenKind::Question) {
            let range = self.end_range(start);
            let kind = TyKind::Option(Box::new(ty));
            Ok(Ty { kind, range })
        } else {
            Ok(ty)
        }
    }
}
