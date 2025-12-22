//! The parser.

#![expect(clippy::shadow_unrelated, clippy::string_slice)]

use std::{iter::Peekable, sync::OnceLock};

use itertools::Itertools;
use rustc_literal_escaper::{unescape_byte_str, unescape_str};
use text_size::{TextRange, TextSize};

use super::{
    ast::*,
    error::CompilerError,
    interning::StringInterner,
    lexer::tokenize,
    token::{Token, TokenKind},
    value::ValueType,
};

/// Parse the token iter into AST.
pub fn parse<S: StringInterner>(
    interner: S,
    input: &str,
) -> (Program<S::String>, Vec<CompilerError>) {
    Parser::new(interner, input, tokenize(input)).parse()
}

#[derive(Debug, Clone)]
struct Checkpoint<I: Iterator<Item = Token> + Clone> {
    token_iter: Peekable<I>,
    prev_token_end: TextSize,
    expected_kinds_pos: usize,
    errors_pos: usize,
}

struct Parser<'input, S: StringInterner, I: Iterator<Item = Token> + Clone> {
    interner: S,
    input: &'input str,
    token_iter: Peekable<I>,
    prev_token_end: TextSize,
    expected_kinds: Vec<TokenKind>,
    errors: Vec<CompilerError>,
}

impl<'input, S: StringInterner, I: Iterator<Item = Token> + Clone> Parser<'input, S, I> {
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
            let range = self.current_range()?;
            self.bump();
            Some(range)
        } else {
            None
        }
    }

    /// Returns an error for an unexpected token.
    fn unexpected(&mut self) -> CompilerError {
        let (found, range) = self.current_token().map_or(
            (TokenKind::Eof, TextRange::empty(self.prev_token_end)),
            |token| (token.kind, token.range),
        );

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

    /// Creates a checkpoint of the current parser state.
    /// Can be used to rewind the parser state via `rewind`.
    fn checkpoint(&self) -> Checkpoint<I> {
        Checkpoint {
            token_iter: self.token_iter.clone(),
            prev_token_end: self.prev_token_end,
            expected_kinds_pos: self.expected_kinds.len(),
            errors_pos: self.errors.len(),
        }
    }

    /// Rewinds the parser state to the given checkpoint.
    fn rewind(&mut self, checkpoint: Checkpoint<I>) {
        let Checkpoint {
            token_iter,
            prev_token_end,
            expected_kinds_pos,
            errors_pos,
        } = checkpoint;
        self.token_iter = token_iter;
        self.prev_token_end = prev_token_end;
        self.expected_kinds.truncate(expected_kinds_pos);
        self.errors.truncate(errors_pos);
    }

    /// Attempts to parse something using the provided function.
    fn try_parse<T>(
        &mut self,
        func: impl FnOnce(&mut Self) -> Result<T, CompilerError>,
    ) -> Option<T> {
        let checkpoint = self.checkpoint();
        let errors_len = self.errors.len();
        let node = func(self);
        if node.is_err() || self.errors.len() > errors_len {
            self.rewind(checkpoint);
        }
        node.ok()
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

    /// Parses zero or more items separated by `sep` until `end`.
    /// Allowing trailing `seq` and eol before `sep`. The `end` will be consumed.
    fn parse_items_until<T, F: Fn(&mut Self) -> Result<T, CompilerError>>(
        &mut self,
        parse_func: F,
        end: TokenKind,
    ) -> Result<Vec<T>, CompilerError> {
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

    /// Parses zero or more items separated by `sep` until `end`.
    /// Allowing trailing `seq` and eol before `sep`. The `end` will be consumed.
    /// The last element is specified by `parse_last_func`.
    fn parse_items_until_with_last<Item, Last, ItemFunc, LastFunc>(
        &mut self,
        parse_item_func: ItemFunc,
        last: TokenKind,
        parse_last_func: LastFunc,
        end: TokenKind,
    ) -> Result<(Vec<Item>, Option<Last>), CompilerError>
    where
        ItemFunc: Fn(&mut Self) -> Result<Item, CompilerError>,
        LastFunc: Fn(&mut Self) -> Result<Last, CompilerError>,
    {
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
}

impl<S: StringInterner, I: Iterator<Item = Token> + Clone> Parser<'_, S, I> {
    /// Parse token iter into AST.
    fn parse(mut self) -> (Program<S::String>, Vec<CompilerError>) {
        let start = self.start_range();
        let body = self.parse_exprs(TokenKind::Eof);
        let range = self.end_range(start);
        let body = Box::new(Block {
            body,
            range,
            scope_id: OnceLock::new(),
        });
        let function = Box::new(Function {
            name: None,
            kind: FunctionKind::Function,
            params: Box::new(Params::empty()),
            returns: None,
            body,
            range,
            function_id: OnceLock::new(),
        });
        (Program { function }, self.errors)
    }

    fn parse_exprs(&mut self, end: TokenKind) -> Vec<Expr<S::String>> {
        let mut items = Vec::new();
        self.eat_eol();
        while !self.eat(end) {
            match self.parse_stmt() {
                Ok(expr) => items.push(expr),
                Err(e) => {
                    self.errors.push(e);
                    while !(self.check(TokenKind::Eol)
                        || self.check(TokenKind::Eof)
                        || self.check(end))
                    {
                        self.bump();
                    }
                }
            }
            if self.eat(end) {
                break;
            }
            if self.check(TokenKind::Eof) {
                let e = self.unexpected();
                self.errors.push(e);
                break;
            }
            if let Err(e) = self.expect(TokenKind::Eol) {
                self.errors.push(e);
            }
            self.eat_eol();
        }
        items
    }

    fn parse_block(&mut self) -> Result<Box<Block<S::String>>, CompilerError> {
        self.expect(TokenKind::OpenBrace)?;
        let start = self.start_range();
        let body = self.parse_exprs(TokenKind::CloseBrace);
        let range = self.end_range(start);
        Ok(Box::new(Block {
            body,
            range,
            scope_id: OnceLock::new(),
        }))
    }

    // There are no statements in Lucia, statements in other languages, such as `if` and `while`,
    // are expressions in Lucia. This function is used to parse syntax-level statements, such as
    // `fn name() { ... }`, `glo name = value`, and `name: Type = value`. While these are
    // represented as expressions in the AST, they should be treated like statements in other
    // languages at the syntax-level. These constructs should not appear in places where an
    // expression is expected, for example, `name1 = fn name2() { ... }` is confusing. However,
    // they are still expressions in some levels, like an assignment on the last line of a block is
    // treated as an expression that returns null. So we should treated these as expressions that
    // only allowed to appear in block expression.
    fn parse_stmt(&mut self) -> Result<Expr<S::String>, CompilerError> {
        // The fn keyword can be a fn expression or a function statement (fn expression with name).
        // We try to parse it as a function statement first, and if it fails, we parse it as a
        // fn expression.
        if self.check(TokenKind::Fn)
            && let Some(function) = self.try_parse(Self::parse_function_stmt)
        {
            return Ok(function);
        }
        let start = self.start_range();
        let kind = if self.eat(TokenKind::Import) {
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
                    break if self.eat(TokenKind::OpenBrace) {
                        let items = self.parse_items_until(
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
                .intern(&path.iter().map(|ident| &ident.name).join("::"));
            ExprKind::Import {
                path,
                path_str,
                kind,
            }
        } else if self.check(TokenKind::Effect) {
            let (effect, name) = self.parse_effect_stmt()?;
            ExprKind::Effect {
                glo: None,
                name,
                effect,
            }
        } else if let Some(glo_range) = self.eat_range(TokenKind::Glo) {
            if self.eat(TokenKind::Fn) {
                let name = Box::new(self.parse_ident()?);
                let function = Box::new(self.parse_function(Some(name.name.clone()), start)?);
                ExprKind::Fn {
                    glo: Some(glo_range),
                    name: Some(name),
                    function,
                }
            } else if self.check(TokenKind::Effect) {
                let (effect, name) = self.parse_effect_stmt()?;
                ExprKind::Effect {
                    glo: Some(glo_range),
                    name,
                    effect,
                }
            } else {
                let left = Box::new(self.parse_typed_ident()?);
                self.expect(TokenKind::Assign)?;
                let right = Box::new(self.parse_expr()?);
                ExprKind::GloAssign { left, right }
            }
        } else {
            let expr = self.parse_expr()?;
            if self.check(TokenKind::Comma) {
                let mut left = Vec::new();
                left.push(
                    self.expr_to_assign_left(expr)
                        .ok_or_else(|| self.unexpected())?,
                );
                while let Some(comma_range) = self.eat_range(TokenKind::Comma) {
                    let expr = self.parse_expr()?;
                    left.push(self.expr_to_assign_left(expr).ok_or_else(|| {
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
                    ExprKind::AssignMulti { left, right }
                } else {
                    let right = Box::new(right_expr);
                    ExprKind::AssignUnpack { left, right }
                }
            } else if self.check(TokenKind::Colon) {
                let ExprKind::Ident(ident) = expr.kind else {
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
                ExprKind::Assign { left, right }
            } else if self.check(TokenKind::Assign) {
                let left = self
                    .expr_to_assign_left(expr)
                    .ok_or_else(|| self.unexpected())?;
                self.bump();
                let right = Box::new(self.parse_expr()?);
                ExprKind::Assign { left, right }
            } else if self.check(TokenKind::AddAssign)
                || self.check(TokenKind::SubAssign)
                || self.check(TokenKind::MulAssign)
                || self.check(TokenKind::DivAssign)
                || self.check(TokenKind::RemAssign)
            {
                let left = self
                    .expr_to_assign_left(expr)
                    .ok_or_else(|| self.unexpected())?;
                #[expect(clippy::wildcard_enum_match_arm)]
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
                ExprKind::AssignOp {
                    operator,
                    left,
                    right,
                }
            } else {
                return Ok(expr);
            }
        };
        let range = self.end_range(start);
        Ok(Expr { kind, range })
    }

    fn parse_function_stmt(&mut self) -> Result<Expr<S::String>, CompilerError> {
        let start = self.start_range();
        self.expect(TokenKind::Fn)?;
        let name = Box::new(self.parse_ident()?);
        let function = Box::new(self.parse_function(Some(name.name.clone()), start)?);
        let kind = ExprKind::Fn {
            glo: None,
            name: Some(name),
            function,
        };
        let range = self.end_range(start);
        Ok(Expr { kind, range })
    }

    #[expect(clippy::type_complexity)]
    fn parse_effect_stmt(
        &mut self,
    ) -> Result<(Box<Effect<S::String>>, Box<Ident<S::String>>), CompilerError> {
        let start = self.start_range();
        self.expect(TokenKind::Effect)?;
        let name = Box::new(self.parse_ident()?);
        let params = self.parse_params()?;
        let returns = self.parse_returns()?;
        let range = self.end_range(start);
        let effect = Box::new(Effect {
            name: name.name.clone(),
            params,
            returns,
            range,
        });
        Ok((effect, name))
    }

    #[expect(clippy::unused_self)]
    fn expr_to_assign_left(&self, expr: Expr<S::String>) -> Option<AssignLeft<S::String>> {
        #[expect(clippy::wildcard_enum_match_arm)]
        let kind = match expr.kind {
            ExprKind::Ident(ident) => AssignLeftKind::Ident(Box::new(TypedIdent {
                ty: None,
                range: ident.range,
                ident,
            })),
            ExprKind::Member {
                table,
                property,
                safe,
            } if safe.is_none() => {
                if let ExprKind::Ident(ident) = table.kind {
                    AssignLeftKind::Member { ident, property }
                } else {
                    return None;
                }
            }
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
        start: TextSize,
    ) -> Result<Function<S::String>, CompilerError> {
        Ok(Function {
            name,
            kind: FunctionKind::Function,
            params: self.parse_params()?,
            returns: self.parse_returns()?,
            body: self.parse_block()?,
            range: self.end_range(start),
            function_id: OnceLock::new(),
        })
    }

    fn parse_expr(&mut self) -> Result<Expr<S::String>, CompilerError> {
        self.parse_expr_with_option(true)
    }

    #[expect(clippy::type_complexity)]
    fn parse_expr_and_block(
        &mut self,
    ) -> Result<(Box<Expr<S::String>>, Box<Block<S::String>>), CompilerError> {
        Ok((
            Box::new(self.parse_expr_with_option(false)?),
            self.parse_block()?,
        ))
    }

    fn parse_expr_with_option(
        &mut self,
        allow_trailing_lambda: bool,
    ) -> Result<Expr<S::String>, CompilerError> {
        self.parse_expr_precedence(1, allow_trailing_lambda)
    }

    fn parse_expr_precedence(
        &mut self,
        min_precedence: u8,
        allow_trailing_lambda: bool,
    ) -> Result<Expr<S::String>, CompilerError> {
        let start = self.start_range();
        let mut left = self.parse_expr_unary(allow_trailing_lambda)?;
        loop {
            #[expect(clippy::wildcard_enum_match_arm)]
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
                TokenKind::Is => BinOp::Is,
                _ => break,
            };
            if operator.precedence() < min_precedence {
                break;
            }
            self.bump();
            left = if operator == BinOp::Is {
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
            } else {
                let right =
                    self.parse_expr_precedence(operator.precedence() + 1, allow_trailing_lambda)?;
                let kind = ExprKind::Binary {
                    operator,
                    left: Box::new(left),
                    right: Box::new(right),
                };
                let range = self.end_range(start);
                Expr { kind, range }
            };
        }
        Ok(left)
    }

    fn parse_expr_unary(
        &mut self,
        allow_trailing_lambda: bool,
    ) -> Result<Expr<S::String>, CompilerError> {
        let start = self.start_range();
        let kind = if self.eat(TokenKind::Not) {
            let argument = Box::new(self.parse_expr_primary(allow_trailing_lambda)?);
            ExprKind::Unary {
                operator: UnOp::Not,
                argument,
            }
        } else if self.eat(TokenKind::Sub) {
            let argument = Box::new(self.parse_expr_primary(allow_trailing_lambda)?);
            ExprKind::Unary {
                operator: UnOp::Neg,
                argument,
            }
        } else if let Some(try_range) = self.eat_range(TokenKind::Try) {
            let mut expr = self.parse_expr_primary(allow_trailing_lambda)?;
            return if let ExprKind::Call { handlers, .. } = &mut expr.kind {
                loop {
                    let start = self.start_range();
                    self.expect(TokenKind::With)?;
                    let effect = Box::new(self.parse_ident()?);
                    let params = self.parse_params()?;
                    let body = self.parse_block()?;
                    let range = self.end_range(start);
                    handlers.push(EffectHandler {
                        effect,
                        params,
                        body,
                        range,
                    });
                    if !self.check(TokenKind::With) {
                        break;
                    }
                }
                Ok(expr)
            } else {
                Err(CompilerError::UnexpectedToken {
                    expected: Vec::new(),
                    found: TokenKind::Try,
                    range: try_range,
                })
            };
        } else {
            return self.parse_expr_primary(allow_trailing_lambda);
        };
        let range = self.end_range(start);
        Ok(Expr { kind, range })
    }

    fn parse_expr_primary(
        &mut self,
        allow_trailing_lambda: bool,
    ) -> Result<Expr<S::String>, CompilerError> {
        let start = self.start_range();
        let mut expr = self.parse_expr_atom()?;
        macro_rules! member_expr {
            (Bracket, BracketMeta, $safe:expr) => {
                if self.eat(TokenKind::Pound) {
                    self.expect(TokenKind::CloseBracket)?;
                    ExprKind::Member {
                        table: Box::new(expr),
                        property: MemberKind::BracketMeta,
                        safe: $safe,
                    }
                } else {
                    let property = self.parse_expr()?;
                    self.expect(TokenKind::CloseBracket)?;
                    ExprKind::Member {
                        table: Box::new(expr),
                        property: MemberKind::Bracket(Box::new(property)),
                        safe: $safe,
                    }
                }
            };

            ($kind:ident, $kind_meta:ident, $safe:expr) => {
                if self.eat(TokenKind::Pound) {
                    ExprKind::Member {
                        table: Box::new(expr),
                        property: MemberKind::$kind_meta,
                        safe: $safe,
                    }
                } else {
                    let ident = self.parse_ident()?;
                    ExprKind::Member {
                        table: Box::new(expr),
                        property: MemberKind::$kind(Box::new(ident)),
                        safe: $safe,
                    }
                }
            };
        }
        loop {
            let kind = if self.eat(TokenKind::OpenParen) {
                let arguments =
                    self.parse_items_until(Parser::parse_expr, TokenKind::CloseParen)?;
                let trailing_lambda = if allow_trailing_lambda && self.check(TokenKind::OpenBrace) {
                    let body = self.parse_block()?;
                    let range = body.range;
                    Some(Box::new(Function {
                        name: None,
                        kind: FunctionKind::Closure,
                        params: Box::new(Params::empty()),
                        returns: None,
                        body,
                        range,
                        function_id: OnceLock::new(),
                    }))
                } else {
                    None
                };
                ExprKind::Call {
                    callee: Box::new(expr),
                    arguments,
                    trailing_lambda,
                    handlers: Vec::new(),
                }
            } else if allow_trailing_lambda && self.check(TokenKind::OpenBrace) {
                let body = self.parse_block()?;
                let range = body.range;
                let trailing_lambda = Some(Box::new(Function {
                    name: None,
                    kind: FunctionKind::Closure,
                    params: Box::new(Params::empty()),
                    returns: None,
                    body,
                    range,
                    function_id: OnceLock::new(),
                }));
                ExprKind::Call {
                    callee: Box::new(expr),
                    arguments: Vec::new(),
                    trailing_lambda,
                    handlers: Vec::new(),
                }
            } else {
                let safe = self.eat_range(TokenKind::Question);
                if self.eat(TokenKind::OpenBracket) {
                    member_expr!(Bracket, BracketMeta, safe)
                } else if self.eat(TokenKind::Dot) {
                    member_expr!(Dot, DotMeta, safe)
                } else if self.eat(TokenKind::DoubleColon) {
                    member_expr!(DoubleColon, DoubleColonMeta, safe)
                } else if safe.is_some() {
                    return Err(self.unexpected());
                } else {
                    break;
                }
            };
            let range = self.end_range(start);
            expr = Expr { kind, range }
        }
        Ok(expr)
    }

    fn parse_expr_atom(&mut self) -> Result<Expr<S::String>, CompilerError> {
        let start = self.start_range();
        let kind = if self.eat(TokenKind::OpenParen) {
            let expr = self.parse_expr()?;
            self.expect(TokenKind::CloseParen)?;
            ExprKind::Paren(Box::new(expr))
        } else if self.check(TokenKind::Ident) {
            ExprKind::Ident(Box::new(self.parse_ident()?))
        } else if self.eat(TokenKind::OpenBrace) {
            self.eat_eol();
            if self.eat(TokenKind::CloseBrace) {
                ExprKind::Table {
                    properties: Vec::new(),
                }
            } else {
                // The `{` can be a table expression or a block expression. We try to parse it as a
                // table expression first, and if it fails, we parse it as a block expression.
                let start = self.start_range();
                if let Some(key) = self.try_parse(Parser::parse_table_key_and_colon) {
                    let value = self.parse_expr()?;
                    let range = self.end_range(start);
                    let table_property = TableProperty { key, value, range };
                    self.eat_eol();
                    if self.eat(TokenKind::CloseBrace) {
                        ExprKind::Table {
                            properties: vec![table_property],
                        }
                    } else {
                        self.expect(TokenKind::Comma)?;
                        let mut properties = self.parse_items_until(
                            Parser::parse_table_property,
                            TokenKind::CloseBrace,
                        )?;
                        properties.insert(0, table_property);
                        ExprKind::Table { properties }
                    }
                } else {
                    let body = self.parse_exprs(TokenKind::CloseBrace);
                    let range = self.end_range(start);
                    ExprKind::Block(Box::new(Block {
                        body,
                        range,
                        scope_id: OnceLock::new(),
                    }))
                }
            }
        } else if self.eat(TokenKind::OpenBracket) {
            let items = self.parse_items_until(Parser::parse_expr, TokenKind::CloseBracket)?;
            ExprKind::List { items }
        } else if self.eat(TokenKind::Fn) {
            let function = Box::new(self.parse_function(None, start)?);
            ExprKind::Fn {
                glo: None,
                name: None,
                function,
            }
        } else if self.check(TokenKind::VBar) {
            let function = Box::new(Function {
                name: None,
                kind: FunctionKind::Closure,
                params: self.parse_closure_params()?,
                returns: self.parse_returns()?,
                body: self.parse_block()?,
                range: self.end_range(start),
                function_id: OnceLock::new(),
            });
            ExprKind::Fn {
                glo: None,
                name: None,
                function,
            }
        } else if self.eat(TokenKind::Do) {
            let function = Box::new(Function {
                name: None,
                kind: FunctionKind::Do,
                params: Box::new(Params::empty()),
                body: self.parse_block()?,
                returns: None,
                range: self.end_range(start),
                function_id: OnceLock::new(),
            });
            ExprKind::Fn {
                glo: None,
                name: None,
                function,
            }
        } else if self.eat(TokenKind::If) {
            let (test, consequent) = self.parse_expr_and_block()?;
            let alternate = if self.eat(TokenKind::Else) {
                let expr = if self.check(TokenKind::If) {
                    self.parse_expr()?
                } else {
                    let block = self.parse_block()?;
                    Expr {
                        range: block.range,
                        kind: ExprKind::Block(block),
                    }
                };
                Some(Box::new(expr))
            } else {
                None
            };
            ExprKind::If {
                test,
                consequent,
                alternate,
            }
        } else if self.eat(TokenKind::Match) {
            let expr = Box::new(self.parse_expr_with_option(false)?);
            self.expect(TokenKind::OpenBrace)?;
            let mut cases = Vec::new();
            while !self.eat(TokenKind::CloseBrace) {
                self.eat_eol();
                cases.push(self.parse_match_case()?);
                self.eat_eol();
            }
            ExprKind::Match { expr, cases }
        } else if self.eat(TokenKind::Loop) {
            let body = self.parse_block()?;
            ExprKind::Loop { body }
        } else if self.eat(TokenKind::While) {
            let (test, body) = self.parse_expr_and_block()?;
            ExprKind::While { test, body }
        } else if self.eat(TokenKind::For) {
            let left = self.parse_items(Parser::parse_ident)?;
            self.expect(TokenKind::In)?;
            let (right, body) = self.parse_expr_and_block()?;
            ExprKind::For { left, right, body }
        } else if self.eat(TokenKind::Break) {
            ExprKind::Break
        } else if self.eat(TokenKind::Continue) {
            ExprKind::Continue
        } else if self.eat(TokenKind::Return) {
            let argument = Box::new(self.parse_expr()?);
            ExprKind::Return { argument }
        } else {
            ExprKind::Lit(Box::new(self.parse_lit()?))
        };
        let range = self.end_range(start);
        Ok(Expr { kind, range })
    }

    fn parse_table_property(&mut self) -> Result<TableProperty<S::String>, CompilerError> {
        let start = self.start_range();
        let key = self.parse_table_key_and_colon()?;
        let value = self.parse_expr()?;
        let range = self.end_range(start);
        Ok(TableProperty { key, value, range })
    }

    fn parse_table_key_and_colon(&mut self) -> Result<Expr<S::String>, CompilerError> {
        let start = self.start_range();
        let kind = if self.eat(TokenKind::OpenParen) {
            let expr = self.parse_expr()?;
            self.expect(TokenKind::CloseParen)?;
            ExprKind::Paren(Box::new(expr))
        } else {
            ExprKind::Lit(Box::new(self.parse_lit()?))
        };
        let range = self.end_range(start);
        self.expect(TokenKind::Colon)?;
        Ok(Expr { kind, range })
    }

    fn parse_params(&mut self) -> Result<Box<Params<S::String>>, CompilerError> {
        self.expect(TokenKind::OpenParen)?;
        let (params, variadic) = self.parse_items_until_with_last(
            Parser::parse_typed_ident,
            TokenKind::Ellipsis,
            |p| {
                p.expect(TokenKind::Ellipsis)?;
                Ok(Box::new(p.parse_typed_ident()?))
            },
            TokenKind::CloseParen,
        )?;
        Ok(Box::new(Params { params, variadic }))
    }

    fn parse_closure_params(&mut self) -> Result<Box<Params<S::String>>, CompilerError> {
        self.expect(TokenKind::VBar)?;
        let (params, variadic) = self.parse_items_until_with_last(
            Parser::parse_atom_typed_ident,
            TokenKind::Ellipsis,
            |p| {
                p.expect(TokenKind::Ellipsis)?;
                Ok(Box::new(p.parse_atom_typed_ident()?))
            },
            TokenKind::VBar,
        )?;
        Ok(Box::new(Params { params, variadic }))
    }

    fn parse_returns(&mut self) -> Result<Option<Box<Ty<S::String>>>, CompilerError> {
        Ok(if self.eat(TokenKind::Arrow) {
            Some(Box::new(self.parse_type()?))
        } else {
            None
        })
    }

    fn parse_lit(&mut self) -> Result<Lit<S::String>, CompilerError> {
        let token = self.current_token().ok_or_else(|| self.unexpected())?;
        let range = token.range;
        let text = &self.input[range];
        #[expect(clippy::wildcard_enum_match_arm)]
        let kind = match token.kind {
            TokenKind::Null => LitKind::Null,
            TokenKind::True => LitKind::Bool(true),
            TokenKind::False => LitKind::Bool(false),
            TokenKind::Int => {
                let mut s = text.to_owned();
                s.retain(|c| c != '_');
                let base = match s.as_bytes() {
                    [b'0', b'x', ..] => 16,
                    [b'0', b'o', ..] => 8,
                    [b'0', b'b', ..] => 2,
                    _ => 10,
                };
                let s = &s[if base == 10 { 0 } else { 2 }..];
                i64::from_str_radix(s, base)
                    .map(LitKind::Int)
                    .map_err(|error| CompilerError::ParseIntError { error, range })?
            }
            TokenKind::Float => {
                let mut s = text.to_owned();
                s.retain(|c| c != '_');
                s.parse::<f64>()
                    .map(|f| LitKind::Float(f.into()))
                    .map_err(|error| CompilerError::ParseFloatError { error, range })?
            }
            TokenKind::Str => {
                debug_assert!(
                    text.starts_with('"') && text.ends_with('"')
                        || text.starts_with('\'') && text.ends_with('\'')
                );
                let text = &text[1..text.len() - 1];
                let mut s = Ok(String::new());
                unescape_str(text, |_, c| match (&mut s, c) {
                    (Ok(s), Ok(c)) => s.push(c),
                    (Ok(_), Err(e)) => s = Err(e),
                    _ => (),
                });
                match s {
                    Ok(s) => LitKind::Str(self.interner.intern(&s)),
                    Err(error) => return Err(CompilerError::EscapeError { error, range }),
                }
            }
            TokenKind::RawStr => {
                debug_assert!(
                    text.starts_with("r\"") && text.ends_with('"')
                        || text.starts_with("r'") && text.ends_with('\'')
                );
                let text = &text[2..text.len() - 1];
                LitKind::Str(self.interner.intern(text))
            }
            TokenKind::ByteStr => {
                debug_assert!(
                    text.starts_with("b\"") && text.ends_with('"')
                        || text.starts_with("b'") && text.ends_with('\'')
                );
                let text = &text[2..text.len() - 1];
                let mut s = Ok(Vec::new());
                unescape_byte_str(text, |_, c| match (&mut s, c) {
                    (Ok(s), Ok(c)) => s.push(c),
                    (Ok(_), Err(e)) => s = Err(e),
                    _ => (),
                });
                match s {
                    Ok(s) => LitKind::Bytes(s),
                    Err(error) => return Err(CompilerError::EscapeError { error, range }),
                }
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
        let body = Box::new(self.parse_expr()?);
        let range = self.end_range(start);
        Ok(MatchCase {
            patterns,
            body,
            range,
            scope_id: OnceLock::new(),
        })
    }

    fn parse_pattern(&mut self) -> Result<Pattern<S::String>, CompilerError> {
        let start = self.start_range();
        let kind = if self.check(TokenKind::Ident) {
            PatternKind::Ident(Box::new(self.parse_ident()?))
        } else if self.eat(TokenKind::OpenBrace) {
            let (pairs, others) = self.parse_items_until_with_last(
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
        } else if self.eat(TokenKind::OpenBracket) {
            let (items, others) = self.parse_items_until_with_last(
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
        } else if self.eat(TokenKind::OpenBrace) {
            let (pairs, others) = self.parse_items_until_with_last(
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
            let (params, variadic) = self.parse_items_until_with_last(
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
            TyKind::Function {
                params,
                variadic,
                returns,
            }
        } else {
            TyKind::Lit(Box::new(self.parse_lit()?))
        };
        let range = self.end_range(start);
        Ok(Ty { kind, range })
    }
}
