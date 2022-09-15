use crate::errors::{LResult, LucyError, ParserErrorKind, SyntaxErrorKind};
use crate::lexer::{LiteralValue, Location, TokenKind};
use crate::parser_error;

#[derive(Debug, Clone)]
pub struct Lit {
    pub value: LiteralValue,
    pub start: Location,
    pub end: Location,
}

#[derive(Debug, Clone)]
pub struct Ident {
    pub name: String,
    pub start: Location,
    pub end: Location,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub body: Vec<Stmt>,
    pub start: Location,
    pub end: Location,
}

impl Block {
    pub fn to_stmt(self) -> Stmt {
        Stmt {
            start: self.start,
            end: self.end,
            kind: StmtKind::Block(Box::new(self)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub start: Location,
    pub end: Location,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub start: Location,
    pub end: Location,
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    If {
        test: Box<Expr>,
        consequent: Box<Block>,
        alternate: Option<Box<Block>>,
    },
    Loop {
        body: Box<Block>,
    },
    While {
        test: Box<Expr>,
        body: Box<Block>,
    },
    For {
        left: Box<Ident>,
        right: Box<Expr>,
        body: Box<Block>,
    },
    Break,
    Continue,
    Return {
        argument: Box<Expr>,
    },
    Global {
        arguments: Vec<Ident>,
    },
    Import {
        path: Vec<Ident>,
        kind: ImportKind,
    },
    Assign {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    AssignOp {
        operator: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Block(Box<Block>),
    Expr(Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Lit(Box<Lit>),
    Ident(Box<Ident>),
    Function {
        params: Vec<Ident>,
        variadic: Option<Box<Ident>>,
        body: Box<Block>,
        is_closure: bool,
    },
    Table {
        properties: Vec<TableProperty>,
    },
    Unary {
        operator: UnOp,
        argument: Box<Expr>,
    },
    Binary {
        operator: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Member {
        table: Box<Expr>,
        property: Box<Expr>,
        kind: MemberExprKind,
    },
    Call {
        callee: Box<Expr>,
        arguments: Vec<Expr>,
    },
}

#[derive(Clone, PartialEq, Eq, Debug, Copy)]
pub enum UnOp {
    /// The `!` operator for logical inversion
    Not,
    /// The `-` operator for negation
    Neg,
}

impl UnOp {
    pub fn from_token(token_kind: &TokenKind) -> LResult<Self> {
        Ok(match token_kind {
            TokenKind::Not => UnOp::Not,
            TokenKind::Sub => UnOp::Neg,
            _ => return Err(parser_error!(ParserErrorKind::ParserUnOpError)),
        })
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Copy)]
pub enum BinOp {
    /// The `+` operator (addition)
    Add,
    /// The `-` operator (subtraction)
    Sub,
    /// The `*` operator (multiplication)
    Mul,
    /// The `/` operator (division)
    Div,
    /// The `%` operator (modulus)
    Mod,
    /// The `&&` operator (logical and)
    And,
    /// The `||` operator (logical or)
    Or,
    /// The `==` operator (equality)
    Eq,
    /// The `<` operator (less than)
    Lt,
    /// The `<=` operator (less than or equal to)
    Le,
    /// The `!=` operator (not equal to)
    Ne,
    /// The `>=` operator (greater than or equal to)
    Ge,
    /// The `>` operator (greater than)
    Gt,
    /// The 'is' operator (identity comparison)
    Is,
}

impl BinOp {
    pub fn from_token(token_kind: &TokenKind) -> LResult<Self> {
        Ok(match token_kind {
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
            _ => return Err(parser_error!(ParserErrorKind::ParserBinOpError)),
        })
    }

    pub fn from_assign_token(token_kind: &TokenKind) -> LResult<Self> {
        Ok(match token_kind {
            TokenKind::AddAssign => Self::Add,
            TokenKind::SubAssign => Self::Sub,
            TokenKind::MulAssign => Self::Mul,
            TokenKind::DivAssign => Self::Div,
            TokenKind::ModAssign => Self::Mod,
            _ => return Err(parser_error!(ParserErrorKind::ParserBinOpError)),
        })
    }

    pub fn get_precedence(&self) -> u32 {
        match self {
            BinOp::Mul => 5,
            BinOp::Div => 5,
            BinOp::Mod => 5,

            BinOp::Add => 4,
            BinOp::Sub => 4,

            BinOp::Eq => 3,
            BinOp::Lt => 3,
            BinOp::Le => 3,
            BinOp::Ne => 3,
            BinOp::Ge => 3,
            BinOp::Gt => 3,
            BinOp::Is => 3,

            BinOp::And => 2,
            BinOp::Or => 1,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Copy)]
pub enum MemberExprKind {
    OpenBracket,
    Dot,
    DoubleColon,
}

impl MemberExprKind {
    pub fn from_token(token_kind: &TokenKind) -> LResult<Self> {
        Ok(match token_kind {
            TokenKind::OpenBracket => MemberExprKind::OpenBracket,
            TokenKind::Dot => MemberExprKind::Dot,
            TokenKind::DoubleColon => MemberExprKind::DoubleColon,
            _ => return Err(parser_error!(ParserErrorKind::ParserMemberExprError)),
        })
    }
}

#[derive(Debug, Clone)]
pub enum ImportKind {
    /// `import path::xxx`
    Simple(Ident),
    /// `import path::{...}`
    Nested(Vec<(Ident, Ident)>),
    /// `import path::*`
    Glob,
}

#[derive(Debug, Clone)]
pub struct TableProperty {
    pub key: Box<Expr>,
    pub value: Box<Expr>,
    pub start: Location,
    pub end: Location,
}
