use std::convert::TryFrom;

use crate::errors::LuciaError;
use crate::lexer::{LiteralKind, Location};

/// A statement.
#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub start: Location,
    pub end: Location,
}

impl From<Block> for Stmt {
    fn from(value: Block) -> Self {
        Stmt {
            start: value.start,
            end: value.end,
            kind: StmtKind::Block(Box::new(value)),
        }
    }
}

impl From<Expr> for Stmt {
    fn from(value: Expr) -> Self {
        Stmt {
            start: value.start,
            end: value.end,
            kind: StmtKind::Expr(Box::new(value)),
        }
    }
}

/// Kind of statement.
#[derive(Debug, Clone)]
pub enum StmtKind {
    If {
        test: Box<Expr>,
        consequent: Box<Block>,
        alternate: Option<Box<Stmt>>,
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

/// A block.
#[derive(Debug, Clone)]
pub struct Block {
    pub body: Vec<Stmt>,
    pub start: Location,
    pub end: Location,
}

/// An expression.
#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub start: Location,
    pub end: Location,
}

/// Kind of expression.
#[derive(Debug, Clone)]
pub enum ExprKind {
    Lit(Box<Lit>),
    Ident(Box<Ident>),
    Do(Box<Block>),
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
        kind: MemberKind,
    },
    Call {
        callee: Box<Expr>,
        arguments: Vec<Expr>,
    },
}

impl From<Lit> for Expr {
    fn from(value: Lit) -> Self {
        Expr {
            start: value.start,
            end: value.end,
            kind: ExprKind::Lit(Box::new(value)),
        }
    }
}

impl From<Ident> for Expr {
    fn from(value: Ident) -> Self {
        Expr {
            start: value.start,
            end: value.end,
            kind: ExprKind::Ident(Box::new(value)),
        }
    }
}

/// A literal.
#[derive(Debug, Clone)]
pub struct Lit {
    pub value: LitKind,
    pub start: Location,
    pub end: Location,
}

/// Kind of literal.
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum LitKind {
    /// "null"
    Null,
    /// "true", "false"
    Bool(bool),
    /// "12", "0o100", "0b110"
    Int(i64),
    /// "12.34", "0b100.100"
    Float(f64),
    /// ""abc"", ""abc"
    Str(String),
}

impl TryFrom<LiteralKind> for LitKind {
    type Error = LuciaError;

    fn try_from(value: LiteralKind) -> Result<Self, Self::Error> {
        Ok(match value {
            LiteralKind::Int(v) => LitKind::Int(v?),
            LiteralKind::Float(v) => LitKind::Float(v?),
            LiteralKind::Str(v) => LitKind::Str(v?),
        })
    }
}

/// An ident.
#[derive(Debug, Clone)]
pub struct Ident {
    pub name: String,
    pub start: Location,
    pub end: Location,
}

/// Unary operator.
#[derive(Clone, PartialEq, Eq, Debug, Copy)]
pub enum UnOp {
    /// The `not` operator for logical inversion
    Not,
    /// The `-` operator for negation
    Neg,
}

/// Binary operator.
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
    /// The `and` operator (logical and)
    And,
    /// The `or` operator (logical or)
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
    pub fn precedence(&self) -> u32 {
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

/// Kind of member expression.
#[derive(Clone, PartialEq, Eq, Debug, Copy)]
pub enum MemberKind {
    /// `[]`
    Bracket,
    /// `.`
    Dot,
    /// `::`
    DoubleColon,
}

/// Kind of import statement.
#[derive(Debug, Clone)]
pub enum ImportKind {
    /// `import path::xxx as xxx`
    Simple(Box<Ident>),
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
