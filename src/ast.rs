use crate::token::Location;

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
        left: Vec<Ident>,
        right: Box<Expr>,
        body: Box<Block>,
    },
    Break,
    Continue,
    Return {
        argument: Box<Expr>,
    },
    Throw {
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
    AssignUnpack {
        left: Vec<Expr>,
        right: Box<Expr>,
    },
    AssignMulti {
        left: Vec<Expr>,
        right: Vec<Expr>,
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
        safe: bool,
    },
    MetaMember {
        table: Box<Expr>,
        safe: bool,
    },
    Call {
        callee: Box<Expr>,
        arguments: Vec<Expr>,
        propagating_error: bool,
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
#[derive(Debug, Clone, PartialEq, PartialOrd)]
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
