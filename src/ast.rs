//! The Lucia Abstract Syntax Tree (AST).

use std::fmt::Display;

use crate::utils::{escape_str, Join, Location};

/// Kind of function.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionKind {
    Funciton,
    Closure,
    Do,
}

/// A statement.
#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub start: Location,
    pub end: Location,
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
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

impl Display for StmtKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StmtKind::If {
                test,
                consequent,
                alternate,
            } => {
                if let Some(alternate) = alternate {
                    write!(f, "if {test} {consequent} else {alternate}")
                } else {
                    write!(f, "if {test} {consequent}")
                }
            }
            StmtKind::Loop { body } => write!(f, "loop {body}"),
            StmtKind::While { test, body } => write!(f, "while {test} {body}"),
            StmtKind::For { left, right, body } => {
                write!(f, "for {} in {right} {body}", left.iter().join(", "))
            }
            StmtKind::Break => write!(f, "break"),
            StmtKind::Continue => write!(f, "continue"),
            StmtKind::Return { argument } => write!(f, "return {argument}"),
            StmtKind::Throw { argument } => write!(f, "throw {argument}"),
            StmtKind::Global { arguments } => write!(f, "global {}", arguments.iter().join(", ")),
            StmtKind::Import { path, kind } => match kind {
                ImportKind::Simple(alias) => {
                    write!(f, "import {} as {alias}", path.iter().join("::"))
                }
                ImportKind::Nested(v) => write!(
                    f,
                    "import {}::{{{}}}",
                    path.iter().join("::"),
                    v.iter()
                        .map(|(name, alias)| format!("{name} as {alias}"))
                        .join(", ")
                ),
                ImportKind::Glob => write!(f, "import {}::*", path.iter().join("::")),
            },
            StmtKind::Assign { left, right } => write!(f, "{left} = {right}"),
            StmtKind::AssignOp {
                operator,
                left,
                right,
            } => write!(f, "{left} {operator}= {right}"),
            StmtKind::AssignUnpack { left, right } => {
                write!(f, "{} = {right}", left.iter().join(", "))
            }
            StmtKind::AssignMulti { left, right } => {
                write!(
                    f,
                    "{} = {}",
                    left.iter().join(", "),
                    right.iter().join(", ")
                )
            }
            StmtKind::Block(block) => write!(f, "{}", block),
            StmtKind::Expr(expr) => write!(f, "{}", expr),
        }
    }
}

/// A block.
#[derive(Debug, Clone)]
pub struct Block {
    pub body: Vec<Stmt>,
    pub start: Location,
    pub end: Location,
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{{")?;
        for stmt in &self.body {
            writeln!(
                f,
                "{}",
                format!("{}", stmt)
                    .split('\n')
                    .map(|x| format!("    {x}"))
                    .join("\n")
            )?;
        }
        write!(f, "}}")
    }
}

/// An expression.
#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub start: Location,
    pub end: Location,
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
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

/// Kind of expression.
#[derive(Debug, Clone)]
pub enum ExprKind {
    Lit(Box<Lit>),
    Ident(Box<Ident>),
    Function {
        kind: FunctionKind,
        params: Vec<Ident>,
        variadic: Option<Box<Ident>>,
        body: Box<Block>,
    },
    FunctionId(usize),
    Table {
        properties: Vec<TableProperty>,
    },
    List {
        items: Vec<Expr>,
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

impl Display for ExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprKind::Lit(lit) => write!(f, "{lit}"),
            ExprKind::Ident(ident) => write!(f, "{ident}"),
            ExprKind::Function {
                kind,
                params,
                variadic,
                body,
            } => {
                if let Some(variadic) = variadic {
                    match kind {
                        FunctionKind::Funciton => write!(
                            f,
                            "fn ({}, {}) {}",
                            params.iter().join(", "),
                            variadic,
                            body
                        ),
                        FunctionKind::Closure => {
                            write!(f, "|{}, {}| {}", params.iter().join(", "), variadic, body)
                        }
                        FunctionKind::Do => panic!(),
                    }
                } else {
                    match kind {
                        FunctionKind::Funciton => {
                            write!(f, "fn ({}) {}", params.iter().join(", "), body)
                        }
                        FunctionKind::Closure => {
                            write!(f, "|{}| {}", params.iter().join(", "), body)
                        }
                        FunctionKind::Do => write!(f, "do {body}"),
                    }
                }
            }
            ExprKind::FunctionId(id) => write!(f, "<function: {id}>"),
            ExprKind::Table { properties } => {
                if properties.is_empty() {
                    write!(f, "{{}}")
                } else {
                    write!(
                        f,
                        "{{\n{}\n}}",
                        properties
                            .iter()
                            .join(",\n")
                            .split('\n')
                            .map(|x| format!("    {x}"))
                            .join("\n")
                    )
                }
            }
            ExprKind::List { items } => {
                if items.is_empty() {
                    write!(f, "[]")
                } else {
                    write!(
                        f,
                        "[\n{}\n]",
                        items
                            .iter()
                            .join(",\n")
                            .split('\n')
                            .map(|x| format!("    {x}"))
                            .join("\n")
                    )
                }
            }
            ExprKind::Unary { operator, argument } => write!(f, "{operator} {argument}"),
            ExprKind::Binary {
                operator,
                left,
                right,
            } => write!(f, "({left} {operator} {right})"),
            ExprKind::Member {
                table,
                property,
                kind,
                safe,
            } => {
                if *safe {
                    match kind {
                        MemberKind::Bracket => write!(f, "{table}?[{property}]"),
                        MemberKind::Dot => write!(f, "{table}?.{property}"),
                        MemberKind::DoubleColon => write!(f, "{table}?::{property}"),
                    }
                } else {
                    match kind {
                        MemberKind::Bracket => write!(f, "{table}[{property}]"),
                        MemberKind::Dot => write!(f, "{table}.{property}"),
                        MemberKind::DoubleColon => write!(f, "{table}::{property}"),
                    }
                }
            }
            ExprKind::MetaMember { table, safe } => {
                if *safe {
                    write!(f, "{table}?[#]")
                } else {
                    write!(f, "{table}[#]")
                }
            }
            ExprKind::Call {
                callee,
                arguments,
                propagating_error,
            } => {
                if *propagating_error {
                    write!(f, "{callee}({})?", arguments.iter().join(", "))
                } else {
                    write!(f, "{callee}({})", arguments.iter().join(", "))
                }
            }
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

impl Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
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

impl Display for LitKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LitKind::Null => write!(f, "null"),
            LitKind::Bool(v) => write!(f, "{v}"),
            LitKind::Int(v) => write!(f, "{v}"),
            LitKind::Float(v) => write!(f, "{v}"),
            LitKind::Str(v) => write!(f, "\"{}\"", escape_str(v, false)),
        }
    }
}

/// An ident.
#[derive(Debug, Clone)]
pub struct Ident {
    pub name: String,
    pub start: Location,
    pub end: Location,
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

/// Unary operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    /// The `not` operator for logical inversion
    Not,
    /// The `-` operator for negation
    Neg,
}

impl Display for UnOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnOp::Not => write!(f, "not"),
            UnOp::Neg => write!(f, "-"),
        }
    }
}

/// Binary operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Mod => write!(f, "%"),
            BinOp::And => write!(f, "and"),
            BinOp::Or => write!(f, "or"),
            BinOp::Eq => write!(f, "=="),
            BinOp::Lt => write!(f, "<"),
            BinOp::Le => write!(f, "<="),
            BinOp::Ne => write!(f, "!="),
            BinOp::Ge => write!(f, ">"),
            BinOp::Gt => write!(f, ">="),
            BinOp::Is => write!(f, "is"),
        }
    }
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

/// A TableProperty.
#[derive(Debug, Clone)]
pub struct TableProperty {
    pub key: Box<Expr>,
    pub value: Box<Expr>,
    pub start: Location,
    pub end: Location,
}

impl Display for TableProperty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.key, self.value)
    }
}
