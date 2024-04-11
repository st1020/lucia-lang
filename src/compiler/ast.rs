//! The Lucia Abstract Syntax Tree (AST).

use std::fmt;

use crate::utils::{escape_str, Float, Indent, Join, Location};

use super::typing::Type;

/// Kind of function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum FunctionKind {
    #[default]
    Function,
    Closure,
    Do,
}

/// The root AST node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AST {
    pub first_comment: String,
    pub body: Box<Block>,
}

impl fmt::Display for AST {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.first_comment.is_empty() {
            write!(f, "{}", self.body)
        } else {
            write!(f, "//{}\n\n{}", self.first_comment, self.body)
        }
    }
}

/// A statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stmt {
    pub kind: StmtKind,
    pub start: Location,
    pub end: Location,
}

impl fmt::Display for Stmt {
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
#[derive(Debug, Clone, PartialEq, Eq)]
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
        arguments: Vec<TypedIdent>,
    },
    Import {
        path: Vec<Ident>,
        kind: ImportKind,
    },
    Assign {
        left: AssignLeft,
        right: Box<Expr>,
    },
    AssignOp {
        operator: BinOp,
        left: AssignLeft,
        right: Box<Expr>,
    },
    AssignUnpack {
        left: Vec<AssignLeft>,
        right: Box<Expr>,
    },
    AssignMulti {
        left: Vec<AssignLeft>,
        right: Vec<Expr>,
    },
    Block(Box<Block>),
    Expr(Box<Expr>),
}

impl fmt::Display for StmtKind {
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
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Block {
    pub body: Vec<Stmt>,
    pub start: Location,
    pub end: Location,
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{{")?;
        for stmt in &self.body {
            writeln!(f, "{}", stmt.indent(4))?;
        }
        write!(f, "}}")
    }
}

/// An expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expr {
    pub kind: ExprKind,
    pub start: Location,
    pub end: Location,
}

impl fmt::Display for Expr {
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprKind {
    Lit(Box<Lit>),
    Ident(Box<Ident>),
    Function {
        kind: FunctionKind,
        params: Vec<TypedIdent>,
        variadic: Option<Box<TypedIdent>>,
        body: Box<Block>,
        returns: Option<Box<Type>>,
        throws: Option<Box<Type>>,
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
        property: MemberKind,
        safe: bool,
    },
    MetaMember {
        table: Box<Expr>,
        safe: bool,
    },
    Call {
        callee: Box<Expr>,
        arguments: Vec<Expr>,
        kind: CallKind,
    },
}

impl fmt::Display for ExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprKind::Lit(lit) => write!(f, "{lit}"),
            ExprKind::Ident(ident) => write!(f, "{ident}"),
            ExprKind::Function {
                kind,
                params,
                variadic,
                body,
                returns,
                throws,
            } => {
                let params_str = params.iter().join(", ");
                let variadic_str = if let Some(variadic) = variadic {
                    format!(" ,*{}", variadic)
                } else {
                    "".to_string()
                };
                let returns_str = if let Some(returns) = returns {
                    format!(" -> {}", returns)
                } else {
                    "".to_string()
                };
                let throws_str = if let Some(throws) = throws {
                    format!(" throw {}", throws)
                } else {
                    "".to_string()
                };
                match kind {
                    FunctionKind::Function => write!(
                        f,
                        "fn ({}{}){}{} {}",
                        params_str, variadic_str, returns_str, throws_str, body
                    ),
                    FunctionKind::Closure => write!(
                        f,
                        "|{}{}|{}{} {}",
                        params_str, variadic_str, returns_str, throws_str, body
                    ),
                    FunctionKind::Do => write!(f, "do {body}"),
                }
            }
            ExprKind::FunctionId(id) => write!(f, "<function: {id}>"),
            ExprKind::Table { properties } => {
                if properties.is_empty() {
                    write!(f, "{{}}")
                } else {
                    write!(f, "{{\n{}\n}}", properties.iter().join(",\n").indent(4))
                }
            }
            ExprKind::List { items } => {
                if items.is_empty() {
                    write!(f, "[]")
                } else {
                    write!(f, "[\n{}\n]", items.iter().join(",\n").indent(4))
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
                safe,
            } => write!(f, "{table}{}{property}", if *safe { "?" } else { "" }),
            ExprKind::MetaMember { table, safe } => {
                write!(f, "{table}{}[#]", if *safe { "?" } else { "" })
            }
            ExprKind::Call {
                callee,
                arguments,
                kind,
            } => {
                let args = arguments.iter().join(", ");
                match kind {
                    CallKind::None => write!(f, "{callee}({args})"),
                    CallKind::Try => write!(f, "try {callee}({args})"),
                    CallKind::TryOption => write!(f, "try? {callee}({args})"),
                    CallKind::TryPanic => write!(f, "try! {callee}({args})"),
                }
            }
        }
    }
}

/// A literal.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Lit {
    pub value: LitKind,
    pub start: Location,
    pub end: Location,
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

/// Kind of literal.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub enum LitKind {
    /// "null"
    Null,
    /// "true", "false"
    Bool(bool),
    /// "12", "0o100", "0b110"
    Int(i64),
    /// "12.34", "0b100.100"
    Float(Float),
    /// ""abc"", ""abc"
    Str(String),
}

impl fmt::Display for LitKind {
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub name: String,
    pub start: Location,
    pub end: Location,
}

impl fmt::Display for Ident {
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

impl fmt::Display for UnOp {
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
    /// The `%` operator (remainder)
    Rem,
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

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Rem => write!(f, "%"),
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
            BinOp::Rem => 5,

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
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MemberKind {
    /// `[]`
    Bracket(Box<Expr>),
    /// `.`
    Dot(Box<Ident>),
    /// `::`
    DoubleColon(Box<Ident>),
}

impl fmt::Display for MemberKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MemberKind::Bracket(expr) => write!(f, "[{}]", expr),
            MemberKind::Dot(ident) => write!(f, ".{}", ident),
            MemberKind::DoubleColon(ident) => write!(f, "::{}", ident),
        }
    }
}

/// Kind of import statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImportKind {
    /// `import path::xxx as xxx`
    Simple(Box<Ident>),
    /// `import path::{...}`
    Nested(Vec<(Ident, Ident)>),
    /// `import path::*`
    Glob,
}

/// Kind of call expression.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallKind {
    None,
    Try,
    TryOption,
    TryPanic,
}

/// A TableProperty.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TableProperty {
    pub key: Box<Expr>,
    pub value: Box<Expr>,
    pub start: Location,
    pub end: Location,
}

impl fmt::Display for TableProperty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.key, self.value)
    }
}

/// The left part of assign.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssignLeft {
    Ident(Box<TypedIdent>),
    Member {
        table: Box<Expr>,
        property: MemberKind,
    },
    MetaMember {
        table: Box<Expr>,
    },
}

impl fmt::Display for AssignLeft {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AssignLeft::Ident(ident) => write!(f, "{ident}"),
            AssignLeft::Member { table, property } => write!(f, "{table}{property}"),
            AssignLeft::MetaMember { table } => write!(f, "{table}[#]"),
        }
    }
}

impl From<Ident> for AssignLeft {
    fn from(value: Ident) -> Self {
        AssignLeft::Ident(Box::new(TypedIdent::from(value)))
    }
}

impl From<AssignLeft> for Expr {
    fn from(value: AssignLeft) -> Self {
        match value {
            AssignLeft::Ident(ident) => ident.ident.into(),
            AssignLeft::Member { table, property } => Expr {
                start: table.start,
                end: match &property {
                    MemberKind::Bracket(expr) => expr.end,
                    MemberKind::Dot(ident) => ident.end,
                    MemberKind::DoubleColon(ident) => ident.end,
                },
                kind: ExprKind::Member {
                    table,
                    property,
                    safe: false,
                },
            },
            AssignLeft::MetaMember { table } => Expr {
                start: table.start,
                end: table.end,
                kind: ExprKind::MetaMember { table, safe: false },
            },
        }
    }
}

/// The ident with type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedIdent {
    pub ident: Ident,
    pub t: Option<Type>,
}

impl fmt::Display for TypedIdent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(t) = &self.t {
            write!(f, "{}: {}", self.ident, t)
        } else {
            write!(f, "{}", self.ident)
        }
    }
}

impl From<Ident> for TypedIdent {
    fn from(value: Ident) -> Self {
        TypedIdent {
            ident: value,
            t: None,
        }
    }
}
