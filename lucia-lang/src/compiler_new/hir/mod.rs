//! The Lucia Abstract Syntax Tree (AST).

use std::fmt;

use smol_str::SmolStr;

use crate::utils::{escape_str, Float, Indent, Join};

pub mod db;
pub mod typing;

use typing::Type;

/// Kind of function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum FunctionKind {
    #[default]
    Function,
    Closure,
    Do,
}

/// The root Hir node.
#[derive(Debug, PartialEq, Eq)]
pub struct Hir<'a> {
    pub first_comment: SmolStr,
    pub body: &'a mut Block<'a>,
}

impl fmt::Display for Hir<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.first_comment.is_empty() {
            write!(f, "{}", self.body)
        } else {
            write!(f, "//{}\n\n{}", self.first_comment, self.body)
        }
    }
}

/// A statement.
#[derive(Debug, PartialEq, Eq)]
pub enum Stmt<'a> {
    If {
        test: &'a mut Expr<'a>,
        consequent: &'a mut Block<'a>,
        alternate: Option<&'a mut Stmt<'a>>,
    },
    Loop {
        body: &'a mut Block<'a>,
    },
    While {
        test: &'a mut Expr<'a>,
        body: &'a mut Block<'a>,
    },
    For {
        left: Vec<Ident>,
        right: &'a mut Expr<'a>,
        body: &'a mut Block<'a>,
    },
    Break,
    Continue,
    Return {
        argument: &'a mut Expr<'a>,
    },
    Throw {
        argument: &'a mut Expr<'a>,
    },
    Global {
        arguments: Vec<TypedIdent<'a>>,
    },
    Import {
        path: Vec<Ident>,
        kind: ImportKind<'a>,
    },
    Assign {
        left: AssignLeft<'a>,
        right: &'a mut Expr<'a>,
    },
    AssignOp {
        operator: BinOp,
        left: AssignLeft<'a>,
        right: &'a mut Expr<'a>,
    },
    AssignUnpack {
        left: Vec<AssignLeft<'a>>,
        right: &'a mut Expr<'a>,
    },
    AssignMulti {
        left: Vec<AssignLeft<'a>>,
        right: Vec<Expr<'a>>,
    },
    Block(&'a mut Block<'a>),
    Expr(&'a mut Expr<'a>),
}

impl fmt::Display for Stmt<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::If {
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
            Stmt::Loop { body } => write!(f, "loop {body}"),
            Stmt::While { test, body } => write!(f, "while {test} {body}"),
            Stmt::For { left, right, body } => {
                write!(f, "for {} in {right} {body}", left.iter().join(", "))
            }
            Stmt::Break => write!(f, "break"),
            Stmt::Continue => write!(f, "continue"),
            Stmt::Return { argument } => write!(f, "return {argument}"),
            Stmt::Throw { argument } => write!(f, "throw {argument}"),
            Stmt::Global { arguments } => write!(f, "global {}", arguments.iter().join(", ")),
            Stmt::Import { path, kind } => match kind {
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
            Stmt::Assign { left, right } => write!(f, "{left} = {right}"),
            Stmt::AssignOp {
                operator,
                left,
                right,
            } => write!(f, "{left} {operator}= {right}"),
            Stmt::AssignUnpack { left, right } => {
                write!(f, "{} = {right}", left.iter().join(", "))
            }
            Stmt::AssignMulti { left, right } => {
                write!(
                    f,
                    "{} = {}",
                    left.iter().join(", "),
                    right.iter().join(", ")
                )
            }
            Stmt::Block(block) => write!(f, "{}", block),
            Stmt::Expr(expr) => write!(f, "{}", expr),
        }
    }
}

/// A block.
#[derive(Debug, PartialEq, Eq, Default)]
pub struct Block<'a> {
    pub body: Vec<Stmt<'a>>,
}

impl fmt::Display for Block<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{{")?;
        for stmt in &self.body {
            writeln!(f, "{}", stmt.indent(4))?;
        }
        write!(f, "}}")
    }
}

/// An expression.
#[derive(Debug, PartialEq, Eq)]
pub enum Expr<'a> {
    Lit(&'a mut Lit),
    Ident(&'a mut Ident),
    Function {
        kind: FunctionKind,
        params: Vec<TypedIdent<'a>>,
        variadic: Option<&'a mut TypedIdent<'a>>,
        body: &'a mut Block<'a>,
        returns: Option<&'a mut Type>,
        throws: Option<&'a mut Type>,
    },
    FunctionId(usize),
    Table {
        properties: Vec<TableProperty<'a>>,
    },
    List {
        items: Vec<Expr<'a>>,
    },
    Unary {
        operator: UnOp,
        argument: &'a mut Expr<'a>,
    },
    Binary {
        operator: BinOp,
        left: &'a mut Expr<'a>,
        right: &'a mut Expr<'a>,
    },
    Member {
        table: &'a mut Expr<'a>,
        property: MemberKind<'a>,
        safe: bool,
    },
    MetaMember {
        table: &'a mut Expr<'a>,
        safe: bool,
    },
    Call {
        callee: &'a mut Expr<'a>,
        arguments: Vec<Expr<'a>>,
        kind: CallKind,
    },
}

impl fmt::Display for Expr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Lit(lit) => write!(f, "{lit}"),
            Expr::Ident(ident) => write!(f, "{ident}"),
            Expr::Function {
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
            Expr::FunctionId(id) => write!(f, "<function: {id}>"),
            Expr::Table { properties } => {
                if properties.is_empty() {
                    write!(f, "{{}}")
                } else {
                    write!(f, "{{\n{}\n}}", properties.iter().join(",\n").indent(4))
                }
            }
            Expr::List { items } => {
                if items.is_empty() {
                    write!(f, "[]")
                } else {
                    write!(f, "[\n{}\n]", items.iter().join(",\n").indent(4))
                }
            }
            Expr::Unary { operator, argument } => write!(f, "{operator} {argument}"),
            Expr::Binary {
                operator,
                left,
                right,
            } => write!(f, "({left} {operator} {right})"),
            Expr::Member {
                table,
                property,
                safe,
            } => write!(f, "{table}{}{property}", if *safe { "?" } else { "" }),
            Expr::MetaMember { table, safe } => {
                write!(f, "{table}{}[#]", if *safe { "?" } else { "" })
            }
            Expr::Call {
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub enum Lit {
    /// "null"
    Null,
    /// "true", "false"
    Bool(bool),
    /// "12", "0o100", "0b110"
    Int(i64),
    /// "12.34", "0b100.100"
    Float(Float),
    /// ""abc"", ""abc"
    Str(SmolStr),
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lit::Null => write!(f, "null"),
            Lit::Bool(v) => write!(f, "{v}"),
            Lit::Int(v) => write!(f, "{v}"),
            Lit::Float(v) => write!(f, "{v}"),
            Lit::Str(v) => write!(f, "\"{}\"", escape_str(v, false)),
        }
    }
}

/// An ident.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub name: SmolStr,
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
#[derive(Debug, PartialEq, Eq)]
pub enum MemberKind<'a> {
    /// `[]`
    Bracket(&'a mut Expr<'a>),
    /// `.`
    Dot(&'a mut Ident),
    /// `::`
    DoubleColon(&'a mut Ident),
}

impl fmt::Display for MemberKind<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MemberKind::Bracket(expr) => write!(f, "[{}]", expr),
            MemberKind::Dot(ident) => write!(f, ".{}", ident),
            MemberKind::DoubleColon(ident) => write!(f, "::{}", ident),
        }
    }
}

/// Kind of import statement.
#[derive(Debug, PartialEq, Eq)]
pub enum ImportKind<'a> {
    /// `import path::xxx as xxx`
    Simple(&'a mut Ident),
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
#[derive(Debug, PartialEq, Eq)]
pub struct TableProperty<'a> {
    pub key: &'a mut Expr<'a>,
    pub value: &'a mut Expr<'a>,
}

impl fmt::Display for TableProperty<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.key, self.value)
    }
}

/// The left part of assign.
#[derive(Debug, PartialEq, Eq)]
pub enum AssignLeft<'a> {
    Ident(&'a mut TypedIdent<'a>),
    Member {
        table: &'a mut Expr<'a>,
        property: MemberKind<'a>,
    },
    MetaMember {
        table: &'a mut Expr<'a>,
    },
}

impl fmt::Display for AssignLeft<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AssignLeft::Ident(ident) => write!(f, "{ident}"),
            AssignLeft::Member { table, property } => write!(f, "{table}{property}"),
            AssignLeft::MetaMember { table } => write!(f, "{table}[#]"),
        }
    }
}

/// The ident with type.
#[derive(Debug, PartialEq, Eq)]
pub struct TypedIdent<'a> {
    pub ident: &'a mut Ident,
    pub t: Option<Type>,
}

impl fmt::Display for TypedIdent<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(t) = &self.t {
            write!(f, "{}: {}", self.ident, t)
        } else {
            write!(f, "{}", self.ident)
        }
    }
}
