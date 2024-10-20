//! The Lucia Abstract Syntax Tree (AST).

use std::{cell::Cell, fmt};

use bumpalo::{
    boxed::Box,
    collections::{String, Vec},
};
use text_size::TextRange;

use crate::utils::{escape_str, Float, Indent, Join};

use super::index::{FunctionId, ScopeId, SymbolId};

/// The root AST node.
#[derive(Debug, PartialEq, Eq)]
pub struct Program<'a> {
    pub function: Box<'a, Function<'a>>,
}

impl fmt::Display for Program<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.function.body)
    }
}

/// A function.
#[derive(Debug, PartialEq, Eq)]
pub struct Function<'a> {
    pub kind: FunctionKind,
    pub params: Vec<'a, TypedIdent<'a>>,
    pub variadic: Option<Box<'a, TypedIdent<'a>>>,
    pub returns: Option<Box<'a, Ty<'a>>>,
    pub throws: Option<Box<'a, Ty<'a>>>,
    pub body: Box<'a, Block<'a>>,
    pub function_id: Cell<Option<FunctionId>>,
}

impl fmt::Display for Function<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params_str = self
            .params
            .iter()
            .map(|param| param.to_string())
            .chain(
                self.variadic
                    .iter()
                    .map(|variadic| format!("*{}", variadic)),
            )
            .join(", ");
        let returns_str = self
            .returns
            .as_ref()
            .map(|returns| format!(" -> {returns}"))
            .unwrap_or_default();
        let throws_str = self
            .throws
            .as_ref()
            .map(|throws| format!(" throw {throws}"))
            .unwrap_or_default();
        match self.kind {
            FunctionKind::Function => write!(
                f,
                "({}){}{} {}",
                params_str, returns_str, throws_str, self.body
            ),
            FunctionKind::Closure => {
                write!(
                    f,
                    "|{}|{}{} {}",
                    params_str, returns_str, throws_str, self.body
                )
            }
            FunctionKind::Do => write!(f, "do {}", self.body),
        }
    }
}

/// Kind of function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum FunctionKind {
    #[default]
    Function,
    Closure,
    Do,
}

/// A block.
#[derive(Debug, PartialEq, Eq)]
pub struct Block<'a> {
    pub body: Vec<'a, Stmt<'a>>,
    pub range: TextRange,
    pub scope_id: Cell<Option<ScopeId>>,
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

/// A statement.
#[derive(Debug, PartialEq, Eq)]
pub struct Stmt<'a> {
    pub kind: StmtKind<'a>,
    pub range: TextRange,
}

impl fmt::Display for Stmt<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl<'a> From<Box<'a, Block<'a>>> for Stmt<'a> {
    fn from(value: Box<'a, Block<'a>>) -> Self {
        Stmt {
            range: value.range,
            kind: StmtKind::Block(value),
        }
    }
}

impl<'a> From<Box<'a, Expr<'a>>> for Stmt<'a> {
    fn from(value: Box<'a, Expr<'a>>) -> Self {
        Stmt {
            range: value.range,
            kind: StmtKind::Expr(value),
        }
    }
}

/// Kind of statement.
#[derive(Debug, PartialEq, Eq)]
pub enum StmtKind<'a> {
    If {
        test: Box<'a, Expr<'a>>,
        consequent: Box<'a, Block<'a>>,
        alternate: Option<Box<'a, Stmt<'a>>>,
    },
    Loop {
        body: Box<'a, Block<'a>>,
    },
    While {
        test: Box<'a, Expr<'a>>,
        body: Box<'a, Block<'a>>,
    },
    For {
        left: Vec<'a, Ident<'a>>,
        right: Box<'a, Expr<'a>>,
        body: Box<'a, Block<'a>>,
    },
    Break,
    Continue,
    Return {
        argument: Box<'a, Expr<'a>>,
    },
    Throw {
        argument: Box<'a, Expr<'a>>,
    },
    Global {
        arguments: Vec<'a, TypedIdent<'a>>,
    },
    Import {
        path: Vec<'a, Ident<'a>>,
        kind: ImportKind<'a>,
    },
    Fn {
        name: Box<'a, Ident<'a>>,
        function: Function<'a>,
    },
    Assign {
        left: AssignLeft<'a>,
        right: Box<'a, Expr<'a>>,
    },
    AssignOp {
        operator: BinOp,
        left: AssignLeft<'a>,
        right: Box<'a, Expr<'a>>,
    },
    AssignUnpack {
        left: Vec<'a, AssignLeft<'a>>,
        right: Box<'a, Expr<'a>>,
    },
    AssignMulti {
        left: Vec<'a, AssignLeft<'a>>,
        right: Vec<'a, Expr<'a>>,
    },
    Block(Box<'a, Block<'a>>),
    Expr(Box<'a, Expr<'a>>),
}

impl fmt::Display for StmtKind<'_> {
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
            StmtKind::Import { path, kind } => {
                write!(f, "import {}{}", path.iter().join("::"), kind)
            }
            StmtKind::Fn { name, function } => {
                write!(f, "fn {}{}", name, function)
            }
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

/// An expression.
#[derive(Debug, PartialEq, Eq)]
pub struct Expr<'a> {
    pub kind: ExprKind<'a>,
    pub range: TextRange,
}

impl fmt::Display for Expr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl<'a> From<Box<'a, Lit<'a>>> for Expr<'a> {
    fn from(value: Box<'a, Lit<'a>>) -> Self {
        Expr {
            range: value.range,
            kind: ExprKind::Lit(value),
        }
    }
}

impl<'a> From<Box<'a, Ident<'a>>> for Expr<'a> {
    fn from(value: Box<'a, Ident<'a>>) -> Self {
        Expr {
            range: value.range,
            kind: ExprKind::Ident(value),
        }
    }
}

/// Kind of expression.
#[derive(Debug, PartialEq, Eq)]
pub enum ExprKind<'a> {
    Lit(Box<'a, Lit<'a>>),
    Ident(Box<'a, Ident<'a>>),
    Function(Function<'a>),
    Table {
        properties: Vec<'a, TableProperty<'a>>,
    },
    List {
        items: Vec<'a, Expr<'a>>,
    },
    Unary {
        operator: UnOp,
        argument: Box<'a, Expr<'a>>,
    },
    Binary {
        operator: BinOp,
        left: Box<'a, Expr<'a>>,
        right: Box<'a, Expr<'a>>,
    },
    Member {
        table: Box<'a, Expr<'a>>,
        property: MemberKind<'a>,
        safe: bool,
    },
    MetaMember {
        table: Box<'a, Expr<'a>>,
        safe: bool,
    },
    Call {
        callee: Box<'a, Expr<'a>>,
        arguments: Vec<'a, Expr<'a>>,
        kind: CallKind,
    },
}

impl fmt::Display for ExprKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprKind::Lit(lit) => write!(f, "{lit}"),
            ExprKind::Ident(ident) => write!(f, "{ident}"),
            ExprKind::Function(function) => {
                if function.kind == FunctionKind::Function {
                    write!(f, "fn {function}")
                } else {
                    write!(f, "{function}")
                }
            }
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
#[derive(Debug, PartialEq, Eq)]
pub struct Lit<'a> {
    pub kind: LitKind<'a>,
    pub range: TextRange,
}

impl fmt::Display for Lit<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

/// Kind of literal.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub enum LitKind<'a> {
    /// "null"
    Null,
    /// "true", "false"
    Bool(bool),
    /// "12", "0o100", "0b110"
    Int(i64),
    /// "12.34", "0b100.100"
    Float(Float),
    /// ""abc"", ""abc"
    Str(&'a String<'a>),
}

impl fmt::Display for LitKind<'_> {
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
#[derive(Debug, PartialEq, Eq)]
pub struct Ident<'a> {
    pub name: &'a String<'a>,
    pub range: TextRange,
    pub symbol_id: Cell<Option<SymbolId>>,
}

impl fmt::Display for Ident<'_> {
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
    pub fn precedence(&self) -> u8 {
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
    Bracket(Box<'a, Expr<'a>>),
    /// `.`
    Dot(Box<'a, Ident<'a>>),
    /// `::`
    DoubleColon(Box<'a, Ident<'a>>),
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
    Simple(Option<Box<'a, Ident<'a>>>),
    /// `import path::{...}`
    Nested(Vec<'a, (Ident<'a>, Option<Ident<'a>>)>),
    /// `import path::*`
    Glob,
}

impl fmt::Display for ImportKind<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ImportKind::Simple(alias) => {
                if let Some(alias) = alias {
                    write!(f, " as {alias}")
                } else {
                    write!(f, "")
                }
            }
            ImportKind::Nested(v) => write!(
                f,
                "::{{{}}}",
                v.iter()
                    .map(|(name, alias)| match alias {
                        Some(alias) => format!("{name} as {alias}"),
                        None => format!("{name}"),
                    })
                    .join(", ")
            ),
            ImportKind::Glob => write!(f, "::*"),
        }
    }
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
    pub key: Expr<'a>,
    pub value: Expr<'a>,
    pub range: TextRange,
}

impl fmt::Display for TableProperty<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.key, self.value)
    }
}

/// The left part of assign.
#[derive(Debug, PartialEq, Eq)]
pub enum AssignLeft<'a> {
    Ident(Box<'a, TypedIdent<'a>>),
    Member {
        table: Box<'a, Expr<'a>>,
        property: MemberKind<'a>,
    },
    MetaMember {
        table: Box<'a, Expr<'a>>,
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
    pub ident: Box<'a, Ident<'a>>,
    pub ty: Option<Ty<'a>>,
    pub range: TextRange,
}

impl fmt::Display for TypedIdent<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(t) = &self.ty {
            write!(f, "{}: {}", self.ident, t)
        } else {
            write!(f, "{}", self.ident)
        }
    }
}

impl<'a> From<Box<'a, Ident<'a>>> for TypedIdent<'a> {
    fn from(value: Box<'a, Ident<'a>>) -> Self {
        TypedIdent {
            range: value.range,
            ident: value,
            ty: None,
        }
    }
}

/// A type.
#[derive(Debug, PartialEq, Eq)]
pub struct Ty<'a> {
    pub kind: TyKind<'a>,
    pub range: TextRange,
}

impl fmt::Display for Ty<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

/// Kind of type.
#[derive(Debug, PartialEq, Eq)]
pub enum TyKind<'a> {
    Lit(Box<'a, Lit<'a>>),
    Ident(Box<'a, Ident<'a>>),
    Table {
        pairs: Vec<'a, (&'a String<'a>, Ty<'a>)>,
        others: Option<Box<'a, (Ty<'a>, Ty<'a>)>>,
    },
    Function {
        params: Vec<'a, Ty<'a>>,
        variadic: Option<Box<'a, Ty<'a>>>,
        returns: Option<Box<'a, Ty<'a>>>,
        throws: Option<Box<'a, Ty<'a>>>,
    },
    Option(Box<'a, Ty<'a>>),
    Union(Vec<'a, Ty<'a>>),
}

impl fmt::Display for TyKind<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TyKind::Lit(lit) => write!(f, "{lit}"),
            TyKind::Ident(ident) => write!(f, "{ident}"),
            TyKind::Table { pairs, others } => {
                write!(
                    f,
                    "{{\n{}\n}}",
                    pairs
                        .iter()
                        .map(|(k, v)| format!("{k}: {v}"))
                        .chain(
                            others
                                .iter()
                                .map(|others| format!("[{}]{}", others.0, others.1)),
                        )
                        .join(",\n")
                        .indent(4)
                )
            }
            TyKind::Function {
                params,
                variadic,
                returns,
                throws,
            } => {
                let params_str = params
                    .iter()
                    .map(|param| param.to_string())
                    .chain(variadic.iter().map(|variadic| format!("*{}", variadic)))
                    .join(", ");
                let returns_str = returns
                    .as_ref()
                    .map(|returns| format!(" -> {returns}"))
                    .unwrap_or_default();
                let throws_str = throws
                    .as_ref()
                    .map(|throws| format!(" throw {throws}"))
                    .unwrap_or_default();
                write!(f, "fn ({}){}{}", params_str, returns_str, throws_str)
            }
            TyKind::Option(t) => write!(f, "({}?)", t),
            TyKind::Union(union) => write!(f, "({})", union.iter().join(" | ")),
        }
    }
}
