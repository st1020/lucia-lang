//! The Lucia Abstract Syntax Tree (AST).

use std::{cell::Cell, fmt, marker::PhantomData};

use bumpalo::{boxed::Box, collections::Vec};
use text_size::TextRange;

use crate::utils::{escape_str, Float, Indent, Join};

use super::{
    index::{FunctionId, ScopeId, SymbolId},
    value::ValueType,
};

/// The root AST node.
#[derive(Debug, PartialEq, Eq)]
pub struct Program<'a, S> {
    pub function: Box<'a, Function<'a, S>>,
}

impl<S: AsRef<str>> fmt::Display for Program<'_, S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.function.body)
    }
}

/// A function.
#[derive(Debug, PartialEq, Eq)]
pub struct Function<'a, S> {
    pub name: Option<S>,
    pub kind: FunctionKind,
    pub params: Vec<'a, TypedIdent<'a, S>>,
    pub variadic: Option<Box<'a, TypedIdent<'a, S>>>,
    pub returns: Option<Box<'a, Ty<'a, S>>>,
    pub throws: Option<Box<'a, Ty<'a, S>>>,
    pub body: Box<'a, Block<'a, S>>,
    pub function_id: Cell<Option<FunctionId>>,
}

impl<S: AsRef<str>> fmt::Display for Function<'_, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params_str = self
            .params
            .iter()
            .map(|param| param.to_string())
            .chain(
                self.variadic
                    .iter()
                    .map(|variadic| format!("...{}", variadic)),
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

impl fmt::Display for FunctionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionKind::Function => write!(f, "Function"),
            FunctionKind::Closure => write!(f, "Closure"),
            FunctionKind::Do => write!(f, "Do"),
        }
    }
}

/// A block.
#[derive(Debug, PartialEq, Eq)]
pub struct Block<'a, S> {
    pub body: Vec<'a, Stmt<'a, S>>,
    pub range: TextRange,
    pub scope_id: Cell<Option<ScopeId>>,
}

impl<S: AsRef<str>> fmt::Display for Block<'_, S> {
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
pub struct Stmt<'a, S> {
    pub kind: StmtKind<'a, S>,
    pub range: TextRange,
}

impl<S: AsRef<str>> fmt::Display for Stmt<'_, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl<'a, S> From<Box<'a, Block<'a, S>>> for Stmt<'a, S> {
    fn from(value: Box<'a, Block<'a, S>>) -> Self {
        Stmt {
            range: value.range,
            kind: StmtKind::Block(value),
        }
    }
}

impl<'a, S> From<Box<'a, Expr<'a, S>>> for Stmt<'a, S> {
    fn from(value: Box<'a, Expr<'a, S>>) -> Self {
        Stmt {
            range: value.range,
            kind: StmtKind::Expr(value),
        }
    }
}

/// Kind of statement.
#[derive(Debug, PartialEq, Eq)]
pub enum StmtKind<'a, S> {
    If {
        test: Box<'a, Expr<'a, S>>,
        consequent: Box<'a, Block<'a, S>>,
        alternate: Option<Box<'a, Stmt<'a, S>>>,
    },
    Match {
        expr: Box<'a, Expr<'a, S>>,
        cases: Vec<'a, MatchCase<'a, S>>,
    },
    Loop {
        body: Box<'a, Block<'a, S>>,
    },
    While {
        test: Box<'a, Expr<'a, S>>,
        body: Box<'a, Block<'a, S>>,
    },
    For {
        left: Vec<'a, Ident<'a, S>>,
        right: Box<'a, Expr<'a, S>>,
        body: Box<'a, Block<'a, S>>,
    },
    Break,
    Continue,
    Return {
        argument: Box<'a, Expr<'a, S>>,
    },
    Throw {
        argument: Box<'a, Expr<'a, S>>,
    },
    Import {
        path: Vec<'a, Ident<'a, S>>,
        path_str: S,
        kind: ImportKind<'a, S>,
    },
    Fn {
        glo: bool,
        name: Box<'a, Ident<'a, S>>,
        function: Function<'a, S>,
    },
    GloAssign {
        left: Box<'a, TypedIdent<'a, S>>,
        right: Box<'a, Expr<'a, S>>,
    },
    Assign {
        left: AssignLeft<'a, S>,
        right: Box<'a, Expr<'a, S>>,
    },
    AssignOp {
        operator: BinOp,
        left: AssignLeft<'a, S>,
        right: Box<'a, Expr<'a, S>>,
    },
    AssignUnpack {
        left: Vec<'a, AssignLeft<'a, S>>,
        right: Box<'a, Expr<'a, S>>,
    },
    AssignMulti {
        left: Vec<'a, AssignLeft<'a, S>>,
        right: Vec<'a, Expr<'a, S>>,
    },
    Block(Box<'a, Block<'a, S>>),
    Expr(Box<'a, Expr<'a, S>>),
}

impl<S: AsRef<str>> fmt::Display for StmtKind<'_, S> {
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
            StmtKind::Match { expr, cases } => write!(
                f,
                "match {expr} {{\n{}\n}}",
                cases.iter().join("\n").indent(4)
            ),
            StmtKind::Loop { body } => write!(f, "loop {body}"),
            StmtKind::While { test, body } => write!(f, "while {test} {body}"),
            StmtKind::For { left, right, body } => {
                write!(f, "for {} in {right} {body}", left.iter().join(", "))
            }
            StmtKind::Break => write!(f, "break"),
            StmtKind::Continue => write!(f, "continue"),
            StmtKind::Return { argument } => write!(f, "return {argument}"),
            StmtKind::Throw { argument } => write!(f, "throw {argument}"),
            StmtKind::Import {
                path,
                path_str: _,
                kind,
            } => {
                write!(f, "import {}{}", path.iter().join("::"), kind)
            }
            StmtKind::Fn {
                glo,
                name,
                function,
            } => {
                write!(
                    f,
                    "{}fn {}{}",
                    if *glo { "glo " } else { "" },
                    name,
                    function
                )
            }
            StmtKind::GloAssign { left, right } => {
                write!(f, "glo {left} = {right}")
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
pub struct Expr<'a, S> {
    pub kind: ExprKind<'a, S>,
    pub range: TextRange,
}

impl<S: AsRef<str>> fmt::Display for Expr<'_, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl<'a, S> From<Box<'a, Lit<'a, S>>> for Expr<'a, S> {
    fn from(value: Box<'a, Lit<'a, S>>) -> Self {
        Expr {
            range: value.range,
            kind: ExprKind::Lit(value),
        }
    }
}

impl<'a, S> From<Box<'a, Ident<'a, S>>> for Expr<'a, S> {
    fn from(value: Box<'a, Ident<'a, S>>) -> Self {
        Expr {
            range: value.range,
            kind: ExprKind::Ident(value),
        }
    }
}

/// Kind of expression.
#[derive(Debug, PartialEq, Eq)]
pub enum ExprKind<'a, S> {
    Lit(Box<'a, Lit<'a, S>>),
    Ident(Box<'a, Ident<'a, S>>),
    Function(Function<'a, S>),
    Table {
        properties: Vec<'a, TableProperty<'a, S>>,
    },
    List {
        items: Vec<'a, Expr<'a, S>>,
    },
    Unary {
        operator: UnOp,
        argument: Box<'a, Expr<'a, S>>,
    },
    Binary {
        operator: BinOp,
        left: Box<'a, Expr<'a, S>>,
        right: Box<'a, Expr<'a, S>>,
    },
    TypeCheck {
        left: Box<'a, Expr<'a, S>>,
        right: ValueType,
    },
    Member {
        table: Box<'a, Expr<'a, S>>,
        property: MemberKind<'a, S>,
        safe: bool,
    },
    MetaMember {
        table: Box<'a, Expr<'a, S>>,
        safe: bool,
    },
    Call {
        callee: Box<'a, Expr<'a, S>>,
        arguments: Vec<'a, Expr<'a, S>>,
        kind: CallKind,
    },
}

impl<S: AsRef<str>> fmt::Display for ExprKind<'_, S> {
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
            ExprKind::TypeCheck { left, right } => write!(f, "{left} is {right}"),
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
pub struct Lit<'a, S> {
    pub kind: LitKind<S>,
    pub range: TextRange,
    pub marker: PhantomData<&'a ()>,
}

impl<S: AsRef<str>> fmt::Display for Lit<'_, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

/// Kind of literal.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Hash)]
pub enum LitKind<S> {
    /// "null"
    Null,
    /// "true", "false"
    Bool(bool),
    /// "12", "0o100", "0b110"
    Int(i64),
    /// "12.34", "0b100.100"
    Float(Float),
    /// ""abc"", ""abc"
    Str(S),
}

impl<S: AsRef<str>> fmt::Display for LitKind<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LitKind::Null => write!(f, "null"),
            LitKind::Bool(v) => write!(f, "{v}"),
            LitKind::Int(v) => write!(f, "{v}"),
            LitKind::Float(v) => write!(f, "{v:?}"),
            LitKind::Str(v) => write!(f, "\"{}\"", escape_str(v.as_ref(), false)),
        }
    }
}

/// An ident.
#[derive(Debug, PartialEq, Eq)]
pub struct Ident<'a, S> {
    pub name: S,
    pub range: TextRange,
    pub symbol_id: Cell<Option<SymbolId>>,
    pub marker: PhantomData<&'a ()>,
}

impl<S: AsRef<str>> fmt::Display for Ident<'_, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name.as_ref())
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
    /// The `!=` operator (not equal to)
    Ne,
    /// The `<` operator (less than)
    Lt,
    /// The `<=` operator (less than or equal to)
    Le,
    /// The `>` operator (greater than)
    Gt,
    /// The `>=` operator (greater than or equal to)
    Ge,
    /// The '===' operator (identical)
    Identical,
    /// The '!==' operator (not identical)
    NotIdentical,
    /// The `as` operator (type check)
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
            BinOp::Ne => write!(f, "!="),
            BinOp::Lt => write!(f, "<"),
            BinOp::Le => write!(f, "<="),
            BinOp::Gt => write!(f, ">"),
            BinOp::Ge => write!(f, ">="),
            BinOp::Identical => write!(f, "==="),
            BinOp::NotIdentical => write!(f, "!=="),
            BinOp::Is => write!(f, "is"),
        }
    }
}

impl BinOp {
    pub fn precedence(&self) -> u8 {
        match self {
            BinOp::Mul | BinOp::Div | BinOp::Rem => 5,
            BinOp::Add | BinOp::Sub => 4,
            BinOp::Eq
            | BinOp::Ne
            | BinOp::Lt
            | BinOp::Le
            | BinOp::Gt
            | BinOp::Ge
            | BinOp::Identical
            | BinOp::NotIdentical
            | BinOp::Is => 3,
            BinOp::And => 2,
            BinOp::Or => 1,
        }
    }
}

/// Kind of member expression.
#[derive(Debug, PartialEq, Eq)]
pub enum MemberKind<'a, S> {
    /// `[]`
    Bracket(Box<'a, Expr<'a, S>>),
    /// `.`
    Dot(Box<'a, Ident<'a, S>>),
    /// `::`
    DoubleColon(Box<'a, Ident<'a, S>>),
}

impl<S: AsRef<str>> fmt::Display for MemberKind<'_, S> {
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
pub enum ImportKind<'a, S> {
    /// `import path::xxx as xxx`
    Simple(Option<Box<'a, Ident<'a, S>>>),
    /// `import path::{...}`
    Nested(Vec<'a, (Ident<'a, S>, Option<Ident<'a, S>>)>),
    /// `import path::*`
    Glob,
}

impl<S: AsRef<str>> fmt::Display for ImportKind<'_, S> {
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
pub struct TableProperty<'a, S> {
    pub key: Expr<'a, S>,
    pub value: Expr<'a, S>,
    pub range: TextRange,
}

impl<S: AsRef<str>> fmt::Display for TableProperty<'_, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.key, self.value)
    }
}

/// The left part of assign.
#[derive(Debug, PartialEq, Eq)]
pub enum AssignLeft<'a, S> {
    Ident(Box<'a, TypedIdent<'a, S>>),
    Member {
        table: Box<'a, Expr<'a, S>>,
        property: MemberKind<'a, S>,
    },
    MetaMember {
        table: Box<'a, Expr<'a, S>>,
    },
}

impl<S: AsRef<str>> fmt::Display for AssignLeft<'_, S> {
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
pub struct TypedIdent<'a, S> {
    pub ident: Box<'a, Ident<'a, S>>,
    pub ty: Option<Ty<'a, S>>,
    pub range: TextRange,
}

impl<S: AsRef<str>> fmt::Display for TypedIdent<'_, S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(t) = &self.ty {
            write!(f, "{}: {}", self.ident, t)
        } else {
            write!(f, "{}", self.ident)
        }
    }
}

impl<'a, S> From<Box<'a, Ident<'a, S>>> for TypedIdent<'a, S> {
    fn from(value: Box<'a, Ident<'a, S>>) -> Self {
        TypedIdent {
            range: value.range,
            ident: value,
            ty: None,
        }
    }
}

/// A match case.
#[derive(Debug, PartialEq, Eq)]
pub struct MatchCase<'a, S> {
    pub patterns: Patterns<'a, S>,
    pub body: Box<'a, Block<'a, S>>,
    pub range: TextRange,
}

impl<S: AsRef<str>> fmt::Display for MatchCase<'_, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} => {}", self.patterns, self.body)
    }
}

/// Patterns.
#[derive(Debug, PartialEq, Eq)]
pub struct Patterns<'a, S> {
    pub patterns: Vec<'a, Pattern<'a, S>>,
    pub range: TextRange,
}

impl<S: AsRef<str>> fmt::Display for Patterns<'_, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.patterns.iter().join(" | "))
    }
}

/// A pattern.
#[derive(Debug, PartialEq, Eq)]
pub struct Pattern<'a, S> {
    pub kind: PatternKind<'a, S>,
    pub range: TextRange,
}

impl<S: AsRef<str>> fmt::Display for Pattern<'_, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

/// Kind of pattern.
#[derive(Debug, PartialEq, Eq)]
pub enum PatternKind<'a, S> {
    Lit(Box<'a, Lit<'a, S>>),
    Ident(Box<'a, Ident<'a, S>>),
    Table {
        pairs: Vec<'a, (Lit<'a, S>, Pattern<'a, S>)>,
        others: bool,
    },
    List {
        items: Vec<'a, Pattern<'a, S>>,
        others: bool,
    },
}

impl<S: AsRef<str>> fmt::Display for PatternKind<'_, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PatternKind::Lit(lit) => write!(f, "{}", lit),
            PatternKind::Ident(ident) => write!(f, "{}", ident),
            PatternKind::Table { pairs, others } => write!(
                f,
                "{{{}{}}}",
                pairs
                    .iter()
                    .map(|(k, v)| { format!("{k}: {v}") })
                    .join(", "),
                if *others { ", ..." } else { "" }
            ),
            PatternKind::List { items, others } => write!(
                f,
                "[{}{}]",
                items.iter().join(", "),
                if *others { ", ..." } else { "" }
            ),
        }
    }
}

/// A type.
#[derive(Debug, PartialEq, Eq)]
pub struct Ty<'a, S> {
    pub kind: TyKind<'a, S>,
    pub range: TextRange,
}

impl<S: AsRef<str>> fmt::Display for Ty<'_, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

/// Kind of type.
#[derive(Debug, PartialEq, Eq)]
pub enum TyKind<'a, S> {
    Lit(Box<'a, Lit<'a, S>>),
    Ident(Box<'a, Ident<'a, S>>),
    Table {
        pairs: Vec<'a, (S, Ty<'a, S>)>,
        others: Option<Box<'a, (Ty<'a, S>, Ty<'a, S>)>>,
    },
    Function {
        params: Vec<'a, Ty<'a, S>>,
        variadic: Option<Box<'a, Ty<'a, S>>>,
        returns: Option<Box<'a, Ty<'a, S>>>,
        throws: Option<Box<'a, Ty<'a, S>>>,
    },
    Option(Box<'a, Ty<'a, S>>),
    Union(Vec<'a, Ty<'a, S>>),
}

impl<S: AsRef<str>> fmt::Display for TyKind<'_, S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TyKind::Lit(lit) => write!(f, "{lit}"),
            TyKind::Ident(ident) => write!(f, "{ident}"),
            TyKind::Table { pairs, others } => match (pairs.len(), others) {
                (0, None) => write!(f, "{{}}"),
                (0, Some(others)) => write!(f, "{{[{}]: {}}}", others.0, others.1),
                (_, None) => write!(
                    f,
                    "{{{}}}",
                    pairs
                        .iter()
                        .map(|(name, t)| format!("{}: {}", name.as_ref(), t))
                        .join(", ")
                ),
                (_, Some(others)) => write!(
                    f,
                    "{{{}, [{}]: {}}}",
                    pairs
                        .iter()
                        .map(|(name, t)| format!("{}: {}", name.as_ref(), t))
                        .join(", "),
                    others.0,
                    others.1
                ),
            },
            TyKind::Function {
                params,
                variadic,
                returns,
                throws,
            } => {
                let params_str = params
                    .iter()
                    .map(|param| param.to_string())
                    .chain(variadic.iter().map(|variadic| format!("...{}", variadic)))
                    .join(", ");
                let returns_str = returns
                    .as_ref()
                    .map(|returns| format!(" -> {returns}"))
                    .unwrap_or_default();
                let throws_str = throws
                    .as_ref()
                    .map(|throws| format!(" throw {throws}"))
                    .unwrap_or_default();
                write!(f, "fn({}){}{}", params_str, returns_str, throws_str)
            }
            TyKind::Option(t) => write!(f, "({}?)", t),
            TyKind::Union(union) => write!(f, "({})", union.iter().join(" | ")),
        }
    }
}
