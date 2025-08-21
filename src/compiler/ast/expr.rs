use std::{fmt, sync::OnceLock};

use itertools::Itertools;
use text_size::TextRange;

use crate::{
    compiler::{index::ScopeId, value::ValueType},
    utils::Indent,
};

use super::*;

/// An expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expr<S> {
    pub kind: ExprKind<S>,
    pub range: TextRange,
}

impl_locatable!(Expr);

impl<S: AsRef<str>> fmt::Display for Expr<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

/// Kind of expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprKind<S> {
    Lit(Box<Lit<S>>),
    Ident(Box<Ident<S>>),
    Paren(Box<Expr<S>>),
    Block(Box<Block<S>>),
    Fn {
        glo: Option<TextRange>,
        name: Option<Box<Ident<S>>>,
        function: Box<Function<S>>,
    },
    Table {
        properties: Vec<TableProperty<S>>,
    },
    List {
        items: Vec<Expr<S>>,
    },
    Unary {
        operator: UnOp,
        argument: Box<Expr<S>>,
    },
    Binary {
        operator: BinOp,
        left: Box<Expr<S>>,
        right: Box<Expr<S>>,
    },
    TypeCheck {
        left: Box<Expr<S>>,
        right: ValueType,
    },
    Member {
        table: Box<Expr<S>>,
        property: MemberKind<S>,
        safe: Option<TextRange>,
    },
    Call {
        callee: Box<Expr<S>>,
        arguments: Vec<Expr<S>>,
        kind: CallKind,
    },
    If {
        test: Box<Expr<S>>,
        consequent: Box<Block<S>>,
        alternate: Option<Box<Expr<S>>>,
    },
    Match {
        expr: Box<Expr<S>>,
        cases: Vec<MatchCase<S>>,
    },
    Loop {
        body: Box<Block<S>>,
    },
    While {
        test: Box<Expr<S>>,
        body: Box<Block<S>>,
    },
    For {
        left: Vec<Ident<S>>,
        right: Box<Expr<S>>,
        body: Box<Block<S>>,
    },
    Break,
    Continue,
    Return {
        argument: Box<Expr<S>>,
    },
    Throw {
        argument: Box<Expr<S>>,
    },
    Import {
        path: Vec<Ident<S>>,
        path_str: S,
        kind: ImportKind<S>,
    },
    GloAssign {
        left: Box<TypedIdent<S>>,
        right: Box<Expr<S>>,
    },
    Assign {
        left: AssignLeft<S>,
        right: Box<Expr<S>>,
    },
    AssignOp {
        operator: BinOp,
        left: AssignLeft<S>,
        right: Box<Expr<S>>,
    },
    AssignUnpack {
        left: Vec<AssignLeft<S>>,
        right: Box<Expr<S>>,
    },
    AssignMulti {
        left: Vec<AssignLeft<S>>,
        right: Vec<Expr<S>>,
    },
}

impl<S: AsRef<str>> fmt::Display for ExprKind<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExprKind::Lit(lit) => write!(f, "{lit}"),
            ExprKind::Ident(ident) => write!(f, "{ident}"),
            ExprKind::Paren(expr) => write!(f, "({expr})"),
            ExprKind::Block(block) => write!(f, "{block}"),
            ExprKind::Fn {
                glo,
                name,
                function,
            } => {
                write!(
                    f,
                    "{}{}{}{}",
                    if glo.is_some() { "glo " } else { "" },
                    if function.kind == FunctionKind::Function {
                        "fn "
                    } else {
                        ""
                    },
                    if let Some(name) = name {
                        format!("{name} ")
                    } else {
                        String::new()
                    },
                    function
                )
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
            ExprKind::Unary { operator, argument } => write!(f, "{operator}{argument}"),
            ExprKind::Binary {
                operator,
                left,
                right,
            } => write!(f, "{left} {operator} {right}"),
            ExprKind::TypeCheck { left, right } => write!(f, "{left} is {right}"),
            ExprKind::Member {
                table,
                property,
                safe,
            } => write!(
                f,
                "{table}{}{property}",
                if safe.is_some() { "?" } else { "" }
            ),
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
            ExprKind::If {
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
            ExprKind::Match { expr, cases } => write!(
                f,
                "match {expr} {{\n{}\n}}",
                cases.iter().join("\n").indent(4)
            ),
            ExprKind::Loop { body } => write!(f, "loop {body}"),
            ExprKind::While { test, body } => write!(f, "while {test} {body}"),
            ExprKind::For { left, right, body } => {
                write!(f, "for {} in {right} {body}", left.iter().join(", "))
            }
            ExprKind::Break => write!(f, "break"),
            ExprKind::Continue => write!(f, "continue"),
            ExprKind::Return { argument } => write!(f, "return {argument}"),
            ExprKind::Throw { argument } => write!(f, "throw {argument}"),
            ExprKind::Import {
                path,
                path_str: _,
                kind,
            } => write!(f, "import {}{}", path.iter().join("::"), kind),
            ExprKind::GloAssign { left, right } => write!(f, "glo {left} = {right}"),
            ExprKind::Assign { left, right } => write!(f, "{left} = {right}"),
            ExprKind::AssignOp {
                operator,
                left,
                right,
            } => write!(f, "{left} {operator}= {right}"),
            ExprKind::AssignUnpack { left, right } => {
                write!(f, "{} = {right}", left.iter().join(", "))
            }
            ExprKind::AssignMulti { left, right } => write!(
                f,
                "{} = {}",
                left.iter().join(", "),
                right.iter().join(", ")
            ),
        }
    }
}

/// A TableProperty.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TableProperty<S> {
    pub key: Expr<S>,
    pub value: Expr<S>,
    pub range: TextRange,
}

impl_locatable!(TableProperty);

impl<S: AsRef<str>> fmt::Display for TableProperty<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.key, self.value)
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

/// A match case.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MatchCase<S> {
    pub patterns: Vec<Pattern<S>>,
    pub body: Box<Expr<S>>,
    pub range: TextRange,
    pub scope_id: OnceLock<ScopeId>,
}

impl_locatable!(MatchCase);

impl<S: AsRef<str>> fmt::Display for MatchCase<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} => {}", self.patterns.iter().join(" | "), self.body)
    }
}
