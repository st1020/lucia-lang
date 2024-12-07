//! The Lucia Abstract Syntax Tree (AST).

use std::{fmt, sync::OnceLock};

use text_size::TextRange;

use crate::utils::{escape_str, Float, Indent, Join, Locatable};

use super::{
    index::{FunctionId, ReferenceId, ScopeId},
    value::ValueType,
};

macro_rules! impl_locatable {
    ($name:ident) => {
        impl<S> Locatable for $name<S> {
            fn range(&self) -> TextRange {
                self.range
            }
        }
    };
}

/// The root AST node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program<S> {
    pub function: Box<Function<S>>,
}

impl<S: AsRef<str>> fmt::Display for Program<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.function.body)
    }
}

/// A function.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function<S> {
    pub name: Option<S>,
    pub kind: FunctionKind,
    pub params: Vec<TypedIdent<S>>,
    pub variadic: Option<Box<TypedIdent<S>>>,
    pub returns: Option<Box<Ty<S>>>,
    pub throws: Option<Box<Ty<S>>>,
    pub body: Box<Block<S>>,
    pub function_id: OnceLock<FunctionId>,
}

impl<S: AsRef<str>> fmt::Display for Function<S> {
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block<S> {
    pub body: Vec<Stmt<S>>,
    pub range: TextRange,
    pub scope_id: OnceLock<ScopeId>,
}

impl_locatable!(Block);

impl<S: AsRef<str>> fmt::Display for Block<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{{")?;
        for stmt in &self.body {
            writeln!(f, "{}", stmt.indent(4))?;
        }
        write!(f, "}}")
    }
}

/// A statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stmt<S> {
    pub kind: StmtKind<S>,
    pub range: TextRange,
}

impl_locatable!(Stmt);

impl<S: AsRef<str>> fmt::Display for Stmt<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl<S> From<Box<Block<S>>> for Stmt<S> {
    fn from(value: Box<Block<S>>) -> Self {
        Stmt {
            range: value.range,
            kind: StmtKind::Block(value),
        }
    }
}

impl<S> From<Box<Expr<S>>> for Stmt<S> {
    fn from(value: Box<Expr<S>>) -> Self {
        Stmt {
            range: value.range,
            kind: StmtKind::Expr(value),
        }
    }
}

/// Kind of statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StmtKind<S> {
    If {
        test: Box<Expr<S>>,
        consequent: Box<Block<S>>,
        alternate: Option<Box<Stmt<S>>>,
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
    Fn {
        glo: Option<TextRange>,
        name: Box<Ident<S>>,
        function: Box<Function<S>>,
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
    Block(Box<Block<S>>),
    Expr(Box<Expr<S>>),
}

impl<S: AsRef<str>> fmt::Display for StmtKind<S> {
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
                    if glo.is_some() { "glo " } else { "" },
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expr<S> {
    pub kind: ExprKind<S>,
    pub range: TextRange,
}

impl_locatable!(Expr);

impl<S: AsRef<str>> fmt::Display for Expr<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl<S> From<Box<Lit<S>>> for Expr<S> {
    fn from(value: Box<Lit<S>>) -> Self {
        Expr {
            range: value.range,
            kind: ExprKind::Lit(value),
        }
    }
}

impl<S> From<Box<Ident<S>>> for Expr<S> {
    fn from(value: Box<Ident<S>>) -> Self {
        Expr {
            range: value.range,
            kind: ExprKind::Ident(value),
        }
    }
}

/// Kind of expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprKind<S> {
    Lit(Box<Lit<S>>),
    Ident(Box<Ident<S>>),
    Paren(Box<Expr<S>>),
    Function(Box<Function<S>>),
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
    MetaMember {
        table: Box<Expr<S>>,
        property: MetaMemberKind,
        safe: Option<TextRange>,
    },
    Call {
        callee: Box<Expr<S>>,
        arguments: Vec<Expr<S>>,
        kind: CallKind,
    },
}

impl<S: AsRef<str>> fmt::Display for ExprKind<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprKind::Lit(lit) => write!(f, "{lit}"),
            ExprKind::Ident(ident) => write!(f, "{ident}"),
            ExprKind::Paren(expr) => write!(f, "({expr})"),
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
            ExprKind::MetaMember {
                table,
                property,
                safe,
            } => {
                write!(
                    f,
                    "{table}{}{property}",
                    if safe.is_some() { "?" } else { "" }
                )
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
pub struct Lit<S> {
    pub kind: LitKind<S>,
    pub range: TextRange,
}

impl_locatable!(Lit);

impl<S: AsRef<str>> fmt::Display for Lit<S> {
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident<S> {
    pub name: S,
    pub range: TextRange,
    pub reference_id: OnceLock<ReferenceId>,
}

impl_locatable!(Ident);

impl<S: AsRef<str>> fmt::Display for Ident<S> {
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
    /// The `is` operator (type check)
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MemberKind<S> {
    /// `[]`
    Bracket(Box<Expr<S>>),
    /// `.`
    Dot(Box<Ident<S>>),
    /// `::`
    DoubleColon(Box<Ident<S>>),
}

impl<S: AsRef<str>> fmt::Display for MemberKind<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MemberKind::Bracket(expr) => write!(f, "[{}]", expr),
            MemberKind::Dot(ident) => write!(f, ".{}", ident),
            MemberKind::DoubleColon(ident) => write!(f, "::{}", ident),
        }
    }
}

/// Kind of meta member expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MetaMemberKind {
    /// `[]`
    Bracket,
    /// `.`
    Dot,
    /// `::`
    DoubleColon,
}

impl fmt::Display for MetaMemberKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MetaMemberKind::Bracket => write!(f, "[#]"),
            MetaMemberKind::Dot => write!(f, ".#"),
            MetaMemberKind::DoubleColon => write!(f, "::#"),
        }
    }
}

/// Kind of import statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImportKind<S> {
    /// `import path::xxx as xxx`
    Simple(Option<Box<Ident<S>>>),
    /// `import path::{...}`
    Nested(Vec<ImportItem<S>>),
    /// `import path::*`
    Glob,
}

impl<S: AsRef<str>> fmt::Display for ImportKind<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ImportKind::Simple(alias) => {
                if let Some(alias) = alias {
                    write!(f, " as {alias}")
                } else {
                    write!(f, "")
                }
            }
            ImportKind::Nested(v) => write!(f, "::{{{}}}", v.iter().join(", ")),
            ImportKind::Glob => write!(f, "::*"),
        }
    }
}

/// An import item.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportItem<S> {
    pub name: Ident<S>,
    pub alias: Option<Ident<S>>,
    pub range: TextRange,
}

impl_locatable!(ImportItem);

impl<S: AsRef<str>> fmt::Display for ImportItem<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.alias {
            Some(alias) => write!(f, "{} as {}", self.name, alias),
            None => write!(f, "{}", self.name),
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TableProperty<S> {
    pub key: Expr<S>,
    pub value: Expr<S>,
    pub range: TextRange,
}

impl_locatable!(TableProperty);

impl<S: AsRef<str>> fmt::Display for TableProperty<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.key, self.value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssignLeft<S> {
    pub kind: AssignLeftKind<S>,
    pub range: TextRange,
}

impl_locatable!(AssignLeft);

impl<S: AsRef<str>> fmt::Display for AssignLeft<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

/// The left part of assign.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssignLeftKind<S> {
    Ident(Box<TypedIdent<S>>),
    Member {
        table: Box<Expr<S>>,
        property: MemberKind<S>,
    },
    MetaMember {
        table: Box<Expr<S>>,
        property: MetaMemberKind,
    },
}

impl<S: AsRef<str>> fmt::Display for AssignLeftKind<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AssignLeftKind::Ident(ident) => write!(f, "{ident}"),
            AssignLeftKind::Member { table, property } => write!(f, "{table}{property}"),
            AssignLeftKind::MetaMember { table, property } => write!(f, "{table}{property}"),
        }
    }
}

/// The ident with type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedIdent<S> {
    pub ident: Box<Ident<S>>,
    pub ty: Option<Ty<S>>,
    pub range: TextRange,
}

impl_locatable!(TypedIdent);

impl<S: AsRef<str>> fmt::Display for TypedIdent<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(t) = &self.ty {
            write!(f, "{}: {}", self.ident, t)
        } else {
            write!(f, "{}", self.ident)
        }
    }
}

impl<S> From<Box<Ident<S>>> for TypedIdent<S> {
    fn from(value: Box<Ident<S>>) -> Self {
        TypedIdent {
            range: value.range,
            ident: value,
            ty: None,
        }
    }
}

/// A match case.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MatchCase<S> {
    pub patterns: Vec<Pattern<S>>,
    pub body: Box<Block<S>>,
    pub range: TextRange,
}

impl_locatable!(MatchCase);

impl<S: AsRef<str>> fmt::Display for MatchCase<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} => {}", self.patterns.iter().join(" | "), self.body)
    }
}

/// A pattern.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pattern<S> {
    pub kind: PatternKind<S>,
    pub range: TextRange,
}

impl_locatable!(Pattern);

impl<S: AsRef<str>> fmt::Display for Pattern<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

/// Kind of pattern.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PatternKind<S> {
    Lit(Box<Lit<S>>),
    Ident(Box<Ident<S>>),
    Table {
        pairs: Vec<TablePatternPair<S>>,
        others: Option<TextRange>,
    },
    List {
        items: Vec<Pattern<S>>,
        others: Option<TextRange>,
    },
}

impl<S: AsRef<str>> fmt::Display for PatternKind<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PatternKind::Lit(lit) => write!(f, "{}", lit),
            PatternKind::Ident(ident) => write!(f, "{}", ident),
            PatternKind::Table { pairs, others } => write!(
                f,
                "{{{}{}}}",
                pairs.iter().join(", "),
                if others.is_some() { ", ..." } else { "" }
            ),
            PatternKind::List { items, others } => write!(
                f,
                "[{}{}]",
                items.iter().join(", "),
                if others.is_some() { ", ..." } else { "" }
            ),
        }
    }
}

/// A table pattern pair.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TablePatternPair<S> {
    pub key: Lit<S>,
    pub value: Pattern<S>,
    pub range: TextRange,
}

impl_locatable!(TablePatternPair);

impl<S: AsRef<str>> fmt::Display for TablePatternPair<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.key, self.value)
    }
}

/// A type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ty<S> {
    pub kind: TyKind<S>,
    pub range: TextRange,
}

impl_locatable!(Ty);

impl<S: AsRef<str>> fmt::Display for Ty<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

/// Kind of type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyKind<S> {
    Lit(Box<Lit<S>>),
    Ident(Box<Ident<S>>),
    Paren(Box<Ty<S>>),
    Table {
        pairs: Vec<TableTyPair<S>>,
        others: Option<Box<TableTyOther<S>>>,
    },
    Function {
        params: Vec<Ty<S>>,
        variadic: Option<Box<Ty<S>>>,
        returns: Option<Box<Ty<S>>>,
        throws: Option<Box<Ty<S>>>,
    },
    Option(Box<Ty<S>>),
    Union(Vec<Ty<S>>),
}

impl<S: AsRef<str>> fmt::Display for TyKind<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TyKind::Lit(lit) => write!(f, "{lit}"),
            TyKind::Ident(ident) => write!(f, "{ident}"),
            TyKind::Paren(ty) => write!(f, "({ty})"),
            TyKind::Table { pairs, others } => match (pairs.len(), others) {
                (0, None) => write!(f, "{{}}"),
                (0, Some(others)) => write!(f, "{{{}}}", others),
                (_, None) => write!(f, "{{{}}}", pairs.iter().join(", ")),
                (_, Some(others)) => write!(f, "{{{}, {}}}", pairs.iter().join(", "), others),
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
            TyKind::Option(ty) => write!(f, "{}?", ty),
            TyKind::Union(union) => write!(f, "{}", union.iter().join(" | ")),
        }
    }
}

/// A table type pair.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TableTyPair<S> {
    pub key: Ident<S>,
    pub value: Ty<S>,
    pub range: TextRange,
}

impl_locatable!(TableTyPair);

impl<S: AsRef<str>> fmt::Display for TableTyPair<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.key, self.value)
    }
}

/// A table type other.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TableTyOther<S> {
    pub key: Ty<S>,
    pub value: Ty<S>,
    pub range: TextRange,
}

impl_locatable!(TableTyOther);

impl<S: AsRef<str>> fmt::Display for TableTyOther<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]: {}", self.key, self.value)
    }
}
