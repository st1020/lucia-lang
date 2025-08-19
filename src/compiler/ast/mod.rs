//! The Lucia Abstract Syntax Tree (AST).

mod assign;
mod block;
mod expr;
mod function;
mod ident;
mod import;
mod lit;
mod member;
mod operator;
mod pattern;
mod program;
mod ty;

pub use assign::*;
pub use block::*;
pub use expr::*;
pub use function::*;
pub use ident::*;
pub use import::*;
pub use lit::*;
pub use member::*;
pub use operator::*;
pub use pattern::*;
pub use program::*;
pub use ty::*;

macro_rules! impl_locatable {
    ($name:ident) => {
        impl<S> $crate::utils::Locatable for $name<S> {
            fn range(&self) -> text_size::TextRange {
                self.range
            }
        }
    };
}

use impl_locatable;

/// Traverse the syntax tree.
pub trait Visit<S> {
    type Return;

    fn visit_program(&mut self, program: &Program<S>) -> Self::Return;
    fn visit_function(&mut self, function: &Function<S>) -> Self::Return;
    fn visit_block(&mut self, block: &Block<S>) -> Self::Return;
    fn visit_expr(&mut self, expr: &Expr<S>) -> Self::Return;
    fn visit_assign_left(&mut self, assign_left: &AssignLeft<S>) -> Self::Return;
    fn visit_pattern(&mut self, pattern: &Pattern<S>) -> Self::Return;
    fn visit_member_property(&mut self, property: &MemberKind<S>) -> Self::Return;
    fn visit_lit(&mut self, lit: &Lit<S>) -> Self::Return;
    fn visit_ident(&mut self, ident: &Ident<S>) -> Self::Return;
    fn visit_typed_ident(&mut self, ident: &TypedIdent<S>) -> Self::Return;
    fn visit_ty(&mut self, ty: &Ty<S>) -> Self::Return;
}
