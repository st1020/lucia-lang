use bumpalo::Bump;

use crate::compiler_new::ast;

use super::*;

#[derive(Debug)]
pub struct Database<'a> {
    arena: &'a Bump,
}

impl<'a> Database<'a> {
    fn lower_stmt(&mut self, ast: ast::Stmt) -> Stmt<'a> {
        match ast {
            ast::Stmt::Block(_) => todo!(),
            ast::Stmt::IfStmt(_) => todo!(),
            ast::Stmt::LoopStmt(_) => todo!(),
            ast::Stmt::WhileStmt(_) => todo!(),
            ast::Stmt::ForStmt(_) => todo!(),
            ast::Stmt::BreakStmt(_) => todo!(),
            ast::Stmt::ContinueStmt(_) => todo!(),
            ast::Stmt::ReturnStmt(_) => todo!(),
            ast::Stmt::ThrowStmt(_) => todo!(),
            ast::Stmt::GlobalStmt(_) => todo!(),
            ast::Stmt::ImportStmt(_) => todo!(),
            ast::Stmt::FnStmt(_) => todo!(),
            ast::Stmt::AssignStmt(_) => todo!(),
            ast::Stmt::AssignOpStmt(_) => todo!(),
            ast::Stmt::AssignUnpackStmt(_) => todo!(),
            ast::Stmt::AssignMultiStmt(_) => todo!(),
            ast::Stmt::Expr(_) => todo!(),
        }
    }
}
