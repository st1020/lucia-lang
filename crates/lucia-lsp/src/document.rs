use std::{fmt::Display, hash::Hash};

use lucia_lang::compiler::{
    analyzer::analyze,
    ast::Program,
    codegen::gen_code,
    error::CompilerError,
    interning::StringInterner,
    lexer::tokenize,
    parser::parse,
    semantic::{Reference, Semantic, Symbol},
    token::Token,
};
use ropey::Rope;

#[derive(Debug)]
pub struct Document<S> {
    pub rope: Rope,
    pub tokens: Vec<Token>,
    pub ast: Program<S>,
    pub semantic: Semantic<S>,
}

impl<S: Clone + Eq + Hash + Display> Document<S> {
    pub fn build<I: StringInterner<String = S>>(
        text: &str,
        interner: I,
    ) -> (Self, Vec<CompilerError>) {
        let rope = Rope::from_str(text);
        let tokens = tokenize(text).collect();
        let (ast, parser_errors) = parse(interner, text);
        let (semantic, analyzer_errors) = analyze(&ast);
        let (_code, codegen_errors) = gen_code(&ast, &semantic);
        (
            Self {
                rope,
                tokens,
                ast,
                semantic,
            },
            parser_errors
                .into_iter()
                .chain(analyzer_errors)
                .chain(codegen_errors)
                .collect(),
        )
    }

    pub fn get_symbol_from_reference_offset(&self, offset: usize) -> Option<&Symbol<S>> {
        let reference = self
            .semantic
            .references
            .iter()
            .find(|x| x.range.contains(offset.try_into().unwrap()))?;

        Some(&self.semantic.symbols[reference.symbol_id])
    }

    pub fn get_references(&self, offset: usize) -> Option<impl Iterator<Item = &Reference>> {
        let symbol = self.get_symbol_from_reference_offset(offset)?;
        Some(
            symbol
                .references
                .iter()
                .copied()
                .map(|reference_id| &self.semantic.references[reference_id]),
        )
    }
}
