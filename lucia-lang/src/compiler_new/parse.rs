use rowan::GreenNode;

use super::{
    lexer::tokenize,
    parser::{ParseError, Parser},
    sink::Sink,
    syntax::SyntaxNode,
};

pub struct Parse {
    pub green_node: GreenNode,
    pub errors: Vec<ParseError>,
}

impl Parse {
    pub fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green_node.clone())
    }
}

pub fn parse(input: &str) -> Parse {
    let tokens: Vec<_> = tokenize(input).collect();
    let events = Parser::new(&mut tokens.iter()).parse();
    let parse = Sink::new(&mut tokens.iter(), events).finish();
    parse
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_temp() {
        let input = "a = 1
";
        println!("{:#?}", parse(input).syntax());
    }
}
