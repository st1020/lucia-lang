use lucia_lang::compiler::token::TokenKind;
use tower_lsp::lsp_types::SemanticTokenType;

pub const TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::PARAMETER,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::FUNCTION,
    SemanticTokenType::KEYWORD,
    SemanticTokenType::MODIFIER,
    SemanticTokenType::COMMENT,
    SemanticTokenType::STRING,
    SemanticTokenType::NUMBER,
    SemanticTokenType::OPERATOR,
];

pub trait IntoSemanticTokenType {
    fn into_semantic_token_type(self) -> Option<SemanticTokenType>;
}

impl IntoSemanticTokenType for TokenKind {
    fn into_semantic_token_type(self) -> Option<SemanticTokenType> {
        match self {
            TokenKind::LineComment | TokenKind::BlockComment => Some(SemanticTokenType::COMMENT),
            TokenKind::Whitespace => None,
            TokenKind::Ident => Some(SemanticTokenType::VARIABLE),
            TokenKind::Int | TokenKind::Float => Some(SemanticTokenType::NUMBER),
            TokenKind::Str | TokenKind::RawStr => Some(SemanticTokenType::STRING),
            TokenKind::If
            | TokenKind::Else
            | TokenKind::Match
            | TokenKind::Loop
            | TokenKind::While
            | TokenKind::For
            | TokenKind::In
            | TokenKind::Break
            | TokenKind::Continue
            | TokenKind::Return
            | TokenKind::Throw
            | TokenKind::Glo
            | TokenKind::Import
            | TokenKind::As
            | TokenKind::Is
            | TokenKind::Not
            | TokenKind::And
            | TokenKind::Or
            | TokenKind::Try
            | TokenKind::Fn
            | TokenKind::Do
            | TokenKind::Null
            | TokenKind::True
            | TokenKind::False => Some(SemanticTokenType::KEYWORD),
            TokenKind::Identical
            | TokenKind::NotIdentical
            | TokenKind::Ellipsis
            | TokenKind::DoubleColon
            | TokenKind::Arrow
            | TokenKind::FatArrow
            | TokenKind::Eq
            | TokenKind::NotEq
            | TokenKind::LtEq
            | TokenKind::GtEq
            | TokenKind::AddAssign
            | TokenKind::SubAssign
            | TokenKind::MulAssign
            | TokenKind::DivAssign
            | TokenKind::RemAssign
            | TokenKind::Comma
            | TokenKind::Dot
            | TokenKind::OpenParen
            | TokenKind::CloseParen
            | TokenKind::OpenBrace
            | TokenKind::CloseBrace
            | TokenKind::OpenBracket
            | TokenKind::CloseBracket
            | TokenKind::Pound
            | TokenKind::Question
            | TokenKind::Exclamation
            | TokenKind::Colon
            | TokenKind::Assign
            | TokenKind::Lt
            | TokenKind::Gt
            | TokenKind::VBar
            | TokenKind::Add
            | TokenKind::Sub
            | TokenKind::Mul
            | TokenKind::Div
            | TokenKind::Rem => Some(SemanticTokenType::OPERATOR),
            TokenKind::Eol | TokenKind::Eof => None,
            TokenKind::Unknown
            | TokenKind::UnterminatedBlockComment
            | TokenKind::EmptyInt
            | TokenKind::EmptyExponentFloat
            | TokenKind::NonDecimalFloat
            | TokenKind::UnterminatedStr => None,
        }
    }
}
