mod generated;

pub use generated::SyntaxKind;

impl From<u16> for SyntaxKind {
    #[inline]
    fn from(d: u16) -> SyntaxKind {
        assert!(d <= (SyntaxKind::__LAST as u16));
        unsafe { std::mem::transmute::<u16, SyntaxKind>(d) }
    }
}

impl From<SyntaxKind> for u16 {
    #[inline]
    fn from(k: SyntaxKind) -> u16 {
        k as u16
    }
}

impl SyntaxKind {
    #[inline]
    pub fn is_trivia(self) -> bool {
        matches!(
            self,
            SyntaxKind::LineComment | SyntaxKind::BlockComment | SyntaxKind::Whitespace
        )
    }
}

pub type SyntaxNode = rowan::SyntaxNode<LuciaLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<LuciaLanguage>;
pub type SyntaxElement = rowan::SyntaxElement<LuciaLanguage>;
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<LuciaLanguage>;
pub type SyntaxElementChildren = rowan::SyntaxElementChildren<LuciaLanguage>;

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum LuciaLanguage {}

impl rowan::Language for LuciaLanguage {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        SyntaxKind::from(raw.0)
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind.into())
    }
}
