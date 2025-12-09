use ropey::Rope;
use text_size::TextRange;
use tower_lsp_server::ls_types::{Position, Range};

pub trait OffsetPosition {
    fn offset_to_position(&self, offset: usize) -> Option<Position>;
    fn position_to_offset(&self, position: Position) -> Option<usize>;
    fn text_range_to_range(&self, range: TextRange) -> Option<Range>;
}

impl OffsetPosition for Rope {
    fn offset_to_position(&self, offset: usize) -> Option<Position> {
        let line = self.try_char_to_line(offset).ok()?;
        let first_char_of_line = self.try_line_to_char(line).ok()?;
        let column = offset - first_char_of_line;
        Some(Position::new(line as u32, column as u32))
    }

    fn position_to_offset(&self, position: Position) -> Option<usize> {
        let line_char_offset = self.try_line_to_char(position.line as usize).ok()?;
        let slice = self.slice(0..line_char_offset + position.character as usize);
        Some(slice.len_bytes())
    }

    fn text_range_to_range(&self, range: TextRange) -> Option<Range> {
        let start = self.offset_to_position(range.start().into())?;
        let end = self.offset_to_position(range.end().into())?;
        Some(Range::new(start, end))
    }
}
