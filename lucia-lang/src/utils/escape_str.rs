/// Escape lucia str.
///
/// The exact rules are:
///
/// * Tab is escaped as `\t`.
/// * Carriage return is escaped as `\r`.
/// * Line feed is escaped as `\n`.
/// * Backslash is escaped as `\\`.
/// * Single quote is escaped as `\'`.
/// * Double quote is escaped as `\"`.
/// * Any character in the 'printable ASCII' range `0x20` .. `0x7e`
///   inclusive is not escaped.
/// * If `ascii_only` is true, all other characters are given
///   hexadecimal Unicode escapes.
pub fn escape_str(value: &str, ascii_only: bool) -> String {
    let mut ans = String::new();
    for c in value.chars() {
        match c {
            '\0' => ans.push_str("\\0"),
            '\t' => ans.push_str("\\t"),
            '\r' => ans.push_str("\\r"),
            '\n' => ans.push_str("\\n"),
            '\\' => ans.push_str("\\\\"),
            '"' => ans.push_str("\\\""),
            '\'' => ans.push_str("\\\'"),
            '\x20'..='\x7e' if ascii_only => ans.push(c),
            _ if ascii_only => c.escape_default().for_each(|c| ans.push(c)),
            _ => ans.push(c),
        }
    }
    ans
}
