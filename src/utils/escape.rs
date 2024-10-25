use std::{fmt, str::Chars};

use thiserror::Error;

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

pub(crate) fn unescape_str<F, T: From<char> + From<u8>>(
    src: &str,
    callback: &mut F,
) -> Result<(), EscapeError>
where
    F: FnMut(T),
{
    let mut chars = src.chars();
    while let Some(c) = chars.next() {
        let res = match c {
            '\\' => match chars.clone().next() {
                Some('\n') => {
                    chars.next();
                    continue;
                }
                _ => scan_escape::<T>(&mut chars),
            },
            '"' => Err(EscapeError::EscapeOnlyChar),
            '\r' => Err(EscapeError::BareCarriageReturn),
            _ => Ok(T::from(c)),
        };
        callback(res?);
    }
    Ok(())
}

pub(crate) fn scan_escape<T: From<char> + From<u8>>(
    chars: &mut Chars<'_>,
) -> Result<T, EscapeError> {
    // Previous character was '\\', unescape what follows.
    let res: char = match chars.next().ok_or(EscapeError::LoneSlash)? {
        '"' => '"',
        'n' => '\n',
        'r' => '\r',
        't' => '\t',
        '\\' => '\\',
        '\'' => '\'',
        '0' => '\0',
        'x' => {
            // Parse hexadecimal character code.

            let hi = chars.next().ok_or(EscapeError::TooShortHexEscape)?;
            let hi = hi.to_digit(16).ok_or(EscapeError::InvalidCharInHexEscape)?;

            let lo = chars.next().ok_or(EscapeError::TooShortHexEscape)?;
            let lo = lo.to_digit(16).ok_or(EscapeError::InvalidCharInHexEscape)?;

            let value = (hi * 16 + lo) as u8;

            return if !value.is_ascii() {
                Err(EscapeError::OutOfRangeHexEscape)
            } else {
                // This may be a high byte, but that will only happen if `T` is
                // `MixedUnit`, because of the `allow_high_bytes` check above.
                Ok(T::from(value))
            };
        }
        'u' => return scan_unicode(chars).map(T::from),
        _ => return Err(EscapeError::InvalidEscape),
    };
    Ok(T::from(res))
}

fn scan_unicode(chars: &mut Chars<'_>) -> Result<char, EscapeError> {
    // We've parsed '\u', now we have to parse '{..}'.

    if chars.next() != Some('{') {
        return Err(EscapeError::NoBraceInUnicodeEscape);
    }

    // First character must be a hexadecimal digit.
    let mut n_digits = 1;
    let mut value: u32 = match chars.next().ok_or(EscapeError::UnclosedUnicodeEscape)? {
        '_' => return Err(EscapeError::LeadingUnderscoreUnicodeEscape),
        '}' => return Err(EscapeError::EmptyUnicodeEscape),
        c => c
            .to_digit(16)
            .ok_or(EscapeError::InvalidCharInUnicodeEscape)?,
    };

    // First character is valid, now parse the rest of the number
    // and closing brace.
    loop {
        match chars.next() {
            None => return Err(EscapeError::UnclosedUnicodeEscape),
            Some('_') => continue,
            Some('}') => {
                if n_digits > 6 {
                    return Err(EscapeError::OverlongUnicodeEscape);
                }

                break std::char::from_u32(value).ok_or({
                    if value > 0x10FFFF {
                        EscapeError::OutOfRangeUnicodeEscape
                    } else {
                        EscapeError::LoneSurrogateUnicodeEscape
                    }
                });
            }
            Some(c) => {
                let digit: u32 = c
                    .to_digit(16)
                    .ok_or(EscapeError::InvalidCharInUnicodeEscape)?;
                n_digits += 1;
                if n_digits > 6 {
                    // Stop updating value since we're sure that it's incorrect already.
                    continue;
                }
                value = value * 16 + digit;
            }
        };
    }
}

/// Errors and warnings that can occur during string unescaping.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Error)]
pub enum EscapeError {
    /// Escaped '\' character without continuation.
    LoneSlash,
    /// Invalid escape character (e.g. '\z').
    InvalidEscape,
    /// Raw '\r' encountered.
    BareCarriageReturn,
    /// Unescaped character that was expected to be escaped (e.g. raw '\t').
    EscapeOnlyChar,

    /// Numeric character escape is too short (e.g. '\x1').
    TooShortHexEscape,
    /// Invalid character in numeric escape (e.g. '\xz')
    InvalidCharInHexEscape,
    /// Character code in numeric escape is non-ascii (e.g. '\xFF').
    OutOfRangeHexEscape,

    /// '\u' not followed by '{'.
    NoBraceInUnicodeEscape,
    /// Non-hexadecimal value in '\u{..}'.
    InvalidCharInUnicodeEscape,
    /// '\u{}'
    EmptyUnicodeEscape,
    /// No closing brace in '\u{..}', e.g. '\u{12'.
    UnclosedUnicodeEscape,
    /// '\u{_12}'
    LeadingUnderscoreUnicodeEscape,
    /// More than 6 characters in '\u{..}', e.g. '\u{10FFFF_FF}'
    OverlongUnicodeEscape,
    /// Invalid in-bound unicode character code, e.g. '\u{DFFF}'.
    LoneSurrogateUnicodeEscape,
    /// Out of bounds unicode character code, e.g. '\u{FFFFFF}'.
    OutOfRangeUnicodeEscape,
}

impl fmt::Display for EscapeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}
