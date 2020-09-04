use core::ops::Range;
use core::str::Chars;

pub fn unescape_string(quoted: &str) -> Option<String> {
  let content = find_quoted_content(quoted)?;
  let str_content = &quoted[content.range];
  let mut unescaped: String = String::with_capacity(str_content.len());
  let mut has_error = false;
  // TODO: Return iterator, so we can stop at the first error
  unescape_string_content(
    str_content,
    content.quotes,
    &mut |_, unescaped_char| match unescaped_char {
      Ok(c) => unescaped.push(c),
      Err(_) => has_error = true,
    },
  );
  if has_error {
    None
  } else {
    Some(unescaped)
  }
}

fn find_quoted_content(quoted: &str) -> Option<QuotedContent> {
  let chars = quoted.char_indices();
  let (first_idx, first_char) = chars.clone().next()?;
  let (last_idx, last_char) = chars.clone().next_back()?;
  if first_idx == last_idx {
    // There is only one character in `quoted`
    return None;
  }
  debug_assert!(first_idx < last_idx);
  match (first_char, last_char) {
    ('"', '"') => Some(QuotedContent {
      quotes: QuoteKind::Double,
      range: first_char.len_utf8()..last_idx,
    }),
    ('\'', '\'') => Some(QuotedContent {
      quotes: QuoteKind::Single,
      range: first_char.len_utf8()..last_idx,
    }),
    _ => None,
  }
}

#[derive(Debug, Eq, PartialEq, Clone)]
struct QuotedContent {
  quotes: QuoteKind,
  range: Range<usize>,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum QuoteKind {
  Single,
  Double,
}

fn unescape_string_content<F>(str_content: &str, quotes: QuoteKind, callback: &mut F)
  where
    F: FnMut(Range<usize>, Result<char, UnescapeError>),
{
  let content_len: usize = str_content.len();
  let mut chars = str_content.chars();
  loop {
    let start = content_len - chars.as_str().len();
    let unescaped_char = match chars.next() {
      None => break,
      Some(first_char) => unescape_char(first_char, &mut chars, quotes),
    };
    let end = content_len - chars.as_str().len();
    debug_assert!(start < end);
    callback(start..end, unescaped_char);
  }
}

fn unescape_char(first_char: char, chars: &mut Chars, quotes: QuoteKind) -> Result<char, UnescapeError> {
  // TODO: Support escaped line terminator?
  // TODO: Support line separator and paragraph separator?
  // let first_char = chars.next();
  match first_char {
    '"' if quotes == QuoteKind::Double => Err(UnescapeError::EscapeOnlyChar),
    '\'' if quotes == QuoteKind::Single => Err(UnescapeError::EscapeOnlyChar),
    '\\' => match chars.next() {
      None => Err(UnescapeError::LoneSlash),
      Some('\'') => Ok('\''),
      Some('"') => Ok('"'),
      Some('\\') => Ok('\\'),
      Some('b') => Ok('\x08'),
      Some('f') => Ok('\x0c'),
      Some('n') => Ok('\n'),
      Some('r') => Ok('\r'),
      Some('t') => Ok('\t'),
      Some('v') => Ok('\x0b'),
      Some('x') => unimplemented!("UnescapeHexSequence"),
      Some('u') => unimplemented!("UnescapeUnicodeSequence"),
      Some('0') => unimplemented!("UnescapeNulOrOctal"),
      Some(_) => unimplemented!("UnescapeNonEscapeChar"),
    },
    _ => Ok(first_char),
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
enum UnescapeError {
  LoneSlash,
  /// Found a non-escaped character that can only appear as escaped
  /// For example `"` must always be escaped inside a double-quoted string literal
  EscapeOnlyChar,
}
