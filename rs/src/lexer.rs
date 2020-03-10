use std::str::Chars;

use crate::types::syntax::{SyntaxKind, SyntaxToken};

// #[derive(Debug, Clone)]
// pub struct TokenIterator<'text> {
//   text: &'text str,
// }
//
// impl<'text> Iterator for TokenIterator {
//   type Item = SyntaxToken<'text>,
// }

pub fn tokenize(mut text: &str) -> Vec<SyntaxToken> {
  let mut tokens = Vec::new();
  while let Some(token) = next_token(text) {
    text = &text[token.text.len()..];
    tokens.push(token);
  }
  tokens
}

fn next_token(input: &str) -> Option<SyntaxToken> {
  let input_len = input.len();
  let mut chars = input.chars();
  let chars = &mut chars;
  let first = match chars.next() {
    None => return None,
    Some(c) => c
  };
  let kind: SyntaxKind = match first {
    '/' => match chars.next() {
      Some('/') => end_trailing_comment(chars),
      _ => unimplemented!()
    },
    c if is_id_start(c) => end_id_or_keyword(c, chars),
    c if is_whitespace(c) => end_whitespace(c, chars),
    ';' => SyntaxKind::Semicolon,
    '(' => SyntaxKind::OpenParen,
    ')' => SyntaxKind::CloseParen,
    '"' => end_double_quoted_string(chars),
    c => {
      unimplemented!("Tokens starting with: {:?}", c)
    }
  };
  let token_len = input_len - chars.as_str().len();
  Some(SyntaxToken { kind, text: &input[..token_len] })
}

/// Consumes a trailing comment.
/// The starting `//` must already be consumed.
fn end_trailing_comment(chars: &mut Chars) -> SyntaxKind {
  loop {
    match chars.next() {
      None => break,
      Some(c) if is_line_terminator_sequence_start(c) => {
        end_line_terminator_sequence(c, chars);
        break;
      }
      _ => {}
    }
  }
  SyntaxKind::TrailingComment
}

fn end_double_quoted_string(chars: &mut Chars) -> SyntaxKind {
  // TODO: Handle line terminators
  loop {
    match chars.next() {
      None => return SyntaxKind::Error,
      Some('"') => return SyntaxKind::StrLit,
      Some('\\') => {
        chars.next(); // Skip next char
      }
      _ => {}
    }
  }
}

/// Consumes an identifier or keyword
// TODO: Simplify this function!
#[allow(clippy::cognitive_complexity)]
fn end_id_or_keyword(first: char, chars: &mut Chars) -> SyntaxKind {
  debug_assert!(is_id_start(first));
  match first {
    't' => {
      match chars.next() {
        Some('h') => {
          match chars.next() {
            Some('i') => {
              match chars.next() {
                Some('s') => {
                  let old_chars = chars.clone();
                  match chars.next() {
                    Some(c) if is_id_continue(c) => end_id(chars),
                    _ => {
                      *chars = old_chars;
                      SyntaxKind::ThisKw
                    }
                  }
                }
                Some(c) if is_id_continue(c) => end_id(chars),
                _ => SyntaxKind::Identifier,
              }
            }
            Some('r') => {
              match chars.next() {
                Some('o') => {
                  match chars.next() {
                    Some('w') => {
                      let old_chars = chars.clone();
                      match chars.next() {
                        Some(c) if is_id_continue(c) => end_id(chars),
                        _ => {
                          *chars = old_chars;
                          SyntaxKind::ThrowKw
                        }
                      }
                    }
                    Some(c) if is_id_continue(c) => end_id(chars),
                    _ => SyntaxKind::Identifier,
                  }
                }
                Some(c) if is_id_continue(c) => end_id(chars),
                _ => SyntaxKind::Identifier,
              }
            }
            Some(c) if is_id_continue(c) => end_id(chars),
            _ => SyntaxKind::Identifier,
          }
        }
        Some('r') => {
          match chars.next() {
            Some('u') => {
              match chars.next() {
                Some('e') => {
                  let old_chars = chars.clone();
                  match chars.next() {
                    Some(c) if is_id_continue(c) => end_id(chars),
                    _ => {
                      *chars = old_chars;
                      SyntaxKind::TrueKw
                    }
                  }
                }
                Some(c) if is_id_continue(c) => end_id(chars),
                _ => SyntaxKind::Identifier,
              }
            }
            Some('y') => {
              let old_chars = chars.clone();
              match chars.next() {
                Some(c) if is_id_continue(c) => end_id(chars),
                _ => {
                  *chars = old_chars;
                  SyntaxKind::TryKw
                }
              }
            }
            Some(c) if is_id_continue(c) => end_id(chars),
            _ => SyntaxKind::Identifier,
          }
        }
        Some(c) if is_id_continue(c) => end_id(chars),
        _ => SyntaxKind::Identifier,
      }
    }
    _ => unimplemented!(),
  }
}

/// Ends an identifier
fn end_id(chars: &mut Chars) -> SyntaxKind {
  loop {
    let old_chars = chars.clone();
    match chars.next() {
      Some(c) if is_id_continue(c) => {}
      _ => {
        *chars = old_chars;
        break; }
    }
  }
  SyntaxKind::Identifier
}

fn end_whitespace(first: char, chars: &mut Chars) -> SyntaxKind {
  debug_assert!(is_whitespace(first));
  let mut multiline = if is_line_terminator_sequence_start(first) {
    end_line_terminator_sequence(first, chars);
    true
  } else {
    false
  };

  loop {
    let old_chars = chars.clone();
    match chars.next() {
      Some(c) if is_line_terminator_sequence_start(c) => {
        end_line_terminator_sequence(c, chars);
        multiline = true;
      }
      Some(c) if is_whitespace(c) => {},
      _ => {
        *chars = old_chars;
        break; }
    }
  }

  if multiline { SyntaxKind::MultilineWhitespace } else { SyntaxKind::UnilineWhitespace }
}

fn end_line_terminator_sequence(first: char, chars: &mut Chars) {
  debug_assert!(is_line_terminator_sequence_start(first));
  if first == '\r' {
    let old_chars = chars.clone();
    match chars.next() {
      Some('\n') => {},
      _ => {*chars = old_chars;},
    };
  }
}

fn is_whitespace(c: char) -> bool {
  c == '\n' || c == '\r' || c == ' '
}

fn is_line_terminator_sequence_start(c: char) -> bool {
  c == '\n' || c == '\r'
}

fn is_id_start(c: char) -> bool {
  match c {
    'a'..='z' | 'A'..='Z' | '$' | '_' => true,
    _ => false,
  }
}

fn is_id_continue(c: char) -> bool {
  match c {
    'a'..='z' | 'A'..='Z' | '0'..='9' | '$' | '_' => true,
    _ => false,
  }
}

#[cfg(test)]
mod parser_tests {
  use ::test_generator::test_resources;
  use std::path::Path;
  use crate::lexer::tokenize;
  use crate::types::syntax::{SyntaxToken, SyntaxKind};

  #[test_resources("../tests/as2/[!.]*/*/")]
  fn test_lex_as2(path: &str) {
    let path: &Path = Path::new(path);
    let _name = path
      .components()
      .last()
      .unwrap()
      .as_os_str()
      .to_str()
      .expect("Failed to retrieve sample name");

    //    if name == "hello-world" || name == "homestuck-beta2" {
    //      return;
    //    }

    let as2_path = path.join("main.as2");
    let as2_text: String = ::std::fs::read_to_string(as2_path).expect("Failed to read input");

    let tokens = tokenize(&as2_text);

    let expected = vec![
      SyntaxToken { kind: SyntaxKind::Identifier, text: "trace" },
      SyntaxToken { kind: SyntaxKind::OpenParen, text: "(" },
      SyntaxToken { kind: SyntaxKind::StrLit, text: "\"Hello, World!\"" },
      SyntaxToken { kind: SyntaxKind::CloseParen, text: ")" },
      SyntaxToken { kind: SyntaxKind::Semicolon, text: ";" },
      SyntaxToken { kind: SyntaxKind::MultilineWhitespace, text: "\n" },
    ];

    assert_eq!(&tokens, &expected);
  }
}
