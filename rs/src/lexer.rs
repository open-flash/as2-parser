use std::str::Chars;

use crate::types::syntax::SyntaxKind;
use rowan::SmolStr;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LexerToken {
  pub kind: SyntaxKind,
  pub text: SmolStr,
}

#[derive(Debug, Clone)]
pub struct Lexer<'text> {
  text: &'text str,
}

impl Lexer<'_> {
  pub fn new(text: &str) -> Lexer {
    Lexer { text }
  }
}

impl<'text> Iterator for Lexer<'text> {
  type Item = LexerToken;

  fn next(&mut self) -> Option<Self::Item> {
    let token = next_token(self.text);
    if let Some(ref token) = &token {
      self.text = &self.text[token.text.len()..];
    }
    token
  }
}

pub fn lex(text: &str) -> Vec<LexerToken> {
  Lexer::new(text).collect()
}

fn next_token(input: &str) -> Option<LexerToken> {
  let input_len = input.len();
  let mut chars = input.chars();
  let chars = &mut chars;
  let first = match chars.next() {
    None => return None,
    Some(c) => c,
  };
  let kind: SyntaxKind = match first {
    '/' => match chars.next() {
      Some('/') => end_trailing_comment(chars),
      Some('*') => end_block_comment(chars),
      _ => SyntaxKind::TokenSlash,
    },
    c @ '0'..='9' => end_num_lit(c, chars),
    c if is_id_start(c) => end_id_or_keyword(c, chars),
    c if is_whitespace(c) => end_whitespace(c, chars),
    ';' => SyntaxKind::TokenSemicolon,
    ':' => SyntaxKind::TokenColon,
    ',' => SyntaxKind::TokenComa,
    '{' => SyntaxKind::TokenOpenBrace,
    '}' => SyntaxKind::TokenCloseBrace,
    '[' => SyntaxKind::TokenOpenBracket,
    ']' => SyntaxKind::TokenCloseBracket,
    '(' => SyntaxKind::TokenOpenParen,
    ')' => SyntaxKind::TokenCloseParen,
    '=' => {
      let old_chars = chars.clone();
      match chars.next() {
        Some('=') => {
          let old_chars = chars.clone();
          match chars.next() {
            Some('=') => SyntaxKind::TokenEqEqEq,
            _ => {
              *chars = old_chars;
              SyntaxKind::TokenEqEq
            }
          }
        }
        _ => {
          *chars = old_chars;
          SyntaxKind::TokenEq
        }
      }
    }
    '!' => SyntaxKind::TokenExcl,
    '?' => SyntaxKind::TokenQuestion,
    '<' => {
      let old_chars = chars.clone();
      match chars.next() {
        Some('<') => SyntaxKind::TokenLtLt,
        Some('=') => SyntaxKind::TokenLtEq,
        _ => {
          *chars = old_chars;
          SyntaxKind::TokenLt
        }
      }
    }
    '>' => match chars.next() {
      Some('>') => match chars.next() {
        Some('>') => SyntaxKind::TokenGtGtGt,
        _ => SyntaxKind::TokenGtGt,
      },
      Some('=') => SyntaxKind::TokenGtEq,
      _ => SyntaxKind::TokenGt,
    },
    '+' => {
      let old_chars = chars.clone();
      match chars.next() {
        Some('+') => SyntaxKind::TokenPlusPlus,
        _ => {
          *chars = old_chars;
          SyntaxKind::TokenPlus
        }
      }
    }
    '-' => {
      let old_chars = chars.clone();
      match chars.next() {
        Some('-') => SyntaxKind::TokenMinusMinus,
        _ => {
          *chars = old_chars;
          SyntaxKind::TokenMinus
        }
      }
    }
    '|' => {
      let old_chars = chars.clone();
      match chars.next() {
        Some('|') => SyntaxKind::TokenPipePipe,
        _ => {
          *chars = old_chars;
          SyntaxKind::TokenPipe
        }
      }
    }
    '&' => {
      let old_chars = chars.clone();
      match chars.next() {
        Some('&') => SyntaxKind::TokenAmpAmp,
        _ => {
          *chars = old_chars;
          SyntaxKind::TokenAmp
        }
      }
    }
    '%' => SyntaxKind::TokenPercent,
    '*' => SyntaxKind::TokenStar,
    '^' => SyntaxKind::TokenCaret,
    '.' => SyntaxKind::TokenDot,
    '"' => end_double_quoted_string(chars),
    c => unimplemented!("Tokens starting with: {:?}", c),
  };
  let token_len = input_len - chars.as_str().len();
  Some(LexerToken {
    kind,
    text: input[..token_len].into(),
  })
}

/// Consumes a trailing comment.
///
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
  SyntaxKind::TokenTrailingComment
}

/// Consumes a block comment.
///
/// The starting `/*` must already be consumed.
fn end_block_comment(chars: &mut Chars) -> SyntaxKind {
  let mut is_multiline = false;
  loop {
    match chars.next() {
      None => panic!("Non-closed block comment"),
      Some(c) if is_line_terminator_sequence_start(c) => {
        end_line_terminator_sequence(c, chars);
        is_multiline = true;
      }
      Some('*') => {
        let old_chars = chars.clone();
        match chars.next() {
          Some('/') => break,
          _ => {
            *chars = old_chars;
          }
        }
      }
      _ => {}
    }
  }
  if is_multiline {
    SyntaxKind::TokenMultilineComment
  } else {
    SyntaxKind::TokenUnilineComment
  }
}

fn end_double_quoted_string(chars: &mut Chars) -> SyntaxKind {
  // TODO: Handle line terminators
  loop {
    match chars.next() {
      None => return SyntaxKind::TokenError,
      Some('"') => return SyntaxKind::TokenStrLit,
      Some('\\') => {
        chars.next(); // Skip next char
      }
      _ => {}
    }
  }
}

fn end_num_lit(first: char, chars: &mut Chars) -> SyntaxKind {
  fn end_decimal(_first: char, chars: &mut Chars) -> SyntaxKind {
    // Consume leading digits until `e` or `.`
    let has_dot: bool = loop {
      let old_chars = chars.clone();
      match chars.next() {
        Some('0'..='9') => {}
        Some('.') => {
          break true;
        }
        Some('e') | Some('E') => {
          break false;
        }
        _ => {
          *chars = old_chars;
          return SyntaxKind::TokenNumLit;
        }
      }
    };
    let has_exponent = if has_dot {
      // Consume fractional digits
      loop {
        let old_chars = chars.clone();
        match chars.next() {
          Some('0'..='9') => {}
          Some('e') | Some('E') => {
            break true;
          }
          _ => {
            *chars = old_chars;
            break false;
          }
        }
      }
    } else {
      true
    };
    if has_exponent {
      loop {
        let old_chars = chars.clone();
        match chars.next() {
          Some('0'..='9') => {}
          _ => {
            *chars = old_chars;
            break;
          }
        }
      }
    }

    SyntaxKind::TokenNumLit
  }
  fn end_octal(_chars: &mut Chars) -> SyntaxKind {
    // TODO: Remember you may fallback to decimal
    unimplemented!()
  }
  fn end_hex(_chars: &mut Chars) -> SyntaxKind {
    unimplemented!()
  }

  if first == '0' {
    let old_chars = chars.clone();
    match chars.next() {
      Some('o') | Some('O') => end_octal(chars),
      Some('x') | Some('X') => end_hex(chars),
      _ => {
        *chars = old_chars;
        end_decimal(first, chars)
      }
    }
  } else {
    end_decimal(first, chars)
  }
}

/// Consumes an identifier or keyword
// TODO: Simplify this function!
#[allow(clippy::cognitive_complexity)]
fn end_id_or_keyword(first: char, chars: &mut Chars) -> SyntaxKind {
  debug_assert!(is_id_start(first));
  match first {
    'a' => unimplemented!(),
    'b' => unimplemented!(),
    'c' => unimplemented!(),
    'd' => match chars.next() {
      Some('e') => match chars.next() {
        Some('b') => unimplemented!(),
        Some('f') => unimplemented!(),
        Some('l') => match chars.next() {
          Some('e') => match chars.next() {
            Some('t') => match chars.next() {
              Some('e') => {
                let old_chars = chars.clone();
                match chars.next() {
                  Some(c) if is_id_continue(c) => end_id(chars),
                  _ => {
                    *chars = old_chars;
                    SyntaxKind::TokenDelete
                  }
                }
              }
              Some(c) if is_id_continue(c) => end_id(chars),
              _ => SyntaxKind::TokenIdent,
            },
            Some(c) if is_id_continue(c) => end_id(chars),
            _ => SyntaxKind::TokenIdent,
          },
          Some(c) if is_id_continue(c) => end_id(chars),
          _ => SyntaxKind::TokenIdent,
        },
        Some(c) if is_id_continue(c) => end_id(chars),
        _ => SyntaxKind::TokenIdent,
      },
      Some('o') => unimplemented!(),
      Some(c) if is_id_continue(c) => end_id(chars),
      _ => SyntaxKind::TokenIdent,
    },
    'e' => unimplemented!(),
    'f' => match chars.next() {
      Some('a') => unimplemented!(),
      Some('i') => unimplemented!(),
      Some('o') => match chars.next() {
        Some('r') => {
          let old_chars = chars.clone();
          match chars.next() {
            Some(c) if is_id_continue(c) => end_id(chars),
            _ => {
              *chars = old_chars;
              SyntaxKind::TokenFor
            }
          }
        }
        Some(c) if is_id_continue(c) => end_id(chars),
        _ => SyntaxKind::TokenIdent,
      },
      Some('u') => unimplemented!(),
      Some(c) if is_id_continue(c) => end_id(chars),
      _ => SyntaxKind::TokenIdent,
    },
    'i' => unimplemented!(),
    'n' => unimplemented!(),
    'p' => unimplemented!(),
    'r' => unimplemented!(),
    's' => unimplemented!(),
    't' => match chars.next() {
      Some('h') => match chars.next() {
        Some('i') => match chars.next() {
          Some('s') => {
            let old_chars = chars.clone();
            match chars.next() {
              Some(c) if is_id_continue(c) => end_id(chars),
              _ => {
                *chars = old_chars;
                SyntaxKind::TokenThis
              }
            }
          }
          Some(c) if is_id_continue(c) => end_id(chars),
          _ => SyntaxKind::TokenIdent,
        },
        Some('r') => match chars.next() {
          Some('o') => match chars.next() {
            Some('w') => {
              let old_chars = chars.clone();
              match chars.next() {
                Some(c) if is_id_continue(c) => end_id(chars),
                _ => {
                  *chars = old_chars;
                  SyntaxKind::TokenThrow
                }
              }
            }
            Some(c) if is_id_continue(c) => end_id(chars),
            _ => SyntaxKind::TokenIdent,
          },
          Some(c) if is_id_continue(c) => end_id(chars),
          _ => SyntaxKind::TokenIdent,
        },
        Some(c) if is_id_continue(c) => end_id(chars),
        _ => SyntaxKind::TokenIdent,
      },
      Some('r') => match chars.next() {
        Some('u') => match chars.next() {
          Some('e') => {
            let old_chars = chars.clone();
            match chars.next() {
              Some(c) if is_id_continue(c) => end_id(chars),
              _ => {
                *chars = old_chars;
                SyntaxKind::TokenTrue
              }
            }
          }
          Some(c) if is_id_continue(c) => end_id(chars),
          _ => SyntaxKind::TokenIdent,
        },
        Some('y') => {
          let old_chars = chars.clone();
          match chars.next() {
            Some(c) if is_id_continue(c) => end_id(chars),
            _ => {
              *chars = old_chars;
              SyntaxKind::TokenTry
            }
          }
        }
        Some(c) if is_id_continue(c) => end_id(chars),
        _ => SyntaxKind::TokenIdent,
      },
      Some(c) if is_id_continue(c) => end_id(chars),
      _ => SyntaxKind::TokenIdent,
    },
    'v' => match chars.next() {
      Some('a') => match chars.next() {
        Some('r') => {
          let old_chars = chars.clone();
          match chars.next() {
            Some(c) if is_id_continue(c) => end_id(chars),
            _ => {
              *chars = old_chars;
              SyntaxKind::TokenVar
            }
          }
        }
        Some(c) if is_id_continue(c) => end_id(chars),
        _ => SyntaxKind::TokenIdent,
      },
      Some('o') => match chars.next() {
        Some('i') => match chars.next() {
          Some('d') => {
            let old_chars = chars.clone();
            match chars.next() {
              Some(c) if is_id_continue(c) => end_id(chars),
              _ => {
                *chars = old_chars;
                SyntaxKind::TokenVoid
              }
            }
          }
          Some(c) if is_id_continue(c) => end_id(chars),
          _ => SyntaxKind::TokenIdent,
        },
        Some(c) if is_id_continue(c) => end_id(chars),
        _ => SyntaxKind::TokenIdent,
      },
      Some(c) if is_id_continue(c) => end_id(chars),
      _ => SyntaxKind::TokenIdent,
    },
    'w' => unimplemented!(),
    'y' => {
      let old_chars = chars.clone();
      match chars.next() {
        Some('i') => unimplemented!(),
        Some(c) if is_id_continue(c) => end_id(chars),
        _ => {
          *chars = old_chars;
          SyntaxKind::TokenIdent
        }
      }
    }
    _ => end_id(chars),
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
        break;
      }
    }
  }
  SyntaxKind::TokenIdent
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
      Some(c) if is_whitespace(c) => {}
      _ => {
        *chars = old_chars;
        break;
      }
    }
  }

  if multiline {
    SyntaxKind::TokenMultilineWhitespace
  } else {
    SyntaxKind::TokenUnilineWhitespace
  }
}

fn end_line_terminator_sequence(first: char, chars: &mut Chars) {
  debug_assert!(is_line_terminator_sequence_start(first));
  if first == '\r' {
    let old_chars = chars.clone();
    match chars.next() {
      Some('\n') => {}
      _ => {
        *chars = old_chars;
      }
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
mod lexer_tests {
  use crate::lexer::{lex, LexerToken};
  use ::test_generator::test_resources;
  use std::path::Path;
  use std::{fs, io};

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

    let actual_tokens = lex(&as2_text);

    let actual_lex_text = {
      let mut actual_lex_text: Vec<u8> = Vec::new();
      dump_tokens(&mut actual_lex_text, &actual_tokens).unwrap();
      String::from_utf8(actual_lex_text).unwrap()
    };

    fs::write(path.join("local-main.lex.txt"), &actual_lex_text).unwrap();

    let expected_lex_text: String = fs::read_to_string(path.join("main.lex.txt")).expect("Failed to read Lex");

    assert_eq!(&actual_lex_text, &expected_lex_text);
  }

  fn dump_tokens<W: io::Write>(writer: &mut W, tokens: &[LexerToken]) -> Result<(), io::Error> {
    for token in tokens {
      writeln!(writer, "{:?} {:?}", token.kind, token.text,)?;
    }
    Ok(())
  }
}
