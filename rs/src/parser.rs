use crate::lexer::{Lexer, LexerToken};
use crate::types::owned;
use crate::types::syntax::{SyntaxKind, SyntaxNode};

// use crate::types::cast::ExprCast;
//
// struct OwnedAstBuilder {}
//
// impl OwnedAstBuilder {
//   fn str_lit(value: String) -> owned::StrLit {
//     owned::StrLit { _loc: (), _value: value }
//   }
//
//   fn num_lit(value: f64) -> owned::NumLit {
//     owned::NumLit { _loc: (), _value: value }
//   }
// }

pub struct Parsed {
  green_node: rowan::GreenNode,
}

impl Parsed {
  pub fn syntax(&self) -> SyntaxNode {
    SyntaxNode::new_root(self.green_node.clone())
  }
}

struct PeekableLexer<'text> {
  lexer: Lexer<'text>,
  /// Next non-trivia token, or None if at the end
  peeked: Option<LexerToken>,
  /// Cached status of the kind of trivia in `trivia_buffer`:
  /// - `None` indicates an empty `trivia_buffer`,
  /// - `Uniline` indicates there's only uniline trivia
  /// - `Multiline` otherwise (at least one multiline trivia)
  trivia_kind: TriviaKind,
  /// Unconsumed trivia before `peeked`.
  trivia_buffer: Vec<LexerToken>,
}

impl<'text> PeekableLexer<'text> {
  /// Creates a new peekable lexer
  ///
  /// This will initialize it eagerly by peeking into the inner lexer
  /// during initialization to find the next non-trivia token.
  pub(crate) fn new(mut lexer: Lexer<'text>) -> Self {
    let mut peeked: Option<LexerToken> = None;
    let mut has_multiline_trivia: bool = false;
    let mut trivia_buffer: Vec<LexerToken> = Vec::with_capacity(1);
    while let Some(token) = lexer.next() {
      if token.kind.is_trivia() {
        has_multiline_trivia = has_multiline_trivia || token.kind.is_multiline_trivia();
        trivia_buffer.push(token);
      } else {
        peeked = Some(token);
        break;
      }
    }
    let trivia_kind = if trivia_buffer.is_empty() {
      TriviaKind::None
    } else if has_multiline_trivia {
      TriviaKind::Multiline
    } else {
      TriviaKind::Uniline
    };
    Self {
      lexer,
      peeked,
      trivia_kind,
      trivia_buffer,
    }
  }

  /// Peeks the next non-trivia token
  ///
  /// # Precondition
  ///
  /// `self.trivia_kind == TriviaKind::None`
  pub(crate) fn peek(&self) -> Option<&LexerToken> {
    debug_assert_eq!(self.trivia_kind, TriviaKind::None);
    self.peeked.as_ref()
  }

  /// Peeks the kind of the next non-trivia token
  ///
  /// # Precondition
  ///
  /// `self.trivia_kind == TriviaKind::None`
  pub(crate) fn peek_kind(&self) -> Option<SyntaxKind> {
    self.peek().map(|token| token.kind)
  }

  // /// Peeks the next non-trivia token and preceding trivia kind.
  // ///
  // /// No token is consumed.
  // pub(crate) fn peek_with_trivia(&self) -> (TriviaKind, Option<&LexerToken>) {
  //   (self.trivia_kind, self.peeked.as_ref())
  // }

  pub(crate) fn eat_trivia<F>(&mut self, callback: &mut F)
  where
    F: FnMut(LexerToken),
  {
    for trivia in self.trivia_buffer.drain(..) {
      debug_assert!(trivia.kind.is_trivia());
      callback(trivia);
    }
    self.trivia_kind = TriviaKind::None;
  }

  /// Consumes to the next non-trivia token.
  ///
  /// Advances the inner lexer.
  ///
  /// # Preconditions
  ///
  /// - `trivia_kind` must be `None`
  /// - `peeked` must be `Some`
  pub(crate) fn pop(&mut self) -> LexerToken {
    debug_assert_eq!(self.trivia_kind, TriviaKind::None);
    debug_assert!(self.trivia_buffer.is_empty());
    let token = match self.peeked.take() {
      None => unreachable!("Precondition violation: peeked token must be defined"),
      Some(token) => token,
    };
    let mut has_multiline_trivia: bool = false;
    while let Some(token) = self.lexer.next() {
      if token.kind.is_trivia() {
        has_multiline_trivia = has_multiline_trivia || token.kind.is_multiline_trivia();
        self.trivia_buffer.push(token);
      } else {
        self.peeked = Some(token);
        break;
      }
    }
    self.trivia_kind = if self.trivia_buffer.is_empty() {
      TriviaKind::None
    } else if has_multiline_trivia {
      TriviaKind::Multiline
    } else {
      TriviaKind::Uniline
    };
    token
  }
}

struct Parser<'text> {
  lexer: PeekableLexer<'text>,
  builder: rowan::GreenNodeBuilder<'static>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum TriviaKind {
  None,
  Multiline,
  Uniline,
}

impl<'text> Parser<'text> {
  /// Consume tokens while they are trivia
  fn eat_trivia(&mut self) {
    let lexer = &mut self.lexer;
    let builder = &mut self.builder;
    lexer.eat_trivia(&mut |token| builder.token(token.kind.into(), token.text))
  }

  fn script(mut self) -> Parsed {
    self.builder.start_node(SyntaxKind::NodeScript.into());
    self.stmt_list(None);
    self.builder.finish_node();
    let green_node: rowan::GreenNode = self.builder.finish();
    Parsed { green_node }
  }

  fn stmt_list(&mut self, end: Option<SyntaxKind>) {
    self.eat_trivia();
    while self.lexer.peek_kind() != end {
      self.stmt();
      self.eat_trivia();
    }
  }

  fn stmt(&mut self) {
    self.builder.start_node(SyntaxKind::NodeStatement.into());
    let first = match self.lexer.peek() {
      None => return,
      Some(token) => token,
    };
    match first.kind {
      kind if is_expr_start(kind) => {
        self.expr(Some(SyntaxKind::TokenSemicolon));
        // Labelled statement or expression
      }
      kind => unimplemented!("{:?}", kind),
    }
    debug_assert!(matches!(
      self.lexer.peek(),
      Some(LexerToken {
        kind: SyntaxKind::TokenSemicolon,
        ..
      })
    ));
    self.eat_trivia();
    self.bump();
    self.builder.finish_node();
  }

  fn expr(&mut self, end: Option<SyntaxKind>) {
    let first = match self.lexer.peek() {
      None => return,
      Some(token) => token,
    };
    match first.kind {
      SyntaxKind::TokenIdent => {
        self.expr_bp(0, end);
        // Labelled statement or expression
      }
      _ => unimplemented!(),
    }
  }

  fn expr_bp(&mut self, _bp: u8, end: Option<SyntaxKind>) {
    let cp = self.builder.checkpoint();
    let first = match self.lexer.peek() {
      Some(first) => first,
      None => return,
    };
    match first.kind {
      SyntaxKind::TokenIdent => self.ident(),
      SyntaxKind::TokenStrLit => self.str_lit(),
      _ => unimplemented!(),
    }
    let operator = match self.lexer.peek() {
      e if e.map(|t| t.kind) == end => return,
      None => panic!("Unexpected end of file"),
      Some(token) => token,
    };
    match operator.kind {
      SyntaxKind::TokenOpenParen => self.end_call(cp),
      _ => unimplemented!(),
    }
  }

  fn ident(&mut self) {
    self.builder.start_node(SyntaxKind::NodeIdent.into());
    debug_assert!(matches!(
      self.lexer.peek(),
      Some(LexerToken {
        kind: SyntaxKind::TokenIdent,
        ..
      })
    ));
    self.bump();
    self.builder.finish_node();
  }

  fn str_lit(&mut self) {
    self.builder.start_node(SyntaxKind::NodeStrLit.into());
    debug_assert!(matches!(
      self.lexer.peek(),
      Some(LexerToken {
        kind: SyntaxKind::TokenStrLit,
        ..
      })
    ));
    self.bump();
    self.builder.finish_node();
  }

  fn end_call(&mut self, cp: rowan::Checkpoint) {
    self.builder.start_node_at(cp, SyntaxKind::NodeCall.into());
    debug_assert!(matches!(
      self.lexer.peek(),
      Some(LexerToken {
        kind: SyntaxKind::TokenOpenParen,
        ..
      })
    ));
    self.eat_trivia();
    self.bump();
    self.expr_bp(0, Some(SyntaxKind::TokenCloseParen));
    debug_assert!(matches!(
      self.lexer.peek(),
      Some(LexerToken {
        kind: SyntaxKind::TokenCloseParen,
        ..
      })
    ));
    self.eat_trivia();
    self.bump();
    self.builder.finish_node();
  }

  fn bump(&mut self) {
    let token = self.lexer.pop();
    self.builder.token(token.kind.into(), token.text);
  }
}

pub fn parse(text: &str) -> Parsed {
  let lexer = Lexer::new(text);
  let lexer = PeekableLexer::new(lexer);
  let builder = rowan::GreenNodeBuilder::new();
  let parser = Parser { lexer, builder };
  parser.script()
}

fn is_expr_start(token_kind: SyntaxKind) -> bool {
  debug_assert!(token_kind.is_token());
  use SyntaxKind::*;
  match token_kind {
    TokenIdent | TokenStrLit | TokenExcl => true,
    _ => false,
  }
}

//
// struct Parser<'i> {
//   input: &'i str,
// }
//
// #[derive(Debug, Eq, PartialEq, Copy, Clone, Ord, PartialOrd, Hash)]
// enum Token<'a> {
//   Ident(&'a str),
//   Error(&'a str),
//   End,
// }
//
// impl Parser {
//   fn parse_script() {
//
//   }
//
//   fn parse_statements(directives: bool, top_level: bool, end: Token) {
//     let mut stmts: Vec<owned::Stmt> = Vec::new();
//   }
//
//   fn peek(&self) -> Token {
//     let chars = self.input.char_indices();
//     match chars.next() {
//       None => Token::End,
//       Some((i, c @ 'a'..='z')) => {
//
//       },
//       Some((i, c)) => Token::Error(self.input[i..c.len_utf8()])
//     }
//   }
// }

pub fn parse_script(_input: &str) -> owned::StrLit {
  owned::StrLit {
    loc: (),
    value: String::new(),
  }
}

// pub fn eval_expr(input: &owned::Expr) -> f64 {
//   match input.downcast() {
//     ExprCast::BinExpr(e) => eval_bin_expr(e),
//     ExprCast::NumLit(e) => eval_num_lit(e),
//     ExprCast::StrLit(e) => unimplemented!("StrLit"),
//   }
// }
//
// pub fn eval_num_lit(input: &owned::NumLit) -> f64 {
//   input.value()
// }
//
// pub fn eval_bin_expr(input: &owned::BinExpr) -> f64 {
//   let left = eval_expr(input.left());
//   let right = eval_expr(input.right());
//   left + right
// }

#[cfg(test)]
mod parser_tests {
  use crate::parser::parse;
  use crate::types::syntax::SyntaxNode;
  use ::test_generator::test_resources;
  use rowan::WalkEvent;
  use std::fs;
  use std::io;
  use std::path::Path;

  #[test_resources("../tests/as2/[!.]*/*/")]
  fn test_parse_as2(path: &str) {
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

    let parsed = parse(&as2_text);

    let actual_cst = SyntaxNode::new_root(parsed.green_node);

    let actual_cst_text = {
      let mut actual_cst_text: Vec<u8> = Vec::new();
      dump_node(&mut actual_cst_text, &actual_cst).unwrap();
      String::from_utf8(actual_cst_text).unwrap()
    };

    fs::write(path.join("local-main.cst.txt"), &actual_cst_text).unwrap();

    let cst_text: String = fs::read_to_string(path.join("main.cst.txt")).expect("Failed to read CST");

    assert_eq!(&actual_cst_text, &cst_text);
  }

  fn dump_node<W: io::Write>(writer: &mut W, node: &SyntaxNode) -> Result<(), io::Error> {
    let mut indent = 0;
    for event in node.preorder_with_tokens() {
      match &event {
        WalkEvent::Enter(symbol) => {
          write!(
            writer,
            "{:i$}{:?}@{:?}",
            "",
            symbol.kind(),
            symbol.text_range(),
            i = indent
          )?;
          match symbol {
            rowan::NodeOrToken::Node(_) => writeln!(writer, " {{")?,
            rowan::NodeOrToken::Token(token) => writeln!(writer, " \"{}\"", token.text().escape_default())?,
          };
          indent += 2
        }
        WalkEvent::Leave(symbol) => {
          indent -= 2;
          if matches!(symbol, rowan::NodeOrToken::Node(_)) {
            writeln!(writer, "{:i$}}}", "", i = indent)?
          }
        }
      };
    }
    Ok(())
  }
}
