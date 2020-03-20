use crate::lexer::{Lexer, LexerToken};
use crate::token_set::TokenSet;
use crate::types::owned;
use crate::types::syntax::{SyntaxKind, SyntaxNode};
use std::convert::TryFrom;

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
  pub(crate) fn peek_over_trivia(&self) -> Option<&LexerToken> {
    self.peeked.as_ref()
  }

  /// Peeks the kind of the next non-trivia token
  pub(crate) fn peek_kind_over_trivia(&self) -> Option<SyntaxKind> {
    self.peek_over_trivia().map(|token| token.kind)
  }

  /// Peeks the kind of the next token
  pub(crate) fn peek_kind(&self) -> Option<SyntaxKind> {
    self.peek().map(|token| token.kind)
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
    self.stmt_list(token_set!(None));
    self.builder.finish_node();
    let green_node: rowan::GreenNode = self.builder.finish();
    Parsed { green_node }
  }

  fn stmt_list(&mut self, end: TokenSet) {
    self.eat_trivia();
    while !end.contains(self.lexer.peek_kind_over_trivia()) {
      self.stmt();
      self.eat_trivia();
    }
  }

  fn stmt(&mut self) {
    let first = match self.lexer.peek() {
      None => panic!("UnexpectedEnd"),
      Some(token) => token,
    };
    match first.kind {
      SyntaxKind::TokenVar => self.var_decl(),
      kind if EXPR_START.contains(Some(kind)) => {
        self.expr_stmt();
        // Labelled statement or expression
      }
      kind => unimplemented!("{:?}", kind),
    }
  }

  fn expr_stmt(&mut self) {
    self.builder.start_node(SyntaxKind::NodeExprStmt.into());
    debug_assert!(EXPR_START.contains(self.lexer.peek_kind()));
    self.expr(token_set!(SyntaxKind::TokenSemicolon));
    debug_assert!(matches!(
      self.lexer.peek_kind_over_trivia(),
      Some(SyntaxKind::TokenSemicolon)
    ));
    self.eat_trivia();
    self.bump();
    self.builder.finish_node();
  }

  fn var_decl(&mut self) {
    self.builder.start_node(SyntaxKind::NodeVarDecl.into());
    debug_assert!(matches!(
      self.lexer.peek(),
      Some(LexerToken {
        kind: SyntaxKind::TokenVar,
        ..
      })
    ));
    self.bump();
    self.eat_trivia();
    self.ident();
    self.eat_trivia();
    debug_assert!(matches!(
      self.lexer.peek(),
      Some(LexerToken {
        kind: SyntaxKind::TokenEq,
        ..
      })
    ));
    self.bump();
    self.eat_trivia();
    self.expr(token_set!(None, SyntaxKind::TokenSemicolon));
    self.eat_trivia();
    self.bump();

    self.builder.finish_node();
  }

  fn expr(&mut self, end: TokenSet) {
    match self.lexer.peek_kind_over_trivia() {
      kind if end.contains(kind) => {}
      kind if EXPR_START.contains(kind) => self.expr_bp(InfixPrecedence::Sequence, end),
      kind => unimplemented!("Unexpected expr start {:?}", kind),
    };
  }

  fn expr_bp(&mut self, expr_precedence: InfixPrecedence, end: TokenSet) {
    let cp = self.builder.checkpoint();
    let first = match self.lexer.peek() {
      Some(first) => first,
      None => return,
    };
    match first.kind {
      SyntaxKind::TokenIdent => self.ident(),
      SyntaxKind::TokenFalse | SyntaxKind::TokenTrue => self.bool_lit(),
      SyntaxKind::TokenNumLit => self.num_lit(),
      SyntaxKind::TokenOpenBrace => self.object_lit(),
      SyntaxKind::TokenOpenParen => self.paren_expr(),
      SyntaxKind::TokenStrLit => self.str_lit(),
      SyntaxKind::TokenDelete | SyntaxKind::TokenPlus | SyntaxKind::TokenPlusPlus => self.prefix_expr(end),
      kind => unimplemented!("Unexpected epxression start: {:?}", kind),
    }
    loop {
      let operator_kind = match self.lexer.peek_kind_over_trivia() {
        kind if end.contains(kind) => return,
        None => panic!("Unexpected end of file"),
        Some(kind) => kind,
      };
      match operator_kind {
        SyntaxKind::TokenComa => {
          if expr_precedence == InfixPrecedence::Sequence {
            self.end_seq_expr(cp, end);
          } else {
            break;
          }
        }
        SyntaxKind::TokenEq => self.end_assignment_expr(cp, end),
        SyntaxKind::TokenQuestion => self.end_cond_expr(cp, end),
        SyntaxKind::TokenDot => self.end_ident_member_expr(cp),
        SyntaxKind::TokenOpenBracket => self.end_computed_member_expr(cp),
        kind if InfixPrecedence::try_from(kind) == Ok(InfixPrecedence::Postfix) => {
          if InfixPrecedence::Postfix > expr_precedence {
            self.eat_trivia();
            self.end_postfix_expr(cp);
          } else {
            break;
          }
        }
        kind if InfixPrecedence::try_from(kind).is_ok() => {
          let operator_precedence = InfixPrecedence::try_from(kind).unwrap();
          if operator_precedence > expr_precedence {
            self.eat_trivia();
            self.end_bin_expr(cp, end);
          } else {
            break;
          }
        }
        SyntaxKind::TokenOpenParen => self.end_call(cp),
        kind => unimplemented!("Unexpected expression operator: {:?}", kind),
      }
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

  fn bool_lit(&mut self) {
    self.builder.start_node(SyntaxKind::NodeBoolLit.into());
    debug_assert!(matches!(
      self.lexer.peek(),
      Some(LexerToken {kind: SyntaxKind::TokenFalse, ..})
      | Some(LexerToken {kind: SyntaxKind::TokenTrue, ..})
    ));
    self.bump();
    self.builder.finish_node();
  }

  fn num_lit(&mut self) {
    self.builder.start_node(SyntaxKind::NodeNumLit.into());
    debug_assert!(matches!(
      self.lexer.peek_kind_over_trivia(),
      Some(SyntaxKind::TokenNumLit)
    ));
    self.bump();
    self.builder.finish_node();
  }

  fn object_lit(&mut self) {
    self.builder.start_node(SyntaxKind::NodeObjectLit.into());
    debug_assert!(matches!(
      self.lexer.peek_kind_over_trivia(),
      Some(SyntaxKind::TokenOpenBrace)
    ));
    self.bump();

    loop {
      self.eat_trivia();
      self.object_lit_prop();
      match self.lexer.peek_kind_over_trivia() {
        Some(SyntaxKind::TokenComa) => {}
        Some(SyntaxKind::TokenCloseBrace) => break,
        _ => panic!("Unexpected token"),
      }
    }

    debug_assert!(matches!(
      self.lexer.peek_kind_over_trivia(),
      Some(SyntaxKind::TokenCloseBrace)
    ));
    self.eat_trivia();
    self.bump();

    self.builder.finish_node();
  }

  fn object_lit_prop(&mut self) {
    let prop_follow = token_set!(SyntaxKind::TokenComa, SyntaxKind::TokenCloseBrace);

    self.builder.start_node(SyntaxKind::NodeObjectLitProp.into());
    self.ident();

    match self.lexer.peek_kind_over_trivia() {
      Some(SyntaxKind::TokenColon) => {}
      _ => panic!("Unexpected token"),
    }
    self.eat_trivia();
    self.bump();

    self.eat_trivia();
    self.expr(prop_follow);

    self.builder.finish_node();
  }

  fn paren_expr(&mut self) {
    self.builder.start_node(SyntaxKind::NodeParenExpr.into());

    debug_assert!(matches!(
      self.lexer.peek_kind_over_trivia(),
      Some(SyntaxKind::TokenOpenParen)
    ));
    self.eat_trivia();
    self.bump();

    self.expr_bp(InfixPrecedence::Sequence, token_set!(SyntaxKind::TokenCloseParen));

    debug_assert!(matches!(
      self.lexer.peek_kind_over_trivia(),
      Some(SyntaxKind::TokenCloseParen)
    ));
    self.eat_trivia();
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

  fn prefix_expr(&mut self, end: TokenSet) {
    self.builder.start_node(SyntaxKind::NodePrefixExpr.into());

    debug_assert!(token_set!(
      SyntaxKind::TokenDelete,
      SyntaxKind::TokenPlus,
      SyntaxKind::TokenPlusPlus
    )
    .contains(self.lexer.peek_kind_over_trivia()));
    self.bump();

    self.eat_trivia();
    self.expr_bp(InfixPrecedence::Prefix, end);

    self.builder.finish_node();
  }

  fn end_seq_expr(&mut self, cp: rowan::Checkpoint, end: TokenSet) {
    self.builder.start_node_at(cp, SyntaxKind::NodeSeqExpr.into());

    debug_assert!(matches!(
      self.lexer.peek_kind_over_trivia(),
      Some(SyntaxKind::TokenComa)
    ));
    self.eat_trivia();
    self.bump();

    loop {
      self.eat_trivia();
      self.expr_bp(InfixPrecedence::Assignment, end);
      match self.lexer.peek_kind_over_trivia() {
        Some(SyntaxKind::TokenComa) => {
          self.eat_trivia();
          self.bump();
        }
        kind if end.contains(kind) => break,
        _ => panic!(),
      };
    }

    self.builder.finish_node();
  }

  fn end_assignment_expr(&mut self, cp: rowan::Checkpoint, end: TokenSet) {
    self.builder.start_node_at(cp, SyntaxKind::NodeAssignExpr.into());

    debug_assert!(matches!(self.lexer.peek_kind_over_trivia(), Some(SyntaxKind::TokenEq)));
    self.eat_trivia();
    self.bump();

    self.eat_trivia();
    self.expr_bp(InfixPrecedence::Assignment, end);

    self.builder.finish_node();
  }

  fn end_cond_expr(&mut self, cp: rowan::Checkpoint, end: TokenSet) {
    self.builder.start_node_at(cp, SyntaxKind::NodeCondExpr.into());

    debug_assert!(matches!(
      self.lexer.peek_kind_over_trivia(),
      Some(SyntaxKind::TokenQuestion)
    ));
    self.eat_trivia();
    self.bump();

    self.eat_trivia();
    self.expr_bp(InfixPrecedence::Assignment, token_set!(SyntaxKind::TokenColon));

    debug_assert!(matches!(
      self.lexer.peek_kind_over_trivia(),
      Some(SyntaxKind::TokenColon)
    ));
    self.eat_trivia();
    self.bump();

    self.eat_trivia();
    self.expr_bp(InfixPrecedence::Assignment, end);

    self.builder.finish_node();
  }

  fn end_bin_expr(&mut self, cp: rowan::Checkpoint, end: TokenSet) {
    self.builder.start_node_at(cp, SyntaxKind::NodeBinExpr.into());

    let bp: InfixPrecedence = match self.lexer.peek_kind_over_trivia() {
      Some(kind) if InfixPrecedence::try_from(kind).is_ok() => InfixPrecedence::try_from(kind).unwrap(),
      _ => panic!("UnexpectedBinOp"),
    };
    self.eat_trivia();
    self.bump();

    self.eat_trivia();
    self.expr_bp(bp, end);

    self.builder.finish_node();
  }

  fn end_ident_member_expr(&mut self, cp: rowan::Checkpoint) {
    self.builder.start_node_at(cp, SyntaxKind::NodeIdentMemberExpr.into());

    debug_assert!(matches!(self.lexer.peek_kind_over_trivia(), Some(SyntaxKind::TokenDot)));
    self.eat_trivia();
    self.bump();

    debug_assert!(matches!(
      self.lexer.peek_kind_over_trivia(),
      Some(SyntaxKind::TokenIdent)
    ));
    self.eat_trivia();
    self.ident();

    self.builder.finish_node();
  }

  fn end_computed_member_expr(&mut self, cp: rowan::Checkpoint) {
    self
      .builder
      .start_node_at(cp, SyntaxKind::NodeComputedMemberExpr.into());

    debug_assert!(matches!(
      self.lexer.peek_kind_over_trivia(),
      Some(SyntaxKind::TokenOpenBracket)
    ));
    self.eat_trivia();
    self.bump();

    self.expr_bp(InfixPrecedence::Sequence, token_set!(SyntaxKind::TokenCloseBracket));

    debug_assert!(matches!(
      self.lexer.peek_kind_over_trivia(),
      Some(SyntaxKind::TokenCloseBracket)
    ));
    self.eat_trivia();
    self.bump();

    self.builder.finish_node();
  }

  fn end_postfix_expr(&mut self, cp: rowan::Checkpoint) {
    self.builder.start_node_at(cp, SyntaxKind::NodePostfixExpr.into());

    debug_assert!(matches!(
      self.lexer.peek_kind_over_trivia(),
      Some(SyntaxKind::TokenPlusPlus)
    ));
    self.eat_trivia();
    self.bump();

    self.builder.finish_node();
  }

  fn end_call(&mut self, cp: rowan::Checkpoint) {
    self.builder.start_node_at(cp, SyntaxKind::NodeCall.into());
    debug_assert!(matches!(
      self.lexer.peek_kind_over_trivia(),
      Some(SyntaxKind::TokenOpenParen)
    ));
    self.eat_trivia();
    self.bump();
    self.expr_bp(InfixPrecedence::Assignment, token_set!(SyntaxKind::TokenCloseParen));
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

const EXPR_START: TokenSet = token_set!(
  SyntaxKind::TokenTrue,
  SyntaxKind::TokenFalse,
  SyntaxKind::TokenIdent,
  SyntaxKind::TokenNumLit,
  SyntaxKind::TokenStrLit,
  SyntaxKind::TokenOpenBrace,
  SyntaxKind::TokenOpenBracket,
  SyntaxKind::TokenExcl
);

pub fn parse_script(_input: &str) -> owned::StrLit {
  owned::StrLit {
    loc: (),
    value: String::new(),
  }
}

// TODO: Rename to just `precedence`
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum InfixPrecedence {
  Sequence,
  Assignment,
  // Conditional,
  LogicalOr,
  LogicalAnd,
  BitwiseOr,
  BitwiseXor,
  BitwiseAnd,
  Equality,
  Relational,
  Shift,
  Additive,
  Multiplicative,
  Prefix,
  Postfix,
  // Member,
}

impl TryFrom<SyntaxKind> for InfixPrecedence {
  type Error = ();

  fn try_from(value: SyntaxKind) -> Result<Self, Self::Error> {
    match value {
      SyntaxKind::TokenPipePipe => Ok(InfixPrecedence::LogicalOr),
      SyntaxKind::TokenAmpAmp => Ok(InfixPrecedence::LogicalAnd),
      SyntaxKind::TokenPipe => Ok(InfixPrecedence::BitwiseOr),
      SyntaxKind::TokenCaret => Ok(InfixPrecedence::BitwiseXor),
      SyntaxKind::TokenAmp => Ok(InfixPrecedence::BitwiseAnd),
      SyntaxKind::TokenEqEq => Ok(InfixPrecedence::Equality),
      SyntaxKind::TokenLt => Ok(InfixPrecedence::Relational),
      SyntaxKind::TokenLtLt => Ok(InfixPrecedence::Shift),
      SyntaxKind::TokenPlus | SyntaxKind::TokenMinus => Ok(InfixPrecedence::Additive),
      SyntaxKind::TokenStar | SyntaxKind::TokenSlash => Ok(InfixPrecedence::Multiplicative),
      SyntaxKind::TokenPlusPlus | SyntaxKind::TokenMinusMinus => Ok(InfixPrecedence::Postfix),
      // SyntaxKind::TokenOpenParen | SyntaxKind::TokenDot | SyntaxKind::TokenOpenBracket => Ok(InfixPrecedence::Member),
      _ => Err(()),
    }
  }
}

#[cfg(test)]
mod parser_tests {
  use crate::parser::parse;
  use crate::types::syntax::SyntaxNode;
  use ::test_generator::test_resources;
  use rowan::WalkEvent;
  use std::convert::TryFrom;
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

    let actual_ast = crate::types::syntax::Script::try_from(actual_cst).unwrap();
    let actual_ast_json = serde_json::to_string_pretty(&actual_ast).unwrap();
    fs::write(path.join("local-main.ast.json"), &actual_ast_json).unwrap();
    let expected_ast_json: String = fs::read_to_string(path.join("main.ast.json")).expect("Failed to read AST");

    assert_eq!(actual_ast_json, expected_ast_json);
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
