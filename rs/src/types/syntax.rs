use crate::types::ast::traits;
use crate::types::ast::traits::{ExprCast, PatCast, StmtCast, Syntax};
use rowan::{SyntaxElementChildren, SyntaxNodeChildren};
use std::borrow::Cow;
use std::convert::TryFrom;
use std::ops::Range;
use std::str::Chars;
use variant_count::VariantCount;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, VariantCount)]
#[repr(u16)]
pub enum SyntaxKind {
  /// Invalid token
  TokenError,

  // Trivia
  /// Whitespace without any newline
  TokenUnilineWhitespace,

  /// Whitespace containing at least one newline
  TokenMultilineWhitespace,

  /// Trailing comment: from `//` to the end of the line
  TokenTrailingComment,

  /// Comment between `/*` and `*/`, containing a newline
  TokenMultilineComment,

  /// Comment between `/*` and `*/`, without a newline
  TokenUnilineComment,

  // Keywords
  /// The keyword `delete`
  TokenDelete,

  /// The keyword `false`
  TokenFalse,

  /// The keyword `for`
  TokenFor,

  /// The keyword `throw`
  TokenThrow,

  /// The keyword `this`
  TokenThis,

  /// The keyword `true`
  TokenTrue,

  /// The keyword `try`
  TokenTry,

  /// The keyword `var`
  TokenVar,

  /// The keyword `void`
  TokenVoid,

  // Atoms
  /// Identifier name
  ///
  /// In ActionScript, keywords are never valid identifiers so there is no
  /// ambiguity here. (e.g. `throw` is _not_ an identifier)
  ///
  /// Examples:
  /// - `foo`
  /// - `$foo`
  /// - `_foo_$123`
  TokenIdent,

  /// String literal
  ///
  /// Examples:
  /// - `"foo"`
  /// - `"foo\nbar\0baz"`
  /// - `'"'`
  /// - `'\''`
  /// - `"\""`
  TokenStrLit,

  /// Number literal
  TokenNumLit,

  // Punctuators
  /// `;`
  TokenSemicolon,

  /// `:`
  TokenColon,

  /// `,`
  TokenComa,

  /// `{`
  TokenOpenBrace,

  /// `}`
  TokenCloseBrace,

  /// `[`
  TokenOpenBracket,

  /// `]`
  TokenCloseBracket,

  /// `(`
  TokenOpenParen,

  /// `)`
  TokenCloseParen,

  /// `=`
  TokenEq,

  /// `==`
  TokenEqEq,

  /// `===`
  TokenEqEqEq,

  /// `!=`
  TokenNotEq,

  /// `!==`
  TokenNotEqEq,

  /// `!`
  TokenExcl,

  /// `?`
  TokenQuestion,

  /// `<`
  TokenLt,

  /// `<<`
  TokenLtLt,

  /// `<=`
  TokenLtEq,

  /// `>`
  TokenGt,

  /// `>>`
  TokenGtGt,

  /// `>>>`
  TokenGtGtGt,

  /// `>=`
  TokenGtEq,

  /// `+`
  TokenPlus,

  /// `++`
  TokenPlusPlus,

  /// `-`
  TokenMinus,

  /// `--`
  TokenMinusMinus,

  /// `|`
  TokenPipe,

  /// `||`
  TokenPipePipe,

  /// `&`
  ///
  /// "amp" is an abbreviation for "ampersand".
  TokenAmp,

  /// `&&`
  ///
  /// "amp" is an abbreviation for "ampersand".
  TokenAmpAmp,

  /// `/`
  TokenSlash,

  /// `%`
  TokenPercent,

  /// `*`
  TokenStar,

  /// `^`
  TokenCaret,

  /// `.`
  TokenDot,

  // Simple nodes
  /// Boolean literal expression
  NodeBoolLit,

  /// Number literal expression
  NodeNumLit,

  /// Object literal expression
  NodeObjectLit,

  /// A single property from an object literal expression
  NodeObjectLitProp,

  /// String literal expression
  NodeStrLit,

  /// Identifier reference expression, or identifier pattern, or label identifier
  NodeIdent,

  /// Sequence expression
  NodeSeqExpr,

  /// Assignment expression
  NodeAssignExpr,

  /// Conditional expression
  NodeCondExpr,

  /// Binary expression
  NodeInfixExpr,

  /// Prefix expression
  NodePrefixExpr,

  /// Postfix expression
  NodePostfixExpr,

  /// Parenthesized expression
  NodeParenExpr,

  /// Identifier member expression
  ///
  /// ```as2
  /// foo.bar
  /// ```
  NodeIdentMemberExpr,

  /// Computed member expression
  ///
  /// ```as2
  /// foo["bar"]
  /// ```
  NodeComputedMemberExpr,

  NodeCall,

  // Composite nodes
  /// Break statement
  NodeBreakStmt,

  /// Expression statement
  NodeExprStmt,

  /// Variable declaration
  NodeVarDecl,

  /// Root node
  NodeScript,
}

impl From<SyntaxKind> for rowan::SyntaxKind {
  fn from(kind: SyntaxKind) -> Self {
    Self(u16::from(kind))
  }
}

impl From<SyntaxKind> for u16 {
  fn from(value: SyntaxKind) -> Self {
    value.into_u16()
  }
}

impl TryFrom<u16> for SyntaxKind {
  type Error = ();

  fn try_from(value: u16) -> Result<Self, Self::Error> {
    if usize::from(value) < SyntaxKind::VARIANT_COUNT {
      Ok(unsafe { std::mem::transmute::<u16, SyntaxKind>(value) })
    } else {
      Err(())
    }
  }
}

impl SyntaxKind {
  // TODO: Move this function to the `From` trait once possible (requires const fn in traits)
  //       (and remove this function)
  pub const fn into_u16(self) -> u16 {
    self as u16
  }

  pub fn is_trivia(self) -> bool {
    use SyntaxKind::*;
    match self {
      TokenMultilineWhitespace
      | TokenUnilineWhitespace
      | TokenTrailingComment
      | TokenMultilineComment
      | TokenUnilineComment => true,
      _ => false,
    }
  }

  pub fn is_multiline_trivia(self) -> bool {
    use SyntaxKind::*;
    match self {
      TokenMultilineWhitespace | TokenTrailingComment | TokenMultilineComment => true,
      _ => false,
    }
  }

  pub fn is_token(self) -> bool {
    use SyntaxKind::*;
    match self {
      TokenError
      | TokenUnilineWhitespace
      | TokenMultilineWhitespace
      | TokenTrailingComment
      | TokenMultilineComment
      | TokenUnilineComment
      | TokenThrow
      | TokenThis
      | TokenTrue
      | TokenTry
      | TokenIdent
      | TokenStrLit
      | TokenSemicolon
      | TokenOpenParen
      | TokenCloseParen => true,
      _ => false,
    }
  }

  pub fn is_stmt(self) -> bool {
    use SyntaxKind::*;
    match self {
      NodeExprStmt | NodeVarDecl => true,
      _ => false,
    }
  }

  pub fn is_expr(self) -> bool {
    use SyntaxKind::*;
    match self {
      NodeAssignExpr | NodeInfixExpr | NodeBoolLit | NodeCall | NodeNumLit | NodeParenExpr | NodePostfixExpr
      | NodePrefixExpr | NodeSeqExpr | NodeStrLit => true,
      _ => false,
    }
  }

  pub fn is_assign_op(self) -> bool {
    use SyntaxKind::*;
    match self {
      TokenEq => true,
      _ => false,
    }
  }

  pub fn is_logical_op(self) -> bool {
    use SyntaxKind::*;
    match self {
      TokenPipePipe | TokenAmpAmp => true,
      _ => false,
    }
  }

  pub fn is_bin_op(self) -> bool {
    use SyntaxKind::*;
    match self {
      TokenAmp | TokenCaret | TokenEqEq | TokenEqEqEq | TokenLt | TokenLtLt | TokenMinus | TokenPipe | TokenPlus
      | TokenSlash | TokenStar => true,
      _ => false,
    }
  }

  pub fn is_infix_op(self) -> bool {
    self.is_assign_op() || self.is_bin_op() || self.is_logical_op()
  }
}

/// Enum representing the ActionScript 2 language supported by OpenFlash
/// It is used as a bridge between Rowan's untyped node and AS2's syntax kinds.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum As2Lang {}

impl rowan::Language for As2Lang {
  type Kind = SyntaxKind;

  fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
    SyntaxKind::try_from(raw.0).unwrap()
  }

  fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
    rowan::SyntaxKind(u16::from(kind))
  }
}

/// Represents an AS2 syntax token
///
/// A token is a terminal symbol of the syntax tree. It has a kind, range and text.
/// The text is owned by the token and stored as a `rowan::SmolStr`.
pub type SyntaxToken = rowan::SyntaxToken<As2Lang>;

/// Represents an AS2 syntax node
///
/// A node is a non-terminal symbol of the syntax tree. It has a kind, range and
/// child symbols. It does not own text directly: the text is retrieved by
/// iterating on the descendant children.
pub type SyntaxNode = rowan::SyntaxNode<As2Lang>;
/// Represents an AS2 syntax symbol: token (terminal) or node (non-terminal).
pub type SyntaxSymbol = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;

////////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub enum ConcreteSyntax {}

impl traits::Syntax for ConcreteSyntax {
  type Script = Script;

  type Stmt = Stmt;
  type BreakStmt = BreakStmt;
  type ExprStmt = ExprStmt;
  type ErrorStmt = ErrorStmt;
  type TraceStmt = TraceStmt;
  type VarDecl = VarDecl;

  type Expr = Expr;
  type AssignExpr = AssignExpr;
  type BinExpr = BinExpr;
  type ErrorExpr = ErrorExpr;
  type LogicalExpr = LogicalExpr;
  type SeqExpr = SeqExpr;
  type StrLit = StrLit;

  type Pat = Pat;
  type MemberPat = MemberPat;
  type IdentPat = IdentPat;
}

macro_rules! impl_serialize {
  ($struct_name:ident, $adapter_name:ident) => {
    impl serde::Serialize for $struct_name {
      fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        crate::types::ast::ser::$adapter_name::<ConcreteSyntax>(self).serialize(serializer)
      }
    }
  };
}

/// Represents an AST Script root backed by a concrete syntax node.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Script {
  syntax: SyntaxNode,
}

impl_serialize!(Script, SerializeScript);

impl TryFrom<SyntaxNode> for Script {
  type Error = ();

  fn try_from(syntax: SyntaxNode) -> Result<Self, Self::Error> {
    match syntax.kind() {
      SyntaxKind::NodeScript => Ok(Self { syntax }),
      _ => Err(()),
    }
  }
}

impl traits::Script<ConcreteSyntax> for Script {
  #[cfg(not(feature = "gat"))]
  fn stmts<'a>(&'a self) -> Box<dyn Iterator<Item = traits::MaybeOwned<'a, Stmt>> + 'a> {
    Box::new(ScriptStmts {
      inner: self.syntax.children(),
    })
  }

  #[cfg(feature = "gat")]
  type Stmts<'a> = ScriptStmts;

  #[cfg(feature = "gat")]
  fn stmts(&self) -> Self::Stmts<'_> {
    ScriptStmts {
      inner: self.syntax.children(),
    }
  }
}

pub struct ScriptStmts {
  inner: SyntaxNodeChildren<As2Lang>,
}

impl Iterator for ScriptStmts {
  type Item = traits::MaybeOwned<'static, Stmt>;

  fn next(&mut self) -> Option<Self::Item> {
    while let Some(node) = self.inner.next() {
      match Stmt::try_from(node) {
        Ok(s) => return Some(traits::MaybeOwned::Owned(s)),
        Err(()) => {}
      }
    }
    None
  }
}

/// Represents a statement backed by a concrete syntax node.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Stmt {
  syntax: SyntaxNode,
}

impl TryFrom<SyntaxNode> for Stmt {
  type Error = ();

  fn try_from(syntax: SyntaxNode) -> Result<Self, Self::Error> {
    if syntax.kind().is_stmt() {
      Ok(Self { syntax })
    } else {
      Err(())
    }
  }
}

impl traits::Stmt<ConcreteSyntax> for Stmt {
  fn cast(&self) -> StmtCast<ConcreteSyntax> {
    match self.syntax.kind() {
      SyntaxKind::NodeExprStmt => traits::StmtCast::Expr(traits::MaybeOwned::Owned(ExprStmt {
        syntax: self.syntax.clone(),
      })),
      SyntaxKind::NodeVarDecl => traits::StmtCast::VarDecl(traits::MaybeOwned::Owned(VarDecl {
        syntax: self.syntax.clone(),
      })),
      _ => traits::StmtCast::Error(traits::MaybeOwned::Owned(ErrorStmt {
        syntax: self.syntax.clone(),
      })),
    }
  }
}

/// Represents a break statement backed by a concrete syntax node.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct BreakStmt {
  syntax: SyntaxNode,
}

impl traits::BreakStmt<ConcreteSyntax> for BreakStmt {}

/// Represents an expression statement backed by a concrete syntax node.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct ExprStmt {
  syntax: SyntaxNode,
}

impl traits::ExprStmt<ConcreteSyntax> for ExprStmt {
  fn expr(&self) -> traits::MaybeOwned<Expr> {
    for child in self.syntax.children() {
      match Expr::try_from(child) {
        Ok(e) => return traits::MaybeOwned::Owned(e),
        Err(()) => {}
      }
    }
    panic!("InvalidExpressionNode");
  }
}

/// Represents a break statement backed by a concrete syntax node.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct ErrorStmt {
  syntax: SyntaxNode,
}

impl traits::ErrorStmt<ConcreteSyntax> for ErrorStmt {}

/// Represents an abstract trace statement backed by a concrete syntax node.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct TraceStmt {
  syntax: SyntaxNode,
}

impl traits::TraceStmt<ConcreteSyntax> for TraceStmt {
  fn value(&self) -> &<ConcreteSyntax as Syntax>::Expr {
    unimplemented!()
  }
}

/// Represents a break statement backed by a concrete syntax node.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct VarDecl {
  syntax: SyntaxNode,
}

impl traits::VarDecl<ConcreteSyntax> for VarDecl {}

/// Represents an expression backed by a concrete syntax node.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Expr {
  syntax: SyntaxNode,
}

/// Find the first non-paren expression (including self)
fn trim_paren(mut node: SyntaxNode) -> SyntaxNode {
  loop {
    assert!(node.kind().is_expr(), "kind = {:?}", node.kind());
    if node.kind() != SyntaxKind::NodeParenExpr {
      return node;
    } else {
      node = node.first_child().unwrap();
    }
  }
}

fn find_infix_op(symbols: &mut SyntaxElementChildren<As2Lang>) -> SyntaxToken {
  let mut found_left: bool = false;
  for symbol in symbols {
    match symbol {
      rowan::NodeOrToken::Token(t) => {
        if found_left && t.kind().is_infix_op() {
          return t;
        }
      }
      rowan::NodeOrToken::Node(_) => {
        found_left = true;
      }
    }
  }
  panic!("NoInfixOp");
}

impl traits::Expr for Expr {
  type Ast = ConcreteSyntax;

  fn cast(&self) -> ExprCast<ConcreteSyntax> {
    match trim_paren(self.syntax.clone()).kind() {
      SyntaxKind::NodeAssignExpr => traits::ExprCast::Assign(traits::MaybeOwned::Owned(AssignExpr {
        syntax: self.syntax.clone(),
      })),
      SyntaxKind::NodeInfixExpr => {
        let op_kind = find_infix_op(&mut self.syntax.children_with_tokens()).kind();
        match op_kind {
          k if k.is_logical_op() => traits::ExprCast::Logical(traits::MaybeOwned::Owned(LogicalExpr {
            syntax: self.syntax.clone(),
          })),
          k if k.is_bin_op() => traits::ExprCast::Bin(traits::MaybeOwned::Owned(BinExpr {
            syntax: self.syntax.clone(),
          })),
          _ => unreachable!(),
        }
      }
      SyntaxKind::NodeSeqExpr => traits::ExprCast::Seq(traits::MaybeOwned::Owned(SeqExpr {
        syntax: self.syntax.clone(),
      })),
      _ => traits::ExprCast::Error(traits::MaybeOwned::Owned(ErrorExpr {
        syntax: self.syntax.clone(),
      })),
    }
  }
}

impl TryFrom<SyntaxNode> for Expr {
  type Error = ();

  fn try_from(syntax: SyntaxNode) -> Result<Self, Self::Error> {
    if syntax.kind().is_expr() {
      Ok(Self { syntax })
    } else {
      Err(())
    }
  }
}

/// Represents an assignment expression backed by a concrete syntax node.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct AssignExpr {
  syntax: SyntaxNode,
}

impl traits::AssignExpr for AssignExpr {
  type Ast = ConcreteSyntax;

  fn target(&self) -> &<ConcreteSyntax as Syntax>::Pat {
    unimplemented!()
  }

  fn value(&self) -> traits::MaybeOwned<Expr> {
    let mut found_assign_op: bool = false;
    for symbol in self.syntax.children_with_tokens() {
      match symbol {
        rowan::NodeOrToken::Token(t) => {
          if t.kind().is_assign_op() {
            found_assign_op = true;
          }
        }
        rowan::NodeOrToken::Node(node) => {
          if found_assign_op {
            match Expr::try_from(node) {
              Ok(e) => return traits::MaybeOwned::Owned(e),
              Err(()) => {}
            }
          }
        }
      }
    }
    panic!("InvalidAssignExpr");
  }
}

/// Represents a binary expression backed by a concrete syntax node.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct BinExpr {
  syntax: SyntaxNode,
}

impl traits::BinExpr for BinExpr {
  type Ast = ConcreteSyntax;

  fn left(&self) -> traits::MaybeOwned<Expr> {
    let left_node = self.syntax.first_child().unwrap();
    match Expr::try_from(left_node) {
      Ok(e) => traits::MaybeOwned::Owned(e),
      Err(()) => unimplemented!(),
    }
  }

  fn right(&self) -> traits::MaybeOwned<Expr> {
    let mut nodes = self.syntax.children();
    nodes.next().unwrap();
    let right_node = nodes.next().unwrap();
    match Expr::try_from(right_node) {
      Ok(e) => traits::MaybeOwned::Owned(e),
      Err(()) => unimplemented!(),
    }
  }
}

/// Represents an error in expression position.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct ErrorExpr {
  syntax: SyntaxNode,
}

impl traits::ErrorExpr for ErrorExpr {
  type Ast = ConcreteSyntax;
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct LogicalExpr {
  syntax: SyntaxNode,
}

impl<'a> traits::LogicalExpr for LogicalExpr {
  type Ast = ConcreteSyntax;

  fn op(&self) -> traits::LogicalOp {
    let op_kind = find_infix_op(&mut self.syntax.children_with_tokens()).kind();
    match op_kind {
      SyntaxKind::TokenAmpAmp => traits::LogicalOp::Or,
      SyntaxKind::TokenPipePipe => traits::LogicalOp::And,
      _ => unreachable!(),
    }
  }

  fn left(&self) -> traits::MaybeOwned<Expr> {
    let left_node = self.syntax.first_child().unwrap();
    match Expr::try_from(left_node) {
      Ok(e) => traits::MaybeOwned::Owned(e),
      Err(()) => unimplemented!(),
    }
  }

  fn right(&self) -> traits::MaybeOwned<Expr> {
    let mut nodes = self.syntax.children();
    nodes.next().unwrap();
    let right_node = nodes.next().unwrap();
    match Expr::try_from(right_node) {
      Ok(e) => traits::MaybeOwned::Owned(e),
      Err(()) => unimplemented!(),
    }
  }
}

/// Represents a sequence expression backed by a concrete syntax node.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct SeqExpr {
  syntax: SyntaxNode,
}

impl traits::SeqExpr for SeqExpr {
  type Ast = ConcreteSyntax;

  #[cfg(not(feature = "gat"))]
  fn exprs<'a>(&'a self) -> Box<dyn Iterator<Item = traits::MaybeOwned<'a, Expr>> + 'a> {
    Box::new(ExprIter {
      inner: trim_paren(self.syntax.clone()).children(),
    })
  }

  #[cfg(feature = "gat")]
  type Exprs<'a> = ExprIter;

  #[cfg(feature = "gat")]
  fn exprs(&self) -> Self::Exprs<'_> {
    ExprIter {
      inner: trim_paren(self.syntax.clone()).children(),
    }
  }
}

pub struct ExprIter {
  inner: SyntaxNodeChildren<As2Lang>,
}

impl Iterator for ExprIter {
  type Item = traits::MaybeOwned<'static, Expr>;

  fn next(&mut self) -> Option<Self::Item> {
    while let Some(node) = self.inner.next() {
      match Expr::try_from(node) {
        Ok(e) => return Some(traits::MaybeOwned::Owned(e)),
        Err(()) => {}
      }
    }
    None
  }
}

/// Represents a pattern backed by a concrete syntax node.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Pat {
  syntax: SyntaxNode,
}

impl traits::Pat for Pat {
  type Ast = ConcreteSyntax;

  fn cast(&self) -> PatCast<ConcreteSyntax> {
    unimplemented!()
  }
}

/// Represents a member pattern backed by a concrete syntax node.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct MemberPat {
  syntax: SyntaxNode,
}

impl traits::MemberPat for MemberPat {
  type Ast = ConcreteSyntax;

  fn base(&self) -> &<ConcreteSyntax as Syntax>::Expr {
    unimplemented!()
  }

  fn key(&self) -> &<ConcreteSyntax as Syntax>::Expr {
    unimplemented!()
  }
}

/// Represents an identifier pattern backed by a concrete syntax node.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct IdentPat {
  syntax: SyntaxNode,
}

impl TryFrom<SyntaxNode> for IdentPat {
  type Error = ();

  fn try_from(syntax: SyntaxNode) -> Result<Self, Self::Error> {
    match syntax.kind() {
      SyntaxKind::NodeIdent => Ok(Self { syntax }),
      _ => Err(()),
    }
  }
}

impl traits::IdentPat for IdentPat {
  fn name(&self) -> &str {
    unimplemented!()
    // self.syntax.first_token().unwrap().text().as_str()
  }
}

/// Represents a string literal backed by a lossless syntax node.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct StrLit {
  syntax: SyntaxNode,
}

impl TryFrom<SyntaxNode> for StrLit {
  type Error = ();

  fn try_from(syntax: SyntaxNode) -> Result<Self, Self::Error> {
    match syntax.kind() {
      SyntaxKind::NodeStrLit => Ok(StrLit { syntax }),
      _ => Err(()),
    }
  }
}

impl traits::StrLit for StrLit {
  fn value(&self) -> Cow<str> {
    let token = self.syntax.first_token().unwrap();
    let text = token.text().as_str();
    Cow::Owned(unescape_string(text).unwrap())
  }
}

fn unescape_string(quoted: &str) -> Option<String> {
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

#[cfg(test)]
mod tests {
  use super::SyntaxKind;

  #[test]
  fn test_syntax_kind_variant_count() {
    assert_eq!(SyntaxKind::VARIANT_COUNT, 74);
  }
}
