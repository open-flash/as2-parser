use std::borrow::Cow;
use crate::ast;
use crate::syntax::{SyntaxNode, SyntaxKind, As2Lang, SyntaxToken};
use rowan::{SyntaxNodeChildren, SyntaxElementChildren};
use core::marker::PhantomData;
use core::convert::TryFrom;
use crate::escape::unescape_string;

macro_rules! impl_deref {
  ($struct_name:ident) => {
    impl core::ops::Deref for $struct_name {
      type Target = $struct_name;

      fn deref(&self) -> &Self::Target {
        &self
      }
    }
  };
}

macro_rules! impl_serialize {
  ($struct_name:ident, $adapter_name:ident) => {
    impl serde::Serialize for $struct_name {
      fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<<S as serde::Serializer>::Ok, <S as serde::Serializer>::Error> {
        crate::ast::ser::$adapter_name(self).serialize(serializer)
      }
    }
  };
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

pub struct StmtIter<'a> {
  inner: SyntaxNodeChildren<As2Lang>,
  phantom: PhantomData<&'a ()>,
}

impl <'a> StmtIter<'a> {
  pub fn new(children: SyntaxNodeChildren<As2Lang>) -> Self {
    Self {
      inner: children,
      phantom: PhantomData,
    }
  }
}

impl<'a> Iterator for StmtIter<'a> {
  type Item = Stmt;

  fn next(&mut self) -> Option<Self::Item> {
    while let Some(node) = self.inner.next() {
      match Stmt::try_from(node) {
        Ok(s) => return Some(s),
        Err(()) => {}
      }
    }
    None
  }
}

pub struct ExprIter<'a> {
  inner: SyntaxNodeChildren<As2Lang>,
  phantom: PhantomData<&'a ()>,
}

impl <'a> ExprIter<'a> {
  pub fn new(children: SyntaxNodeChildren<As2Lang>) -> Self {
    Self {
      inner: children,
      phantom: PhantomData,
    }
  }
}

impl<'a> Iterator for ExprIter<'a> {
  type Item = Expr;

  fn next(&mut self) -> Option<Self::Item> {
    while let Some(node) = self.inner.next() {
      match Expr::try_from(node) {
        Ok(s) => return Some(s),
        Err(()) => {}
      }
    }
    None
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub enum ConcreteSyntax {}

impl ast::Syntax for ConcreteSyntax {
  type Script<'a> = Script;

  type Stmt<'a> = Stmt;
  type BreakStmt<'a> = BreakStmt;
  type ErrorStmt<'a> = ErrorStmt;
  type ExprStmt<'a> = ExprStmt;
  type TraceStmt<'a> = TraceStmt;
  type VarDecl<'a> = VarDecl;

  type Expr<'a> = Expr;
  type AssignExpr<'a> = AssignExpr;
  type BinExpr<'a> = BinExpr;
  type BoolLit<'a> = BoolLit;
  type CallExpr<'a> = CallExpr;
  type ErrorExpr<'a> = ErrorExpr;
  type IdentExpr<'a> = IdentExpr;
  type LogicalExpr<'a> = LogicalExpr;
  type NumLit<'a> = NumLit;
  type SeqExpr<'a> = SeqExpr;
  type StrLit<'a> = StrLit;

  type Pat<'a> = Pat;
  type IdentPat<'a> = IdentPat;
  type MemberPat<'a> = MemberPat;

  type StmtRef<'a> = Stmt;
  type BreakStmtRef<'a> = BreakStmt;
  type ErrorStmtRef<'a> = ErrorStmt;
  type ExprStmtRef<'a> = ExprStmt;
  type TraceStmtRef<'a> = TraceStmt;
  type VarDeclRef<'a> = VarDecl;

  type ExprRef<'a> = Expr;
  type AssignExprRef<'a> = AssignExpr;
  type BinExprRef<'a> = BinExpr;
  type BoolLitRef<'a> = BoolLit;
  type CallExprRef<'a> = CallExpr;
  type ErrorExprRef<'a> = ErrorExpr;
  type IdentExprRef<'a> = IdentExpr;
  type LogicalExprRef<'a> = LogicalExpr;
  type NumLitRef<'a> = NumLit;
  type SeqExprRef<'a> = SeqExpr;
  type StrLitRef<'a> = StrLit;

  type PatRef<'a> = Pat;
  type IdentPatRef<'a> = &'a IdentPat;
  type MemberPatRef<'a> = &'a MemberPat;
}

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

impl ast::Script for Script {
  type Ast = ConcreteSyntax;
  type StmtIter<'a> = StmtIter<'a>;

  fn stmts<'a>(&'a self) -> Self::StmtIter<'a> {
    StmtIter::new(self.syntax.children())
  }
}

/// Represents a statement backed by a concrete syntax node.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Stmt {
  syntax: SyntaxNode,
}

impl_deref!(Stmt);

impl TryFrom<SyntaxNode> for Stmt {
  type Error = ();

  fn try_from(syntax: SyntaxNode) -> Result<Self, ()> {
    if syntax.kind().is_stmt() {
      Ok(Self { syntax })
    } else {
      Err(())
    }
  }
}

impl ast::Stmt for Stmt {
  type Ast = ConcreteSyntax;

  fn cast(&self) -> ast::StmtCast<ConcreteSyntax> {
    match self.syntax.kind() {
      SyntaxKind::NodeExprStmt => ast::StmtCast::Expr(ExprStmt {
        syntax: self.syntax.clone(),
      }),
      SyntaxKind::NodeVarDecl => ast::StmtCast::VarDecl(VarDecl {
        syntax: self.syntax.clone(),
      }),
      _ => ast::StmtCast::Error(ErrorStmt {
        syntax: self.syntax.clone(),
      }),
    }
  }
}

/// Represents a break statement backed by a concrete syntax node.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct BreakStmt {
  syntax: SyntaxNode,
}

impl_deref!(BreakStmt);

impl ast::BreakStmt for BreakStmt {
  type Ast = ConcreteSyntax;
}

/// Represents an error in statement position.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct ErrorStmt {
  syntax: SyntaxNode,
}

impl_deref!(ErrorStmt);

impl ast::ErrorStmt for ErrorStmt {
  type Ast = ConcreteSyntax;
}

/// Represents an expression statement backed by a concrete syntax node.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct ExprStmt {
  syntax: SyntaxNode,
}

impl_deref!(ExprStmt);

impl ast::ExprStmt for ExprStmt {
  type Ast = ConcreteSyntax;

  fn expr(&self) -> Expr {
    for child in self.syntax.children() {
      match Expr::try_from(child) {
        Ok(e) => return e,
        Err(()) => {}
      }
    }
    panic!("InvalidExprStmtNode");
  }
}

/// Represents an abstract trace statement backed by a concrete syntax node.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct TraceStmt {
  syntax: SyntaxNode,
}

impl_deref!(TraceStmt);

impl ast::TraceStmt for TraceStmt {
  type Ast = ConcreteSyntax;

  fn value(&self) -> Expr {
    unimplemented!()
  }
}

/// Represents a break statement backed by a concrete syntax node.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct VarDecl {
  syntax: SyntaxNode,
}

impl_deref!(VarDecl);

impl ast::VarDecl for VarDecl {
  type Ast = ConcreteSyntax;
}

/// Represents an expression backed by a concrete syntax node.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Expr {
  syntax: SyntaxNode,
}

impl_deref!(Expr);

impl TryFrom<SyntaxNode> for Expr {
  type Error = ();

  fn try_from(syntax: SyntaxNode) -> Result<Self, ()> {
    let trimmed = trim_paren(syntax);
    if trimmed.kind().is_expr() {
      Ok(Self { syntax: trimmed })
    } else {
      Err(())
    }
  }
}

impl ast::Expr for Expr {
  type Ast = ConcreteSyntax;

  fn cast(&self) -> ast::ExprCast<ConcreteSyntax> {
    match trim_paren(self.syntax.clone()).kind() {
      SyntaxKind::NodeAssignExpr => ast::ExprCast::Assign(AssignExpr {
        syntax: self.syntax.clone(),
      }),
      SyntaxKind::NodeCallExpr => ast::ExprCast::Call(CallExpr {
        syntax: self.syntax.clone(),
      }),
      SyntaxKind::NodeIdent => ast::ExprCast::Ident(IdentExpr {
        syntax: self.syntax.clone(),
      }),
      SyntaxKind::NodeInfixExpr => {
        let op_kind = find_infix_op(&mut self.syntax.children_with_tokens()).kind();
        match op_kind {
          k if k.is_logical_op() => ast::ExprCast::Logical(LogicalExpr {
            syntax: self.syntax.clone(),
          }),
          k if k.is_bin_op() => ast::ExprCast::Bin(BinExpr {
            syntax: self.syntax.clone(),
          }),
          _ => unreachable!(),
        }
      }
      SyntaxKind::NodeSeqExpr => ast::ExprCast::Seq(SeqExpr {
        syntax: self.syntax.clone(),
      }),
      SyntaxKind::NodeStrLit => ast::ExprCast::Str(StrLit {
        syntax: self.syntax.clone(),
      }),
      _ => ast::ExprCast::Error(ErrorExpr {
        syntax: self.syntax.clone(),
      }),
    }
  }
}

/// Represents an assignment expression backed by a concrete syntax node.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct AssignExpr {
  syntax: SyntaxNode,
}

impl_deref!(AssignExpr);

impl ast::AssignExpr for AssignExpr {
  type Ast = ConcreteSyntax;

  fn target(&self) -> Pat {
    unimplemented!()
  }

  fn value(&self) -> Expr {
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
              Ok(e) => return e,
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

impl_deref!(BinExpr);

impl ast::BinExpr for BinExpr {
  type Ast = ConcreteSyntax;

  fn op(&self) -> ast::BinOp {
    let op_kind = find_infix_op(&mut self.syntax.children_with_tokens()).kind();
    match op_kind {
      SyntaxKind::TokenPlus => ast::BinOp::Add,
      SyntaxKind::TokenAmp => ast::BinOp::BitAnd,
      _ => unimplemented!(),
    }
  }

  fn left(&self) -> Expr {
    let left_node = self.syntax.first_child().unwrap();
    match Expr::try_from(left_node) {
      Ok(e) => e,
      Err(()) => unimplemented!(),
    }
  }

  fn right(&self) -> Expr {
    let mut nodes = self.syntax.children();
    nodes.next().unwrap();
    let right_node = nodes.next().unwrap();
    match Expr::try_from(right_node) {
      Ok(e) => e,
      Err(()) => unimplemented!(),
    }
  }
}

/// Represents a boolean literal backed by a lossless syntax node.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct BoolLit {
  syntax: SyntaxNode,
}

impl_deref!(BoolLit);

impl TryFrom<SyntaxNode> for BoolLit {
  type Error = ();

  fn try_from(syntax: SyntaxNode) -> Result<Self, ()> {
    match syntax.kind() {
      SyntaxKind::NodeBoolLit => Ok(BoolLit { syntax }),
      _ => Err(()),
    }
  }
}

impl ast::BoolLit for BoolLit {
  fn value(&self) -> bool {
    let token = self.syntax.first_token().unwrap();
    let text = token.text().as_str();
    text.len() > 0
  }
}

/// Represents a call expression backed by a concrete syntax node.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct CallExpr {
  syntax: SyntaxNode,
}

impl_deref!(CallExpr);

impl ast::CallExpr for CallExpr {
  type Ast = ConcreteSyntax;
  type ExprIter<'a> = ExprIter<'a>;

  fn callee(&self) -> Expr {
    let callee_node = self.syntax.first_child().unwrap();
    match Expr::try_from(callee_node) {
      Ok(e) => e,
      Err(()) => unimplemented!(),
    }
  }

  fn args(&self) -> ExprIter {
    let call_expr = trim_paren(self.syntax.clone());
    let mut nodes = call_expr.children();
    nodes.next().unwrap(); // Skip callee
    let args: SyntaxNode = nodes.next().unwrap();

    ExprIter::new(args.children())
  }
}

/// Represents an error in expression position.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct ErrorExpr {
  syntax: SyntaxNode,
}

impl_deref!(ErrorExpr);

impl ast::ErrorExpr for ErrorExpr {
  type Ast = ConcreteSyntax;
}

/// Represents a call expression backed by a concrete syntax node.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct IdentExpr {
  syntax: SyntaxNode,
}

impl_deref!(IdentExpr);

impl ast::IdentExpr for IdentExpr {
  fn name(&self) -> Cow<str> {
    let token = self.syntax.first_token().unwrap();
    let text = token.text().as_str();
    Cow::Owned(text.to_owned())
  }
}

/// Represents a logical expression backed by a concrete syntax node.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct LogicalExpr {
  syntax: SyntaxNode,
}

impl_deref!(LogicalExpr);

impl ast::LogicalExpr for LogicalExpr {
  type Ast = ConcreteSyntax;

  fn op(&self) -> ast::LogicalOp {
    let op_kind = find_infix_op(&mut self.syntax.children_with_tokens()).kind();
    match op_kind {
      SyntaxKind::TokenAmpAmp => ast::LogicalOp::And,
      SyntaxKind::TokenPipePipe => ast::LogicalOp::Or,
      _ => unimplemented!(),
    }
  }

  fn left(&self) -> Expr {
    let left_node = self.syntax.first_child().unwrap();
    match Expr::try_from(left_node) {
      Ok(e) => e,
      Err(()) => unimplemented!(),
    }
  }

  fn right(&self) -> Expr {
    let mut nodes = self.syntax.children();
    nodes.next().unwrap();
    let right_node = nodes.next().unwrap();
    match Expr::try_from(right_node) {
      Ok(e) => e,
      Err(()) => unimplemented!(),
    }
  }
}

/// Represents a number literal backed by a lossless syntax node.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct NumLit {
  syntax: SyntaxNode,
}

impl_deref!(NumLit);

impl TryFrom<SyntaxNode> for NumLit {
  type Error = ();

  fn try_from(syntax: SyntaxNode) -> Result<Self, ()> {
    match syntax.kind() {
      SyntaxKind::NodeNumLit => Ok(NumLit { syntax }),
      _ => Err(()),
    }
  }
}

impl ast::NumLit for NumLit {
  fn value(&self) -> f64 {
    let token = self.syntax.first_token().unwrap();
    let text = token.text().as_str();
    text.len() as f64
  }
}

/// Represents a sequence expression backed by a concrete syntax node.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct SeqExpr {
  syntax: SyntaxNode,
}

impl_deref!(SeqExpr);

impl ast::SeqExpr for SeqExpr {
  type Ast = ConcreteSyntax;
  type ExprIter<'a> = ExprIter<'a>;

  fn exprs<'a>(&'a self) -> Self::ExprIter<'a> {
    ExprIter::new(self.syntax.children())
  }
}

/// Represents a string literal backed by a lossless syntax node.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct StrLit {
  syntax: SyntaxNode,
}

impl_deref!(StrLit);

impl TryFrom<SyntaxNode> for StrLit {
  type Error = ();

  fn try_from(syntax: SyntaxNode) -> Result<Self, ()> {
    match syntax.kind() {
      SyntaxKind::NodeStrLit => Ok(StrLit { syntax }),
      _ => Err(()),
    }
  }
}

impl ast::StrLit for StrLit {
  fn value(&self) -> Cow<str> {
    let token = self.syntax.first_token().unwrap();
    let text = token.text().as_str();
    Cow::Owned(unescape_string(text).unwrap())
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum Pat {
  Ident(IdentPat),
  Member(MemberPat),
}

impl_deref!(Pat);

impl ast::Pat for Pat {
  type Ast = ConcreteSyntax;

  fn cast(&self) -> ast::PatCast<ConcreteSyntax> {
    match self {
      Pat::Ident(ref e) => ast::PatCast::Ident(e),
      Pat::Member(ref e) => ast::PatCast::Member(e),
    }
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct IdentPat {
  syntax: SyntaxNode,
}

impl ast::IdentPat for IdentPat {
  fn name(&self) -> Cow<str> {
    unimplemented!()
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct MemberPat {
  syntax: SyntaxNode,
}

impl ast::MemberPat for MemberPat {
  type Ast = ConcreteSyntax;

  fn base(&self) -> Expr {
    unimplemented!()
  }

  fn key(&self) -> Expr {
    unimplemented!()
  }
}
