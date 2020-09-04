use std::borrow::Cow;
use crate::ast;
use ordered_float::OrderedFloat;
use core::cmp::Ordering;
use core::hash::Hasher;

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub enum BorrowedSyntax {}

impl ast::Syntax for BorrowedSyntax {
  type Script<'a> = Script<'a>;

  type Stmt<'a> = Stmt<'a>;
  type BreakStmt<'a> = BreakStmt;
  type ErrorStmt<'a> = ErrorStmt;
  type ExprStmt<'a> = ExprStmt<'a>;
  type TraceStmt<'a> = TraceStmt<'a>;
  type VarDecl<'a> = VarDecl;

  type Expr<'a> = Expr<'a>;
  type AssignExpr<'a> = AssignExpr<'a>;
  type BinExpr<'a> = BinExpr<'a>;
  type BoolLit<'a> = BoolLit;
  type CallExpr<'a> = CallExpr<'a>;
  type ErrorExpr<'a> = ErrorExpr;
  type IdentExpr<'a> = IdentExpr<'a>;
  type LogicalExpr<'a> = LogicalExpr<'a>;
  type NumLit<'a> = NumLit;
  type SeqExpr<'a> = SeqExpr<'a>;
  type StrLit<'a> = StrLit<'a>;

  type Pat<'a> = Pat<'a>;
  type IdentPat<'a> = IdentPat<'a>;
  type MemberPat<'a> = MemberPat<'a>;

  type StmtRef<'a> = &'a Stmt<'a>;
  type BreakStmtRef<'a> = &'a BreakStmt;
  type ErrorStmtRef<'a> = &'a ErrorStmt;
  type ExprStmtRef<'a> = &'a ExprStmt<'a>;
  type TraceStmtRef<'a> = &'a TraceStmt<'a>;
  type VarDeclRef<'a> = &'a VarDecl;

  type ExprRef<'a> = &'a Expr<'a>;
  type AssignExprRef<'a> = &'a AssignExpr<'a>;
  type BinExprRef<'a> = &'a BinExpr<'a>;
  type BoolLitRef<'a> = &'a BoolLit;
  type CallExprRef<'a> = &'a CallExpr<'a>;
  type ErrorExprRef<'a> = &'a ErrorExpr;
  type IdentExprRef<'a> = &'a IdentExpr<'a>;
  type LogicalExprRef<'a> = &'a LogicalExpr<'a>;
  type NumLitRef<'a> = &'a NumLit;
  type SeqExprRef<'a> = &'a SeqExpr<'a>;
  type StrLitRef<'a> = &'a StrLit<'a>;

  type PatRef<'a> = &'a Pat<'a>;
  type IdentPatRef<'a> = &'a IdentPat<'a>;
  type MemberPatRef<'a> = &'a MemberPat<'a>;
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct Script<'s> {
  pub loc: (),
  pub stmts: &'s [Stmt<'s>],
}

impl<'s> ast::Script for Script<'s> {
  type Ast = BorrowedSyntax;
  type StmtIter<'a> = core::slice::Iter<'a, Stmt<'a>>;

  fn stmts<'a>(&'a self) -> Self::StmtIter<'a> {
    self.stmts.iter()
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub enum Stmt<'s> {
  Break(BreakStmt),
  Error(ErrorStmt),
  Expr(ExprStmt<'s>),
  Trace(TraceStmt<'s>),
  VarDecl(VarDecl),
}

impl<'s> ast::Stmt for Stmt<'s> {
  type Ast = BorrowedSyntax;

  fn cast(&self) -> ast::StmtCast<BorrowedSyntax> {
    match self {
      Stmt::Break(ref s) => ast::StmtCast::Break(s),
      Stmt::Error(ref s) => ast::StmtCast::Error(s),
      Stmt::Expr(ref s) => ast::StmtCast::Expr(s),
      Stmt::Trace(ref s) => ast::StmtCast::Trace(s),
      Stmt::VarDecl(ref s) => ast::StmtCast::VarDecl(s),
    }
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct BreakStmt {
  pub loc: (),
}

impl ast::BreakStmt for BreakStmt {
  type Ast = BorrowedSyntax;
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct ErrorStmt {
  pub loc: (),
}

impl ast::ErrorStmt for ErrorStmt {
  type Ast = BorrowedSyntax;
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct ExprStmt<'s> {
  pub loc: (),
  pub expr: &'s Expr<'s>,
}

impl<'s> ast::ExprStmt for ExprStmt<'s> {
  type Ast = BorrowedSyntax;

  fn expr(&self) -> &Expr {
    self.expr
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct TraceStmt<'s> {
  pub loc: (),
  pub value: &'s Expr<'s>,
}

impl<'s> ast::TraceStmt for TraceStmt<'s> {
  type Ast = BorrowedSyntax;

  fn value(&self) -> &Expr {
    self.value
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct VarDecl {
  pub loc: (),
}

impl ast::VarDecl for VarDecl {
  type Ast = BorrowedSyntax;
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub enum Expr<'s> {
  Assign(AssignExpr<'s>),
  Bin(BinExpr<'s>),
  Bool(BoolLit),
  Call(CallExpr<'s>),
  Error(ErrorExpr),
  Ident(IdentExpr<'s>),
  Logical(LogicalExpr<'s>),
  Num(NumLit),
  Seq(SeqExpr<'s>),
  Str(StrLit<'s>),
}

impl<'s> ast::Expr for Expr<'s> {
  type Ast = BorrowedSyntax;

  fn cast(&self) -> ast::ExprCast<BorrowedSyntax> {
    match self {
      Expr::Assign(ref e) => ast::ExprCast::Assign(e),
      Expr::Bin(ref e) => ast::ExprCast::Bin(e),
      Expr::Bool(ref e) => ast::ExprCast::Bool(e),
      Expr::Call(ref e) => ast::ExprCast::Call(e),
      Expr::Error(ref e) => ast::ExprCast::Error(e),
      Expr::Ident(ref e) => ast::ExprCast::Ident(e),
      Expr::Logical(ref e) => ast::ExprCast::Logical(e),
      Expr::Num(ref e) => ast::ExprCast::Num(e),
      Expr::Seq(ref e) => ast::ExprCast::Seq(e),
      Expr::Str(ref e) => ast::ExprCast::Str(e),
    }
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct AssignExpr<'s> {
  pub loc: (),
  pub target: &'s Pat<'s>,
  pub value: &'s Expr<'s>,
}

impl<'s> ast::AssignExpr for AssignExpr<'s> {
  type Ast = BorrowedSyntax;

  fn target(&self) -> &Pat {
    self.target
  }

  fn value(&self) -> &Expr {
    self.value
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct BinExpr<'s> {
  pub loc: (),
  pub op: ast::BinOp,
  pub left: &'s Expr<'s>,
  pub right: &'s Expr<'s>,
}

impl<'s> ast::BinExpr for BinExpr<'s> {
  type Ast = BorrowedSyntax;

  fn op(&self) -> ast::BinOp {
    self.op
  }

  fn left(&self) -> &Expr {
    self.left
  }

  fn right(&self) -> &Expr {
    self.right
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct BoolLit {
  pub loc: (),
  pub value: bool,
}

impl ast::BoolLit for BoolLit {
  fn value(&self) -> bool {
    self.value
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct CallExpr<'s> {
  pub loc: (),
  pub callee: &'s Expr<'s>,
  pub args: &'s [Expr<'s>],
}

impl<'s> ast::CallExpr for CallExpr<'s> {
  type Ast = BorrowedSyntax;
  type ExprIter<'a> = core::slice::Iter<'a, Expr<'a>>;

  fn callee(&self) -> &Expr {
    self.callee
  }

  fn args<'a>(&'a self) -> Self::ExprIter<'a> {
    self.args.iter()
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct ErrorExpr {
  pub loc: (),
}

impl ast::ErrorExpr for ErrorExpr {
  type Ast = BorrowedSyntax;
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct IdentExpr<'s> {
  pub loc: (),
  pub name: &'s str,
}

impl<'s> ast::IdentExpr for IdentExpr<'s> {
  fn name(&self) -> Cow<str> {
    Cow::Borrowed(self.name)
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct LogicalExpr<'s> {
  pub loc: (),
  pub op: ast::LogicalOp,
  pub left: &'s Expr<'s>,
  pub right: &'s Expr<'s>,
}

impl<'s> ast::LogicalExpr for LogicalExpr<'s> {
  type Ast = BorrowedSyntax;

  fn op(&self) -> ast::LogicalOp {
    self.op
  }

  fn left(&self) -> &Expr {
    self.left
  }

  fn right(&self) -> &Expr {
    self.right
  }
}

#[derive(Debug, Clone)]
pub struct NumLit {
  pub loc: (),
  pub value: f64,
}

impl core::cmp::PartialEq for NumLit {
  fn eq(&self, other: &Self) -> bool {
    self.loc == other.loc && OrderedFloat(self.value) == OrderedFloat(other.value)
  }
}

impl core::cmp::Eq for NumLit {}

impl core::cmp::PartialOrd for NumLit {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl core::cmp::Ord for NumLit {
  fn cmp(&self, other: &Self) -> Ordering {
    match self.loc.cmp(&other.loc) {
      Ordering::Less => return Ordering::Less,
      Ordering::Greater => return Ordering::Greater,
      Ordering::Equal => OrderedFloat(self.value).cmp(&OrderedFloat(other.value)),
    }
  }
}

impl core::hash::Hash for NumLit {
  fn hash<H: Hasher>(&self, state: &mut H) {
    self.loc.hash(state);
    OrderedFloat(self.value).hash(state);
  }
}

impl ast::NumLit for NumLit {
  fn value(&self) -> f64 {
    self.value
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct SeqExpr<'s> {
  pub loc: (),
  pub exprs: &'s [Expr<'s>],
}

impl<'s> ast::SeqExpr for SeqExpr<'s> {
  type Ast = BorrowedSyntax;
  type ExprIter<'a> = core::slice::Iter<'a, Expr<'a>>;

  fn exprs<'a>(&'a self) -> Self::ExprIter<'a> {
    self.exprs.iter()
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct StrLit<'s> {
  pub loc: (),
  pub name: &'s str,
}

impl<'s> ast::StrLit for StrLit<'s> {
  fn value(&self) -> Cow<str> {
    Cow::Borrowed(self.name)
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub enum Pat<'s> {
  Ident(IdentPat<'s>),
  Member(MemberPat<'s>),
}

impl<'s> ast::Pat for Pat<'s> {
  type Ast = BorrowedSyntax;

  fn cast(&self) -> ast::PatCast<BorrowedSyntax> {
    match self {
      Pat::Ident(ref e) => ast::PatCast::Ident(e),
      Pat::Member(ref e) => ast::PatCast::Member(e),
    }
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct IdentPat<'s> {
  pub loc: (),
  pub name: &'s str,
}

impl<'s> ast::IdentPat for IdentPat<'s> {
  fn name(&self) -> Cow<str> {
    Cow::Borrowed(self.name)
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct MemberPat<'s> {
  pub loc: (),
  pub base: &'s Expr<'s>,
  pub key: &'s Expr<'s>,
}

impl<'s> ast::MemberPat for MemberPat<'s> {
  type Ast = BorrowedSyntax;

  fn base(&self) -> &Expr {
    self.base
  }

  fn key(&self) -> &Expr {
    self.key
  }
}
