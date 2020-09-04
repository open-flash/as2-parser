use std::borrow::Cow;
use crate::ast;
use ordered_float::OrderedFloat;
use core::cmp::Ordering;
use core::hash::Hasher;

macro_rules! impl_serialize {
  ($struct_name:ident, $adapter_name:ident) => {
    impl serde::Serialize for $struct_name {
      fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<<S as serde::Serializer>::Ok, <S as serde::Serializer>::Error> {
        crate::ast::ser::$adapter_name(self).serialize(serializer)
      }
    }
  };
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub enum OwnedSyntax {}

impl ast::Syntax for OwnedSyntax {
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

  type StmtRef<'a> = &'a Stmt;
  type BreakStmtRef<'a> = &'a BreakStmt;
  type ErrorStmtRef<'a> = &'a ErrorStmt;
  type ExprStmtRef<'a> = &'a ExprStmt;
  type TraceStmtRef<'a> = &'a TraceStmt;
  type VarDeclRef<'a> = &'a VarDecl;

  type ExprRef<'a> = &'a Expr;
  type AssignExprRef<'a> = &'a AssignExpr;
  type BinExprRef<'a> = &'a BinExpr;
  type BoolLitRef<'a> = &'a BoolLit;
  type CallExprRef<'a> = &'a CallExpr;
  type ErrorExprRef<'a> = &'a ErrorExpr;
  type IdentExprRef<'a> = &'a IdentExpr;
  type LogicalExprRef<'a> = &'a LogicalExpr;
  type NumLitRef<'a> = &'a NumLit;
  type SeqExprRef<'a> = &'a SeqExpr;
  type StrLitRef<'a> = &'a StrLit;

  type PatRef<'a> = &'a Pat;
  type IdentPatRef<'a> = &'a IdentPat;
  type MemberPatRef<'a> = &'a MemberPat;
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct Script {
  pub loc: (),
  pub stmts: Vec<Stmt>,
}

impl_serialize!(Script, SerializeScript);

impl ast::Script for Script {
  type Ast = OwnedSyntax;
  type StmtIter<'a> = core::slice::Iter<'a, Stmt>;

  fn stmts<'a>(&'a self) -> Self::StmtIter<'a> {
    self.stmts.iter()
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub enum Stmt {
  Break(BreakStmt),
  Error(ErrorStmt),
  Expr(ExprStmt),
  Trace(TraceStmt),
  VarDecl(VarDecl),
}

impl ast::Stmt for Stmt {
  type Ast = OwnedSyntax;

  fn cast(&self) -> ast::StmtCast<OwnedSyntax> {
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
  type Ast = OwnedSyntax;
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct ErrorStmt {
  pub loc: (),
}

impl ast::ErrorStmt for ErrorStmt {
  type Ast = OwnedSyntax;
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct ExprStmt {
  pub loc: (),
  pub expr: Expr,
}

impl ast::ExprStmt for ExprStmt {
  type Ast = OwnedSyntax;

  fn expr(&self) -> &Expr {
    &self.expr
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct TraceStmt {
  pub loc: (),
  pub value: Expr,
}

impl ast::TraceStmt for TraceStmt {
  type Ast = OwnedSyntax;

  fn value(&self) -> &Expr {
    &self.value
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct VarDecl {
  pub loc: (),
}

impl ast::VarDecl for VarDecl {
  type Ast = OwnedSyntax;
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub enum Expr {
  Assign(AssignExpr),
  Bin(BinExpr),
  Bool(BoolLit),
  Call(CallExpr),
  Error(ErrorExpr),
  Ident(IdentExpr),
  Logical(LogicalExpr),
  Num(NumLit),
  Seq(SeqExpr),
  Str(StrLit),
}

impl ast::Expr for Expr {
  type Ast = OwnedSyntax;

  fn cast(&self) -> ast::ExprCast<OwnedSyntax> {
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
pub struct AssignExpr {
  pub loc: (),
  pub target: Pat,
  pub value: Box<Expr>,
}

impl ast::AssignExpr for AssignExpr {
  type Ast = OwnedSyntax;

  fn target(&self) -> &Pat {
    &self.target
  }

  fn value(&self) -> &Expr {
    &self.value
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct BinExpr {
  pub loc: (),
  pub op: ast::BinOp,
  pub left: Box<Expr>,
  pub right: Box<Expr>,
}

impl ast::BinExpr for BinExpr {
  type Ast = OwnedSyntax;

  fn op(&self) -> ast::BinOp {
    self.op
  }

  fn left(&self) -> &Expr {
    &self.left
  }

  fn right(&self) -> &Expr {
    &self.right
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
pub struct CallExpr {
  pub loc: (),
  pub callee: Box<Expr>,
  pub args: Vec<Expr>,
}

impl ast::CallExpr for CallExpr {
  type Ast = OwnedSyntax;
  type ExprIter<'a> = core::slice::Iter<'a, Expr>;

  fn callee(&self) -> &Expr {
    &self.callee
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
  type Ast = OwnedSyntax;
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct IdentExpr {
  pub loc: (),
  pub name: String,
}

impl ast::IdentExpr for IdentExpr {
  fn name(&self) -> Cow<str> {
    Cow::Borrowed(&self.name)
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct LogicalExpr {
  pub loc: (),
  pub op: ast::LogicalOp,
  pub left: Box<Expr>,
  pub right: Box<Expr>,
}

impl ast::LogicalExpr for LogicalExpr {
  type Ast = OwnedSyntax;

  fn op(&self) -> ast::LogicalOp {
    self.op
  }

  fn left(&self) -> &Expr {
    &self.left
  }

  fn right(&self) -> &Expr {
    &self.right
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
pub struct SeqExpr {
  pub loc: (),
  pub exprs: Vec<Expr>,
}

impl ast::SeqExpr for SeqExpr {
  type Ast = OwnedSyntax;
  type ExprIter<'a> = core::slice::Iter<'a, Expr>;

  fn exprs<'a>(&'a self) -> Self::ExprIter<'a> {
    self.exprs.iter()
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct StrLit {
  pub loc: (),
  pub value: String,
}

impl ast::StrLit for StrLit {
  fn value(&self) -> Cow<str> {
    Cow::Borrowed(&self.value)
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub enum Pat {
  Ident(IdentPat),
  Member(MemberPat),
}

impl ast::Pat for Pat {
  type Ast = OwnedSyntax;

  fn cast(&self) -> ast::PatCast<OwnedSyntax> {
    match self {
      Pat::Ident(ref e) => ast::PatCast::Ident(e),
      Pat::Member(ref e) => ast::PatCast::Member(e),
    }
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct IdentPat {
  pub loc: (),
  pub name: String,
}

impl ast::IdentPat for IdentPat {
  fn name(&self) -> Cow<str> {
    Cow::Borrowed(&self.name)
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct MemberPat {
  pub loc: (),
  pub base: Box<Expr>,
  pub key: Box<Expr>,
}

impl ast::MemberPat for MemberPat {
  type Ast = OwnedSyntax;

  fn base(&self) -> &Expr {
    &self.base
  }

  fn key(&self) -> &Expr {
    &self.key
  }
}


#[cfg(test)]
mod seq_expr_tests {
  use super::{Expr, SeqExpr, StrLit};
  use crate::ast::SeqExpr as _;

  #[test]
  fn test_eq_empty() {
    let left: SeqExpr = SeqExpr {
      loc: (),
      exprs: vec![
        Expr::Str(StrLit {
          loc: (),
          value: String::from("foo"),
        }),
        Expr::Str(StrLit {
          loc: (),
          value: String::from("bar"),
        }),
      ],
    };

    let right: SeqExpr = SeqExpr {
      loc: (),
      exprs: vec![
        Expr::Str(StrLit {
          loc: (),
          value: String::from("foo"),
        }),
        Expr::Str(StrLit {
          loc: (),
          value: String::from("bar"),
        }),
      ],
    };

    assert_eq!(left.exprs().size_hint(), (2, Some(2)));
    assert_eq!(left, right);
  }
}

#[cfg(test)]
mod script_tests {
  use super::{BreakStmt, Script, Stmt};

  #[test]
  fn test_serialize() {
    let script: Script = Script {
      loc: (),
      stmts: vec![Stmt::Break(BreakStmt { loc: () }), Stmt::Break(BreakStmt { loc: () })],
    };

    let actual_json = serde_json::to_string(&script).unwrap();
    let expected_json = String::from("{\"type\":\"Script\",\"body\":[{\"type\":\"BreakStmt\"},{\"type\":\"BreakStmt\"}]}");

    assert_eq!(actual_json, expected_json);
  }
}
