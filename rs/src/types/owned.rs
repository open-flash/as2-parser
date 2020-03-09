use crate::types::ast::syntax;

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub enum OwnedSyntax {}

impl syntax::Syntax for OwnedSyntax {
  type Script = Script;

  type Stmt = Stmt;
  type ExprStmt = ExprStmt;
  type TraceStmt = TraceStmt;

  type Expr = Expr;
  type SeqExpr = SeqExpr;
  type AssignExpr = AssignExpr;
  type BinExpr = BinExpr;
  type StrLit = StrLit;

  type Pat = Pat;
  type MemberPat = MemberPat;
  type IdentPat = IdentPat;
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct Script {
  pub loc: (),
  pub stmts: Vec<Stmt>,
}

impl syntax::Script<OwnedSyntax> for Script {
  #[cfg(not(feature = "gat"))]
  fn stmts<'a>(&'a self) -> Box<dyn ExactSizeIterator<Item = &'a Stmt> + 'a> {
    Box::new(self.stmts.iter())
  }

  #[cfg(feature = "gat")]
  type Stmts<'a> = core::slice::Iter<'a, Stmt>;

  #[cfg(feature = "gat")]
  fn stmts(&self) -> Self::Stmts<'_> {
    self.stmts.iter()
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub enum Stmt {
  Expr(ExprStmt),
  /// Abstract Trace Statement
  ///
  /// ```aas2
  /// @trace("Hello, World!");
  /// ```
  Trace(TraceStmt),
  SyntaxError,
}

impl syntax::Stmt<OwnedSyntax> for Stmt {
  fn cast(&self) -> syntax::StmtCast<OwnedSyntax> {
    match self {
      Stmt::Expr(ref e) => syntax::StmtCast::Expr(e),
      Stmt::Trace(ref e) => syntax::StmtCast::Trace(e),
      Stmt::SyntaxError => syntax::StmtCast::SyntaxError,
    }
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct ExprStmt {
  pub loc: (),
  pub expr: Box<Expr>,
}

impl syntax::ExprStmt<OwnedSyntax> for ExprStmt {
  fn expr(&self) -> &Expr {
    &self.expr
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct TraceStmt {
  pub loc: (),
  pub value: Box<Expr>,
}

impl syntax::TraceStmt<OwnedSyntax> for TraceStmt {
  fn value(&self) -> &Expr {
    &self.value
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub enum Expr {
  Seq(SeqExpr),
  // Assign(AssignExpr),
  StrLit(StrLit),
  SyntaxError,
}

impl syntax::Expr<OwnedSyntax> for Expr {
  fn cast(&self) -> syntax::ExprCast<OwnedSyntax> {
    match self {
      Expr::Seq(ref e) => syntax::ExprCast::Seq(e),
      Expr::StrLit(ref e) => syntax::ExprCast::StrLit(e),
      Expr::SyntaxError => syntax::ExprCast::Error,
    }
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct SeqExpr {
  pub loc: (),
  pub exprs: Vec<Expr>,
}

impl syntax::SeqExpr<OwnedSyntax> for SeqExpr {
  #[cfg(not(feature = "gat"))]
  fn exprs<'a>(&'a self) -> Box<dyn ExactSizeIterator<Item = &'a Expr> + 'a> {
    Box::new(self.exprs.iter())
  }

  #[cfg(feature = "gat")]
  type Iter<'a> = core::slice::Iter<'a, Expr>;

  #[cfg(feature = "gat")]
  fn exprs(&self) -> Self::Iter<'_> {
    self.exprs.iter()
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct AssignExpr {
  pub loc: (),
  pub target: Box<Pat>,
  pub value: Box<Expr>,
}

impl syntax::AssignExpr<OwnedSyntax> for AssignExpr {
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
  pub left: Box<Expr>,
  pub right: Box<Expr>,
}

impl syntax::BinExpr<OwnedSyntax> for BinExpr {
  fn left(&self) -> &Expr {
    &self.left
  }

  fn right(&self) -> &Expr {
    &self.right
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct StrLit {
  pub loc: (),
  pub value: String,
}

impl syntax::StrLit for StrLit {
  fn value(&self) -> &str {
    &self.value
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub enum Pat {
  MemberPat(MemberPat),
  IdentPat(IdentPat),
  SyntaxError,
}

impl syntax::Pat<OwnedSyntax> for Pat {
  fn cast(&self) -> syntax::PatCast<OwnedSyntax> {
    match self {
      Pat::MemberPat(ref e) => syntax::PatCast::Member(e),
      Pat::IdentPat(ref e) => syntax::PatCast::Ident(e),
      Pat::SyntaxError => syntax::PatCast::SyntaxError,
    }
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct MemberPat {
  pub loc: (),
  pub base: Box<Expr>,
  pub key: Box<Expr>,
}

impl syntax::MemberPat<OwnedSyntax> for MemberPat {
  fn base(&self) -> &Expr {
    &self.base
  }

  fn key(&self) -> &Expr {
    &self.key
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct IdentPat {
  pub loc: (),
  pub name: String,
}

impl syntax::IdentPat for IdentPat {
  fn name(&self) -> &str {
    &self.name
  }
}

#[cfg(test)]
mod seq_expr_tests {
  use super::{Expr, SeqExpr, StrLit};
  use crate::types::ast::syntax::SeqExpr as _;

  #[test]
  fn test_eq_empty() {
    let left: SeqExpr = SeqExpr {
      loc: (),
      exprs: vec![
        Expr::StrLit(StrLit {
          loc: (),
          value: String::from("foo"),
        }),
        Expr::StrLit(StrLit {
          loc: (),
          value: String::from("bar"),
        }),
      ],
    };

    let right: SeqExpr = SeqExpr {
      loc: (),
      exprs: vec![
        Expr::StrLit(StrLit {
          loc: (),
          value: String::from("foo"),
        }),
        Expr::StrLit(StrLit {
          loc: (),
          value: String::from("bar"),
        }),
      ],
    };

    assert_eq!(left.exprs().len(), 2);
    assert_eq!(left, right);
  }
}
