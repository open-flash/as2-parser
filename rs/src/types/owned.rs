use crate::types::ast::traits;
use serde::{Deserialize, Serialize, Serializer};
use std::borrow::Cow;

macro_rules! impl_serialize {
  ($struct_name:ident, $adapter_name:ident) => {
    impl Serialize for $struct_name {
      fn serialize<S: Serializer>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error> {
        crate::types::ast::ser::$adapter_name::<OwnedSyntax>(self).serialize(serializer)
      }
    }
  };
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub enum OwnedSyntax {}

impl traits::Syntax for OwnedSyntax {
  type Script = Script;

  type Stmt = Stmt;
  type BreakStmt = BreakStmt;
  type ExprStmt = ExprStmt;
  type TraceStmt = TraceStmt;

  type Expr = Expr;
  type AssignExpr = AssignExpr;
  type BinExpr = BinExpr;
  type SeqExpr = SeqExpr;
  type StrLit = StrLit;

  type Pat = Pat;
  type MemberPat = MemberPat;
  type IdentPat = IdentPat;
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash, Deserialize)]
pub struct Script {
  pub loc: (),
  pub stmts: Vec<Stmt>,
}

impl_serialize!(Script, SerializeScript);

impl traits::Script<OwnedSyntax> for Script {
  #[cfg(not(feature = "gat"))]
  fn stmts<'a>(&'a self) -> Box<dyn Iterator<Item = traits::MaybeOwned<'a, Stmt>> + 'a> {
    Box::new(self.stmts.iter().map(|stmt| traits::MaybeOwned::Borrowed(stmt)))
  }

  #[allow(clippy::type_complexity)]
  #[cfg(feature = "gat")]
  type Stmts<'a> = core::iter::Map<core::slice::Iter<'a, Stmt>, for<'r> fn(&'r Stmt) -> traits::MaybeOwned<'r, Stmt>>;

  #[cfg(feature = "gat")]
  fn stmts(&self) -> Self::Stmts<'_> {
    self.stmts.iter().map(|s| traits::MaybeOwned::Borrowed(s))
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash, Deserialize)]
pub enum Stmt {
  Break(BreakStmt),
  Expr(ExprStmt),
  /// Abstract Trace Statement
  ///
  /// ```aas2
  /// @trace("Hello, World!");
  /// ```
  Trace(TraceStmt),
  SyntaxError,
}

impl traits::Stmt<OwnedSyntax> for Stmt {
  fn cast(&self) -> traits::StmtCast<OwnedSyntax> {
    match self {
      Stmt::Break(ref e) => traits::StmtCast::Break(traits::MaybeOwned::Borrowed(e)),
      Stmt::Expr(ref e) => traits::StmtCast::Expr(traits::MaybeOwned::Borrowed(e)),
      Stmt::Trace(ref e) => traits::StmtCast::Trace(traits::MaybeOwned::Borrowed(e)),
      Stmt::SyntaxError => traits::StmtCast::SyntaxError,
    }
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash, Deserialize)]
pub struct ExprStmt {
  pub loc: (),
  pub expr: Box<Expr>,
}

impl traits::ExprStmt<OwnedSyntax> for ExprStmt {
  fn expr(&self) -> &Expr {
    &self.expr
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash, Deserialize)]
pub struct TraceStmt {
  pub loc: (),
  pub value: Box<Expr>,
}

impl traits::TraceStmt<OwnedSyntax> for TraceStmt {
  fn value(&self) -> &Expr {
    &self.value
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash, Deserialize)]
pub struct BreakStmt {
  pub loc: (),
}

impl traits::BreakStmt<OwnedSyntax> for BreakStmt {}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash, Deserialize)]
pub enum Expr {
  Seq(SeqExpr),
  // Assign(AssignExpr),
  StrLit(StrLit),
  SyntaxError,
}

impl traits::Expr<OwnedSyntax> for Expr {
  fn cast(&self) -> traits::ExprCast<OwnedSyntax> {
    match self {
      Expr::Seq(ref e) => traits::ExprCast::Seq(e),
      Expr::StrLit(ref e) => traits::ExprCast::StrLit(e),
      Expr::SyntaxError => traits::ExprCast::Error,
    }
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash, Deserialize)]
pub struct SeqExpr {
  pub loc: (),
  pub exprs: Vec<Expr>,
}

impl traits::SeqExpr<OwnedSyntax> for SeqExpr {
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

impl traits::AssignExpr<OwnedSyntax> for AssignExpr {
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

impl traits::BinExpr<OwnedSyntax> for BinExpr {
  fn left(&self) -> &Expr {
    &self.left
  }

  fn right(&self) -> &Expr {
    &self.right
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash, Deserialize)]
pub struct StrLit {
  pub loc: (),
  pub value: String,
}

impl traits::StrLit for StrLit {
  fn value(&self) -> Cow<str> {
    Cow::Borrowed(&self.value)
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash, Deserialize)]
pub enum Pat {
  MemberPat(MemberPat),
  IdentPat(IdentPat),
  SyntaxError,
}

impl traits::Pat<OwnedSyntax> for Pat {
  fn cast(&self) -> traits::PatCast<OwnedSyntax> {
    match self {
      Pat::MemberPat(ref e) => traits::PatCast::Member(e),
      Pat::IdentPat(ref e) => traits::PatCast::Ident(e),
      Pat::SyntaxError => traits::PatCast::SyntaxError,
    }
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash, Deserialize)]
pub struct MemberPat {
  pub loc: (),
  pub base: Box<Expr>,
  pub key: Box<Expr>,
}

impl traits::MemberPat<OwnedSyntax> for MemberPat {
  fn base(&self) -> &Expr {
    &self.base
  }

  fn key(&self) -> &Expr {
    &self.key
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash, Deserialize)]
pub struct IdentPat {
  pub loc: (),
  pub name: String,
}

impl traits::IdentPat for IdentPat {
  fn name(&self) -> &str {
    &self.name
  }
}

#[cfg(test)]
mod seq_expr_tests {
  use super::{Expr, SeqExpr, StrLit};
  use crate::types::ast::traits::SeqExpr as _;

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
    let expected_json = String::from("{\"body\":[{\"type\":\"BreakStmt\"},{\"type\":\"BreakStmt\"}]}");

    assert_eq!(actual_json, expected_json);
  }
}
