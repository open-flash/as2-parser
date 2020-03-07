use crate::types::syntax::{Expr, ExprCast, StrLit, Syntax, BinExpr};

pub enum OwnedSyntax {}

impl Syntax for OwnedSyntax {
  type StrLit = OwnedStrLit;
  type Expr = OwnedExpr;
}

#[derive(Debug)]
pub struct OwnedStrLit {
  pub _loc: (),
  pub _value: String,
}

impl StrLit for OwnedStrLit {
  fn value(&self) -> &str {
    &self._value
  }
}

#[derive(Debug)]
pub struct OwnedBinExpr<S: Syntax> {
  pub _loc: (),
  pub _left: Box<S::Expr>,
  pub _right: Box<S::Expr>,
}

impl<S: Syntax> BinExpr<S> for OwnedBinExpr<S> {
  fn left(&self) -> &S::Expr {
    &self._left
  }

  fn right(&self) -> &S::Expr {
    &self._right
  }
}

#[derive(Debug)]
pub enum OwnedExpr {
  StrLit(OwnedStrLit),
  Error,
}

impl Expr<OwnedSyntax> for OwnedExpr {
  fn cast<'a>(&'a self) -> ExprCast<'a, OwnedSyntax> {
    match self {
      OwnedExpr::StrLit(ref e) => ExprCast::StrLit(e),
      OwnedExpr::Error => ExprCast::Error,
    }
  }
}
