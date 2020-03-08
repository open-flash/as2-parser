use crate::types::syntax;
use std::marker::PhantomData;

pub struct BorrowedSyntax<'a> {
  _phantom: PhantomData<&'a syntax::Empty>,
}

impl<'a> syntax::Syntax for BorrowedSyntax<'a> {
  // type ExprRef = &'a Self::Expr;

  type StrLit = StrLit<'a>;
  type Expr = Expr<'a>;
  type SeqExpr = SeqExpr<'a>;
  type BinExpr = BinExpr<'a>;
}

#[derive(Debug)]
pub struct SeqExpr<'a> {
  pub _loc: (),
  pub _exprs: &'a[Expr<'a>],
}

impl<'s> syntax::SeqExpr<BorrowedSyntax<'s>> for SeqExpr<'s> {
  type Iter<'a> = core::slice::Iter<'a, Expr<'a>>;

  fn exprs<'a>(&'a self) -> Self::Iter<'a> {
    self._exprs.iter()
  }
}

#[derive(Debug)]
pub struct BinExpr<'a> {
  pub _loc: (),
  pub _left: &'a Expr<'a>,
  pub _right: &'a Expr<'a>,
}

impl<'a> syntax::BinExpr<BorrowedSyntax<'a>> for BinExpr<'a> {
  fn left(&self) -> &Expr<'a> {
    self._left
  }

  fn right(&self) -> &Expr<'a> {
    self._right
  }
}

#[derive(Debug)]
pub struct StrLit<'a> {
  pub _loc: (),
  pub _value: &'a str,
}

impl syntax::StrLit for StrLit<'_> {
  fn value(&self) -> &str {
    self._value
  }
}

#[derive(Debug)]
pub enum Expr<'a> {
  StrLit(StrLit<'a>),
  Error,
}

impl<'a> syntax::Expr<BorrowedSyntax<'a>> for Expr<'a> {
  fn cast<'b>(&'b self) -> syntax::ExprCast<'b, BorrowedSyntax<'a>> {
    match self {
      Expr::StrLit(ref e) => syntax::ExprCast::StrLit(e),
      Expr::Error => syntax::ExprCast::Error,
    }
  }
}
