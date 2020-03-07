use crate::types::syntax::{StrLit, Expr, ExprCast, Empty, Syntax};
use std::marker::PhantomData;

pub struct BorrowedSyntax<'a> {
  _phantom: PhantomData<&'a Empty>,
}

impl<'a> Syntax for BorrowedSyntax<'a> {
  type StrLit = BorrowedStrLit<'a>;
  type Expr = BorrowedExpr<'a>;
}

#[derive(Debug)]
pub struct BorrowedStrLit<'a> {
  pub _loc: (),
  pub _value: &'a str,
}

impl StrLit for BorrowedStrLit<'_> {
  fn value(&self) -> &str {
    self._value
  }
}

#[derive(Debug)]
pub enum BorrowedExpr<'a> {
  StrLit(BorrowedStrLit<'a>),
  Error,
}

impl<'a> Expr<BorrowedSyntax<'a>> for BorrowedExpr<'a> {
  fn cast<'b>(&'b self) -> ExprCast<'b, BorrowedSyntax<'a>> {
    match self {
      BorrowedExpr::StrLit(ref e) => ExprCast::StrLit(e),
      BorrowedExpr::Error => ExprCast::Error,
    }
  }
}
