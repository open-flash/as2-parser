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

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct SeqExpr<'a> {
  pub _loc: (),
  pub _exprs: &'a [Expr<'a>],
}

impl<'s> syntax::SeqExpr<BorrowedSyntax<'s>> for SeqExpr<'s> {
  #[cfg(not(feature = "gat"))]
  fn exprs<'a>(&'a self) -> Box<dyn ExactSizeIterator<Item = &'a Expr<'s>> + 'a> {
    Box::new(self._exprs.iter())
  }

  #[cfg(feature = "gat")]
  type Iter<'a> = core::slice::Iter<'a, Expr<'a>>;

  #[cfg(feature = "gat")]
  fn exprs(&self) -> Self::Iter<'_> {
    self._exprs.iter()
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
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

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct StrLit<'a> {
  pub _loc: (),
  pub _value: &'a str,
}

impl syntax::StrLit for StrLit<'_> {
  fn value(&self) -> &str {
    self._value
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
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

#[cfg(test)]
mod seq_expr_tests {
  use super::{Expr, SeqExpr, StrLit};
  use crate::types::syntax::SeqExpr as _;

  #[test]
  fn test_eq_empty() {
    let left_foo = Expr::StrLit(StrLit {
      _loc: (),
      _value: "foo",
    });
    let left_bar = Expr::StrLit(StrLit {
      _loc: (),
      _value: "bar",
    });
    let left_seq = vec![left_foo, left_bar];
    let left = SeqExpr {
      _loc: (),
      _exprs: &left_seq,
    };

    let right_foo = Expr::StrLit(StrLit {
      _loc: (),
      _value: "foo",
    });
    let right_bar = Expr::StrLit(StrLit {
      _loc: (),
      _value: "bar",
    });
    let right_seq = vec![right_foo, right_bar];
    let right = SeqExpr {
      _loc: (),
      _exprs: &right_seq,
    };

    assert_eq!(left.exprs().len(), 2);
    assert_eq!(left, right);
  }
}
