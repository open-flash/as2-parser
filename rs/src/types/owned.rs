use crate::types::syntax;

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub enum OwnedSyntax {}

impl syntax::Syntax for OwnedSyntax {
  type Expr = Expr;
  type SeqExpr = SeqExpr<Self>;
  type BinExpr = BinExpr<Self>;
  type StrLit = StrLit;
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct SeqExpr<S: syntax::Syntax> {
  pub _loc: (),
  pub _exprs: Vec<S::Expr>,
}

impl syntax::SeqExpr<OwnedSyntax> for SeqExpr<OwnedSyntax> {
  #[cfg(not(feature = "gat"))]
  fn exprs<'a>(&'a self) -> Box<dyn ExactSizeIterator<Item = &'a Expr> + 'a> {
    Box::new(self._exprs.iter())
  }

  #[cfg(feature = "gat")]
  type Iter<'a> = core::slice::Iter<'a, Expr>;

  #[cfg(feature = "gat")]
  fn exprs(&self) -> Self::Iter<'_> {
    self._exprs.iter()
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct BinExpr<S: syntax::Syntax> {
  pub _loc: (),
  pub _left: Box<S::Expr>,
  pub _right: Box<S::Expr>,
}

impl<S: syntax::Syntax> syntax::BinExpr<S> for BinExpr<S> {
  fn left(&self) -> &S::Expr {
    &self._left
  }

  fn right(&self) -> &S::Expr {
    &self._right
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct StrLit {
  pub _loc: (),
  pub _value: String,
}

impl syntax::StrLit for StrLit {
  fn value(&self) -> &str {
    &self._value
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub enum Expr {
  StrLit(StrLit),
  Error,
}

impl syntax::Expr<OwnedSyntax> for Expr {
  fn cast(&self) -> syntax::ExprCast<OwnedSyntax> {
    match self {
      Expr::StrLit(ref e) => syntax::ExprCast::StrLit(e),
      Expr::Error => syntax::ExprCast::Error,
    }
  }
}

#[cfg(test)]
mod seq_expr_tests {
  use super::{Expr, OwnedSyntax, SeqExpr, StrLit};
  use crate::types::syntax::SeqExpr as _;

  #[test]
  fn test_eq_empty() {
    let left: SeqExpr<OwnedSyntax> = SeqExpr {
      _loc: (),
      _exprs: vec![
        Expr::StrLit(StrLit {
          _loc: (),
          _value: String::from("foo"),
        }),
        Expr::StrLit(StrLit {
          _loc: (),
          _value: String::from("bar"),
        }),
      ],
    };

    let right: SeqExpr<OwnedSyntax> = SeqExpr {
      _loc: (),
      _exprs: vec![
        Expr::StrLit(StrLit {
          _loc: (),
          _value: String::from("foo"),
        }),
        Expr::StrLit(StrLit {
          _loc: (),
          _value: String::from("bar"),
        }),
      ],
    };

    assert_eq!(left.exprs().len(), 2);
    assert_eq!(left, right);
  }
}
