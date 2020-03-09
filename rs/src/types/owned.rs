use crate::types::syntax;

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub enum OwnedSyntax {}

impl syntax::Syntax for OwnedSyntax {
  type Expr = Expr;
  type SeqExpr = SeqExpr;
  type BinExpr = BinExpr;
  type StrLit = StrLit;
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
  use super::{Expr, SeqExpr, StrLit};
  use crate::types::syntax::SeqExpr as _;

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
