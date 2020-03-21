use crate::types::ast::traits;
use std::borrow::Cow;
use std::marker::PhantomData;

/// A poor man's never type represented as an empty type so it works on stable Rust.
enum Empty {}

pub struct BorrowedSyntax<'a>(PhantomData<&'a Empty>);

impl<'a> traits::Syntax for BorrowedSyntax<'a> {
  type Script = Script<'a>;

  type Stmt = Stmt<'a>;
  type BreakStmt = BreakStmt<'a>;
  type ErrorStmt = ErrorStmt<'a>;
  type ExprStmt = ExprStmt<'a>;
  type TraceStmt = TraceStmt<'a>;
  type VarDecl = VarDecl<'a>;

  type Expr = Expr<'a>;
  type AssignExpr = AssignExpr<'a>;
  type BinExpr = BinExpr<'a>;
  type CallExpr = CallExpr<'a>;
  type ErrorExpr = ErrorExpr<'a>;
  type IdentExpr = IdentExpr<'a>;
  type LogicalExpr = LogicalExpr<'a>;
  type SeqExpr = SeqExpr<'a>;
  type StrLit = StrLit<'a>;

  #[cfg(feature = "gat")]
  type ExprRef<'r> = &'r Expr<'r>;

  type Pat = Pat<'a>;
  type MemberPat = MemberPat<'a>;
  type IdentPat = IdentPat<'a>;
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct Script<'a> {
  pub loc: (),
  pub stmts: &'a [Stmt<'a>],
}

impl<'s> traits::Script<BorrowedSyntax<'s>> for Script<'s> {
  #[cfg(not(feature = "gat"))]
  fn stmts<'a>(&'a self) -> Box<dyn Iterator<Item = traits::MaybeOwned<'a, Stmt<'s>>> + 'a> {
    Box::new(self.stmts.iter().map(|stmt| traits::MaybeOwned::Borrowed(stmt)))
  }

  #[allow(clippy::type_complexity)]
  #[cfg(feature = "gat")]
  type Stmts<'a> =
    core::iter::Map<core::slice::Iter<'a, Stmt<'a>>, for<'r> fn(&'r Stmt<'a>) -> traits::MaybeOwned<'r, Stmt<'a>>>;

  #[cfg(feature = "gat")]
  fn stmts(&self) -> Self::Stmts<'_> {
    self.stmts.iter().map(|s| traits::MaybeOwned::Borrowed(s))
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub enum Stmt<'a> {
  Trace(TraceStmt<'a>),
  Error(ErrorStmt<'a>),
  Expr(ExprStmt<'a>),
}

impl<'a> traits::Stmt<BorrowedSyntax<'a>> for Stmt<'a> {
  fn cast<'b>(&'b self) -> traits::StmtCast<'b, BorrowedSyntax<'a>> {
    match self {
      Stmt::Trace(ref s) => traits::StmtCast::Trace(traits::MaybeOwned::Borrowed(s)),
      Stmt::Error(ref s) => traits::StmtCast::Error(traits::MaybeOwned::Borrowed(s)),
      Stmt::Expr(ref s) => traits::StmtCast::Expr(traits::MaybeOwned::Borrowed(s)),
    }
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct BreakStmt<'a> {
  pub loc: (),
  pub phantom: PhantomData<&'a ()>,
}

impl<'a> traits::BreakStmt<BorrowedSyntax<'a>> for BreakStmt<'a> {}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct ErrorStmt<'a> {
  pub loc: (),
  pub phantom: PhantomData<&'a ()>,
}

impl<'a> traits::ErrorStmt<BorrowedSyntax<'a>> for ErrorStmt<'a> {}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct ExprStmt<'a> {
  pub loc: (),
  pub expr: &'a Expr<'a>,
}

impl<'a> traits::ExprStmt<BorrowedSyntax<'a>> for ExprStmt<'a> {
  fn expr(&self) -> traits::MaybeOwned<Expr<'a>> {
    traits::MaybeOwned::Borrowed(self.expr)
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct TraceStmt<'a> {
  pub loc: (),
  pub value: &'a Expr<'a>,
}

impl<'a> traits::TraceStmt<BorrowedSyntax<'a>> for TraceStmt<'a> {
  fn value(&self) -> &Expr<'a> {
    self.value
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct VarDecl<'a> {
  pub loc: (),
  pub phantom: PhantomData<&'a ()>,
}

impl<'a> traits::VarDecl<BorrowedSyntax<'a>> for VarDecl<'a> {}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub enum Expr<'a> {
  Assign(AssignExpr<'a>),
  Bin(BinExpr<'a>),
  Error(ErrorExpr<'a>),
  Seq(SeqExpr<'a>),
  StrLit(StrLit<'a>),
}

impl<'a> traits::Expr for Expr<'a> {
  type Ast = BorrowedSyntax<'a>;

  fn cast<'b>(&'b self) -> traits::ExprCast<'b, BorrowedSyntax<'a>> {
    match self {
      Expr::Error(ref e) => traits::ExprCast::Error(traits::MaybeOwned::Borrowed(e)),
      Expr::StrLit(ref e) => traits::ExprCast::StrLit(traits::MaybeOwned::Borrowed(e)),
      _ => unimplemented!(),
    }
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct AssignExpr<'a> {
  pub loc: (),
  pub target: &'a Pat<'a>,
  pub value: &'a Expr<'a>,
}

impl<'a> AssignExpr<'a> {
  fn _value(&self) -> &Expr<'a> {
    self.value
  }
}

impl<'a> traits::AssignExpr for AssignExpr<'a> {
  type Ast = BorrowedSyntax<'a>;

  fn target(&self) -> &Pat<'a> {
    self.target
  }

  maybe_gat_accessor!(value, _value, ref Expr<'_>, ref Expr<'a>);
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct BinExpr<'a> {
  pub loc: (),
  pub op: traits::BinOp,
  pub left: &'a Expr<'a>,
  pub right: &'a Expr<'a>,
}

impl<'a> BinExpr<'a> {
  fn _left(&self) -> &Expr<'a> {
    self.left
  }
  fn _right(&self) -> &Expr<'a> {
    self.right
  }
}

impl<'a> traits::BinExpr for BinExpr<'a> {
  type Ast = BorrowedSyntax<'a>;

  fn op(&self) -> traits::BinOp {
    self.op
  }

  maybe_gat_accessor!(left, _left, ref Expr<'_>, ref Expr<'a>);
  maybe_gat_accessor!(right, _right, ref Expr<'_>, ref Expr<'a>);
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct CallExpr<'a> {
  pub loc: (),
  pub callee: &'a Expr<'a>,
  pub args: &'a [Expr<'a>],
}

impl<'a> CallExpr<'a> {
  fn _callee(&self) -> &Expr<'a> {
    self.callee
  }
}

impl<'a> traits::CallExpr for CallExpr<'a> {
  type Ast = BorrowedSyntax<'a>;

  #[allow(clippy::type_complexity)]
  #[cfg(feature = "gat")]
  type ExprIter<'b> =
    core::iter::Map<core::slice::Iter<'b, Expr<'b>>, for<'r> fn(&'r Expr<'b>) -> traits::MaybeOwned<'r, Expr<'b>>>;

  maybe_gat_accessor!(callee, _callee, ref Expr<'_>, ref Expr<'a>);

  #[cfg(feature = "gat")]
  fn args(&self) -> Self::ExprIter<'_> {
    self.args.iter().map(|e| traits::MaybeOwned::Borrowed(e))
  }

  #[cfg(not(feature = "gat"))]
  fn args<'r>(&'r self) -> Box<dyn Iterator<Item = traits::MaybeOwned<'r, Expr<'a>>> + 'r> {
    Box::new(self.args.iter().map(|e| traits::MaybeOwned::Borrowed(e)))
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct ErrorExpr<'a> {
  pub loc: (),
  pub phantom: PhantomData<&'a ()>,
}

impl<'a> traits::ErrorExpr for ErrorExpr<'a> {
  type Ast = BorrowedSyntax<'a>;
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct IdentExpr<'a> {
  pub loc: (),
  pub name: &'a str,
}

impl traits::IdentExpr for IdentExpr<'_> {
  fn name(&self) -> Cow<str> {
    Cow::Borrowed(self.name)
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct LogicalExpr<'a> {
  pub loc: (),
  pub op: traits::LogicalOp,
  pub left: &'a Expr<'a>,
  pub right: &'a Expr<'a>,
}

impl<'a> LogicalExpr<'a> {
  fn _left(&self) -> &Expr<'a> {
    self.left
  }
  fn _right(&self) -> &Expr<'a> {
    self.right
  }
}

impl<'a> traits::LogicalExpr for LogicalExpr<'a> {
  type Ast = BorrowedSyntax<'a>;

  fn op(&self) -> traits::LogicalOp {
    self.op
  }

  maybe_gat_accessor!(left, _left, ref Expr<'_>, ref Expr<'a>);
  maybe_gat_accessor!(right, _right, ref Expr<'_>, ref Expr<'a>);
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct SeqExpr<'a> {
  pub loc: (),
  pub exprs: &'a [Expr<'a>],
}

impl<'s> traits::SeqExpr for SeqExpr<'s> {
  type Ast = BorrowedSyntax<'s>;

  #[allow(clippy::type_complexity)]
  #[cfg(feature = "gat")]
  type Exprs<'a> =
    core::iter::Map<core::slice::Iter<'a, Expr<'a>>, for<'r> fn(&'r Expr<'a>) -> traits::MaybeOwned<'r, Expr<'a>>>;

  #[cfg(feature = "gat")]
  fn exprs(&self) -> Self::Exprs<'_> {
    self.exprs.iter().map(|e| traits::MaybeOwned::Borrowed(e))
  }

  #[cfg(not(feature = "gat"))]
  fn exprs<'a>(&'a self) -> Box<dyn Iterator<Item = traits::MaybeOwned<'a, Expr<'s>>> + 'a> {
    Box::new(self.exprs.iter().map(|e| traits::MaybeOwned::Borrowed(e)))
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct StrLit<'a> {
  pub loc: (),
  pub value: &'a str,
}

impl traits::StrLit for StrLit<'_> {
  fn value(&self) -> Cow<str> {
    Cow::Borrowed(self.value)
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub enum Pat<'a> {
  Member(MemberPat<'a>),
  Ident(IdentPat<'a>),
  SyntaxError,
}

impl<'a> traits::Pat for Pat<'a> {
  type Ast = BorrowedSyntax<'a>;

  fn cast<'b>(&'b self) -> traits::PatCast<'b, BorrowedSyntax<'a>> {
    match self {
      Pat::Member(ref e) => traits::PatCast::Member(e),
      Pat::Ident(ref e) => traits::PatCast::Ident(e),
      Pat::SyntaxError => traits::PatCast::SyntaxError,
    }
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct MemberPat<'a> {
  pub loc: (),
  pub base: &'a Expr<'a>,
  pub key: &'a Expr<'a>,
}

impl<'a> traits::MemberPat for MemberPat<'a> {
  type Ast = BorrowedSyntax<'a>;

  fn base(&self) -> &Expr<'a> {
    self.base
  }

  fn key(&self) -> &Expr<'a> {
    self.key
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct IdentPat<'a> {
  pub loc: (),
  pub name: &'a str,
}

impl traits::IdentPat for IdentPat<'_> {
  fn name(&self) -> &str {
    self.name
  }
}

#[cfg(test)]
mod seq_expr_tests {
  use super::{Expr, SeqExpr, StrLit};
  use crate::types::ast::traits::SeqExpr as _;

  #[test]
  fn test_eq_empty() {
    let left_foo = Expr::StrLit(StrLit { loc: (), value: "foo" });
    let left_bar = Expr::StrLit(StrLit { loc: (), value: "bar" });
    let left_seq = vec![left_foo, left_bar];
    let left = SeqExpr {
      loc: (),
      exprs: &left_seq,
    };

    let right_foo = Expr::StrLit(StrLit { loc: (), value: "foo" });
    let right_bar = Expr::StrLit(StrLit { loc: (), value: "bar" });
    let right_seq = vec![right_foo, right_bar];
    let right = SeqExpr {
      loc: (),
      exprs: &right_seq,
    };

    assert_eq!(left.exprs().size_hint(), (2, Some(2)));
    assert_eq!(left, right);
  }
}
