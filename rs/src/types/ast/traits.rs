use serde::{Deserialize, Serialize};
use std::borrow::Cow;

/// A poor man's never type represented as an empty type so it works on stable Rust.
pub enum Empty {}

/// Describes all the elements of a syntax.
pub trait Syntax: Sized {
  type Script: Script<Self>;

  type Stmt: Stmt<Self>;
  type BreakStmt: BreakStmt<Self>;
  /// Represents invalid statement
  type ErrorStmt: ErrorStmt<Self>;
  type ExprStmt: ExprStmt<Self>;
  type TraceStmt: TraceStmt<Self>;
  type VarDecl: VarDecl<Self>;

  type Expr: Expr<Ast = Self>;
  type AssignExpr: AssignExpr<Ast = Self>;
  type BinExpr: BinExpr<Ast = Self>;
  type CallExpr: CallExpr<Ast = Self>;
  type ErrorExpr: ErrorExpr<Ast = Self>;
  type IdentExpr: IdentExpr;
  type LogicalExpr: LogicalExpr<Ast = Self>;
  type SeqExpr: SeqExpr<Ast = Self>;
  type StrLit: StrLit;

  type Pat: Pat<Ast = Self>;
  type MemberPat: MemberPat<Ast = Self>;
  type IdentPat: IdentPat;
}

/// A `Cow` variant that does not require `ToOwned`.
///
/// It is intended as a workaround until Generic Associated Types are improved.
/// Once rust-lang/rust#30472 is fixed, this type could be removed.
#[derive(Debug)]
pub enum MaybeOwned<'a, T>
where
  T: 'a,
{
  Borrowed(&'a T),
  Owned(T),
}

impl<T> std::ops::Deref for MaybeOwned<'_, T> {
  type Target = T;
  fn deref(&self) -> &T {
    match self {
      MaybeOwned::Borrowed(ref borrowed) => *borrowed,
      MaybeOwned::Owned(ref owned) => owned,
    }
  }
}

/// Script root node
pub trait Script<S: Syntax> {
  #[cfg(not(feature = "gat"))]
  fn stmts<'a>(&'a self) -> Box<dyn Iterator<Item = MaybeOwned<'a, S::Stmt>> + 'a>;

  // TODO: Use the following code once rust-lang/rust#30472 is fixed. (It a
  // #[cfg(feature = "gat")]
  // type StmtRef<'a>;
  //
  // #[cfg(feature = "gat")]
  // type Stmts<'a>: Iterator<Item = &Self::StmtRef<'a>>;

  #[cfg(feature = "gat")]
  type Stmts<'a>: Iterator<Item = MaybeOwned<'a, S::Stmt>>;

  #[cfg(feature = "gat")]
  fn stmts(&self) -> Self::Stmts<'_>;
}

/// Trait representing any ActionScript statement
pub trait Stmt<S: Syntax> {
  /// Downcast the statement to its concrete type.
  fn cast(&self) -> StmtCast<S>;
}

/// Represents the result of downcasting an expression.
pub enum StmtCast<'a, S: Syntax> {
  Break(MaybeOwned<'a, S::BreakStmt>),
  Error(MaybeOwned<'a, S::ErrorStmt>),
  Trace(MaybeOwned<'a, S::TraceStmt>),
  Expr(MaybeOwned<'a, S::ExprStmt>),
  VarDecl(MaybeOwned<'a, S::VarDecl>),
}

pub trait BreakStmt<S: Syntax> {}

pub trait ErrorStmt<S: Syntax> {}

pub trait ExprStmt<S: Syntax> {
  fn expr(&self) -> MaybeOwned<S::Expr>;
}

pub trait TraceStmt<S: Syntax> {
  fn value(&self) -> &S::Expr;
}

pub trait VarDecl<S: Syntax> {}

/// Trait representing any ActionScript expression
pub trait Expr {
  type Ast: Syntax;

  /// Downcast the expression to its concrete type.
  fn cast(&self) -> ExprCast<Self::Ast>;
}

/// Represents the result of downcasting an expression.
pub enum ExprCast<'a, S: Syntax> {
  Assign(MaybeOwned<'a, S::AssignExpr>),
  Bin(MaybeOwned<'a, S::BinExpr>),
  Call(MaybeOwned<'a, S::CallExpr>),
  Ident(MaybeOwned<'a, S::IdentExpr>),
  Error(MaybeOwned<'a, S::ErrorExpr>),
  Logical(MaybeOwned<'a, S::LogicalExpr>),
  Seq(MaybeOwned<'a, S::SeqExpr>),
  StrLit(MaybeOwned<'a, S::StrLit>),
}

pub trait AssignExpr {
  type Ast: Syntax;

  fn target(&self) -> &<Self::Ast as Syntax>::Pat;
  fn value(&self) -> MaybeOwned<<Self::Ast as Syntax>::Expr>;
}

pub trait BinExpr {
  type Ast: Syntax;

  fn left(&self) -> MaybeOwned<<Self::Ast as Syntax>::Expr>;
  fn right(&self) -> MaybeOwned<<Self::Ast as Syntax>::Expr>;
}

/// Represents all the binary operators.
#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum BinOp {
  /// Binary operator `+`
  Add,
  /// Binary operator `&`
  BitAnd,
  /// Binary operator `|`
  BitOr,
  /// Binary operator `^`
  BitXor,
  /// Binary operator `/`
  Divide,
  /// Binary operator `==`
  Equals,
  /// Binary operator `>`
  Greater,
  /// Binary operator `instanceof`
  InstanceOf,
  /// Binary operator `add`
  LegacyAdd,
  /// Binary operator `<<`
  LeftShift,
  /// Binary operator `<`
  Less,
  /// Binary operator `*`
  Multiply,
  /// Binary operator `!=`
  NotEquals,
  /// Binary operator `!==`
  NotStrictEquals,
  /// Binary operator `%`
  Remainder,
  /// Binary operator `>>`
  SignedRightShift,
  /// Binary operator `-`
  Subtract,
  /// Binary operator `===`
  StrictEquals,
  /// Binary operator `>>>`
  UnsignedRightShift,
}

pub trait CallExpr {
  type Ast: Syntax;

  fn callee(&self) -> MaybeOwned<<Self::Ast as Syntax>::Expr>;
}

pub trait ErrorExpr {
  type Ast: Syntax;
}

pub trait IdentExpr {
  fn name(&self) -> Cow<str>;
}

pub trait LogicalExpr {
  type Ast: Syntax;
  fn op(&self) -> LogicalOp;
  fn left(&self) -> MaybeOwned<<Self::Ast as Syntax>::Expr>;
  fn right(&self) -> MaybeOwned<<Self::Ast as Syntax>::Expr>;
}

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub enum LogicalOp {
  #[serde(rename = "&&")]
  /// Binary operator `&&`
  And,
  #[serde(rename = "||")]
  /// Logical operator `||`
  Or,
}

/// Sequence expression
///
/// Corresponds to two or more expressions separated by commas.
pub trait SeqExpr {
  type Ast: Syntax;

  #[cfg(not(feature = "gat"))]
  fn exprs<'a>(&'a self) -> Box<dyn Iterator<Item = MaybeOwned<'a, <Self::Ast as Syntax>::Expr>> + 'a>;

  #[cfg(feature = "gat")]
  type Exprs<'a>: Iterator<Item = MaybeOwned<'a, <Self::Ast as Syntax>::Expr>>;

  #[cfg(feature = "gat")]
  fn exprs(&self) -> Self::Exprs<'_>;
}

pub trait StrLit {
  fn value(&self) -> Cow<str>;
}

/// Trait representing any ActionScript pattern (assignment left-hand side)
pub trait Pat {
  type Ast: Syntax;

  /// Downcast the pattern to its concrete type.
  fn cast(&self) -> PatCast<Self::Ast>;
}

/// Represents the result of downcasting a pattern.
pub enum PatCast<'a, S: Syntax> {
  Member(&'a S::MemberPat),
  Ident(&'a S::IdentPat),
  SyntaxError,
}

pub trait MemberPat {
  type Ast: Syntax;

  fn base(&self) -> &<Self::Ast as Syntax>::Expr;
  fn key(&self) -> &<Self::Ast as Syntax>::Expr;
}

pub trait IdentPat {
  fn name(&self) -> &str;
}
